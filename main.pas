unit Main;

{$I lape.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SynEdit, SynHighlighterPas,
  lptypes, lpvartypes;

type
  TForm1 = class(TForm)
    btnRun: TButton;
    btnDisassemble: TButton;
    e: TSynEdit;
    m: TMemo;
    pnlTop: TPanel;
    Splitter1: TSplitter;
    PasSyn: TSynFreePascalSyn;
    procedure btnDisassembleClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    procedure WriteHint(Sender: TLapeCompilerBase; Msg: lpString);
  end; 

var
  Form1: TForm1;

implementation

uses
  lpparser, lpcompiler, lputils, lpeval, lpinterpreter, lpdisassembler, lpmessages, lpffi, ffi;

{$R *.lfm}

procedure MyWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    m.Text := m.Text + {$IF DEFINED(Lape_Unicode)}UTF8Encode(PlpString(Params^[1])^){$ELSE}PlpString(Params^[1])^{$IFEND};
  Write(PlpString(Params^[1])^);
end;

procedure MyWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    m.Text := m.Text + LineEnding;
  WriteLn();
end;

procedure Compile(Run, Disassemble: Boolean);
var
  t: UInt64;
  Parser: TLapeTokenizerBase;
  Compiler: TLapeCompiler;
begin
  Parser := nil;
  Compiler := nil;
  with Form1 do
  try
    Parser := TLapeTokenizerString.Create({$IF DEFINED(Lape_Unicode)}UTF8Decode(e.Lines.Text){$ELSE}e.Lines.Text{$IFEND});
    Compiler := TLapeCompiler.Create(Parser);
    Compiler.OnHint := @WriteHint;

    InitializeFFI(Compiler);
    InitializePascalScriptBasics(Compiler, [psiTypeAlias]);

    Compiler.addGlobalMethod('procedure _Write(s: string); override;', @MyWrite, Form1);
    Compiler.addGlobalMethod('procedure _WriteLn; override;', @MyWriteLn, Form1);

    try
      t := GetTickCount64();
      if Compiler.Compile() then
        m.Lines.Add('Compiling Time: ' + IntToStr(GetTickCount64() - t) + 'ms.')
      else
        m.Lines.Add('Error!');
    except
      on E: Exception do
      begin
        m.Lines.Add('Compilation error: "' + E.Message + '"');
        Exit;
      end;
    end;

    try
      if Disassemble then
        DisassembleCode(Compiler.Emitter.Code, [Compiler.ManagedDeclarations.GetByClass(TLapeGlobalVar, bTrue), Compiler.GlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue)]);

      if Run then
      begin
        t := GetTickCount64();
        RunCode(Compiler.Emitter);
        m.Lines.Add('Running Time: ' + IntToStr(GetTickCount64() - t) + 'ms.');
      end;
    except
      on E: lpException do
      begin
        m.Lines.Add(E.Message);
        if (E.StackTrace <> '') then
          m.Lines.Add(E.StackTrace);
      end;
      on E: Exception do
        m.Lines.Add(E.Message);
    end;
  finally
    if (Compiler <> nil) then
      Compiler.Free()
    else if (Parser <> nil) then
      Parser.Free();
  end;
end;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  Compile(True, False);
end;

procedure TForm1.WriteHint(Sender: TLapeCompilerBase; Msg: lpString);
begin
  m.Lines.Add(Msg);
end;

procedure TForm1.btnDisassembleClick(Sender: TObject);
begin
  Compile(True, True);
end;

{$IF DEFINED(MSWINDOWS) AND DECLARED(LoadFFI)}
initialization
  if (not FFILoaded()) then
    LoadFFI(
    {$IFDEF Win32}
    'extensions\ffi\bin\win32'
    {$ELSE}
    'extensions\ffi\bin\win64'
    {$ENDIF}
    );
{$IFEND}
end.

