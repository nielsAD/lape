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
    btnMemLeaks: TButton;
    btnEvalRes: TButton;
    btnEvalArr: TButton;
    btnDisassemble: TButton;
    e: TSynEdit;
    m: TMemo;
    pnlTop: TPanel;
    Splitter1: TSplitter;
    PasSyn: TSynFreePascalSyn;
    procedure btnDisassembleClick(Sender: TObject);
    procedure btnMemLeaksClick(Sender: TObject);
    procedure btnEvalResClick(Sender: TObject);
    procedure btnEvalArrClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    procedure WriteHint(Sender: TLapeCompilerBase; Msg: lpString);
  end; 

var
  Form1: TForm1;

implementation

uses
  lpparser, lpcompiler, lputils, lpeval, lpinterpreter, lpdisassembler, {_lpgenerateevalfunctions,}
  ffi, lpffi, lpffiwrappers;

{$R *.lfm}

procedure IntTest(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Params^[0])^ := PInt32(Params^[0])^ + 1;
end;

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

procedure MyStupidProc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  x: TIntegerArray;
begin
  SetLength(x, 5);
  TIntegerArray(Result^) := x;
end;

procedure Compile(Run, Disassemble: Boolean);

  function CombineDeclArray(a, b: TLapeDeclArray): TLapeDeclArray;
  var
    i, l: Integer;
  begin
    Result := a;
    l := Length(a);
    SetLength(Result, l + Length(b));
    for i := High(b) downto 0 do
      Result[l + i] := b[i];
  end;

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
    ExposeGlobals(Compiler);

    Compiler.StartImporting();
    Compiler.addGlobalMethod('procedure _write(s: string); override;', @MyWrite, Form1);
    Compiler.addGlobalMethod('procedure _writeln; override;', @MyWriteLn, Form1);
    Compiler.addGlobalFunc('function MyStupidProc: array of integer', @MyStupidProc);

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
        DisassembleCode(Compiler.Emitter.Code, CombineDeclArray(Compiler.ManagedDeclarations.GetByClass(TLapeGlobalVar, bTrue), Compiler.GlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue)));

      if Run then
      begin
        t := GetTickCount64();
        RunCode(Compiler.Emitter);
        m.Lines.Add('Running Time: ' + IntToStr(GetTickCount64() - t) + 'ms.');
      end;
    except
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

procedure TForm1.btnMemLeaksClick(Sender: TObject);
var
  i: Integer;
begin
  //WriteLn(Ord(Low(opCode)), '..', Ord(High(opCode)));
  {$IFDEF Lape_TrackObjects}
  for i := 0 to lpgList.Count - 1 do
    WriteLn('unfreed: ', TLapeBaseClass(lpgList[i]).ClassName, ' -- [',  PtrInt(lpgList[i]), ']');
  {$ENDIF}
end;

procedure TForm1.btnEvalResClick(Sender: TObject);
begin
  e.ClearAll;
  //LapePrintEvalRes;
end;

type
  ttest = array of string;

function testHoi(a: string): ttest; cdecl;
begin
  SetLength(Result, 3);
  Result[0] := 'Hello ' + a;
end;

const
  header = 'function testHoi(a: string): array of string;';

procedure testCall;
var
  Compiler: TLapeCompiler;
  Cif: TFFICifManager;
  Arg1: string;
  Res: ttest;
begin
  Arg1 := 'hoi';
  //Res := testHoi(Arg1);
  //Dec(PPtrInt(PtrInt(Arg1) - SizeOf(Pointer))^);
  //res := 'hoi';

  Compiler := TLapeCompiler.Create(nil);
  try
    Cif := LapeHeaderToFFICif(Compiler, header);
    try
      Cif.Call(@testHoi, @res, [@arg1]);
      WriteLn('Result: ', Res[0], ' :: ', Arg1, ' :: ');
      WriteLn(Res[0], PPtrInt(PtrInt(Res) - SizeOf(Pointer)*2)^);
    finally
      Cif.Free();
    end;
  finally
    Compiler.Free();
  end;
end;

procedure TForm1.btnEvalArrClick(Sender: TObject);
begin
  //m.Clear;
  //LapePrintEvalArr;

  testCall();
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

