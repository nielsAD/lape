unit Main;

{$I lape.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SynEdit, SynHighlighterPas, lptypes;

type

  { TForm1 }

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
    PasSyn: TSynPasSyn;
    procedure btnDisassembleClick(Sender: TObject);
    procedure btnMemLeaksClick(Sender: TObject);
    procedure btnEvalResClick(Sender: TObject);
    procedure btnEvalArrClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;

implementation

uses
  lpparser, lpcompiler, lputils, lpvartypes, lpeval, lpinterpreter, lpdisassembler, {_lpgenerateevalfunctions,}
  LCLIntf, Variants, typinfo;

{$R *.lfm}

{ TForm1 }

procedure IntTest(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Params^[0])^ := PInt32(Params^[0])^ + 1;
end;

procedure MyWrite(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    m.Text := m.Text + PlpString(Params^[1])^;
  Write(PlpString(Params^[1])^);
end;

procedure MyWriteLn(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    Form1.m.Text := Form1.m.Text + LineEnding;
  WriteLn();
end;

procedure MyStupidProc(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  raise Exception.Create('Wat! Exception!');
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
  t: Cardinal;
  Parser: TLapeTokenizerBase;
  Compiler: TLapeCompiler;
begin
  Parser := nil;
  Compiler := nil;
  with Form1 do
    try
      Parser := TLapeTokenizerString.Create(e.Lines.Text);
      Compiler := TLapeCompiler.Create(Parser);
      InitializePascalScriptBasics(Compiler);

      Compiler.addGlobalFunc('procedure Integer.test;', @IntTest);

      Compiler.addGlobalMethod('procedure _write(s: string); override;', @MyWrite, Form1);
      Compiler.addGlobalMethod('procedure _writeln; override;', @MyWriteLn, Form1);
      Compiler.addGlobalFunc('procedure MyStupidProc', @MyStupidProc);

      try
        t := getTickCount;
        ExposeGlobals(Compiler);
        if Compiler.Compile() then
          m.Lines.add('Compiling Time: ' + IntToStr(getTickCount - t) + 'ms.')
        else
          m.Lines.add('Error!');
      except
        on E: Exception do
        begin
          m.Lines.add('Compilation error: "' + E.Message + '"');
          Exit;
        end;
      end;

      try
        if Disassemble then
          DisassembleCode(Compiler.Emitter.Code, CombineDeclArray(Compiler.ManagedDeclarations.getByClass(TLapeGlobalVar), Compiler.GlobalDeclarations.getByClass(TLapeGlobalVar)));

        if Run then
        begin
          t := getTickCount;
          RunCode(Compiler.Emitter.Code);
          m.Lines.add('Running Time: ' + IntToStr(getTickCount - t) + 'ms.');
        end;
      except
        on E: Exception do
          m.Lines.add(E.Message);
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

procedure TForm1.btnDisassembleClick(Sender: TObject);
begin
  Compile(True, True);
end;

procedure TForm1.btnMemLeaksClick(Sender: TObject);
var
  i: Integer;
begin
  WriteLn(Ord(Low(opCode)), '..', Ord(High(opCode)));
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

procedure TForm1.btnEvalArrClick(Sender: TObject);
begin
  m.Clear;
  //LapePrintEvalArr;
end;

end.

