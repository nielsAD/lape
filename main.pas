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
  LCLIntf, typinfo, lpffi, ffi;

{$R *.lfm}

{ TForm1 }

procedure IntTest(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Params^[0])^ := PInt32(Params^[0])^ + 1;
end;

procedure MyWrite(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    m.Text := m.Text + {$IF DEFINED(Lape_Unicode)}UTF8Encode(PlpString(Params^[1])^){$ELSE}PlpString(Params^[1])^{$IFEND};
  Write(PlpString(Params^[1])^);
end;

procedure MyWriteLn(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    Form1.m.Append('');
  WriteLn();
end;

procedure MyStupidProc(Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  x: TIntegerArray;
begin
  SetLength(x, 5);
  TIntegerArray(Result^) := x;
end;

function StupidProc(abc: TIntegerArray): TIntegerArray; cdecl;
begin
  SetLength(Result, 5);
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
  c: TImportClosure;
begin
  Parser := nil;
  Compiler := nil;
  with Form1 do
    try
      Parser := TLapeTokenizerString.Create({$IF DEFINED(Lape_Unicode)}UTF8Decode(e.Lines.Text){$ELSE}e.Lines.Text{$IFEND});
      Compiler := TLapeCompiler.Create(Parser);

      InitializePascalScriptBasics(Compiler, [psiTypeAlias]);
      ExposeGlobals(Compiler);

      Compiler.addGlobalMethod('procedure _write(s: string); override;', @MyWrite, Form1);
      Compiler.addGlobalMethod('procedure _writeln; override;', @MyWriteLn, Form1);
      Compiler.addGlobalFunc('function MyStupidProc: array of integer', @MyStupidProc);

      c := LapeImportWrapper(@StupidProc, Compiler, 'function(abc: array of integer): array of integer', FFI_SYSV);
      Compiler.addGlobalFunc('function StupidProc(abc: array of integer): array of integer', c.Func);

      try
        t := getTickCount;
        if Compiler.Compile() then
          m.Append('Compiling Time: ' + IntToStr(getTickCount - t) + 'ms.')
        else
          m.Append('Error!');
      except
        on E: Exception do
        begin
          m.Append('Compilation error: "' + E.Message + '"');
          Exit;
        end;
      end;

      try
        if Disassemble then
          DisassembleCode(Compiler.Emitter.Code, CombineDeclArray(Compiler.ManagedDeclarations.getByClass(TLapeGlobalVar, bTrue), Compiler.GlobalDeclarations.getByClass(TLapeGlobalVar, bTrue)));

        if Run then
        begin
          t := getTickCount;
          m.Append(LineEnding);
          RunCode(Compiler.Emitter.Code);
          m.Append('Running Time: ' + IntToStr(getTickCount - t) + 'ms.');
        end;
      except
        on E: Exception do
          m.Append(E.Message);
      end;
    finally
      if (Compiler <> nil) then
        Compiler.Free()
      else if (Parser <> nil) then
        Parser.Free();
      c.Free();
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

{$IF DEFINED(WINDOWS) AND DECLARED(LoadFFI)}
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

