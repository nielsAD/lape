unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ScriptMemo: TMemo;
    ButtonRun: TButton;
    OutputMemo: TMemo;

    procedure ButtonRunClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  lptypes, lpcompiler, lpparser, lpvartypes, lputils, lpinterpreter;

procedure _Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    OutputMemo.Text := OutputMemo.Text + PlpString(Params^[1])^;
end;

procedure _WriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TForm1(Params^[0]) do
    OutputMemo.Text := OutputMemo.Text + LineEnding;
end;

procedure TForm1.ButtonRunClick(Sender: TObject);
var
  T: UInt64;
  Compiler: TLapeCompiler;
begin
  Compiler := nil;

  OutputMemo.Clear();

  try
    Compiler := TLapeCompiler.Create(TLapeTokenizerString.Create(ScriptMemo.Lines.Text));
    Compiler.addGlobalMethod('procedure _Write(s: String); override;', @_Write, Self);
    Compiler.addGlobalMethod('procedure _WriteLn; override;', @_WriteLn, Self);

    InitializePascalScriptBasics(Compiler, [psiTypeAlias]);
    ExposeGlobals(Compiler);

    try
      T := GetTickCount64();
      if Compiler.Compile() then
        OutputMemo.Lines.Add('Compiling Time: ' + IntToStr(GetTickCount64() - T) + 'ms.')
      else
        OutputMemo.Lines.Add('Compiling failed');
    except
      on E: Exception do
      begin
        OutputMemo.Lines.Add('Compilation error: "' + E.Message + '"');
        Exit;
      end;
    end;

    try
      T := GetTickCount64();
      RunCode(Compiler.Emitter.Code, Compiler.Emitter.CodeLen);
      OutputMemo.Lines.Add('Running Time: ' + IntToStr(GetTickCount64() - T) + 'ms.');
    except
      on E: Exception do
        OutputMemo.Lines.Add(E.Message);
    end;
  finally
    if (Compiler <> nil) then
      Compiler.Free();
  end;
end;

initialization
  AllocConsole();

end.
