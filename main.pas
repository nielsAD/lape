unit Main;

{$I lape.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  StdCtrls, ExtCtrls, AsyncProcess, SynEdit, SynHighlighterPas, IniPropStorage,
  lptypes, lpvartypes;

type
  TMainForm = class(TForm)
    LazBuildProcess: TAsyncProcess;
    LapeTestProcess: TAsyncProcess;
    btnRunTests: TButton;
    btnRun: TButton;
    btnDisassemble: TButton;
    btnBench: TButton;
    pnlTop: TPanel;
    Splitter1: TSplitter;
    PasSyn: TSynFreePascalSyn;

    procedure ProcessReadData(Sender: TObject);
    procedure LazBuildProcessTerminate(Sender: TObject);
    procedure LapeTestProcessTerminate(Sender: TObject);
    procedure btnDisassembleClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnBenchClick(Sender: TObject);
    procedure btnRunTestsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SplitterPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FEditorScript: TSynEdit; // (the script)
    FEditorOutput: TSynEdit; // (the output, under the splitter)
    FStorage: TIniPropStorage;
    FBuildOutput: string; // lazbuild LapeTest.lpi
    FTestParams: string;  // --params for LapeTest.exe
    FTestOutput: string;  // LapeTest.exe

    procedure ApplyTheme;
    procedure AppendOutput(s: string);
    procedure AddLine(s: string);
    procedure BuildAndRunTests(Params: string);
    procedure WriteHint(Sender: TLapeCompilerBase; Msg: lpString);
  end;

var
  MainForm: TMainForm;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SynEditHighlighter, SynEditTypes, SynEditMarkupWordGroup,
  LCLType,
  lptokenizer, lpcompiler, lpinterpreter, lpdisassemblyform, lpmessages, lpffi, ffi;

{$R *.lfm}

const
  DefaultScript =
    'type'                                         + LineEnding +
    '  TPoint = record'                            + LineEnding +
    '    X, Y: Int32;'                             + LineEnding +
    '  end;'                                       + LineEnding +
    '  TPointArray = array of TPoint;'             + LineEnding +
    ''                                             + LineEnding +
    'procedure Test(a: ^Int32);'                   + LineEnding +
    'begin'                                        + LineEnding +
    '  WriteLn(a);'                                + LineEnding +
    'end;'                                         + LineEnding +
    ''                                             + LineEnding +
    'procedure Test(a: ^Int32); override;'         + LineEnding +
    'begin'                                        + LineEnding +
    '  Write(''Dumping: '');'                      + LineEnding +
    '  inherited();'                               + LineEnding +
    'end;'                                         + LineEnding +
    ''                                             + LineEnding +
    'var'                                          + LineEnding +
    '  tpa: TPointArray = [[1,2], [3,4]];'         + LineEnding +
    '  i: Int32;'                                  + LineEnding +
    'begin'                                        + LineEnding +
    '  WriteLn(''tpa: '', tpa + tpa[0] + tpa[1]);' + LineEnding +
    ''                                             + LineEnding +
    '  for i := 1 to 4 do'                         + LineEnding +
    '    if (i mod 2 = 1) then'                    + LineEnding +
    '      WriteLn(''hoi '', i)'                   + LineEnding +
    '    else'                                     + LineEnding +
    '      WriteLn(''hello '', i);'                + LineEnding +
    ''                                             + LineEnding +
    '  Test(@tpa[1].y);'                           + LineEnding +
    'end;'                                         + LineEnding;

function CreateEditor(AOwner: TComponent): TSynEdit;
var
  i: Integer;
begin
  Result := TSynEdit.Create(AOwner);
  Result.BorderStyle := bsNone;
  Result.Font.Size := 10;
  Result.Font.Quality := fqCleartypeNatural;
  Result.Font.Color := TColor($1F1F1F);
  Result.Color := TColor($CFCAC8);
  Result.SelectedColor.Background := TColor($D4B9A6);
  Result.SelectedColor.Foreground := clNone;
  Result.HighlightAllColor.Background := TColor($BDB5B1);
  Result.HighlightAllColor.Foreground := clNone;
  Result.HighlightAllColor.FrameColor := clNone;
  Result.Gutter.Color := TColor($CFCAC8);
  for i := 0 to Result.Gutter.Parts.Count - 1 do
    Result.Gutter.Parts[i].MarkupInfo.Background := TColor($CFCAC8);
  Result.Gutter.LineNumberPart().MarkupInfo.Foreground := TColor($78726E);
  Result.Gutter.SeparatorPart().MarkupInfo.Foreground := TColor($CFCAC8);
  Result.Options := Result.Options + [eoHideRightMargin];

  if (Screen.Fonts.IndexOf('Cascadia Code Semilight') > -1) then
    Result.Font.Name := 'Cascadia Code Semilight';
end;

function HighResolutionTime: Double;
{$IFDEF WINDOWS}
var
  Frequency: Int64 = 0;
  Count: Int64 = 0;
begin
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(Count);

  Result := Count / Frequency * 1000;
end;
{$ELSE}
begin
  Result := GetTickCount64();
end;
{$ENDIF}

procedure ShowDisassembly(Compiler: TLapeCompiler);
var
  Form: TLapeDisassemblyForm;
begin
  Form := TLapeDisassemblyForm.Create(MainForm, @CreateEditor);
  Form.LinkedColor := TColor($BBB4B0);

  with Form.Highlighter do
  begin
    TextAttri.Foreground := TColor($1F1F1F);
    HeaderAttri.Foreground := TColor($7A6200);
    HeaderAttri.Style := [fsBold];
    AddressAttri.Foreground := TColor($78726E);
    NameAttri.Foreground := TColor($B33300);
    DetailAttri.Foreground := TColor($941087);
    ConstantAttri.Foreground := TColor($177D06);
    OffsetAttri.Foreground := TColor($084B8A);
    StringAttri.Foreground := TColor($177D06);
    LinkAttri.Foreground := TColor($7A6200);
    LinkAttri.Style := [fsUnderline];
  end;

  Form.Disassemble(Compiler);
  Form.Show();
end;

procedure _LapeWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMainForm(Params^[0]) do
    AppendOutput(PlpString(Params^[1])^);
end;

procedure _LapeWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMainForm(Params^[0]) do
    AppendOutput(LineEnding);
end;

function AppFolder: string;
begin
  Result := GetUserDir() + '.lape' + PathDelim;
end;

function ScriptFile: string;
begin
  Result := AppFolder() + 'script.lap';
end;

function SettingsFile: string;
begin
  Result := AppFolder() + 'lape.ini';
end;

function BenchmarksFile: string;
begin
  Result := AppFolder() + 'benchmarks.txt';
end;

procedure Compile(Run, Disassemble: Boolean);
var
  t: Double;
  Parser: TLapeTokenizerBase;
  Compiler: TLapeCompiler;
begin
  Parser := nil;
  Compiler := nil;
  with MainForm do
  try
    FEditorScript.Lines.SaveToFile(ScriptFile());

    Parser := TLapeTokenizerString.Create(FEditorScript.Lines.Text, 'main');
    Compiler := TLapeCompiler.Create(Parser);
    Compiler.OnHint := @WriteHint;

    InitializeFFI(Compiler);
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(Byte), False)).CreateAlias(), 'Byte');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(ShortInt), True)).CreateAlias(), 'ShortInt');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(Word), False)).CreateAlias(), 'Word');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(SmallInt), True)).CreateAlias(), 'SmallInt');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(LongWord), False)).CreateAlias(), 'LongWord');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(LongInt), True)).CreateAlias(), 'LongInt');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(Cardinal), False)).CreateAlias(), 'Cardinal');
    Compiler.addGlobalType(Compiler.getBaseType(DetermineIntType(SizeOf(Integer), True)).CreateAlias(), 'Integer');
    Compiler.addGlobalType('^Char', 'PChar');

    Compiler.addGlobalMethod('procedure _Write(s: string); override;', @_LapeWrite, MainForm);
    Compiler.addGlobalMethod('procedure _WriteLn; override;', @_LapeWriteLn, MainForm);

    try
      t := HighResolutionTime();
      if Compiler.Compile() then
        AddLine('Compiling took: ' + IntToStr(Round(HighResolutionTime() - t)) + 'ms.')
      else
        AddLine('Compiling failed');
    except
      on E: Exception do
      begin
        AddLine('Compilation error: "' + E.Message + '"');
        if (E is lpException) and (lpException(E).Hint <> '') then
          AddLine(lpException(E).Hint);
        Exit;
      end;
    end;

    try
      if Disassemble then
        ShowDisassembly(Compiler);

      if Run then
      begin
        t := HighResolutionTime();

        with TLapeCodeRunner.Create(Compiler.Emitter) do
        try
          Run();
        finally
          Free();
        end;

        AddLine('Running Time: ' + IntToStr(Round(HighResolutionTime - t)) + 'ms.');
      end;
    except
      on E: lpException do
      begin
        AddLine(E.Message);
        if (E.StackTrace <> '') then
          AddLine(E.StackTrace);
      end;
      on E: Exception do
        AddLine(E.Message);
    end;
  finally
    if (Compiler <> nil) then
      Compiler.Free()
    else if (Parser <> nil) then
      Parser.Free();
  end;
end;

procedure TMainForm.btnRunClick(Sender: TObject);
begin
  Compile(True, False);
end;

procedure TMainForm.btnBenchClick(Sender: TObject);
begin
  BuildAndRunTests('--bench');
end;

procedure TMainForm.btnRunTestsClick(Sender: TObject);
begin
  BuildAndRunTests('');
end;

procedure TMainForm.BuildAndRunTests(Params: string);
begin
  FEditorOutput.Lines.Clear();
  FTestParams := Params;

  LazBuildProcess.Executable := 'C:/lazarus/lazbuild.exe';
  if (not FileExists(LazBuildProcess.Executable)) then
  begin
    AddLine('Cannot find lazbuild: ' + LazBuildProcess.Executable);
    Exit;
  end;

  AddLine('Building LapeTest...');
  FBuildOutput := '';
  LazBuildProcess.Parameters.Clear();
  LazBuildProcess.Parameters.Add('tests/RunTests/LapeTest.lpi');
  LazBuildProcess.Execute();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ForceDirectories(AppFolder());

  FEditorScript := CreateEditor(Self);
  FEditorScript.Parent := Self;
  FEditorScript.Align := alClient;
  FEditorScript.Highlighter := PasSyn;
  FEditorScript.TabWidth := 2;
  FEditorScript.Lines.Text := DefaultScript;

  FEditorOutput := CreateEditor(Self);
  FEditorOutput.Name := 'FEditorOutput';
  FEditorOutput.Parent := Self;
  FEditorOutput.Top := ClientHeight;
  FEditorOutput.Height := Scale96ToScreen(136);
  FEditorOutput.Align := alBottom;
  FEditorOutput.ReadOnly := True;
  FEditorOutput.ScrollBars := ssAutoBoth;
  FEditorOutput.Text := 'Welcome to Lape';

  Width := Round(Screen.WorkAreaWidth * 0.4);
  Height := Round(Screen.WorkAreaHeight * 0.75);

  FStorage := TIniPropStorage.Create(Self);
  FStorage.IniFileName := SettingsFile();
  SessionProperties := 'Left;Top;Width;Height;WindowState;FEditorOutput.Height';
  if FileExists(SettingsFile()) then
  begin
    Position := poDesigned;
    FStorage.Restore();
  end;
  OnClose := @FormClose;
  Splitter1.OnPaint := @SplitterPaint;

  if FileExists(ScriptFile()) then
    FEditorScript.Lines.LoadFromFile(ScriptFile());

  ApplyTheme();
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FStorage.Save();
end;

procedure TMainForm.SplitterPaint(Sender: TObject);
begin
  Splitter1.Canvas.Brush.Color := FEditorOutput.Color;
  Splitter1.Canvas.FillRect(Splitter1.ClientRect);
end;

procedure TMainForm.ApplyTheme;

  procedure Colors(Attri: TSynHighlighterAttributes; Color: TColor; Style: TFontStyles = []);
  begin
    Attri.Foreground := Color;
    Attri.Background := clNone;
    Attri.Style := Style;
  end;

var
  i: Integer;
begin
  FEditorScript.Gutter.ChangesPart().Free();
  FEditorScript.Gutter.CodeFoldPart().Free();
  FEditorScript.Gutter.MarksPart().Free();
  FEditorScript.Gutter.LeftOffset := Scale96ToScreen(5);
  FEditorScript.Gutter.RightOffset := Scale96ToScreen(5);
  FEditorScript.LineHighlightColor.Background := TColor($C8C1BE);
  FEditorScript.BracketMatchColor.Background := TColor($BDB5B1);
  FEditorScript.BracketMatchColor.Foreground := clNone;
  FEditorScript.BracketMatchColor.Style := [];
  with FEditorScript.MarkupByClass[TSynEditMarkupWordGroup].MarkupInfo do
  begin
    Background := TColor($BDB5B1);
    Foreground := clNone;
    FrameColor := clNone;
    Style := [];
  end;

  for i := 0 to FEditorOutput.Gutter.Parts.Count - 1 do
    FEditorOutput.Gutter.Parts[i].Visible := False;
  FEditorOutput.Gutter.LeftOffset := 0;
  FEditorOutput.Gutter.RightOffset := Scale96ToScreen(3);

  Colors(PasSyn.KeyAttri, TColor($B33300));
  Colors(PasSyn.ModifierAttri, TColor($B33300));
  Colors(PasSyn.CommentAttri, TColor($5E5E5E), [fsItalic]);
  Colors(PasSyn.StringAttri, TColor($177D06));
  Colors(PasSyn.NumberAttri, TColor($EB5017));
  Colors(PasSyn.DirectiveAttri, TColor($0D889E));
  Colors(PasSyn.SymbolAttri, TColor($1F1F1F));
  Colors(PasSyn.IdentifierAttri, TColor($1F1F1F));
  Colors(PasSyn.AsmAttri, TColor($1F1F1F));
  PasSyn.CaseLabelAttri.Style := [];
  for i := 0 to PasSyn.DividerDrawConfigCount - 1 do
    PasSyn.DividerDrawConfig[i].MaxDrawDepth := 0;
end;

procedure TMainForm.AppendOutput(s: string);
var
  Parts: SysUtils.TStringArray;
  i: Integer;
begin
  Parts := s.Replace(#13, '').Split([#10]);
  if (Length(Parts) = 0) then
    Exit;

  if (FEditorOutput.Lines.Count = 0) then
    FEditorOutput.Lines.Add(Parts[0])
  else
    FEditorOutput.Lines[FEditorOutput.Lines.Count - 1] := FEditorOutput.Lines[FEditorOutput.Lines.Count - 1] + Parts[0];
  for i := 1 to High(Parts) do
    FEditorOutput.Lines.Add(Parts[i]);

  FEditorOutput.CaretY := FEditorOutput.Lines.Count;
  FEditorOutput.TopLine := FEditorOutput.Lines.Count - FEditorOutput.LinesInWindow + 1;
end;

procedure TMainForm.AddLine(s: string);
begin
  if (FEditorOutput.Lines.Count > 0) and (FEditorOutput.Lines[FEditorOutput.Lines.Count - 1] <> '') then
    AppendOutput(LineEnding);
  AppendOutput(s + LineEnding);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_R) and (Shift = [ssAlt]) then
  begin
    Key := 0;

    btnRun.Click();
  end;
end;

procedure TMainForm.WriteHint(Sender: TLapeCompilerBase; Msg: lpString);
begin
  AddLine(Msg);
end;

procedure TMainForm.btnDisassembleClick(Sender: TObject);
begin
  Compile(True, True);
end;

procedure TMainForm.ProcessReadData(Sender: TObject);
var
  Data: String;
begin
  Data := '';
  SetLength(Data, TAsyncProcess(Sender).PipeBufferSize);
  SetLength(Data, TAsyncProcess(Sender).Output.Read(Data[1], Length(Data)));
  if (Sender = LazBuildProcess) then
    FBuildOutput := FBuildOutput + Data
  else if (Data <> '') then
  begin
    FTestOutput := FTestOutput + Data;
    AppendOutput(Data);
  end;
end;

procedure TMainForm.LazBuildProcessTerminate(Sender: TObject);
var
  Lines: TStringList;
  Line: string;
begin
  ProcessReadData(Sender);

  if (LazBuildProcess.ExitCode <> 0) then
  begin
    AddLine('Building LapeTest failed:');
    Lines := TStringList.Create();
    try
      Lines.Text := FBuildOutput;
      for Line in Lines do
        if (Pos('Error', Line) > 0) or (Pos('Fatal', Line) > 0) then
          AddLine(Line);
    finally
      Lines.Free();
    end;
  end
  else
  begin
    AddLine('Built LapeTest');
    LapeTestProcess.CurrentDirectory := 'tests/RunTests';
    LapeTestProcess.Executable := 'tests/RunTests/LapeTest.exe';
    LapeTestProcess.Parameters.Clear();
    if (FTestParams <> '') then
      LapeTestProcess.Parameters.Add(FTestParams);
    FTestOutput := '';
    LapeTestProcess.Execute();
  end;
end;

procedure TMainForm.LapeTestProcessTerminate(Sender: TObject);
var
  Results: TextFile;
begin
  ProcessReadData(Sender);
  if (FTestParams <> '--bench') then
    Exit;

  AssignFile(Results, BenchmarksFile());
  if FileExists(BenchmarksFile()) then
    Append(Results)
  else
    Rewrite(Results);
  try
    WriteLn(Results, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now()));
    WriteLn(Results, TrimRight(FTestOutput));
    WriteLn(Results);
  finally
    CloseFile(Results);
  end;
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
{$ENDIF}
end.

