unit lptest;

{$i lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

type
  TLapeTester = class(TLapeBaseClass)
  private
    FFolder: string;
    FDebug: Boolean;
  public
    constructor Create(AFolder: string = '..'; ADebug: Boolean = False); reintroduce;

    class procedure TestFile(FileName: string; out Output: lpString; ExpectFile: string = '');
    function TestFiles: Boolean;

    property Folder: string read FFolder;
    property Debug: Boolean read FDebug;
  end;

implementation

uses
  {$IFDEF FPC}LCLIntf,{$ELSE}{$IFDEF MSWINDOWS}Windows,{$ENDIF}{$ENDIF} strutils,
  lpvartypes, lpcompiler, lptokenizer, lpmessages, lpinterpreter, lputils;

constructor TLapeTester.Create(AFolder: string = '..'; ADebug: Boolean = False);
begin
  inherited Create();

  FDebug := ADebug;
  FFolder := IncludeTrailingPathDelimiter(ExpandFileName(AFolder));
  if (not DirectoryExists(FFolder)) then
    LapeException(lpString('Cannot find folder ' + FFolder));
end;

procedure _Write(var Output: lpString; s: lpString);
begin
  Output := Output + s;
end;

procedure _WriteLn(var Output: lpString; s: lpString = '');
begin
  _Write(Output, s);
  Output := Output + LineEnding;
end;

procedure _WriteWrap(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Write(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

// function TestHeapUsed: PtrUInt
// How many bytes of memory are in use: a script that does the same thing a number of times and sees this stay what it
// was leaks nothing in what it does (tests/Invoke_StressLeaks). (Delphi: always 0.)
procedure _TestHeapUsed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IFDEF FPC}
  PPtrUInt(Result)^ := GetFPCHeapStatus().CurrHeapUsed;
  {$ELSE}
  PPtrUInt(Result)^ := 0;
  {$ENDIF}
end;

procedure _WriteLnWrap(Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _WriteLn(PlpString(Params^[0])^);
end;

class procedure TLapeTester.TestFile(FileName: string; out Output: lpString; ExpectFile: string = '');
var
  Expect: lpString;
begin
  if (ExpectFile = '') then
    Expect := ''
  else
    with TStringList.Create() do
    try
      LoadFromFile(ExpectFile);
      Expect := Trim(lpString(Text));
    finally
      Free();
    end;

  with TLapeCompiler.Create(TLapeTokenizerFile.Create(lpString(FileName))) do
  try
    Tokenizer.FileName := ExtractFileName(Tokenizer.FileName);
    Options := Options - [lcoHints];
    InitializePascalScriptBasics(GetSelf() as TLapeCompiler, [psiTypeAlias]);

    addGlobalMethod('procedure _Write(s: string); override;', @_WriteWrap, @Output);
    addGlobalMethod('procedure _WriteLn; override;', @_WriteLnWrap, @Output);
    addGlobalFunc('function TestHeapUsed: PtrUInt;', @_TestHeapUsed);

    try
      if (not Compile()) then
        LapeException('Error compiling file')
      else
      begin
        Output := Output + Hints;

        with TLapeCodeRunner.Create(Emitter) do
        try
          Run();
        finally
          Free();
        end;
      end;
    except
      on E: Exception do
      begin
        if (Expect = '') then
          raise
        else
        begin
          if (Output <> '') and (not AnsiEndsStr(lpString(LineEnding), Output)) then
            _WriteLn(Output);
          _WriteLn(Output, lpString(StringReplace(E.Message, '"' + ExtractFilePath(FileName), '"', [rfReplaceAll])));
        end;
      end;
    end;

    Output := Trim(Output);
    if (Expect <> '') and (Output <> Expect) then
      LapeExceptionFmt('Expected output: %s', [LineEnding + Expect]);
      //LapeExceptionFmt('Expected output "%s", but got "%s"', [Expect, Output]);
  finally
    Free();
  end;
end;

function TLapeTester.TestFiles: Boolean;
var
  Output: lpString;
  Fail, Pass: Integer;
  StartTime: UInt64;

  // Runs it: whether what it wrote is what was expected. Quiet: nothing is written, also not when it fails.
  function RunOne(const Folder, Name: string; Quiet: Boolean): Boolean;
  var
    StartTime: UInt64;
    ExpectFile: string;
  begin
    Output := '';
    StartTime := GetTickCount();
    try
      try
        ExpectFile := Folder + ChangeFileExt(Name, '.txt');
        {$IFDEF Lape_Unicode}
        if FileExists(Folder + ChangeFileExt(Name, '.unicode.txt')) then
          ExpectFile := Folder + ChangeFileExt(Name, '.unicode.txt');
        {$ENDIF}

        if FileExists(ExpectFile) then
          TestFile(Folder + Name, Output, ExpectFile)
        else
          TestFile(Folder + Name, Output);

        Result := True;
        if (not Quiet) then
        begin
          WriteLn('Passed :: ', GetTickCount() - StartTime, 'ms');
          if FDebug then
            WriteLn(Output);
        end;
      except
        on E: Exception do
        begin
          Result := False;
          if (not Quiet) then
          begin
            WriteLn('Failed :: ', GetTickCount() - StartTime, 'ms');
            if (not FDebug) then
              WriteLn(Output);

            WriteLn('');
            Writeln(E.Message);
            WriteLn('');
          end;
        end;
      end;
    finally
      Output := '';
    end;
  end;

  // How much more memory is in use after it ran and everything of it was cleaned up than before: 0, or it left
  // something behind, which nothing freed when it was done or when what compiled and ran it was
  function LeftBehind(const Folder, Name: string; Quiet: Boolean; out Passed: Boolean): PtrInt;
  {$IFDEF FPC}
  var
    Before: PtrUInt;
  begin
    Before := GetFPCHeapStatus().CurrHeapUsed;
    Passed := RunOne(Folder, Name, Quiet);
    Result := PtrInt(GetFPCHeapStatus().CurrHeapUsed) - PtrInt(Before);
  end;
  {$ELSE}
  begin
    Passed := RunOne(Folder, Name, Quiet);
    Result := 0;
  end;
  {$ENDIF}

  function TestFolder(Folder: string): Integer;
  var
    Res: TSearchRec;
    Passed: Boolean;
    Left: PtrInt;
  begin
    Result := 0;

    if (FindFirst(Folder + '*.lap', faAnyFile - faDirectory, Res) = 0) then
    try
      WriteLn('Entering ', ExtractFileName(ExcludeTrailingPathDelimiter(Folder)), '..');

      repeat
        Write(Format('Testing %35s :: ', [ExtractFileName(Res.Name)]));
        Inc(Result);

        // (something can be made the first time it is needed and kept from then on, by this program: what seems
        // left behind is only that when it is left behind again)
        Left := LeftBehind(Folder, Res.Name, False, Passed);
        if Passed and (Left <> 0) then
          Left := LeftBehind(Folder, Res.Name, True, Passed);

        if Passed and (Left <> 0) then
        begin
          Passed := False;
          WriteLn('Failed :: ', Left, ' bytes of memory are left behind when it is done and cleaned up');
          WriteLn('');
        end;

        if Passed then
          Inc(Pass)
        else
          Inc(Fail);
      until (FindNext(Res) <> 0);
    finally
      SysUtils.FindClose(Res);
    end;

    if (Result > 0) then
      WriteLn('');

    if (FindFirst(Folder + '*', faAnyFile, Res) = 0) then
    try
      repeat
        if ((Res.Attr and faDirectory) <> 0) and (Res.Name <> '.') and (Res.Name <> '..') and (Res.Name <> 'Bench') then
          Result := TestFolder(Folder + Res.Name + PathDelim);
      until (FindNext(Res) <> 0);
    finally
      SysUtils.FindClose(Res);
    end;
  end;

begin
  Fail := 0;
  Pass := 0;
  StartTime := GetTickCount();

  if (TestFolder(FFolder) = 0) then
    WriteLn('');
  WriteLn(Format('Ran %d tests in %.2f seconds', [Pass + Fail, ((GetTickCount() - StartTime) / 1000)]));
  WriteLn(Format('%3d / %d tests failed', [Fail, Pass + Fail]));
  WriteLn(Format('%3d / %d tests passed', [Pass, Pass + Fail]));

  Result := (Fail = 0);
end;

initialization
  FormatSettings.DecimalSeparator := ',';
end.

