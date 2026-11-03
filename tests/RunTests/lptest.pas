unit lptest;

{$i lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes;

type
  TLapeTester = class(TLapeBaseClass)
  private
    FFolder: string;
    FDebug: Boolean;
    FBench: Boolean;
  public
    constructor Create(AFolder: string = '..'; ADebug: Boolean = False; ABench: Boolean = False); reintroduce;

    class procedure TestFile(FileName: string; out Output: lpString; ExpectFile: string = '');
    function TestFiles: Boolean;

    property Folder: string read FFolder;
    property Debug: Boolean read FDebug;
    property Bench: Boolean read FBench; // (only the scripts in the Bench folder are run)
  end;

implementation

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} strutils,
  lpcompiler, lptokenizer, lpmessages, lpinterpreter;

constructor TLapeTester.Create(AFolder: string = '..'; ADebug: Boolean = False; ABench: Boolean = False);
begin
  inherited Create();

  FDebug := ADebug;
  FBench := ABench;
  FFolder := IncludeTrailingPathDelimiter(ExpandFileName(AFolder));
  if (not DirectoryExists(FFolder)) then
    LapeException(lpString('Cannot find folder ' + FFolder));
end;

function HighResolutionTime: Double;
{$IFDEF MSWINDOWS}
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


procedure _GetCurrHeapUsed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

    // the names that Pascal has for the integer types
    addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).CreateAlias(), 'Byte');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(ShortInt), True)).CreateAlias(), 'ShortInt');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(Word), False)).CreateAlias(), 'Word');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(SmallInt), True)).CreateAlias(), 'SmallInt');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(LongWord), False)).CreateAlias(), 'LongWord');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(LongInt), True)).CreateAlias(), 'LongInt');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(Cardinal), False)).CreateAlias(), 'Cardinal');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).CreateAlias(), 'Integer');
    addGlobalType('^Char', 'PChar');

    addGlobalMethod('procedure _Write(s: string); override;', @_WriteWrap, @Output);
    addGlobalMethod('procedure _WriteLn; override;', @_WriteLnWrap, @Output);
    addGlobalFunc('function GetCurrHeapUsed: PtrUInt;', @_GetCurrHeapUsed);
    // (a host's code with a method)
    if (Copy(ExtractFileName(FileName), 1, 11) = 'HostDelayed') then
    begin
      addCode('procedure HostFail(x: Int32);' + LineEnding + 'begin' + LineEnding + '  if (x > 2) then' + LineEnding + '    raise ''host '' + ToString(x);' + LineEnding + 'end;');
      addCode('var HostValue: Int32 := 7;' + LineEnding + 'begin' + LineEnding + '  HostValue := HostValue + 1;' + LineEnding + 'end;', '', False);
    end;

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
          if (E is lpException) and (lpException(E).StackTrace <> '') then
            _WriteLn(Output, lpException(E).StackTrace);
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
  StartTime: Double;

  // Runs it: whether what it wrote is what was expected. Quiet: nothing is written, also not when it fails.
  function RunOne(const Folder, Name: string; Quiet: Boolean): Boolean;
  var
    StartTime: Double;
    ExpectFile: string;
  begin
    Output := '';
    StartTime := HighResolutionTime();
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
          WriteLn('Passed :: ', Round(HighResolutionTime() - StartTime), 'ms');
          if FDebug then
            WriteLn(Output);
        end;
      except
        on E: Exception do
        begin
          Result := False;
          if (not Quiet) then
          begin
            WriteLn('Failed :: ', Round(HighResolutionTime() - StartTime), 'ms');
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

  // Whether the scripts in Folder are run: the benchmarks (the Bench folder) with --bench, the others without
  function Runs(const Folder: string): Boolean;
  begin
    Result := SameText(ExtractFileName(ExcludeTrailingPathDelimiter(Folder)), 'Bench') = FBench;
  end;

  // (the names in a column as wide as the longest of the folder's)
  function NameWidth(Folder: string): Integer;
  var
    Res: TSearchRec;
  begin
    Result := 0;
    if (FindFirst(Folder + '*.lap', faAnyFile - faDirectory, Res) = 0) then
    try
      repeat
        if (Length(Res.Name) > Result) then
          Result := Length(Res.Name);
      until (FindNext(Res) <> 0);
    finally
      SysUtils.FindClose(Res);
    end;
  end;

  function TestFolder(Folder: string): Integer;
  var
    Res: TSearchRec;
    Passed: Boolean;
    Left: PtrInt;
    Width: Integer;
  begin
    Result := 0;
    Width := NameWidth(Folder);

    if Runs(Folder) and (FindFirst(Folder + '*.lap', faAnyFile - faDirectory, Res) = 0) then
    try
      repeat
        if (Result = 0) then
          WriteLn('Entering ', ExtractFileName(ExcludeTrailingPathDelimiter(Folder)), '..');

        Write(Format('%-*s :: ', [Width, ExtractFileName(Res.Name)]));
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
        if ((Res.Attr and faDirectory) <> 0) and (Res.Name <> '.') and (Res.Name <> '..') then
          Inc(Result, TestFolder(Folder + Res.Name + PathDelim));
      until (FindNext(Res) <> 0);
    finally
      SysUtils.FindClose(Res);
    end;
  end;

begin
  Fail := 0;
  Pass := 0;
  StartTime := HighResolutionTime();

  if (TestFolder(FFolder) = 0) then
    WriteLn('');
  WriteLn(Format('Ran %d tests in %.2f seconds', [Pass + Fail, ((HighResolutionTime() - StartTime) / 1000)]));
  WriteLn(Format('%3d / %d tests failed', [Fail, Pass + Fail]));
  WriteLn(Format('%3d / %d tests passed', [Pass, Pass + Fail]));

  Result := (Fail = 0);
end;

initialization
  FormatSettings.DecimalSeparator := ',';
end.

