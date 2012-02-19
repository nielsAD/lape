unit lptest;

{$i lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

type
  TLapeTester = class(TLapeBaseClass)
  private
    FFolder: lpString;
    FDebug: Boolean;
  public
    constructor Create(AFolder: lpString = '..'; ADebug: Boolean = False); reintroduce;

    class function TestFile(FileName: lpString; ExpectFile: lpString = ''): lpString;
    procedure TestFiles;

    property Folder: lpString read FFolder;
    property Debug: Boolean read FDebug;
  end;

implementation

uses
  LCLIntf, strutils,
  lpcompiler, lpparser, lpexceptions, lpinterpreter;

constructor TLapeTester.Create(AFolder: lpString = '..'; ADebug: Boolean = False);
begin
  inherited Create();

  FDebug := ADebug;
  FFolder := IncludeTrailingPathDelimiter(ExpandFileName(AFolder));
  if (not DirectoryExists(FFolder)) then
    LapeException('Cannot find folder ' + FFolder);
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

procedure _WriteWrap(Params: PParamArray);
begin
  _Write(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _WriteLnWrap(Params: PParamArray);
begin
  _WriteLn(PlpString(Params^[0])^);
end;

class function TLapeTester.TestFile(FileName: lpString; ExpectFile: lpString = ''): lpString;
var
  Expect, Output: lpString;
begin
  if (ExpectFile = '') then
    Expect := ''
  else
    with TStringList.Create() do
    try
      LoadFromFile(ExpectFile);
      Expect := Trim(Text);
    finally
      Free();
    end;

  with TLapeCompiler.Create(TLapeTokenizerFile.Create(FileName)) do
  try
    InitializePascalScriptBasics(GetSelf() as TLapeCompiler, [psiTypeAlias]);
    addGlobalMethod('procedure _write(s: string); override;', @_WriteWrap, @Output);
    addGlobalMethod('procedure _writeln; override;', @_WriteLnWrap, @Output);

    try
      if (not Compile()) then
        LapeException('Error compiling file')
      else
        RunCode(Emitter.Code);
    except
      on E: Exception do
      begin
        if (Expect = '') then
          raise
        else
        begin
          if (Output <> '') and (not AnsiEndsStr(LineEnding, Output)) then
            _WriteLn(Output);
          _WriteLn(Output, E.Message);
        end;
      end;
    end;

    Output := Trim(Output);
    if (Expect <> '') and (Output <> Expect) then
      LapeExceptionFmt('Expected output "%s", but got "%s"', [Expect, Output]);
  finally
    Free();
    Result := Output;
  end;
end;

procedure TLapeTester.TestFiles;
var
  Output: lpString;
  Fail, Pass: Integer;
  StartTime: UInt64;

  function TestFolder(Folder: lpString): Integer;
  var
    Res: TSearchRec;
    StartTime: UInt64;
  begin
    Result := 0;

    if (FindFirst(Folder + '*.lap', faAnyFile - faDirectory, Res) = 0) then
    try
      repeat
        Inc(Result);
        Write('Testing ' + PadRight(ExtractFileName(Res.Name), 25), ' :: ');

        StartTime := GetTickCount64();
        try
          if FileExists(Folder + ChangeFileExt(Res.Name, '.txt')) then
            Output := TestFile(Folder + Res.Name, Folder + ChangeFileExt(Res.Name, '.txt'))
          else
            Output := TestFile(Folder + Res.Name);

          Inc(Pass);
          WriteLn('Passed :: ', GetTickCount64() - StartTime, 'ms');
          if FDebug then
            WriteLn(Output);
        except
          on E: Exception do
          begin
            Inc(Fail);
            WriteLn('Failed', GetTickCount64() - StartTime, 'ms');
            Writeln(E.Message);
            WriteLn('');
          end;
        end;
      until (FindNext(Res) <> 0);
    finally
      FindClose(Res);
    end;

    if (Result > 0) then
      WriteLn('');

    if (FindFirst(Folder + '*.*', faAnyFile, Res) = 0) then
    try
      repeat
        if ((Res.Attr and faDirectory) <> 0) and (Res.Name <> '.') and (Res.Name <> '..') then
        begin
          WriteLn('Entering ', Res.Name, '..');
          Result := TestFolder(Folder + Res.Name + DirectorySeparator);
        end;
      until (FindNext(Res) <> 0);
    finally
      FindClose(Res);
    end;
  end;

begin
  Fail := 0;
  Pass := 0;
  StartTime := GetTickCount64();

  if (TestFolder(FFolder) = 0) then
    WriteLn('');
  WriteLn(Format('%d tests in %.2f seconds', [Pass + Fail, ((GetTickCount64() - StartTime) / 1000)]));
  WriteLn(Format('%d/%d tests failed', [Fail, Pass + Fail]));
  WriteLn(Format('%d/%d tests passed', [Pass, Pass + Fail]));
end;

end.

