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
  {$IFDEF Lape_NeedAnsiStringsUnit}AnsiStrings,{$ENDIF}
  lpcompiler, lpparser, lpmessages, lpinterpreter, lputils;

{$IFNDEF FPC} //Internal error workaround
{$IF NOT DECLARED(GetTickCount)}
function GetTickCount: UInt32;
begin
  Result := Trunc(Now() * 24 * 60 * 60 * 1000);
end;
{$IFEND}
{$ENDIF}

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
    InitializePascalScriptBasics(GetSelf() as TLapeCompiler, [psiTypeAlias]);
    addGlobalMethod('procedure _Write(s: string); override;', @_WriteWrap, @Output);
    addGlobalMethod('procedure _WriteLn; override;', @_WriteLnWrap, @Output);

    try
      if (not Compile()) then
        LapeException('Error compiling file')
      else
        RunCode(Emitter);
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

  function TestFolder(Folder: string): Integer;
  var
    Res: TSearchRec;
    StartTime: UInt64;
  begin
    Result := 0;

    if (FindFirst(Folder + '*.lap', faAnyFile - faDirectory, Res) = 0) then
    try
      repeat
        Write(Format('Testing %35s :: ', [ExtractFileName(Res.Name)]));
        Inc(Result);

        Output := '';
        StartTime := GetTickCount();
        try

          if FileExists(Folder + ChangeFileExt(Res.Name, '.txt')) then
            TestFile(Folder + Res.Name, Output, Folder + ChangeFileExt(Res.Name, '.txt'))
          else
            TestFile(Folder + Res.Name, Output);

          Inc(Pass);
          WriteLn('Passed :: ', GetTickCount() - StartTime, 'ms');
          if FDebug then
            WriteLn(Output);
        except
          on E: Exception do
          begin
            Inc(Fail);
            WriteLn('Failed :: ', GetTickCount() - StartTime, 'ms');
            if (not FDebug) then
              WriteLn(Output);

            WriteLn('');
            Writeln(E.Message);
            WriteLn('');
          end;
        end;
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
        begin
          WriteLn('Entering ', Res.Name, '..');
          Result := TestFolder(Folder + Res.Name + PathDelim);
        end;
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

