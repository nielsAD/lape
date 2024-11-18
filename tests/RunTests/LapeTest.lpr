program LapeTest;

{$i lape.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  lptest;

type
  TLapeTestApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp;
  end;

procedure TLapeTestApp.DoRun;
var
  ErrorMsg, Folder: string;
begin
  try
    ErrorMsg := CheckOptions('hfe', ['help', 'folder', 'explicit']);
    if (ErrorMsg <> '') then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Exit;
    end;

    if HasOption('h', 'help') then
    begin
      WriteHelp();
      Exit;
    end;

    if HasOption('f', 'folder') then
      Folder := GetOptionValue('f', 'folder')
    else
      Folder := '..';

    with TLapeTester.Create(Folder, HasOption('e', 'explicit')) do
    try
      if TestFiles() then
        ExitCode := 0
      else
        ExitCode := 1;
    finally
      Free();
    end;
  finally
    Terminate();
  end;
end;

constructor TLapeTestApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

procedure TLapeTestApp.WriteHelp;
begin
  WriteLn('Usage: ', ExtractFileName(ExeName),' -ARGUMENT');
  WriteLn('  h, help     :: You are reading it');
  WriteLn('  f, folder   :: -f "FOLDER" to specify the testing directory');
  WriteLn('  e, explicit :: Explicit debug');
end;

begin
  with TLapeTestApp.Create(nil) do
  try
    Title := 'LapeTest';
    Run();
  finally
    Free();
  end;
end.

