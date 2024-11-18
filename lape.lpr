program lape;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, Main;

{$R *.res}

begin
  Application.Initialize();
  Application.CreateForm(TMainForm, MainForm);
  Application.Run();
end.

