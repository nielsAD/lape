program LapeTest;

{$APPTYPE CONSOLE}

uses
  Classes, SysUtils,
  lptest, lptypes;

begin
  try
    with TLapeTester.Create() do
    try
      if TestFiles() then
        ExitCode := 0
      else
        ExitCode := 1;
    finally
      Free();
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
