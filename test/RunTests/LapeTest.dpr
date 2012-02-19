program LapeTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  lptest, lpparser;

begin
  WriteLn(StrToUint64('18446744073709551615'));
  try
    with TLapeTester.Create() do
    try
      TestFiles();
    finally
      Free();
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
