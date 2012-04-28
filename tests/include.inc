{$I include_in_include.inc}

procedure Included;
begin
  WriteLn('Hi from include!');
  Included_Include();
end;