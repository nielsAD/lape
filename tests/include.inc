{$I include_in_include.inc}

procedure Included;
begin
  WriteLn('Hi from include: ', {$macro FILE});
  Included_Include();
end;
