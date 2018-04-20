{$I include_in_include.inc}

procedure Included;
begin
  WriteLn('Hi from include!: ', {$macro CURRENT_FILE});
  Included_Include();
end;
