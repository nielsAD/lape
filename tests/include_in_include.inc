procedure Included_Include;
begin
  WriteLn('Hi from included include!: ', {$macro FILE});
end;
