procedure Included_Include;
begin
  WriteLn('Hi from included include!: ', {$macro CURRENT_FILE});
end;
