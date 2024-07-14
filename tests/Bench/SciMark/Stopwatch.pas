Type
  TProgramClock = record
    running: boolean;
    last_time: double;
    total: double;
  end;


function seconds: double;
begin
  Result := GetTickCount() / 1000;
end;

procedure Stopwtach_reset(var Q: TProgramClock);
begin
    Q.running := false;
    Q.last_time := 0.0;
    Q.total := 0.0;
end;

function new_Stopwatch(): TProgramClock;
begin
  Stopwtach_reset(Result);
end;

procedure Stopwatch_delete(var S: TProgramClock);
begin
  S.running := false;
  S.last_time := 0.0;
  S.total := 0.0;
end;

procedure Stopwatch_start(var Q: TProgramClock);
begin
  if (not (Q.running)  ) then
  begin
      Q.running := true;
      Q.total := 0.0;
      Q.last_time := seconds();
  end;
end;

procedure Stopwatch_resume(var Q: TProgramClock);
begin
    if (not (Q.running)) then
    begin
        Q.last_time := seconds();
        Q.running := true;
    end;
end;

procedure Stopwatch_stop(var Q: TProgramClock);
begin
    if (Q.running) then
    begin
        Q.total := Q.total + seconds() - Q.last_time;
        Q.running := false;
    end;
end;

function Stopwatch_read(var Q: TProgramClock): double;
var
  t: double;
begin
    if (Q.running) then
    begin
        t := seconds();
        Q.total := Q.total + t - Q.last_time;
        Q.last_time := t;
    end;

    result :=  Q.total;
end;
