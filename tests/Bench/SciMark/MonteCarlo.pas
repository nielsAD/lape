const
  SEED = 113;

(*
function MonteCarlo_integrate(Num_samples: integer): double;
function MonteCarlo_num_flops(Num_samples: integer): double;
*)


function MonteCarlo_integrate(Num_samples: integer): double;
Var
  R: TRandom;
  under_curve, count: integer;
  x, y: double;
  k,i,j:Int32;

begin
  R := new_Random_seed(SEED);

  under_curve := 0;

  for count := 0 to Num_samples - 1 do
  begin
    //x :=
      I := R.i;
      j := R.j;
      k := R.m[I] - R.m[j];
      if (k < 0) then inc(k, m1);
      R.m[j] := k;
      if (I = 0) then I := 16 else dec(I);
      R.i := I;
      if (j = 0) then j := 16 else dec(j);
      R.j := j;
      if (R.haveRange) then
        x := (R.left + dm1 * k * R.width)
      else
        x := dm1 * k;

    //y :=
      I := R.i;
      j := R.j;
      k := R.m[I] - R.m[j];
      if (k < 0) then inc(k, m1);
      R.m[j] := k;
      if (I = 0) then I := 16 else dec(I);
      R.i := I;
      if (j = 0) then j := 16 else dec(j);
      R.j := j;
      if (R.haveRange) then
        y := (R.left + dm1 * k * R.width)
      else
        y := dm1 * k;
  
  
    //x := Random_nextDouble(R);
    //y := Random_nextDouble(R);

    if (x * x + y * y <= 1.0) then
      inc(under_curve);
  end;

  result := (under_curve / Num_samples) * 4.0;
end;

function MonteCarlo_num_flops(Num_samples: integer): double;
begin
  result := Num_samples * 4.0;
end;
