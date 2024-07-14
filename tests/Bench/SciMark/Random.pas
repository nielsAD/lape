type
  Random_struct = record
    m: array [0 .. 16] of integer;
    seed: integer;
    i: integer;           { originally = 4 }
    j: integer;           { originally = 16 }
    haveRange: boolean;   { = False }
    left: double;         { = 0.0 }
    right: double;        { = 1.0 }
    width: double;        { = 1.0 }
  end;

  TRandom = Random_struct;

function Random_nextDouble(var R: TRandom): Double; forward;

const
  MDIG = 32;
  ONE = 1;

  m1 = (ONE shl (MDIG - 2)) + ((ONE shl (MDIG - 2)) - ONE);
  m2 = ONE shl (MDIG div 2);

Var
  dm1: double;

procedure initialize(var R: TRandom; seed: integer);
Var
  jseed, k0, k1, j0, j1, iloop: Integer;

begin
  dm1 := 1.0 / m1;

  R.seed := seed;

  if (seed < 0) then
    seed := -seed; { seed = abs(seed) }
  jseed := min(seed, m1); { jseed = min(seed, m1) }
  if (jseed mod 2) = 0 then
    dec(jseed);
  k0 := 9069 mod m2;
  k1 := 9069 div m2;
  j0 := jseed mod m2;
  j1 := jseed div m2;

  for iloop := 0 to 16 do
  begin
    jseed := j0 * k0;
    j1 := ((jseed div m2) + j0 * k1 + j1 * k0) mod (m2 div 2);
    j0 := jseed mod m2;
    R.m[iloop] := j0 + m2 * j1;
  end;

  R.i := 4;
  R.j := 16;
end;

function new_Random_seed(seed: integer): TRandom;
Var
  R: TRandom;
Begin
  initialize(R, seed);
  R.left := 0.0;
  R.right := 1.0;
  R.width := 1.0;
  R.haveRange := false;

  Result := R;
End;

function new_Random(seed: integer; left, right: double): TRandom;
Var
  R: TRandom;
Begin
  initialize(R, seed);
  R.left := left;
  R.right := right;
  R.width := right - left;
  R.haveRange := true;

  Result := R;
End;

function Random_nextDouble(var R: TRandom): Double;
Var
  k, I, j: Integer;
Begin
  I := R.i;
  j := R.j;

  k := R.m[I] - R.m[j];
  if (k < 0) then
    inc(k, m1);
  R.m[j] := k;

  if (I = 0) then
    I := 16
  else
    dec(I);

  R.i := I;

  if (j = 0) then
    j := 16
  else
    dec(j);

  R.j := j;

  if (R.haveRange) then
    Result := (R.left + dm1 * k * R.width)
  else
    Result := dm1 * k;
End;

function RandomVector(N: integer; var R: TRandom): TDoubleArray;
Var
  i: Integer;
Begin
  SetLength(Result, N);

  for i := 0 to N - 1 do
    Result[i] := Random_nextDouble(R);
End;

function RandomMatrix(m, N: integer; var R: TRandom): array of TDoubleArray;
Var
  i, j: integer;
Begin
  { allocate matrix }
  SetLength(Result,m,n);

  for i := 0 to m - 1 do
    for j := 0 to N - 1 do
      Result[i,j] := Random_nextDouble(R);

End;
