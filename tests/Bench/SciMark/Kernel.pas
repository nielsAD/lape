(*
function kernel_measureFFT(N: integer; min_time: double; R: TRandom): double;
function kernel_measureSOR(N: integer; min_time: double; R: TRandom): double;
function kernel_measureMonteCarlo(min_time: double; R: PRandom): double;
function kernel_measureSparseMatMult(N: integer; nz: integer; min_time: double; R: TRandom): double;
function kernel_measureLU(N: integer; min_time: double; R: TRandom): double;
*)
{$include_once fft.pas}


function kernel_measureFFT(N: integer; min_time: double; R: TRandom): double;
var
  twoN: integer;
  x: TDoubleArray;
  cycles: integer;
  Q: TProgramClock;
  i: integer;
begin
  { initialize FFT data as complex (N real/img pairs) }

  twoN := 2*N;
  x := RandomVector(twoN, R);
  cycles := 1;
  Q := new_Stopwatch();

  while(true) do
  begin
      Stopwatch_start(Q);
      for i:=0 to cycles-1 do
      begin
        FFT_transform(twoN, x);     { forward transform }
        FFT_inverse(twoN, x);       { backward transform }
      end;
      Stopwatch_stop(Q);
      if (Stopwatch_read(Q) >= min_time) then break;

      cycles := cycles*2;
  end;

  { approx Mflops }

  result := FFT_num_flops(N)*cycles/ Stopwatch_read(Q) * 1.0e-6;
  Stopwatch_delete(Q);
end;

function kernel_measureSparseMatMult(N: integer; nz: integer; min_time: double; R: TRandom): double;
Var
  x, y, val: TDoubleArray;
  col, row: TIntegerArray;
  nr, anz, _r, cycles, rowr, step, i: integer;
  Q: TProgramClock;

Begin
  { initialize vector multipliers and storage for result }
  { y = A*y;  }

  x := RandomVector(N, R);
  SetLength(y,N);

  // initialize square sparse matrix
  //
  // for this test, we create a sparse matrix with M/nz nonzeros
  // per row, with spaced-out evenly between the begining of the
  // row to the main diagonal.  Thus, the resulting pattern looks
  // like
  //             +-----------------+
  //             +*                +
  //             +***              +
  //             +* * *            +
  //             +** *  *          +
  //             +**  *   *        +
  //             +* *   *   *      +
  //             +*  *   *    *    +
  //             +*   *    *    *  +
  //             +-----------------+
  //
  // (as best reproducible with integer artihmetic)
  // Note that the first nr rows will have elements past
  // the diagonal.

  nr := nz div N;  { average number of nonzeros per row  }
  anz := nr *N;    { _actual_ number of nonzeros         }

  val := RandomVector(anz, R);
  SetLength(col,nz);
  SetLength(row,(N+1));

  cycles:=1;

  Q := new_Stopwatch();

  row[0] := 0;
  for _r:=0 to N-1 do
  Begin
      { initialize elements for row r }

      rowr := row[_r];
      step := _r div nr;

      row[_r+1] := rowr + nr;
      if (step < 1) then step := 1;   { take at least unit steps }


      for i:=0 to nr-1 do
          col[rowr+i] := i*step;
  end;


  while(true) do
  Begin
      Stopwatch_start(Q);
      SparseCompRow_matmult(N, y, val, row, col, x, cycles);
      Stopwatch_stop(Q);
      if (Stopwatch_read(Q) >= min_time) then break;

      cycles := cycles*2;
  end;

  { approx Mflops }
  result := SparseCompRow_num_flops(N, nz, cycles) / Stopwatch_read(Q) * 1.0e-6;

  Stopwatch_delete(Q);
end;

function kernel_measureSOR(N: integer; min_time: double; R: TRandom): double;
var
  cycles: integer;
  Q: TProgramClock;
  G: array of TDoubleArray;

begin
  G := RandomMatrix(N, N, R);

  Q := new_Stopwatch();
  cycles :=1;

  while(true) do
  begin
      Stopwatch_start(Q);
      SOR_execute(N, N, 1.25, G, cycles);
      Stopwatch_stop(Q);

      if (Stopwatch_read(Q) >= min_time) then break;

      cycles := cycles*2;
  end;

  { approx Mflops }

  result := SOR_num_flops(N, N, cycles) / Stopwatch_read(Q) * 1.0e-6;
  Stopwatch_delete(Q);
end;

function kernel_measureMonteCarlo(min_time: double; R: TRandom): double;
var
  Q: TProgramClock;
  cycles: integer;
begin
  Q := new_Stopwatch();

  cycles:=1;
  while true do
  begin
      Stopwatch_start(Q);
      MonteCarlo_integrate(cycles);
      Stopwatch_stop(Q);
      if (Stopwatch_read(Q) >= min_time) then break;

      cycles := cycles*2;
  end;

  { approx Mflops }
  result := MonteCarlo_num_flops(cycles) / Stopwatch_read(Q) * 1.0e-6;

  Stopwatch_delete(Q);
end;

function kernel_measureLU(N: integer; min_time: double; R: TRandom): double;
Var
  i, cycles: Integer;
  x,y: Integer;
  Q: TProgramClock;
  A, lu: array of TDoubleArray;
  pivot: TIntegerArray;
begin
  Q := new_Stopwatch();
  cycles:=1;

  A := RandomMatrix(N, N,  R);
  SetLength(lu,N,N);

  SetLength(pivot,N);

  while true do
  begin
      Stopwatch_start(Q);
      for i:=0 to cycles-1 do
      begin
        //Array2D_double_copy(N, N, lu, A);
        for y:=0 to n-1 do
          for x:=0 to n-1 do
            lu[y,x] := a[y,x];

        LU_factor(N, N, lu, pivot);
      end;
      Stopwatch_stop(Q);
      if (Stopwatch_read(Q) >= min_time) then break;

      cycles := cycles*2;
  end;

  { approx Mflops }
  result := LU_num_flops(N) * cycles / Stopwatch_read(Q) * 1.0e-6;

  Stopwatch_delete(Q);
end;
