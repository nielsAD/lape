procedure FFT_transform(_N: Integer; data: TDoubleArray); forward;
procedure FFT_inverse(_N: Integer; data: TDoubleArray); forward;


function int_log2(n: Integer): Integer;
var
  k: Integer;
begin
  k := 1;
  result := 0;

  while(k < n) do
  begin
    k := k*2;
    inc(result);
  end;

  if (n <> (1 shl result)) then
  begin
    writeln('FFT: Data length is not a power of 2!: ', n);
    exit(1);
  end;
end;

procedure FFT_bitreverse(_N: Integer; data: TDoubleArray);
{ This is the Goldrader bit-reversal algorithm }
Var
    n, nm1,i, j, ii, jj, k: integer;
    tmp_real, tmp_imag: double;
begin
    n:=_N div 2;
    nm1 := n-1;
    j:=0;
    for i :=0  to nm1-1 do
    begin
      {int ii = 2*i; }
      ii := i shl 1;

      {int jj = 2*j; }
      jj := j shl 1;

      { int k = n / 2 ; }
      k := n shr 1;

      if (i < j) then
      begin
        tmp_real    := data[ii];
        tmp_imag    := data[ii+1];
        data[ii]   := data[jj];
        data[ii+1] := data[jj+1];
        data[jj]   := tmp_real;
        data[jj+1] := tmp_imag;
      end;

      while (k <= j) do
      begin
        {j = j - k ; }
        dec(j, k);

        {k = k / 2 ;  }
        k := k shr 1;
      end;

      inc(j, k);
    end;
end;


procedure FFT_transform_internal(_N: Integer; data: TDoubleArray; direction: Integer);
var
  n, bit, logn, dual, a, b, i, j: Integer;
  w_real, w_imag, theta, s, t, s2, wd_real, wd_imag, tmp_real, tmp_imag,
  z1_real, z1_imag: double;

begin
  n := _N div 2;
  bit := 0;
  dual := 1;

  if (n = 1) then exit;         { Identity operation! }
  logn := int_log2(n);


  if (N = 0) then exit;

  { bit reverse the input data for decimation in time algorithm }
  FFT_bitreverse(_N, data) ;

  { apply fft recursion }
  { this loop executed int_log2(N) times }
  while(bit < logn) do
  begin
    w_real := 1.0;
    w_imag := 0.0;

    theta := 2.0 * direction * PI / (2.0 * dual);
    s := sin(theta);
    t := sin(theta / 2.0);
    s2 := 2.0 * t * t;

    b:=0;
    while b < n do
    begin
      i := 2*b;
      j := 2*(b + dual);

      wd_real := data[j];
      wd_imag := data[j+1];

      data[j]   := data[i]   - wd_real;
      data[j+1] := data[i+1] - wd_imag;
      data[i]   := data[i]   + wd_real;
      data[i+1] := data[i+1] + wd_imag;

      inc(b, 2 * dual);
    end;

    { a = 1 .. (dual-1) }
    for a := 1 to dual-1 do
    begin
      { trignometric recurrence for w-> exp(i theta) w }
      begin
        tmp_real := w_real - s * w_imag - s2 * w_real;
        tmp_imag := w_imag + s * w_real - s2 * w_imag;
        w_real := tmp_real;
        w_imag := tmp_imag;
      end;

      b:=0;
      while b < n do
      begin
        i := 2*(b + a);
        j := 2*(b + a + dual);

        z1_real := data[j];
        z1_imag := data[j+1];

        wd_real := w_real * z1_real - w_imag * z1_imag;
        wd_imag := w_real * z1_imag + w_imag * z1_real;

        data[j]   := data[i]   - wd_real;
        data[j+1] := data[i+1] - wd_imag;
        data[i]   := data[i]   + wd_real;
        data[i+1] := data[i+1] + wd_imag;

        inc(b, 2 * dual);
      end;
    end;

    inc(bit);
    dual := dual*2;
  end;
end;

function FFT_num_flops(_N: Integer): Double;
var
  Nd, logN: Double;
begin
  Nd := _N;
  logN := int_log2(_N);

  Result := (5.0 * Nd - 2) * logN + 2 * (Nd + 1);
end;

procedure FFT_transform(_N: Integer; data: TDoubleArray);
begin
  FFT_transform_internal(_N, data, -1);
end;

procedure FFT_inverse(_N: Integer; data: TDoubleArray);
var
  n, i: Integer;
  norm: Double;
begin
  n := _N div 2;

  FFT_transform_internal(_N, data, 1);

  norm := 1.0 / n;
  for i := 0 to _N - 1 do
    data[i] := data[i] * norm;
end;

