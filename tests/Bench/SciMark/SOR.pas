
function SOR_num_flops(M, N, num_iterations: integer): double;
begin
  Result := (M - 1) * (N - 1) * num_iterations * 6.0;
end;

procedure SOR_execute(M, N: integer; omega: double; G: array of TDoubleArray; num_iterations: integer);
Var
  omega_over_four, one_minus_omega: double;
  Gi, Gim1, Gip1: TDoubleArray;
  p, i, j, Mm1, Nm1: integer;
begin
  omega_over_four := omega * 0.25;
  one_minus_omega := 1.0 - omega;

  { update interior points }

  Mm1 := M - 1;
  Nm1 := N - 1;

  for p := 0 to num_iterations - 1 do
  begin
    for i := 1 to Mm1 - 1 do
    begin
      Gi := G[i];
      Gim1 := G[i - 1];
      Gip1 := G[i + 1];

      for j := 1 to Nm1 - 1 do
        Gi[j] := omega_over_four * (Gim1[j] + Gip1[j] + Gi[j - 1] + Gi[j + 1]) + one_minus_omega * Gi[j];
    end;
  end;
end;
