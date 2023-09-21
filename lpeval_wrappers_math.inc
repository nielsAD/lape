{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for math related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeMin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Min(PInt64(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeMinF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Min(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^);
end;

procedure _LapeMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Max(PInt64(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeMaxF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Max(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^);
end;

procedure _LapeAbs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Abs(PlpFloat(Params^[0])^);
end;

procedure _LapeAbsI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Abs(PInt64(Params^[0])^);
end;

procedure _LapeSign(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt8(Result)^ := Sign(PInt64(Params^[0])^);
end;

procedure _LapeSignF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt8(Result)^ := Sign(PlpFloat(Params^[0])^);
end;

procedure _LapePower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Power(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^);
end;

procedure _LapeSqr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Sqr(PlpFloat(Params^[0])^);
end;

procedure _LapeSqrI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PInt64(Params^[0])^*PInt64(Params^[0])^;
end;

procedure _LapeSqrt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Sqrt(PlpFloat(Params^[0])^);
end;

procedure _LapeArcTan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcTan(PlpFloat(Params^[0])^);
end;

procedure _LapeLn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Ln(PlpFloat(Params^[0])^);
end;

procedure _LapeSin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Sin(PlpFloat(Params^[0])^);
end;

procedure _LapeCos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Cos(PlpFloat(Params^[0])^);
end;

procedure _LapeExp(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Exp(PlpFloat(Params^[0])^);
end;

procedure _LapeHypot(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Hypot(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^);
end;

procedure _LapeArcTan2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcTan2(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^);
end;

procedure _LapeTan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Tan(PlpFloat(Params^[0])^);
end;

procedure _LapeArcSin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcSin(PlpFloat(Params^[0])^);
end;

procedure _LapeArcCos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcCos(PlpFloat(Params^[0])^);
end;

procedure _LapeCotan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Cotan(PlpFloat(Params^[0])^);
end;

procedure _LapeSecant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Secant(PlpFloat(Params^[0])^);
end;

procedure _LapeCosecant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Cosecant(PlpFloat(Params^[0])^);
end;

procedure _LapeRound(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Round(PlpFloat(Params^[0])^);
end;

procedure _LapeRoundTo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := RoundTo(PlpFloat(Params^[0])^, -PInt8(Params^[1])^);
end;

procedure _LapeFrac(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Frac(PlpFloat(Params^[0])^);
end;

procedure _LapeInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Int(PlpFloat(Params^[0])^);
end;

procedure _LapeTrunc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Trunc(PlpFloat(Params^[0])^);
end;

procedure _LapeCeil(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Ceil(PlpFloat(Params^[0])^);
end;

procedure _LapeCeilTo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function CeilTo(const a: Single; const Precision: Int8 = 0): Double;
  begin
    if (Precision = 0) then
      Result := Ceil(a)
    else
      Result := RoundTo(a + 0.5 * Power(10, -Double(Precision)), -Precision);
  end;

begin
  PlpFloat(Result)^ := CeilTo(PlpFloat(Params^[0])^, PInt8(Params^[1])^);
end;

procedure _LapeFloor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Floor(PlpFloat(Params^[0])^);
end;

procedure _LapeCosH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := CosH(PlpFloat(Params^[0])^);
end;

procedure _LapeSinH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := SinH(PlpFloat(Params^[0])^);
end;

procedure _LapeTanH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := TanH(PlpFloat(Params^[0])^);
end;

procedure _LapeArcCosH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcCosH(PlpFloat(Params^[0])^);
end;

procedure _LapeArcSinH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcSinH(PlpFloat(Params^[0])^);
end;

procedure _LapeArcTanH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := ArcTanH(PlpFloat(Params^[0])^);
end;

procedure _LapeSinCos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SinCos(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^, PlpFloat(Params^[2])^);
end;

procedure _LapeDivMod(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DivMod(PUInt32(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^);
end;

procedure _LapeRandomRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pint64(Result)^ := PInt64(Params^[0])^ + Random(PInt64(Params^[1])^ - PInt64(Params^[0])^ + 1);
end;

procedure _LapeRandomRangeF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := PlpFloat(Params^[0])^ + Random() * (PlpFloat(Params^[1])^ - PlpFloat(Params^[0])^);
end;

procedure _LapeRandom(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pint64(Result)^ := Random(PInt64(Params^[0])^);
end;

procedure _LapeRandomF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := Random();
end;

procedure _LapeRandomize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Randomize();
end;

procedure _LapeEnsureRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := EnsureRange(PInt64(Params^[0])^, PInt64(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeEnsureRangeF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := EnsureRange(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^, PlpFloat(Params^[2])^);
end;

procedure _LapeInRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := InRange(PInt64(Params^[0])^, PInt64(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeInRangeF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := InRange(PlpFloat(Params^[0])^, PlpFloat(Params^[1])^, PlpFloat(Params^[2])^);
end;
