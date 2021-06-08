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
  PExtended(Result)^ := Min(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Max(PInt64(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeMaxF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Max(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeAbs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Abs(PExtended(Params^[0])^);
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
  PInt8(Result)^ := Sign(PExtended(Params^[0])^);
end;

procedure _LapePower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Power(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeSqr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sqr(PExtended(Params^[0])^);
end;

procedure _LapeSqrI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PInt64(Params^[0])^*PInt64(Params^[0])^;
end;

procedure _LapeSqrt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sqrt(PExtended(Params^[0])^);
end;

procedure _LapeArcTan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcTan(PExtended(Params^[0])^);
end;

procedure _LapeLn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Ln(PExtended(Params^[0])^);
end;

procedure _LapeSin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Sin(PExtended(Params^[0])^);
end;

procedure _LapeCos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cos(PExtended(Params^[0])^);
end;

procedure _LapeExp(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Exp(PExtended(Params^[0])^);
end;

procedure _LapeHypot(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Hypot(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeArcTan2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcTan2(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeTan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Tan(PExtended(Params^[0])^);
end;

procedure _LapeArcSin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcSin(PExtended(Params^[0])^);
end;

procedure _LapeArcCos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcCos(PExtended(Params^[0])^);
end;

procedure _LapeCotan(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cotan(PExtended(Params^[0])^);
end;

procedure _LapeSecant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Secant(PExtended(Params^[0])^);
end;

procedure _LapeCosecant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Cosecant(PExtended(Params^[0])^);
end;

procedure _LapeRound(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Round(PExtended(Params^[0])^);
end;

procedure _LapeRoundTo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := RoundTo(PExtended(Params^[0])^, -PInt8(Params^[1])^);
end;

procedure _LapeFrac(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Frac(PExtended(Params^[0])^);
end;

procedure _LapeInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Int(PExtended(Params^[0])^);
end;

procedure _LapeTrunc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Trunc(PExtended(Params^[0])^);
end;

procedure _LapeCeil(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Ceil(PExtended(Params^[0])^);
end;

procedure _LapeFloor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := Floor(PExtended(Params^[0])^);
end;

procedure _LapeCosH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := CosH(PExtended(Params^[0])^);
end;

procedure _LapeSinH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := SinH(PExtended(Params^[0])^);
end;

procedure _LapeTanH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := TanH(PExtended(Params^[0])^);
end;

procedure _LapeArcCosH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcCosH(PExtended(Params^[0])^);
end;

procedure _LapeArcSinH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcSinH(PExtended(Params^[0])^);
end;

procedure _LapeArcTanH(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := ArcTanH(PExtended(Params^[0])^);
end;


procedure _LapeRandomRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pint64(Result)^ := PInt64(Params^[0])^ + Random(PInt64(Params^[1])^ - PInt64(Params^[0])^ + 1);
end;

procedure _LapeRandomRangeF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := PExtended(Params^[0])^ + Random()*(PExtended(Params^[1])^ - PExtended(Params^[0])^);
end;

procedure _LapeRandom(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pint64(Result)^ := Random(PInt64(Params^[0])^);
end;

procedure _LapeRandomF(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Random();
end;

procedure _LapeRandomize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Randomize();
end;
