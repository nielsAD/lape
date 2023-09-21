{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for math related functions.
}
{%MainUnit lpcompiler.pas}

addGlobalVar(Pi, 'Pi').isConstant := True;
addGlobalVar(ltUInt32, @RandSeed, 'RandSeed');

{$IFDEF LAPE_NOEXTENDED}
addGlobalFunc('function Min(x,y: Int64): Int64; overload;', @_LapeMin);
addGlobalFunc('function Min(x,y: Double): Double; overload;', @_LapeMinF);
addGlobalFunc('function Max(x,y: Int64): Int64; overload;', @_LapeMax);
addGlobalFunc('function Max(x,y: Double): Double; overload;', @_LapeMaxF);
addGlobalFunc('function EnsureRange(Value, Min, Max: Int64): Int64; overload;', @_LapeEnsureRange);
addGlobalFunc('function EnsureRange(Value, Min, Max: Double): Double; overload;', @_LapeEnsureRangeF);
addGlobalFunc('function InRange(Value, Min, Max: Int64): Boolean; overload;', @_LapeInRange);
addGlobalFunc('function InRange(Value, Min, Max: Double): Boolean; overload;', @_LapeInRangeF);

addGlobalFunc('function Abs(x: Double): Double; overload;', @_LapeAbs);
addGlobalFunc('function Abs(x: Int64): Int64; overload;', @_LapeAbsI);
addGlobalFunc('function Sign(AValue: Int64): Int8; overload;', @_LapeSign);
addGlobalFunc('function Sign(AValue: Double): Int8; overload;', @_LapeSignF);
addGlobalFunc('function Power(Base, Exponent: Double): Double;', @_LapePower);
addGlobalFunc('function Sqr(x: Double): Double; overload;', @_LapeSqr);
addGlobalFunc('function Sqr(x: Int64): Int64; overload;', @_LapeSqrI);
addGlobalFunc('function Sqrt(x: Double): Double;', @_LapeSqrt);
addGlobalFunc('function ArcTan(x: Double): Double;', @_LapeArcTan);
addGlobalFunc('function Ln(x: Double): Double;', @_LapeLn);
addGlobalFunc('function Sin(x: Double): Double;', @_LapeSin);
addGlobalFunc('function Cos(x: Double): Double;', @_LapeCos);
addGlobalFunc('function Exp(x: Double): Double;', @_LapeExp);
addGlobalFunc('function Hypot(x,y: Double): Double', @_LapeHypot);
addGlobalFunc('function ArcTan2(x,y: Double): Double', @_LapeArcTan2);
addGlobalFunc('function Tan(x: Double): Double', @_LapeTan);
addGlobalFunc('function ArcSin(x: Double): Double', @_LapeArcSin);
addGlobalFunc('function ArcCos(x: Double): Double', @_LapeArcCos);
addGlobalFunc('function Cotan(x: Double): Double', @_LapeCotan);
addGlobalFunc('function Secant(x: Double): Double', @_LapeSecant);
addGlobalFunc('function Cosecant(x: Double): Double', @_LapeCosecant);
addGlobalFunc('function Round(x: Double): Int64; overload;', @_LapeRound);
addGlobalFunc('function Round(x: Double; Precision: Int8): Double; overload;', @_LapeRoundTo);
addGlobalFunc('function Frac(x: Double): Double;', @_LapeFrac);
addGlobalFunc('function Int(x: Double): Double;', @_LapeInt);
addGlobalFunc('function Trunc(x: Double): Int64;', @_LapeTrunc);
addGlobalFunc('function Ceil(x: Double): Int64; overload;', @_LapeCeil);
addGlobalFunc('function Ceil(x: Double; Precision: Int8): Double; overload;', @_LapeCeilTo);
addGlobalFunc('function Floor(x: Double): Int64;', @_LapeFloor);
addGlobalFunc('function CosH(x: Double): Double;', @_LapeCosH);
addGlobalFunc('function SinH(x: Double): Double;', @_LapeSinH);
addGlobalFunc('function TanH(x: Double): Double;', @_LapeTanH);
addGlobalFunc('function ArcCosH(x: Double): Double;', @_LapeArcCosH);
addGlobalFunc('function ArcSinH(x: Double): Double;', @_LapeArcSinH);
addGlobalFunc('function ArcTanH(x: Double): Double;', @_LapeArcTanH);
addGlobalFunc('procedure SinCos(theta: Double; out sinus, cosinus: Double);', @_LapeSinCos);
addGlobalFunc('procedure DivMod(Dividend: UInt32; Divisor: UInt16; var Result, Remainder: UInt16);', @_LapeDivMod);

addGlobalFunc('function Random(min, max: Int64): Int64; overload;', @_LapeRandomRange);
addGlobalFunc('function Random(min, max: Double): Double; overload;', @_LapeRandomRangeF);
addGlobalFunc('function Random(l: Int64): Int64; overload;', @_LapeRandom);
addGlobalFunc('function Random: Double; overload;', @_LapeRandomF);
addGlobalFunc('procedure Randomize;', @_LapeRandomize);
{$ELSE}
addGlobalFunc('function Min(x,y: Int64): Int64; overload;', @_LapeMin);
addGlobalFunc('function Min(x,y: Extended): Extended; overload;', @_LapeMinF);
addGlobalFunc('function Max(x,y: Int64): Int64; overload;', @_LapeMax);
addGlobalFunc('function Max(x,y: Extended): Extended; overload;', @_LapeMaxF);
addGlobalFunc('function EnsureRange(Value, Min, Max: Int64): Int64; overload;', @_LapeEnsureRange);
addGlobalFunc('function EnsureRange(Value, Min, Max: Extended): Extended; overload;', @_LapeEnsureRangeF);
addGlobalFunc('function InRange(Value, Min, Max: Int64): Boolean; overload;', @_LapeInRange);
addGlobalFunc('function InRange(Value, Min, Max: Extended): Boolean; overload;', @_LapeInRangeF);

addGlobalFunc('function Abs(x: Extended): Extended; overload;', @_LapeAbs);
addGlobalFunc('function Abs(x: Int64): Int64; overload;', @_LapeAbsI);
addGlobalFunc('function Sign(AValue: Int64): Int8; overload;', @_LapeSign);
addGlobalFunc('function Sign(AValue: Extended): Int8; overload;', @_LapeSignF);
addGlobalFunc('function Power(Base, Exponent: Extended): Extended;', @_LapePower);
addGlobalFunc('function Sqr(x: Extended): Extended; overload;', @_LapeSqr);
addGlobalFunc('function Sqr(x: Int64): Int64; overload;', @_LapeSqrI);
addGlobalFunc('function Sqrt(x: Extended): Extended;', @_LapeSqrt);
addGlobalFunc('function ArcTan(x: Extended): Extended;', @_LapeArcTan);
addGlobalFunc('function Ln(x: Extended): Extended;', @_LapeLn);
addGlobalFunc('function Sin(x: Extended): Extended;', @_LapeSin);
addGlobalFunc('function Cos(x: Extended): Extended;', @_LapeCos);
addGlobalFunc('function Exp(x: Extended): Extended;', @_LapeExp);
addGlobalFunc('function Hypot(x,y: Extended): Extended', @_LapeHypot);
addGlobalFunc('function ArcTan2(x,y: Extended): Extended', @_LapeArcTan2);
addGlobalFunc('function Tan(x: Extended): Extended', @_LapeTan);
addGlobalFunc('function ArcSin(x: Extended): Extended', @_LapeArcSin);
addGlobalFunc('function ArcCos(x: Extended): Extended', @_LapeArcCos);
addGlobalFunc('function Cotan(x: Extended): Extended', @_LapeCotan);
addGlobalFunc('function Secant(x: Extended): Extended', @_LapeSecant);
addGlobalFunc('function Cosecant(x: Extended): Extended', @_LapeCosecant);
addGlobalFunc('function Round(x: Extended): Int64; overload;', @_LapeRound);
addGlobalFunc('function Round(x: Extended; Precision: Int8): Extended; overload;', @_LapeRoundTo);
addGlobalFunc('function Frac(x: Extended): Extended;', @_LapeFrac);
addGlobalFunc('function Int(x: Extended): Extended;', @_LapeInt);
addGlobalFunc('function Trunc(x: Extended): Int64;', @_LapeTrunc);
addGlobalFunc('function Ceil(x: Extended): Int64; overload;', @_LapeCeil);
addGlobalFunc('function Ceil(x: Extended; Precision: Int8): Extended; overload;', @_LapeCeilTo);
addGlobalFunc('function Floor(x: Extended): Int64;', @_LapeFloor);
addGlobalFunc('function CosH(x: Extended): Extended;', @_LapeCosH);
addGlobalFunc('function SinH(x: Extended): Extended;', @_LapeSinH);
addGlobalFunc('function TanH(x: Extended): Extended;', @_LapeTanH);
addGlobalFunc('function ArcCosH(x: Extended): Extended;', @_LapeArcCosH);
addGlobalFunc('function ArcSinH(x: Extended): Extended;', @_LapeArcSinH);
addGlobalFunc('function ArcTanH(x: Extended): Extended;', @_LapeArcTanH);
addGlobalFunc('procedure SinCos(theta: Extended; out sinus, cosinus: Extended);', @_LapeSinCos);
addGlobalFunc('procedure DivMod(Dividend: UInt32; Divisor: UInt16; var Result, Remainder: UInt16);', @_LapeDivMod);

addGlobalFunc('function Random(min, max: Int64): Int64; overload;', @_LapeRandomRange);
addGlobalFunc('function Random(min, max: Extended): Extended; overload;', @_LapeRandomRangeF);
addGlobalFunc('function Random(l: Int64): Int64; overload;', @_LapeRandom);
addGlobalFunc('function Random: Extended; overload;', @_LapeRandomF);
addGlobalFunc('procedure Randomize;', @_LapeRandomize);
{$ENDIF}
