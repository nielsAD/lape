{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the wrappers for math related functions.
}
{%MainUnit lpcompiler.pas}

addGlobalVar(Pi, 'Pi').isConstant := True;
addGlobalVar(ltInt32, @RandSeed, 'RandSeed');

addGlobalFunc('function Abs(d: Extended): Extended; overload;', @_LapeAbs);
addGlobalFunc('function Abs(d: Int64): Int64; overload;', @_LapeAbsI);
addGlobalFunc('function Sqr(d: Extended): Extended;', @_LapeSqr);
addGlobalFunc('function Sqrt(d: Extended): Extended;', @_LapeSqrt);
addGlobalFunc('function ArcTan(d: Extended): Extended;', @_LapeArcTan);
addGlobalFunc('function Ln(d: Extended): Extended;', @_LapeLn);
addGlobalFunc('function Sin(d: Extended): Extended;', @_LapeSin);
addGlobalFunc('function Cos(d: Extended): Extended;', @_LapeCos);
addGlobalFunc('function Exp(d: Extended): Extended;', @_LapeExp);
addGlobalFunc('function Round(d: Extended): Int64; overload;', @_LapeRound);
addGlobalFunc('function Round(d: Extended; Precision: Int8): Extended; overload;', @_LapeRoundTo);
addGlobalFunc('function Frac(d: Extended): Extended;', @_LapeFrac);
addGlobalFunc('function Int(d: Extended): Extended;', @_LapeInt);
addGlobalFunc('function Trunc(d: Extended): Int64;', @_LapeTrunc);
addGlobalFunc('function Ceil(d: Extended): Int64;', @_LapeCeil);
addGlobalFunc('function Floor(d: Extended): Int64;', @_LapeFloor);

addGlobalFunc('function Random(Min, Max: Int64): Int64; overload;', @_LapeRandomRange);
addGlobalFunc('function Random(l: Int64): Int64; overload;', @_LapeRandom);
addGlobalFunc('function Random: Extended; overload;', @_LapeRandomF);
addGlobalFunc('procedure Randomize;', @_LapeRandomize);
