{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the headers for the math related wrappers.
}
{%MainUnit lpeval.pas}

procedure _LapeAbs(const Params: PParamArray; const Result: Pointer);
procedure _LapeAbsI(const Params: PParamArray; const Result: Pointer);
procedure _LapeSqr(const Params: PParamArray; const Result: Pointer);
procedure _LapeSqrt(const Params: PParamArray; const Result: Pointer);
procedure _LapeArcTan(const Params: PParamArray; const Result: Pointer);
procedure _LapeLn(const Params: PParamArray; const Result: Pointer);
procedure _LapeSin(const Params: PParamArray; const Result: Pointer);
procedure _LapeCos(const Params: PParamArray; const Result: Pointer);
procedure _LapeExp(const Params: PParamArray; const Result: Pointer);
procedure _LapeRound(const Params: PParamArray; const Result: Pointer);
procedure _LapeFrac(const Params: PParamArray; const Result: Pointer);
procedure _LapeInt(const Params: PParamArray; const Result: Pointer);
procedure _LapeTrunc(const Params: PParamArray; const Result: Pointer);

procedure _LapeRandomRange(const Params: PParamArray; const Result: Pointer);
procedure _LapeRandom(const Params: PParamArray; const Result: Pointer);
procedure _LapeRandomF(const Params: PParamArray; const Result: Pointer);
procedure _LapeRandomize(const Params: PParamArray);
