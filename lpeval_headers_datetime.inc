{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the headers for the DateTime related wrappers.
}
{%MainUnit lpeval.pas}

procedure _LapeEncodeDate(const Params: PParamArray; const Result: Pointer);
procedure _LapeEncodeTime(const Params: PParamArray; const Result: Pointer);
procedure _LapeDecodeDate(const Params: PParamArray);
procedure _LapeDecodeDateFully(const Params: PParamArray; const Result: Pointer);
procedure _LapeDecodeTime(const Params: PParamArray);

procedure _LapeDate(const Params: PParamArray; const Result: Pointer);
procedure _LapeTime(const Params: PParamArray; const Result: Pointer);
procedure _LapeNow(const Params: PParamArray; const Result: Pointer);

procedure _LapeGetTickCount(const Params: PParamArray; const Result: Pointer);
procedure _LapeSleep(const Params: PParamArray);

procedure _LapeReplaceTime(const Params: PParamArray);
procedure _LapeReplaceDate(const Params: PParamArray);

procedure _LapeFormatDateTime(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToDate(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToDateDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToTime(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToTimeDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToDateTime(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToDateTimeDef(const Params: PParamArray; const Result: Pointer);
