{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the headers for the Variant related wrappers.
}
{%MainUnit lpeval.pas}

procedure _LapeUpperCase(const Params: PParamArray; const Result: Pointer);
procedure _LapeLowerCase(const Params: PParamArray; const Result: Pointer);
procedure _LapeUpCaseA(const Params: PParamArray; const Result: Pointer);
procedure _LapeUpCaseW(const Params: PParamArray; const Result: Pointer);

procedure _LapeCompareStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeCompareMem(const Params: PParamArray; const Result: Pointer);
procedure _LapeCompareText(const Params: PParamArray; const Result: Pointer);
procedure _LapeSameText(const Params: PParamArray; const Result: Pointer);

procedure _LapeAnsiUpperCase(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiLowerCase(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiCompareStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiCompareText(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiSameText(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiSameStr(const Params: PParamArray; const Result: Pointer);

procedure _LapeTrim(const Params: PParamArray; const Result: Pointer);
procedure _LapeTrimLeft(const Params: PParamArray; const Result: Pointer);
procedure _LapeTrimRight(const Params: PParamArray; const Result: Pointer);
procedure _LapeQuotedStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiQuotedStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeAnsiDequotedStr(const Params: PParamArray; const Result: Pointer);

procedure _LapeWrapText(const Params: PParamArray; const Result: Pointer);
procedure _LapeAdjustLineBreaks(const Params: PParamArray; const Result: Pointer);

procedure _LapeIntToHex(const Params: PParamArray; const Result: Pointer);
procedure _LapeUIntToHex(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToInt(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToIntDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToInt64(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToInt64Def(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToUInt64(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToUInt64Def(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToFloat(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToFloatDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToCurr(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToCurrDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToBool(const Params: PParamArray; const Result: Pointer);
procedure _LapeBoolToStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeStrToBoolDef(const Params: PParamArray; const Result: Pointer);

procedure _LapeFormat(const Params: PParamArray; const Result: Pointer);
procedure _LapeFormatFloat(const Params: PParamArray; const Result: Pointer);
procedure _LapeFormatCurr(const Params: PParamArray; const Result: Pointer);

procedure _LapeLastDelimiter(const Params: PParamArray; const Result: Pointer);
procedure _LapeStringReplace(const Params: PParamArray; const Result: Pointer);
procedure _LapeIsDelimiter(const Params: PParamArray; const Result: Pointer);

procedure _LapePos(const Params: PParamArray; const Result: Pointer);
procedure _LapeCopy(const Params: PParamArray; const Result: Pointer);
procedure _LapeInsert(const Params: PParamArray);
procedure _LapeDelete(const Params: PParamArray);
procedure _LapeStringOfChar(const Params: PParamArray; const Result: Pointer);
