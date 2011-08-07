{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the headers for the Variant related wrappers.
}
{%MainUnit lpeval.pas}

procedure _LapeVarType(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarAsType(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarIsByRef(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsEmpty(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsNull(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsClear(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarIsError(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarAsError(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarIsCustom(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsOrdinal(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsFloat(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsNumeric(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarIsArray(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarToStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarToStrDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarToWideStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarToWideStrDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarToUnicodeStr(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarToUnicodeStrDef(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarToDateTime(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarFromDateTime(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarInRange(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarEnsureRange(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarSameValue(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarCompareValue(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarTypeIsValidArrayType(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarTypeIsValidElementType(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarArrayCreate(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarArrayOf(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarArrayRedim(const Params: PParamArray);

procedure _LapeVarArrayAsPSafeArray(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarCopyNoInd(const Params: PParamArray);

procedure _LapeVarArrayDimCount(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarArrayLowBound(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarArrayHighBound(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarArrayLock(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarArrayUnlock(const Params: PParamArray);
procedure _LapeVarArrayRef(const Params: PParamArray; const Result: Pointer);

procedure _LapeVarArrayGet(const Params: PParamArray; const Result: Pointer);
procedure _LapeVarArraySet(const Params: PParamArray);
