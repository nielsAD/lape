{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for Variant related functions.
}
{%MainUnit lpcompiler.pas}

addGlobalVar(Null, 'Null').isConstant := True;
addGlobalVar(Unassigned, 'Unassigned').isConstant := True;

addGlobalType(getBaseType(DetermineIntType(SizeOf(HRESULT), True)).createCopy(), 'HRESULT');
addGlobalType(getBaseType(DetermineIntType(SizeOf(TVarType), False)).createCopy(), 'TVarType');
addGlobalType('(vrEqual, vrLessThan, vrGreaterThan, vrNotEqual)', 'TVariantRelationship');

addGlobalVar(VarEmpty, 'VarEmpty').isConstant := True;
addGlobalVar(VarNull, 'VarNull').isConstant := True;
addGlobalVar(VarSmallInt, 'VarSmallInt').isConstant := True;
addGlobalVar(VarInteger, 'VarInteger').isConstant := True;
addGlobalVar(VarSingle, 'VarSingle').isConstant := True;
addGlobalVar(VarDouble, 'VarDouble').isConstant := True;
addGlobalVar(VarDate, 'VarDate').isConstant := True;
addGlobalVar(VarCurrency, 'VarCurrency').isConstant := True;
addGlobalVar(VarOleStr, 'VarOleStr').isConstant := True;
addGlobalVar(VarDispatch, 'VarDispatch').isConstant := True;
addGlobalVar(VarError, 'VarError').isConstant := True;
addGlobalVar(VarBoolean, 'VarBoolean').isConstant := True;
addGlobalVar(VarVariant, 'VarVariant').isConstant := True;
addGlobalVar(VarUnknown, 'VarUnknown').isConstant := True;
addGlobalVar(VarShortInt, 'VarShortInt').isConstant := True;
addGlobalVar(VarByte, 'VarByte').isConstant := True;
addGlobalVar(VarWord, 'VarWord').isConstant := True;
addGlobalVar(VarLongWord, 'VarLongWord').isConstant := True;
addGlobalVar(VarInt64, 'VarInt64').isConstant := True;
addGlobalVar(VarStrArg, 'VarStrArg').isConstant := True;
addGlobalVar(VarString, 'VarString').isConstant := True;
addGlobalVar(VarAny, 'VarAny').isConstant := True;
addGlobalVar({$IFDEF FPC}$102{$ELSE}VarUString{$ENDIF}, 'VarUString').isConstant := True;
addGlobalVar({$IFDEF FPC}VarQWord{$ELSE}varUInt64{$ENDIF}, 'VarUInt64').isConstant := True;

addGlobalVar(VarTypeMask, 'VarTypeMask').isConstant := True;
addGlobalVar(VarArray, 'VarArray').isConstant := True;
addGlobalVar(VarByRef, 'VarByRef').isConstant := True;

addGlobalFunc('function VarType(const V: Variant): TVarType;', @_LapeVarType);
addGlobalFunc('function VarAsType(const V: Variant; aVarType: TVarType): Variant;', @_LapeVarAsType);
addGlobalFunc('function VarIsByRef(const V: Variant): EvalBool;', @_LapeVarIsByRef);
addGlobalFunc('function VarIsEmpty(const V: Variant): EvalBool;', @_LapeVarIsEmpty);
addGlobalFunc('function VarIsNull(const V: Variant): EvalBool;', @_LapeVarIsNull);
addGlobalFunc('function VarIsClear(const V: Variant): EvalBool;', @_LapeVarIsClear);

addGlobalFunc('function VarIsError(const V: Variant; out AResult: HRESULT): EvalBool;', @_LapeVarIsError);
addGlobalFunc('function VarAsError(AResult: HRESULT): Variant;', @_LapeVarAsError);

addGlobalFunc('function VarIsCustom(const V: Variant): EvalBool;', @_LapeVarIsCustom);
addGlobalFunc('function VarIsOrdinal(const V: Variant): EvalBool;', @_LapeVarIsOrdinal);
addGlobalFunc('function VarIsFloat(const V: Variant): EvalBool;', @_LapeVarIsFloat);
addGlobalFunc('function VarIsNumeric(const V: Variant): EvalBool;', @_LapeVarIsNumeric);
addGlobalFunc('function VarIsStr(const V: Variant): EvalBool;', @_LapeVarIsStr);
addGlobalFunc('function VarIsArray(const A: Variant; AResolveByRef: EvalBool = True): EvalBool;', @_LapeVarIsArray);

addGlobalFunc('function VarToStr(const V: Variant): string;', @_LapeVarToStr);
addGlobalFunc('function VarToStrDef(const V: Variant; ADefault: string): string;', @_LapeVarToStrDef);
addGlobalFunc('function VarToWideStr(const V: Variant): WideString;', @_LapeVarToWideStr);
addGlobalFunc('function VarToWideStrDef(const V: Variant; ADefault: WideString): WideString;', @_LapeVarToWideStrDef);
addGlobalFunc('function VarToUnicodeStr(const V: Variant): UnicodeString;', @_LapeVarToUnicodeStr);
addGlobalFunc('function VarToUnicodeStrDef(const V: Variant; ADefault: UnicodeString): UnicodeString;', @_LapeVarToUnicodeStrDef);
addGlobalFunc('function VarToDateTime(const V: Variant): TDateTime;', @_LapeVarToDateTime);
addGlobalFunc('function VarFromDateTime(DateTime: TDateTime): Variant;', @_LapeVarFromDateTime);

addGlobalFunc('function VarInRange(const AValue, AMin, AMax: Variant): EvalBool;', @_LapeVarInRange);
addGlobalFunc('function VarEnsureRange(const AValue, AMin, AMax: Variant): Variant;', @_LapeVarEnsureRange);
addGlobalFunc('function VarSameValue(const A, B: Variant): EvalBool;', @_LapeVarSameValue);
addGlobalFunc('function VarCompareValue(const A, B: Variant): TVariantRelationship;', @_LapeVarCompareValue);

addGlobalFunc('function VarTypeIsValidArrayType(aVarType: TVarType): EvalBool;', @_LapeVarTypeIsValidArrayType);
addGlobalFunc('function VarTypeIsValidElementType(aVarType: TVarType): EvalBool;', @_LapeVarTypeIsValidElementType);

addGlobalFunc('function VarArrayCreate(Bounds: array of SizeInt; aVarType: TVarType): Variant;', @_LapeVarArrayCreate);
addGlobalFunc('function VarArrayOf(Values: array of Variant): Variant;', @_LapeVarArrayOf);
addGlobalFunc('procedure VarArrayRedim(var A: Variant; HighBound: SizeInt);', @_LapeVarArrayRedim);

addGlobalFunc('function VarArrayAsPSafeArray(const A: Variant): Pointer;', @_LapeVarArrayAsPSafeArray);
addGlobalFunc('procedure VarCopyNoInd(var Dest: Variant; const Source: Variant);', @_LapeVarCopyNoInd);

addGlobalFunc('function VarArrayDimCount(const A: Variant): SizeInt;', @_LapeVarArrayDimCount);
addGlobalFunc('function VarArrayLowBound(const A: Variant; Dim: SizeInt): SizeInt;', @_LapeVarArrayLowBound);
addGlobalFunc('function VarArrayHighBound(const A: Variant; Dim: SizeInt): SizeInt;', @_LapeVarArrayHighBound);

addGlobalFunc('function VarArrayLock(const A: Variant): Pointer;', @_LapeVarArrayLock);
addGlobalFunc('procedure VarArrayUnlock(const A: Variant);', @_LapeVarArrayUnlock);
addGlobalFunc('function VarArrayRef(const A: Variant): Variant;', @_LapeVarArrayRef);

addGlobalFunc('function VarArrayGet(const A: Variant; Indices: array of Int32): Variant;', @_LapeVarArrayGet);
addGlobalFunc('procedure VarArraySet(var A: Variant; const Value: Variant; Indices: array of Int32);', @_LapeVarArraySet);
