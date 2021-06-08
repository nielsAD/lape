{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for Variant related functions.
}
{%MainUnit lpeval.pas}

type
  PVarType = ^TVarType;
procedure _LapeVarType(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVarType(Result)^ := VarType(PVariant(Params^[0])^);
end;

procedure _LapeVarAsType(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarAsType(PVariant(Params^[0])^, PVarType(Params^[1])^);
end;

procedure _LapeVarIsByRef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsByRef(PVariant(Params^[0])^);
end;

procedure _LapeVarIsEmpty(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsEmpty(PVariant(Params^[0])^);
end;

procedure _LapeVarIsNull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsNull(PVariant(Params^[0])^);
end;

procedure _LapeVarIsClear(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsClear(PVariant(Params^[0])^);
end;

type
  PRESULT = ^HRESULT;
procedure _LapeVarIsError(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsError(PVariant(Params^[0])^, PRESULT(Params^[1])^);
end;

procedure _LapeVarAsError(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarAsError(PRESULT(Params^[0])^);
end;

procedure _LapeVarIsCustom(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsCustom(PVariant(Params^[0])^);
end;

procedure _LapeVarIsOrdinal(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsOrdinal(PVariant(Params^[0])^);
end;

procedure _LapeVarIsFloat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsFloat(PVariant(Params^[0])^);
end;

procedure _LapeVarIsNumeric(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsNumeric(PVariant(Params^[0])^);
end;

procedure _LapeVarIsStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsStr(PVariant(Params^[0])^);
end;

procedure _LapeVarIsArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarIsArray(PVariant(Params^[0])^, PEvalBool(Params^[1])^);
end;

procedure _LapeVarToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(VarToStr(PVariant(Params^[0])^));
end;

procedure _LapeVarToStrDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(VarToStrDef(PVariant(Params^[0])^, string(PlpString(Params^[1])^)));
end;

procedure _LapeVarToWideStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := VarToWideStr(PVariant(Params^[0])^);
end;

procedure _LapeVarToWideStrDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := VarToWideStrDef(PVariant(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeVarToUnicodeStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUnicodeString(Result)^ := {$IFDEF FPC}VarToUnicodeStr{$ELSE}VarToStr{$ENDIF}(PVariant(Params^[0])^);
end;

procedure _LapeVarToUnicodeStrDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUnicodeString(Result)^ := {$IFDEF FPC}VarToUnicodeStrDef{$ELSE}VarToStrDef{$ENDIF}(PVariant(Params^[0])^, PUnicodeString(Params^[1])^);
end;

procedure _LapeVarToDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := VarToDateTime(PVariant(Params^[0])^);
end;

procedure _LapeVarFromDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarFromDateTime(PDateTime(Params^[0])^);
end;

procedure _LapeVarInRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarInRange(PVariant(Params^[0])^, PVariant(Params^[1])^, PVariant(Params^[2])^);
end;

procedure _LapeVarEnsureRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarEnsureRange(PVariant(Params^[0])^, PVariant(Params^[1])^, PVariant(Params^[2])^);
end;

procedure _LapeVarSameValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarSameValue(PVariant(Params^[0])^, PVariant(Params^[1])^);
end;

procedure _LapeVarCompareValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLapeSmallEnum(Result)^ := ELapeSmallEnum(VarCompareValue(PVariant(Params^[0])^, PVariant(Params^[1])^));
end;

type
  TSizeIntArray = array of {$IFDEF FPC}SizeInt{$ELSE}Integer{$ENDIF};
  PSizeIntArray = ^TSizeIntArray;

procedure _LapeVarTypeIsValidArrayType(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarTypeIsValidArrayType(PVarType(Params^[0])^);
end;

procedure _LapeVarTypeIsValidElementType(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := VarTypeIsValidElementType(PVarType(Params^[0])^);
end;

procedure _LapeVarArrayCreate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarArrayCreate(PSizeIntArray(Params^[0])^, PVarType(Params^[1])^);
end;

procedure _LapeVarArrayOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarArrayOf(PVariantArray(Params^[0])^);
end;

procedure _LapeVarArrayRedim(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  VarArrayRedim(PVariant(Params^[0])^, PSizeInt(Params^[0])^);
end;

procedure _LapeVarArrayAsPSafeArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := VarArrayAsPSafeArray(PVariant(Params^[0])^);
end;

procedure _LapeVarCopyNoInd(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  VarCopyNoInd(PVariant(Params^[0])^, PVariant(Params^[1])^);
end;

procedure _LapeVarArrayDimCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := VarArrayDimCount(PVariant(Params^[0])^);
end;

procedure _LapeVarArrayLowBound(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := VarArrayLowBound(PVariant(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeVarArrayHighBound(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := VarArrayHighBound(PVariant(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeVarArrayLock(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := VarArrayLock(PVariant(Params^[0])^);
end;

procedure _LapeVarArrayUnlock(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  VarArrayUnlock(PVariant(Params^[0])^);
end;

procedure _LapeVarArrayRef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarArrayRef(PVariant(Params^[0])^);
end;

type
  TIntArray = array of Int32;
  PIntArray = ^TIntArray;

procedure _LapeVarArrayGet(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVariant(Result)^ := VarArrayGet(PVariant(Params^[0])^, PIntArray(Params^[1])^);
end;

procedure _LapeVarArraySet(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  VarArrayPut(PVariant(Params^[0])^, PVariant(Params^[1])^, PIntArray(Params^[2])^);
end;
