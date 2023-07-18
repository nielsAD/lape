{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for string related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeUTF8EncodeW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := UTF8Encode(PWideString(Params^[0])^);
end;

procedure _LapeUTF8EncodeU(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := UTF8Encode(PUnicodeString(Params^[0])^);
end;

procedure _LapeUTF8DecodeW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := UTF8Decode(PAnsiString(Params^[0])^);
end;

procedure _LapeUTF8DecodeU(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUnicodeString(Result)^ := UTF8Decode(PAnsiString(Params^[0])^);
end;

procedure _LapeUpperCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := UpperCase(PlpString(Params^[0])^);
end;

procedure _LapeLowerCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := LowerCase(PlpString(Params^[0])^);
end;

procedure _LapeUpCaseA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiChar(Result)^ := UpCase(PAnsiChar(Params^[0])^);
end;

procedure _LapeUpCaseW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideChar(Result)^ := UpCase(PWideChar(Params^[0])^);
end;

procedure _LapeCompareStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := CompareStr(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeCompareText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := CompareText(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeSameText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := SameText(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeAnsiUpperCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := AnsiUpperCase(PlpString(Params^[0])^);
end;

procedure _LapeAnsiLowerCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := AnsiLowerCase(PlpString(Params^[0])^);
end;

procedure _LapeAnsiCompareStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := AnsiCompareStr(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeAnsiCompareText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := AnsiCompareText(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeAnsiSameText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := AnsiSameText(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeAnsiSameStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := AnsiSameStr(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeWideUpperCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := WideUpperCase(PWideString(Params^[0])^);
end;

procedure _LapeWideLowerCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := WideLowerCase(PWideString(Params^[0])^);
end;

procedure _LapeWideCompareStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := WideCompareStr(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeWideCompareText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := WideCompareText(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeWideSameText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := WideSameText(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeWideSameStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := WideSameStr(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeTrim(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := Trim(PlpString(Params^[0])^);
end;

procedure _LapeTrimLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := TrimLeft(PlpString(Params^[0])^);
end;

procedure _LapeTrimRight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := TrimRight(PlpString(Params^[0])^);
end;

procedure _LapePadL(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := StringOfChar(PlpChar(Params^[2])^, PUInt32(Params^[1])^ - UInt32(Length(PlpString(Params^[0])^))) + PlpString(Params^[0])^;
end;

procedure _LapePadR(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PlpString(Params^[0])^ + StringOfChar(PlpChar(Params^[2])^, PUInt32(Params^[1])^ - UInt32(Length(PlpString(Params^[0])^)));
end;

procedure _LapeQuotedStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := QuotedStr(PlpString(Params^[0])^);
end;

procedure _LapeIntToHex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(IntToHex(PInt64(Params^[0])^, PInt32(Params^[1])^));
end;

procedure _LapeUIntToHex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(IntToHex(PUInt64(Params^[0])^, PInt32(Params^[1])^));
end;

procedure _LapeStrToInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := StrToInt(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToIntDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := StrToIntDef(string(PlpString(Params^[0])^), PInt32(Params^[1])^);
end;

procedure _LapeStrToInt64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := StrToInt64(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToInt64Def(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := StrToInt64Def(string(PlpString(Params^[0])^), PInt64(Params^[1])^);
end;

procedure _LapeStrToUInt64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := StrToUInt64(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToUInt64Def(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt64(Result)^ := StrToUInt64Def(string(PlpString(Params^[0])^), PUInt64(Params^[1])^);
end;

procedure _LapeStrToFloat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := StrToFloat(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToFloatDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpFloat(Result)^ := StrToFloatDef(string(PlpString(Params^[0])^), PlpFloat(Params^[1])^);
end;

procedure _LapeStrToCurr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCurrency(Result)^ := StrToCurr(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToCurrDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCurrency(Result)^ := StrToCurrDef(string(PlpString(Params^[0])^), PCurrency(Params^[1])^);
end;

procedure _LapeStrToBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := StrToBool(string(PlpString(Params^[0])^));
end;

procedure _LapeBoolToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PEvalBool(Params^[0])^ then
    PlpString(Result)^ := PlpString(Params^[1])^
  else
    PlpString(Result)^ := PlpString(Params^[2])^;
end;

procedure _LapeStrToBoolDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := StrToBoolDef(string(PlpString(Params^[0])^), PEvalBool(Params^[1])^);
end;

procedure _LapeFormat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ConstArr: TVarRecList;
begin
  ConstArr := VariantArrToConstArr(PVariantArray(Params^[1])^);
  PlpString(Result)^ := Format(PlpString(Params^[0])^, ConstArr.VarRecs);
end;

procedure _LapeFormatFloat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FormatFloat(string(PlpString(Params^[0])^), PlpFloat(Params^[1])^));
end;

procedure _LapeFormatCurr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FormatCurr(string(PlpString(Params^[0])^), PCurrency(Params^[1])^));
end;

procedure _LapeLastDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := LastDelimiter(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeStringReplace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := StringReplace(PlpString(Params^[0])^, PlpString(Params^[1])^, PlpString(Params^[2])^, TReplaceFlags(PLapeSmallEnum(Params^[3])^));
end;

procedure _LapeWideStringReplace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := WideStringReplace(PWideString(Params^[0])^, PWideString(Params^[1])^, PWideString(Params^[2])^, TReplaceFlags(PLapeSmallEnum(Params^[3])^));
end;

procedure _LapeUnicodeStringReplace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IFDEF FPC}
  PUnicodeString(Result)^ := UnicodeStringReplace(PUnicodeString(Params^[0])^, PUnicodeString(Params^[1])^, PUnicodeString(Params^[2])^, TReplaceFlags(PLapeSmallEnum(Params^[3])^));
  {$ELSE}
  PUnicodeString(Result)^ := StringReplace(PUnicodeString(Params^[0])^, PUnicodeString(Params^[1])^, PUnicodeString(Params^[2])^, TReplaceFlags(PLapeSmallEnum(Params^[3])^));
  {$ENDIF}
end;

procedure _LapeIsDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := IsDelimiter(PlpString(Params^[0])^, PlpString(Params^[1])^, PSizeInt(Params^[2])^);
end;

procedure _LapePosA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Pos(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapePosW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Pos(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapePosU(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Pos(PUnicodeString(Params^[0])^, PUnicodeString(Params^[1])^);
end;

procedure _LapeStringOfChar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := StringOfChar(PlpChar(Params^[0])^, PSizeInt(Params^[1])^);
end;
