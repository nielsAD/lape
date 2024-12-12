{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include contains all the wrappers for string related functions.
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
  PBoolean(Result)^ := SameText(PlpString(Params^[0])^, PlpString(Params^[1])^);
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
  PBoolean(Result)^ := AnsiSameText(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeAnsiSameStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := AnsiSameStr(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeWideUpperCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := WideUpperCase(PWideString(Params^[0])^);
end;

procedure _LapeWideLowerCase(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := WideLowerCase(PWideString(Params^[0])^);
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
  PBoolean(Result)^ := WideSameText(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeWideSameStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := WideSameStr(PWideString(Params^[0])^, PWideString(Params^[1])^);
end;

procedure _LapeWideFormat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ConstArr: TVarRecList;
begin
  ConstArr := VariantArrToConstArr(PVariantArray(Params^[1])^);
  PWideString(Result)^ := WideFormat(PWideString(Params^[0])^, ConstArr.VarRecs);
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
  PBoolean(Result)^ := StrToBool(string(PlpString(Params^[0])^));
end;

procedure _LapeBoolToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PBoolean(Params^[0])^ then
    PlpString(Result)^ := PlpString(Params^[1])^
  else
    PlpString(Result)^ := PlpString(Params^[2])^;
end;

procedure _LapeStrToBoolDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := StrToBoolDef(string(PlpString(Params^[0])^), PBoolean(Params^[1])^);
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
  PUnicodeString(Result)^ := {$IFDEF FPC}UnicodeStringReplace{$ELSE}StringReplace{$ENDIF}(PUnicodeString(Params^[0])^, PUnicodeString(Params^[1])^, PUnicodeString(Params^[2])^, TReplaceFlags(PLapeSmallEnum(Params^[3])^));
end;

procedure _LapeIsDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IsDelimiter(PlpString(Params^[0])^, PlpString(Params^[1])^, PSizeInt(Params^[2])^);
end;

procedure _LapePosA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Pos(PlpString(Params^[0])^, PlpString(Params^[1])^, PSizeInt(Params^[2])^);
end;

procedure _LapePosW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Pos(PWideString(Params^[0])^, PWideString(Params^[1])^, PSizeInt(Params^[2])^);
end;

procedure _LapePosU(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Pos(PUnicodeString(Params^[0])^, PUnicodeString(Params^[1])^, PSizeInt(Params^[2])^);
end;

procedure _LapeStringOfChar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := StringOfChar(PlpChar(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeString_SetLength(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SetLength(PlpString(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeString_Length(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Length(PlpString(Params^[0])^);
end;

procedure _LapeString_Low(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := Low(PlpString(Params^[0])^);
end;

procedure _LapeString_High(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := High(PlpString(Params^[0])^);
end;

procedure _LapeString_Pop(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _Pop(var Str: lpString): Char;
  begin
    if Length(Str) < 1 then
      LapeExceptionFmt(lpeIndexOutOfRange, [Length(Str), Low(Str), Length(Str)]);
    Result := Str[Length(Str)];
    SetLength(Str, Length(Str) - 1);
  end;

begin
  PlpChar(Result)^ := _Pop(PlpString(Params^[0])^);
end;

procedure _LapeString_First(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _First(const Str: lpString): Char;
  begin
    if Length(Str) < 1 then
      LapeExceptionFmt(lpeIndexOutOfRange, [Low(Str), Low(Str), Length(Str)]);
    Result := Str[Low(Str)];
  end;

begin
  PlpChar(Result)^ := _First(PlpString(Params^[0])^);
end;

procedure _LapeString_Last(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _Last(const Str: lpString): Char;
  begin
    if Length(Str) < 1 then
      LapeExceptionFmt(lpeIndexOutOfRange, [High(Str), Low(Str), Length(Str)]);
    Result := Str[High(Str)];
  end;

begin
  PlpChar(Result)^ := _Last(PlpString(Params^[0])^);
end;

procedure _LapeString_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Insert(PlpString(Params^[1])^, PlpString(Params^[0])^, PInteger(Params^[2])^);
end;

procedure _LapeString_DeleteIndex(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Delete(PlpString(Params^[0])^, PInteger(Params^[1])^, 1);
end;

procedure _LapeString_DeleteRange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Delete(PlpString(Params^[0])^, PInteger(Params^[1])^, (PInteger(Params^[2])^ - PInteger(Params^[1])^) + 1);
end;

procedure _LapeString_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := Copy(PlpString(Params^[0])^, 1, Length(PlpString(Params^[0])^));
end;

procedure _LapeString_CopyRange(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := Copy(PlpString(Params^[0])^, PInteger(Params^[1])^, (PInteger(Params^[2])^ - PInteger(Params^[1])^) + 1);
end;

procedure _LapeString_Replace(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := StringReplace(PlpString(Params^[0])^, PlpString(Params^[1])^, PlpString(Params^[2])^, TReplaceFlags(Params^[3]^));
end;

procedure _LapeString_ToUpper(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := UpperCase(PlpString(Params^[0])^);
end;

procedure _LapeString_ToLower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := LowerCase(PlpString(Params^[0])^);
end;

procedure _LapeString_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Pos(PlpString(Params^[1])^, PlpString(Params^[0])^, PInt32(Params^[2])^);
end;

procedure _LapeString_LastIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _LastIndexOf(const Self, AValue: lpString; Offset: SizeInt): SizeInt;
  var
    i, StartIndex: SizeInt;
  begin
    if (Length(Self) > 0) and (Length(AValue) > 0) then
    begin
      StartIndex := (Length(Self) - Length(AValue) + 1) - (Offset - 1);
      if (StartIndex > Length(Self)) then
        StartIndex := Length(Self);

      for i := StartIndex downto 1 do
        if CompareMem(@Self[i], @AValue[1], Length(AValue) * SizeOf(lpChar)) then
          Exit(i);
    end;

    Result := -1;
  end;

begin
  PInt32(Result)^ := _LastIndexOf(PlpString(Params^[0])^, PlpString(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeString_Trim(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := Trim(PlpString(Params^[0])^);
end;

procedure _LapeString_TrimLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := TrimLeft(PlpString(Params^[0])^);
end;

procedure _LapeString_TrimRight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := TrimRight(PlpString(Params^[0])^);
end;

procedure _LapeString_Split(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  lpStringArray = array of lpString;

  function _Split(const Str, Seperator: lpString; ExcludeEmpty: Boolean): lpStringArray;
  var
    StrLen, SepLen, MaxLen, ResultCount: SizeInt;

    procedure Add(const Index, Count: Integer);
    begin
      if ExcludeEmpty and (Count = 0) then
        Exit;

      if (ResultCount >= Length(Result)) then
        SetLength(Result, 4 + (Length(Result) * 2));
      Result[ResultCount] := System.Copy(Str, Index, Count);
      Inc(ResultCount);
    end;

    function NextSep(const StartIndex: SizeInt): SizeInt;
    var
      I: SizeInt;
      Ptr: PlpChar;
    begin
      if (StartIndex <= StrLen) then
      begin
        Ptr := @Str[StartIndex];
        I := StartIndex - 1;
        while (I <= MaxLen) do
        begin
          Inc(I);
          if (PlpChar(Seperator)^ = Ptr^) and CompareMem(Ptr, PlpChar(Seperator), SepLen * SizeOf(lpChar)) then
            Exit(I);
          Inc(Ptr);
        end;
      end;

      Result := -1;
    end;

  var
    FoundSep, LastSep: SizeInt;
  begin
    if (Length(Seperator) = 0) then
      Result := [Str]
    else
    begin
      Result := [];
      ResultCount := 0;

      StrLen := Length(Str);
      SepLen := Length(Seperator);
      MaxLen := StrLen - SepLen;

      LastSep := 1;
      FoundSep := NextSep(1);
      while (FoundSep > 0) do
      begin
        Add(LastSep, FoundSep - LastSep);
        LastSep := FoundSep + SepLen;
        FoundSep := NextSep(LastSep);
      end;
      if (LastSep <= StrLen) then
        Add(LastSep, MaxInt);

      SetLength(Result, ResultCount);
    end;
  end;

begin
  lpStringArray(Result^) := _Split(PlpString(Params^[0])^, PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_Contains(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PBoolean(Params^[2])^ then
    PBoolean(Result)^ := Pos(PlpString(Params^[1])^, PlpString(Params^[0])^) > 0
  else
    PBoolean(Result)^ := Pos(LowerCase(PlpString(Params^[1])^), LowerCase(PlpString(Params^[0])^)) > 0;
end;

procedure _LapeString_Count(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _Count(Str, SubStr: lpString; CaseSens: Boolean): Integer;
  var
    Offset, Index: SizeInt;
  begin
    Result := 0;

    if not CaseSens then
    begin
      Str := LowerCase(Str);
      SubStr := LowerCase(SubStr);
    end;

    Offset := 1;
    repeat
      Index := Pos(SubStr, Str, Offset);
      Offset := Index + Length(SubStr);
      if (Index > 0) then
        Inc(Result);
    until (Index < 1);
  end;

begin
  PInteger(Result)^ := _Count(PlpString(Params^[0])^, PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_Equals(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if PBoolean(Params^[2])^ then
    PBoolean(Result)^ := {$IF DEFINED(LAPE_UNICODE) and DECLARED(UnicodeSameStr)}UnicodeSameStr{$ELSE}SameStr{$ENDIF}(PlpString(Params^[1])^, PlpString(Params^[0])^)
  else
    PBoolean(Result)^ := {$IF DEFINED(LAPE_UNICODE) and DECLARED(UnicodeSameText)}UnicodeSameText{$ELSE}SameText{$ENDIF}(PlpString(Params^[1])^, PlpString(Params^[0])^);
end;

procedure _LapeString_Compare(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := {$IF DEFINED(LAPE_UNICODE) and DECLARED(UnicodeCompareText)}UnicodeCompareText{$ELSE}CompareText{$ENDIF}(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeString_PadLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := StringOfChar(PlpChar(Params^[2])^, PInt32(Params^[1])^ - Int32(Length(PlpString(Params^[0])^))) + PlpString(Params^[0])^;
end;

procedure _LapeString_PadRight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PlpString(Params^[0])^ + StringOfChar(PlpChar(Params^[2])^, PInt32(Params^[1])^ - Int32(Length(PlpString(Params^[0])^)));
end;

procedure _LapeString_StartsWith(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function StartsWith(const Self, Value: String; CaseSenstive: Boolean): Boolean;
  begin
    case CaseSenstive of
      False: Result := (Length(Value) > 0) and SameText(System.Copy(Self, 1, Length(Value)), Value);
      True:  Result := (Length(Value) > 0) and (System.Copy(Self, 1, Length(Value)) = Value);
    end;
  end;

begin
  PBoolean(Result)^ := StartsWith(PlpString(Params^[0])^, PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_EndsWith(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function EndsWith(const Self, Value: String; CaseSenstive: Boolean): Boolean;
  begin
    case CaseSenstive of
      False: Result := (Length(Value) > 0) and SameText(System.Copy(Self, Length(Self) - Length(Value) + 1), Value);
      True:  Result := (Length(Value) > 0) and (System.Copy(Self, Length(Self) - Length(Value) + 1) = Value);
    end;
  end;

begin
  PBoolean(Result)^ := EndsWith(PlpString(Params^[0])^, PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeString_ToBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := StrToBool(PlpString(Params^[0])^);
end;

procedure _LapeString_ToBooleanDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := StrToBoolDef(PlpString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeString_ToInt32(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := StrToInt(PlpString(Params^[0])^);
end;

procedure _LapeString_ToInt32Def(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := StrToIntDef(PlpString(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeString_ToInt64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := StrToInt64(PlpString(Params^[0])^);
end;

procedure _LapeString_ToInt64Def(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := StrToInt64Def(PlpString(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeString_ToSingle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := StrToFloatDot(PlpString(Params^[0])^);
end;

procedure _LapeString_ToSingleDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := StrToFloatDotDef(PlpString(Params^[0])^, PSingle(Params^[1])^);
end;

procedure _LapeString_ToDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := StrToFloatDot(PlpString(Params^[0])^);
end;

procedure _LapeString_ToDoubleDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := StrToFloatDotDef(PlpString(Params^[0])^, PDouble(Params^[1])^);
end;

procedure _LapeString_ToExtended(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := StrToFloatDot(PlpString(Params^[0])^);
end;

procedure _LapeString_ToExtendedDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := StrToFloatDotDef(PlpString(Params^[0])^, PExtended(Params^[1])^);
end;

procedure _LapeString_IsUpper(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function IsUpper(const S: lpString): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if not CharInSet(S[I], ['A'..'Z']) then
        Exit(False);
    Result := S <> '';
  end;

begin
  PBoolean(Result)^ := IsUpper(PlpString(Params^[0])^);
end;

procedure _LapeString_IsLower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function IsLower(const S: lpString): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if (not CharInSet(S[I], ['a'..'z'])) then
        Exit(False);
    Result := S <> '';
  end;

begin
  PBoolean(Result)^ := IsLower(PlpString(Params^[0])^);
end;

procedure _LapeString_IsAlpha(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function IsAlpha(const S: lpString): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if (not CharInSet(S[I], ['a'..'z', 'A'..'Z'])) then
        Exit(False);
    Result := S <> '';
  end;

begin
  PBoolean(Result)^ := IsAlpha(PlpString(Params^[0])^);
end;

procedure _LapeString_IsAlphaNum(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function IsAlphaNum(const S: lpString): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if (not CharInSet(S[I], ['0'..'9', 'a'..'z', 'A'..'Z'])) then
        Exit(False);
    Result := S <> '';
  end;

begin
  PBoolean(Result)^ := IsAlphaNum(PlpString(Params^[0])^);
end;

procedure _LapeString_IsNumeric(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function IsNumeric(const S: lpString): Boolean;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if (not CharInSet(S[I], ['0'..'9'])) then
        Exit(False);
    Result := S <> '';
  end;

begin
  PBoolean(Result)^ := IsNumeric(PlpString(Params^[0])^);
end;

procedure _LapeString_IsFloat(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  _: Single;
begin
  PBoolean(Result)^ := TryStrToFloat(PlpString(Params^[0])^, _);
end;

procedure _LapeString_IsInteger(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  _: Int64;
begin
  PBoolean(Result)^ := TryStrToInt64(PlpString(Params^[0])^, _) or TryStrToUInt64(PlpString(Params^[0])^, UInt64(_));
end;

procedure _LapeChar_ToUpper(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _ToUpper(const C: lpChar): lpChar;
  begin
    if (C in ['a'..'z']) then
      Result := lpChar(Ord(C) - 32)
    else
      Result := C;
  end;

begin
  PlpChar(Result)^ := _ToUpper(PlpChar(Params^[0])^);
end;

procedure _LapeChar_ToLower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  function _ToLower(const C: lpChar): lpChar;
  begin
    if CharInSet(C, ['A'..'Z']) then
      Result := lpChar(Ord(C) + 32)
    else
      Result := C;
  end;

begin
  PlpChar(Result)^ := _ToLower(PlpChar(Params^[0])^);
end;

procedure _LapeChar_IsUpper(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CharInSet(PlpChar(Params^[0])^, ['A'..'Z']);
end;

procedure _LapeChar_IsLower(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CharInSet(PlpChar(Params^[0])^, ['a'..'z']);
end;

procedure _LapeChar_IsAlpha(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CharInSet(PlpChar(Params^[0])^, ['a'..'z', 'A'..'Z']);
end;

procedure _LapeChar_IsAlphaNum(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CharInSet(PlpChar(Params^[0])^, ['0'..'9', 'a'..'z', 'A'..'Z']);
end;

procedure _LapeChar_IsNumeric(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := CharInSet(PlpChar(Params^[0])^, ['0'..'9']);
end;
