{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Evaluation functions.
}
unit lpeval;

{$I lape.inc}

interface

uses
  SysUtils,
  lptypes;

type
  TLapeToStrArr = array[ELapeBaseType] of TLapeImportedFunc;
  TLapeEvalArr = array[EOperator, ELapeBaseType, ELapeBaseType] of TLapeEvalProc;
  TLapeEvalRes = array[EOperator, ELapeBaseType, ELapeBaseType] of ELapeBaseType;
  TGetEvalRes = function(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
  TGetEvalProc = function(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;

procedure _LapeWrite(const Params: PParamArray);
procedure _LapeWriteLn(const Params: PParamArray);

procedure _LapeGetMem(const Params: PParamArray; const Result: Pointer);
procedure _LapeFreeMem(const Params: PParamArray);
procedure _LapeFreeMemSize(const Params: PParamArray);
procedure _LapeReallocMem(const Params: PParamArray);
procedure _LapeFillMem(const Params: PParamArray);
procedure _LapeMove(const Params: PParamArray);

procedure _LapeHigh(const Params: PParamArray; const Result: Pointer);
procedure _LapeLength(const Params: PParamArray; const Result: Pointer);
procedure _LapeAStr_GetLen(const Params: PParamArray; const Result: Pointer);
procedure _LapeWStr_GetLen(const Params: PParamArray; const Result: Pointer);
procedure _LapeUStr_GetLen(const Params: PParamArray; const Result: Pointer);
procedure _LapeAStr_SetLen(const Params: PParamArray);
procedure _LapeWStr_SetLen(const Params: PParamArray);
procedure _LapeUStr_SetLen(const Params: PParamArray);

procedure _LapeToString_UInt8(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Int8(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_UInt16(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Int16(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_UInt32(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Int32(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_UInt64(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Int64(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Single(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Double(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Currency(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Extended(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Boolean(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_ByteBool(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_WordBool(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_LongBool(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_AnsiChar(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_WideChar(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_ShortString(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_AnsiString(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_WideString(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_UnicodeString(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Variant(const Params: PParamArray; const Result: Pointer);
procedure _LapeToString_Pointer(const Params: PParamArray; const Result: Pointer);

procedure ClearToStrArr(var Arr: TLapeToStrArr);
procedure LoadToStrArr(var Arr: TLapeToStrArr);

function ValidEvalFunction(p: Pointer): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ValidEvalFunction(p: TLapeEvalProc): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

procedure LapeEval_Error(const Dest, Left, Right: Pointer);
procedure ClearEvalRes(var Arr: TLapeEvalRes);
procedure ClearEvalArr(var Arr: TLapeEvalArr);
procedure LoadEvalRes(var Arr: TLapeEvalRes);
procedure LoadEvalArr(var Arr: TLapeEvalArr);
function LapeEval_GetRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
function LapeEval_GetProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;

var
  LapeEvalErrorProc: TLapeEvalProc = {$IFDEF FPC}@{$ENDIF}LapeEval_Error;
  getEvalRes: TGetEvalRes = {$IFDEF FPC}@{$ENDIF}LapeEval_GetRes;
  getEvalProc: TGetEvalProc = {$IFDEF FPC}@{$ENDIF}LapeEval_GetProc;

  LapeToStrArr: TLapeToStrArr;
  LapeEvalRes: TLapeEvalRes;
  LapeEvalArr: TLapeEvalArr;

  _LapeToString_Enum: lpString =
    'function _EnumToString(s: ^string; Index, Lo, Hi: Int32): string;'                  + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (Index >= Lo) and (Index <= Hi) then'                                          + LineEnding +
    '    Result := s[Index]^'                                                            + LineEnding +
    '  else '                                                                            + LineEnding +
    '    Result := '#39#39';'                                                            + LineEnding +
    '  if (Result = '#39#39') then'                                                      + LineEnding +
    '    Result := '#39'InvalidEnum('#39'+ToString(Index)+'#39')'#39';'                  + LineEnding +
    'end;';

  _LapeToString_Set: lpString =
    'function _%sSetToString(ASet, AToString: Pointer; Lo, Hi: Int32): string;'          + LineEnding +
    'type'                                                                               + LineEnding +
    '  TEnum = (se0, se1 = %d);'                                                         + LineEnding +
    '  TSet = set of TEnum;'                                                             + LineEnding +
    '  PSet = ^TSet;'                                                                    + LineEnding +
    '  TToString = private function(const Enum: TEnum): string;'                         + LineEnding +
    'var'                                                                                + LineEnding +
    '  i: Int32;'                                                                        + LineEnding +
    'begin'                                                                              + LineEnding +
    '  Result := '#39#39';'                                                              + LineEnding +
    '  for i := Lo to Hi do'                                                             + LineEnding +
    '    if (TEnum(i) in PSet(ASet)^) then'                                              + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      if (Result <> '#39#39') then'                                                 + LineEnding +
    '        Result := Result + '#39', '#39';'                                           + LineEnding +
    '      Result := Result + TToString(AToString)(TEnum(i));'                           + LineEnding +
    '    end;'                                                                           + LineEnding +
    '  Result := '#39'['#39'+Result+'#39']'#39';'                                        + LineEnding +
    'end;';

  _LapeToString_Array: lpString =
    'function _ArrayToString(Arr: Pointer;'                                              + LineEnding +
    '  AToString: private function(const p: Pointer): string;'                           + LineEnding +
    '  Len, Size: Int32): string;'                                                       + LineEnding +
    'var'                                                                                + LineEnding +
    '  i: Int32;'                                                                        + LineEnding +
    'begin'                                                                              + LineEnding +
    '  Result := '#39#39';'                                                              + LineEnding +
    '  for i := 1 to Len do'                                                             + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if (i > 1) then'                                                                + LineEnding +
    '      Result := Result + '#39', '#39';'                                             + LineEnding +
    '    Result := Result + AToString(Arr);'                                             + LineEnding +
    '    Inc(Arr, Size);'                                                                + LineEnding +
    '  end;'                                                                             + LineEnding +
    '  Result := '#39'['#39'+Result+'#39']'#39';'                                        + LineEnding +
    'end;';

  _LapeSetLength: lpString =
    'procedure _ArraySetLength(var p: Pointer; NewLen, ElSize: Int32;'                   + LineEnding +
    '  Dispose: private procedure(p: Pointer);'                                          + LineEnding +
    '  Copy: private procedure(Src, Dst: Pointer));'                                     + LineEnding +
    'const'                                                                              + LineEnding +
    '  HeaderSize = SizeOf(PtrInt) + SizeOf(SizeInt);'                                   + LineEnding +
    'var'                                                                                + LineEnding +
    '  i, OldLen, NewSize: SizeInt;'                                                     + LineEnding +
    '  NewP: Pointer;'                                                                   + LineEnding +
    '  DoFree: Boolean;'                                                                 + LineEnding +
    'begin'                                                                              + LineEnding +
    '  NewSize := NewLen * ElSize;'                                                      + LineEnding +
    '  DoFree := NewSize <= 0;'                                                          + LineEnding +
    '  Inc(NewSize, HeaderSize);'                                                        + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (p = nil) then'                                                                + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if DoFree then'                                                                 + LineEnding +
    '      Exit;'                                                                        + LineEnding +
    '    p := GetMem(NewSize);'                                                          + LineEnding +
    '    FillMem(p^, NewSize);'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '    PtrInt(p^) := 1;'                                                               + LineEnding +
    '    Inc(p, SizeOf(PtrInt));'                                                        + LineEnding +
    '    SizeInt(p^) := NewLen' {$IFDEF FPC}+'-1'{$ENDIF}+';'                            + LineEnding +
    '    Inc(p, SizeOf(SizeInt));'                                                       + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    '  end;'                                                                             + LineEnding +
    ''                                                                                   + LineEnding +
    '  Dec(p, SizeOf(SizeInt));'                                                         + LineEnding +
    '  OldLen := p^' {$IFDEF FPC}+'+1'{$ENDIF}+';'                                       + LineEnding +
    '  Dec(p, SizeOf(PtrInt));'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (PtrInt(p^) <= 1) then'                                                        + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if (NewLen = OldLen) then'                                                      + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      Inc(p, HeaderSize);'                                                          + LineEnding +
    '      Exit;'                                                                        + LineEnding +
    '    end;'                                                                           + LineEnding +
    ''                                                                                   + LineEnding +
    '    if (NewLen < OldLen) and (Pointer(Dispose) <> nil) then'                        + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      Inc(p, HeaderSize);'                                                          + LineEnding +
    '      for i := NewLen to OldLen - 1 do'                                             + LineEnding +
    '        Dispose(p[i * ElSize]);'                                                    + LineEnding +
    '      Dec(p, HeaderSize);'                                                          + LineEnding +
    '    end;'                                                                           + LineEnding +
    ''                                                                                   + LineEnding +
    '    if DoFree then'                                                                 + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      FreeMem(p);'                                                                  + LineEnding +
    '      p := nil;'                                                                    + LineEnding +
    '      Exit;'                                                                        + LineEnding +
    '    end;'                                                                           + LineEnding +
    '    ReallocMem(p, NewSize);'                                                        + LineEnding +
    '    PtrInt(p^) := 1;'                                                               + LineEnding +
    '    Inc(p, SizeOf(PtrInt));'                                                        + LineEnding +
    '    SizeInt(p^) := NewLen' {$IFDEF FPC}+'-1'{$ENDIF}+';'                            + LineEnding +
    '    Inc(p, SizeOf(SizeInt));'                                                       + LineEnding +
    ''                                                                                   + LineEnding +
    '    if (NewLen > OldLen) then'                                                      + LineEnding +
    '      FillMem(p[OldLen * ElSize]^, (NewLen - OldLen) * ElSize);'                    + LineEnding +
    '  end'                                                                              + LineEnding +
    '  else'                                                                             + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    Dec(PtrInt(p^));'                                                               + LineEnding +
    '    NewP := nil;'                                                                   + LineEnding +
    '    _ArraySetLength(NewP, NewLen, ElSize, Dispose, Copy);'                          + LineEnding +
    ''                                                                                   + LineEnding +
    '    i := OldLen;'                                                                   + LineEnding +
    '    if (NewLen < OldLen) then'                                                      + LineEnding +
    '      i := NewLen;'                                                                 + LineEnding +
    '    if (i >= 0) then'                                                               + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      Inc(p, HeaderSize);'                                                          + LineEnding +
    '      if (Pointer(Copy) = nil) then'                                                + LineEnding +
    '        Move(p^, NewP^, i * ElSize)'                                                + LineEnding +
    '      else for i := i - 1 downto 0 do'                                              + LineEnding +
    '        Copy(p[i * ElSize], NewP[i * ElSize]);'                                     + LineEnding +
    '    end;'                                                                           + LineEnding +
    ''                                                                                   + LineEnding +
    '    p := NewP;'                                                                     + LineEnding +
    '  end;'                                                                             + LineEnding +
    'end;';

implementation

uses
  Variants,
  lpexceptions;

procedure _LapeWrite(const Params: PParamArray);
begin
  Write(PlpString(Params^[0])^);
end;

procedure _LapeWriteLn(const Params: PParamArray);
begin
  WriteLn('');
end;

procedure _LapeGetMem(const Params: PParamArray; const Result: Pointer);
begin
  GetMem(PPointer(Result)^, PInt32(Params^[0])^);
  WriteLn(Format('GetMem(%d, %d)', [PtrUInt(Result^), PInt32(Params^[0])^]));
end;

procedure _LapeFreeMem(const Params: PParamArray);
begin
  FreeMem(PPointer(Params^[0])^);
  WriteLn(Format('FreeMem(%d)', [PtrUInt(Params^[0]^)]));
end;

procedure _LapeFreeMemSize(const Params: PParamArray);
begin
  FreeMem(PPointer(Params^[0])^, PInt32(Params^[1])^);
  WriteLn(Format('FreeMem(%d, %d)', [PtrUInt(Params^[0]^), PInt32(Params^[1])^]));
end;

procedure _LapeReallocMem(const Params: PParamArray);
begin
  ReallocMem(PPointer(Params^[0])^, PInt32(Params^[1])^);
  WriteLn(Format('ReallocMem(%d, %d)', [PtrUInt(Params^[0]^), PInt32(Params^[1])^]));
end;

procedure _LapeFillMem(const Params: PParamArray);
begin
  FillChar(Params^[0]^, PInt32(Params^[1])^, PUInt8(Params^[2])^);
  WriteLn(Format('FillMem(%d, %d, %d)', [PtrUInt(Params^[0]), PInt32(Params^[1])^, PUInt8(Params^[2])^]));
end;

procedure _LapeMove(const Params: PParamArray);
begin
  Move(Params^[0]^, Params^[1]^, PInt32(Params^[2])^);
  WriteLn(Format('Move(%d, %d, %d)', [PtrUInt(Params^[0]), PtrUInt(Params^[1]), PInt32(Params^[2])^]));
end;

procedure _LapeHigh(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := High(PCodeArray(Params^[0])^);
end;

procedure _LapeLength(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := Length(PCodeArray(Params^[0])^);
end;

procedure _LapeAStr_GetLen(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := Length(PAnsiString(Params^[0])^);
end;

procedure _LapeWStr_GetLen(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := Length(PWideString(Params^[0])^);
end;

procedure _LapeUStr_GetLen(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := Length(PUnicodeString(Params^[0])^);
end;

procedure _LapeAStr_SetLen(const Params: PParamArray);
begin
  SetLength(PAnsiString(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeWStr_SetLen(const Params: PParamArray);
begin
  SetLength(PWideString(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeUStr_SetLen(const Params: PParamArray);
begin
  SetLength(PUnicodeString(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeToString_UInt8(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PUInt8(Params^[0])^);
end;

procedure _LapeToString_Int8(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PInt8(Params^[0])^);
end;

procedure _LapeToString_UInt16(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PUInt16(Params^[0])^);
end;

procedure _LapeToString_Int16(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PInt16(Params^[0])^);
end;

procedure _LapeToString_UInt32(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PUInt32(Params^[0])^);
end;

procedure _LapeToString_Int32(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PInt32(Params^[0])^);
end;

procedure _LapeToString_UInt64(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PUInt64(Params^[0])^);
end;

procedure _LapeToString_Int64(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PInt64(Params^[0])^);
end;

procedure _LapeToString_Single(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := FloatToStr(PSingle(Params^[0])^);
end;

procedure _LapeToString_Double(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := FloatToStr(PDouble(Params^[0])^);
end;

procedure _LapeToString_Currency(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := FloatToStr(PCurrency(Params^[0])^);
end;

procedure _LapeToString_Extended(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := FloatToStr(PExtended(Params^[0])^);
end;

procedure _LapeToString_Boolean(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := BoolToStr(PBoolean(Params^[0])^, True);
end;

procedure _LapeToString_ByteBool(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := BoolToStr(PBoolean(Params^[0])^, True);
end;

procedure _LapeToString_WordBool(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := BoolToStr(PWordBool(Params^[0])^, True);
end;

procedure _LapeToString_LongBool(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := BoolToStr(PLongBool(Params^[0])^, True);
end;

procedure _LapeToString_AnsiChar(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PAnsiChar(Params^[0])^;
end;

procedure _LapeToString_WideChar(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PWideChar(Params^[0])^;
end;

procedure _LapeToString_ShortString(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PShortString(Params^[0])^;
end;

procedure _LapeToString_AnsiString(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PAnsiString(Params^[0])^;
end;

procedure _LapeToString_WideString(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PWideString(Params^[0])^;
end;

procedure _LapeToString_UnicodeString(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PUnicodeString(Params^[0])^;
end;

procedure _LapeToString_Variant(const Params: PParamArray; const Result: Pointer);
begin
  try
    PlpString(Result)^ := VarToStr(PVariant(Params^[0])^);
  except
    PlpString(Result)^ := VarTypeAsText(VarType(PVariant(Params^[0])^));
  end;
end;

procedure _LapeToString_Pointer(const Params: PParamArray; const Result: Pointer);
begin
  if (PPointer(Params^[0])^ = nil) then
    PlpString(Result)^ := 'nil'
  else
    PlpString(Result)^ := '0x'+IntToHex(PtrUInt(PPointer(Params^[0])^), 1);
end;

procedure ClearToStrArr(var Arr: TLapeToStrArr);
var
  BaseType: ELapeBaseType;
begin
  for BaseType := Low(BaseType) to High(BaseType) do
    Arr[BaseType] := nil;
end;

procedure LoadToStrArr(var Arr: TLapeToStrArr);
begin
  Arr[ltUInt8] := @_LapeToString_UInt8;
  Arr[ltInt8] := @_LapeToString_Int8;
  Arr[ltUInt16] := @_LapeToString_UInt16;
  Arr[ltInt16] := @_LapeToString_Int16;
  Arr[ltUInt32] := @_LapeToString_UInt32;
  Arr[ltInt32] := @_LapeToString_Int32;
  Arr[ltUInt64] := @_LapeToString_UInt64;
  Arr[ltInt64] := @_LapeToString_Int64;
  Arr[ltSingle] := @_LapeToString_Single;
  Arr[ltDouble] := @_LapeToString_Double;
  Arr[ltCurrency] := @_LapeToString_Currency;
  Arr[ltExtended] := @_LapeToString_Extended;
  Arr[ltBoolean] := @_LapeToString_Boolean;
  Arr[ltByteBool] := @_LapeToString_ByteBool;
  Arr[ltWordBool] := @_LapeToString_WordBool;
  Arr[ltLongBool] := @_LapeToString_LongBool;
  Arr[ltAnsiChar] := @_LapeToString_AnsiChar;
  Arr[ltWideChar] := @_LapeToString_WideChar;
  Arr[ltShortString] := @_LapeToString_ShortString;
  Arr[ltAnsiString] := @_LapeToString_AnsiString;
  Arr[ltWideString] := @_LapeToString_WideString;
  Arr[ltUnicodeString] := @_LapeToString_UnicodeString;
  Arr[ltVariant] := @_LapeToString_Variant;
  Arr[ltPointer] := @_LapeToString_Pointer;
end;

function ValidEvalFunction(p: Pointer): Boolean;
begin
   Result := (p <> nil) and (p <> @LapeEvalErrorProc);
end;

function ValidEvalFunction(p: TLapeEvalProc): Boolean;
begin
   Result := ValidEvalFunction(@p);
end;

procedure LapeEval_Error(const Dest, Left, Right: Pointer);
begin
  LapeException(lpeInvalidEvaluation);
end;

procedure ClearEvalRes(var Arr: TLapeEvalRes);
var
  op: EOperator;
  t1, t2: ELapeBaseType;
begin
  for op := Low(EOperator) to High(EOperator) do
    for t1 := Low(ELapeBaseType) to High(ELapeBaseType) do
      for t2 := Low(ELapeBaseType) to High(ELapeBaseType) do
        Arr[op][t1][t2] := ltUnknown;
end;

procedure ClearEvalArr(var Arr: TLapeEvalArr);
var
  op: EOperator;
  BaseTypeLeft, BaseTypeRight: ELapeBaseType;
begin
  for op := Low(EOperator) to High(EOperator) do
    for BaseTypeLeft := Low(ELapeBaseType) to High(ELapeBaseType) do
      for BaseTypeRight := Low(ELapeBaseType) to High(ELapeBaseType) do
        Arr[op][BaseTypeLeft][BaseTypeRight] := @LapeEval_Error;
end;

procedure LoadEvalRes(var Arr: TLapeEvalRes);
begin
  {$I lpeval_res.inc}
end;

{$I lpeval_functions.inc}

procedure LoadEvalArr(var Arr: TLapeEvalArr);
begin
  {$I lpeval_arr.inc}
end;

procedure lpeAddr(const Dest, Left, Right: Pointer);
begin
  PPointer(Dest)^ := Left;
end;

procedure lpeDeref(const Dest, Left, Right: Pointer);
begin
  PPointer(Dest)^ := PPointer(Left)^;
end;

function LapeEval_GetRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
begin
  if (Op = op_Addr) then
    Result := ltPointer
  else
    Result := LapeEvalRes[op, Left, Right];
end;

function LapeEval_GetProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;
begin
  if (Op = op_Addr) then
    Result := @lpeAddr
  else if (Op = op_Deref) then
    Result := @lpeDeref
  else
    Result := LapeEvalArr[op, Left, Right];
end;

initialization
  ClearToStrArr(LapeToStrArr);
  LoadToStrArr(LapeToStrArr);

  ClearEvalRes(LapeEvalRes);
  ClearEvalArr(LapeEvalArr);
  LoadEvalRes(LapeEvalRes);
  LoadEvalArr(LapeEvalArr);

end.

