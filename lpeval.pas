{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Evaluation functions.
}
unit lpeval;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

type
  TLapeToStrArr = array[ELapeBaseType] of TLapeImportedFunc;
  TLapeEvalArr = array[EOperator, ELapeBaseType, ELapeBaseType] of TLapeEvalProc;
  TLapeEvalRes = array[EOperator, ELapeBaseType, ELapeBaseType] of ELapeBaseType;
  TGetEvalRes = function(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
  TGetEvalProc = function(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;

procedure _LapeWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeAssigned(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeRaise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeRaiseString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAssert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAssertMsg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeRangeCheck(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeGetMem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAllocMem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeFreeMem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeReallocMem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeFillMem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeMove(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeCompareMem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeSortWeighted_Int32(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeSortWeighted_Int64(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeSortWeighted_Extended(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeReverse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeHigh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeLength(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeAStr_GetLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWStr_GetLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeUStr_GetLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeSStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeUStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeSStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeUStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeSStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeUStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAStr_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWStr_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeUStr_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeAStr_Unique(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeWStr_Unique(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeUStr_Unique(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeToString_Unknown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_UInt8(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Int8(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_UInt16(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Int16(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_UInt32(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Int32(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_UInt64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Int64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Single(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Double(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Currency(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Extended(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Boolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_ByteBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_WordBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_LongBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_AnsiChar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_WideChar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_ShortString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_AnsiString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_WideString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_UnicodeString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Variant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeToString_Pointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

{$I lpeval_headers_math.inc}
{$I lpeval_headers_string.inc}
{$I lpeval_headers_datetime.inc}
{$I lpeval_headers_variant.inc}
{$I lpeval_headers_file.inc}

procedure ClearToStrArr(var Arr: TLapeToStrArr);
procedure LoadToStrArr(var Arr: TLapeToStrArr);

function ValidEvalFunction(p: Pointer): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ValidEvalFunction(p: TLapeEvalProc): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

procedure LapeEval_Error(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

  LapeDelayedFlags: lpString = '{$ASSERTIONS ON}{$BOOLEVAL ON}{$CONSTADDRESS ON}{$RANGECHECKS ON}{$AUTOINVOKE OFF}{$AUTOPROPERTIES OFF}{$LOOSESEMICOLON OFF}{$EXTENDEDSYNTAX OFF}{$HINTS OFF}' + LineEnding;

  _LapeToString_Enum: lpString =
    'function _EnumToString(s: ^const string; Index, Lo, Hi: SizeInt): string;'          + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (Index >= Lo) and (Index <= Hi) then'                                          + LineEnding +
    '    Result := s[Index-Lo]^'                                                         + LineEnding +
    '  else '                                                                            + LineEnding +
    '    Result := '#39#39';'                                                            + LineEnding +
    '  if (Result = '#39#39') then'                                                      + LineEnding +
    '    Result := '#39'InvalidEnum('#39'+ToString(Index)+'#39')'#39';'                  + LineEnding +
    'end;';

  _LapeToString_Set: lpString =
    'function _%sSetToString(ASet: ConstPointer;'                                        + LineEnding +
    '  AToString: private function(constref Enum): string;'                              + LineEnding +
    '  Lo, Hi: SizeInt): string;'                                                        + LineEnding +
    'type'                                                                               + LineEnding +
    '  TEnum = (se0, se1 = %d);'                                                         + LineEnding +
    '  TSet = set of TEnum;'                                                             + LineEnding +
    '  PSet = ^TSet;'                                                                    + LineEnding +
    'var'                                                                                + LineEnding +
    '  e: TEnum;'                                                                        + LineEnding +
    'begin'                                                                              + LineEnding +
    '  Result := '#39#39';'                                                              + LineEnding +
    '  for e := TEnum(Lo) to TEnum(Hi) do'                                               + LineEnding +
    '    if (e in PSet(ASet)^) then'                                                     + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      if (Result <> '#39#39') then'                                                 + LineEnding +
    '        Result := Result + '#39', '#39';'                                           + LineEnding +
    '      Result := Result + AToString(e);'                                             + LineEnding +
    '    end;'                                                                           + LineEnding +
    '  Result := '#39'['#39'+Result+'#39']'#39';'                                        + LineEnding +
    'end;';

  _LapeToString_Array: lpString =
    'function _ArrayToString(Arr: Pointer;'                                              + LineEnding +
    '  AToString: private function(p: ConstPointer): string;'                            + LineEnding +
    '  Len, Size: SizeInt): string;'                                                     + LineEnding +
    'var'                                                                                + LineEnding +
    '  i: SizeInt;'                                                                      + LineEnding +
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

  _LapeSwap: lpString =
    'procedure _Swap(var a, b; Size: SizeInt);'                                          + LineEnding +
    'var'                                                                                + LineEnding +
    '  c: Pointer;'                                                                      + LineEnding +
    'begin'                                                                              + LineEnding +
    '  c := GetMem(Size);'                                                               + LineEnding +
    '  try'                                                                              + LineEnding +
    '    Move(a, c^, Size);'                                                             + LineEnding +
    '    Move(b, a,  Size);'                                                             + LineEnding +
    '    Move(c^, b, Size);'                                                             + LineEnding +
    '  finally'                                                                          + LineEnding +
    '    FreeMem(c);'                                                                    + LineEnding +
    '  end;'                                                                             + LineEnding +
    'end;';

  _LapeSetLength: lpString =
    'procedure _ArraySetLength(var p: Pointer; NewLen, ElSize: SizeInt;'                 + LineEnding +
    '  Dispose: private procedure(p: Pointer);'                                          + LineEnding +
    '  Copy: private procedure(Src: ConstPointer; Dst: Pointer));'                       + LineEnding +
    'const'                                                                              + LineEnding +
    '  HeaderSize = SizeOf(PtrInt) + SizeOf(SizeInt);'                                   + LineEnding +
    'var'                                                                                + LineEnding +
    '  i, OldLen, NewSize: SizeInt;'                                                     + LineEnding +
    '  NewP: Pointer;'                                                                   + LineEnding +
    '  DoFree: EvalBool;'                                                                + LineEnding +
    'begin'                                                                              + LineEnding +
    '  NewSize := NewLen * ElSize;'                                                      + LineEnding +
    '  DoFree := NewSize <= 0;'                                                          + LineEnding +
    '  Inc(NewSize, HeaderSize);'                                                        + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (p = nil) then'                                                                + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if DoFree then'                                                                 + LineEnding +
    '      Exit;'                                                                        + LineEnding +
    '    p := AllocMem(NewSize);'                                                        + LineEnding +
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
    '      for i := NewLen * ElSize to (OldLen - 1) * ElSize with ElSize do'             + LineEnding +
    '        Dispose(p[i]);'                                                             + LineEnding +
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
    '    if (i >= 1) then'                                                               + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      Inc(p, HeaderSize);'                                                          + LineEnding +
    '      if (Pointer(Copy) = nil) then'                                                + LineEnding +
    '        Move(p^, NewP^, i * ElSize)'                                                + LineEnding +
    '      else for i := (i - 1) * ElSize downto 0 with ElSize do'                       + LineEnding +
    '        Copy(p[i], NewP[i]);'                                                       + LineEnding +
    '    end;'                                                                           + LineEnding +
    ''                                                                                   + LineEnding +
    '    p := NewP;'                                                                     + LineEnding +
    '  end;'                                                                             + LineEnding +
    'end;';

  _LapeCopy: lpString =
    'procedure _ArrayCopy(p: ConstPointer;'                                              + LineEnding +
    '  Start: SizeInt = 0; Count: SizeInt = High(SizeInt);'                              + LineEnding +
    '  Len, ElSize: SizeInt;'                                                            + LineEnding +
    '  Copy: private procedure(Src: ConstPointer; Dst: Pointer);'                        + LineEnding +
    '  out Result: Pointer);'                                                            + LineEnding +
    'var'                                                                                + LineEnding +
    '  i: SizeInt;'                                                                      + LineEnding +
    'begin'                                                                              + LineEnding +
    '  Result := nil;'                                                                   + LineEnding +
    '  if (p = nil) or (Start >= Len) or (Count <= 0) then'                              + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Start < 0) then'                                                              + LineEnding +
    '    Start := 0'                                                                     + LineEnding +
    '  else if (Len - Count < Start) then'                                               + LineEnding +
    '    Count := Len - Start;'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  _ArraySetLength(Result, Count, ElSize, nil, nil);'                                + LineEnding +
    '  Inc(p, Start * ElSize);'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Pointer(Copy) = nil) then'                                                    + LineEnding +
    '    Move(p^, Result^, Count * ElSize)'                                              + LineEnding +
    '  else'                                                                             + LineEnding +
    '    for i := 0 to (Count - 1) * ElSize with ElSize do'                              + LineEnding +
    '      Copy(p[i], Result[i]);'                                                       + LineEnding +
    'end;';

  _LapeDelete: lpString =
    'procedure _ArrayDelete(var p: Pointer;'                                             + LineEnding +
    '  Start: SizeInt; Count: SizeInt = High(SizeInt);'                                  + LineEnding +
    '  ElSize: SizeInt;'                                                                 + LineEnding +
    '  Dispose: private procedure(p: Pointer);'                                          + LineEnding +
    '  Copy: private procedure(Src: ConstPointer; Dst: Pointer));'                       + LineEnding +
    'type'                                                                               + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                             + LineEnding +
    'var'                                                                                + LineEnding +
    '  i, Len: SizeInt;'                                                                 + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (p = nil) or (Count <= 0) then'                                                + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  Len := PSizeInt(p)[-1]^' {$IFDEF FPC}+'+1'{$ENDIF}+';'                            + LineEnding +
    '  if (Start < 0) then'                                                              + LineEnding +
    '    Start := 0'                                                                     + LineEnding +
    '  else if (Start >= Len) then'                                                      + LineEnding +
    '    Exit'                                                                           + LineEnding +
    '  else if (Len - Count < Start) then'                                               + LineEnding +
    '    Count := Len - Start;'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  _ArraySetLength(p, Len, ElSize, Dispose, Copy);'                                  + LineEnding +
    '  Inc(p, Start * ElSize);'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Pointer(Dispose) <> nil) then'                                                + LineEnding +
    '    for i := 0 to (Count - 1) * ElSize with ElSize do'                              + LineEnding +
    '      Dispose(p[i]);'                                                               + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Start + Count < Len) then'                                                    + LineEnding +
    '    Move(p[Count * ElSize]^, p^, (Len - Start - Count) * ElSize);'                  + LineEnding +
    ''                                                                                   + LineEnding +
    '  Dec(p, Start * ElSize);'                                                          + LineEnding +
    '  _ArraySetLength(p, Len-Count, ElSize, nil, nil);'                                 + LineEnding +
    'end;';

  _LapeInsert: lpString =
    'procedure _ArrayInsert(Src: ConstPointer; var Dst: Pointer;'                        + LineEnding +
    '  Start: SizeInt = 0; Count: SizeInt = 0; LenSrc, ElSize: SizeInt;'                 + LineEnding +
    '  Dispose: private procedure(p: Pointer);'                                          + LineEnding +
    '  Copy: private procedure(Src: ConstPointer; Dst: Pointer));'                       + LineEnding +
    'type'                                                                               + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                             + LineEnding +
    'var'                                                                                + LineEnding +
    '  i, LenDst: SizeInt;'                                                              + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (LenSrc < 0) or (Count < 0) then'                                              + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Dst = nil) then'                                                              + LineEnding +
    '    LenDst := 0'                                                                    + LineEnding +
    '  else'                                                                             + LineEnding +
    '    LenDst := PSizeInt(Dst)[-1]^' {$IFDEF FPC}+'+1'{$ENDIF}+';'                     + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Start < 0) then'                                                              + LineEnding +
    '    Start := 0'                                                                     + LineEnding +
    '  else if (Start > LenDst) then'                                                    + LineEnding +
    '    Start := LenDst'                                                                + LineEnding +
    '  else if (LenDst - Count < Start) then'                                            + LineEnding +
    '    Count := LenDst - Start;'                                                       + LineEnding +
    ''                                                                                   + LineEnding +
    '  _ArraySetLength(Dst, LenDst + LenSrc, ElSize, Dispose, Copy);'                    + LineEnding +
    '  Inc(Dst, Start * ElSize);'                                                        + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Count <> LenSrc) then'                                                        + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if (Count > LenSrc) and (Pointer(Dispose) <> nil) then'                         + LineEnding +
    '      for i := LenSrc * ElSize to (Count - 1) * ElSize with ElSize do'              + LineEnding +
    '        Dispose(Dst[i]);'                                                           + LineEnding +
    ''                                                                                   + LineEnding +
    '    Move('                                                                          + LineEnding +
    '      Dst[Count * ElSize]^,'                                                        + LineEnding +
    '      Dst[LenSrc * ElSize]^,'                                                       + LineEnding +
    '      (LenDst - Start - Count) * ElSize'                                            + LineEnding +
    '    );'                                                                             + LineEnding +
    ''                                                                                   + LineEnding +
    '    if (LenSrc > Count) and (Pointer(Copy) <> nil) then'                            + LineEnding +
    '      FillMem(Dst[Count * ElSize]^, (LenSrc - Count) * ElSize);'                    + LineEnding +
    '  end;'                                                                             + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (LenSrc > 0) then'                                                             + LineEnding +
    '    if (Pointer(Copy) = nil) then'                                                  + LineEnding +
    '      Move(Src^, Dst^, LenSrc * ElSize)'                                            + LineEnding +
    '    else for i := 0 to (LenSrc - 1) * ElSize with ElSize do'                        + LineEnding +
    '      Copy(Src[i], Dst[i]);'                                                        + LineEnding +
    ''                                                                                   + LineEnding +
    '  Dec(Dst, Start * ElSize);'                                                        + LineEnding +
    '  _ArraySetLength(Dst, LenDst + LenSrc - Count, ElSize, nil, nil);'                 + LineEnding +
    'end;';

    _LapeSort: lpString =
    'procedure _Sort(p: Pointer; ElSize, Hi: SizeInt;'                                   + LineEnding +
    '  Compare: private function(constref A, B): Int32); overload;'                      + LineEnding +
    'type'                                                                               + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                             + LineEnding +
    'var'                                                                                + LineEnding +
    '  I, J: Int32;'                                                                     + LineEnding +
    '  Item: Pointer;'                                                                   + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (p = nil) then'                                                                + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Hi = -1) then'                                                                + LineEnding +
    '    Hi := PSizeInt(p)[-1]^;'                                                        + LineEnding +

    '  Item := GetMem(ElSize);'                                                          + LineEnding +
    '  for I := 1 to Hi do'                                                              + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    Move(p[I * ElSize]^, Item^, ElSize);'                                           + LineEnding +
    '    J := I - 1;'                                                                    + LineEnding +
    '    while (J >= 0) and (Compare(p[J * ElSize]^, Item^) > 0) do'                     + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      Move(p[J * ElSize]^, p[(J+1) * ElSize]^, ElSize);'                            + LineEnding +
    '      Dec(J);'                                                                      + LineEnding +
    '    end;'                                                                           + LineEnding +
    '    Move(Item^, p[(J+1) * ElSize]^, ElSize);'                                       + LineEnding +
    '  end;'                                                                             + LineEnding +
    '  FreeMem(Item);'                                                                   + LineEnding +
    'end;';

    _LapeIndexOf: lpString =
    'function _IndexOf(p: Pointer; ElSize, Lo, Hi: SizeInt; Item: Pointer;'              + LineEnding +
    '  Equals: private function(constref A, B): EvalBool): SizeInt; overload;'           + LineEnding +
    'type'                                                                               + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                             + LineEnding +
    'var'                                                                                + LineEnding +
    '  i: SizeInt;'                                                                      + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (p = nil) then'                                                                + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Hi = -1) then'                                                                + LineEnding +
    '    Hi := PSizeInt(p)[-1]^;'                                                        + LineEnding +
    '  for i := 0 to Hi do'                                                              + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if Equals(Item^, p^) then'                                                      + LineEnding +
    '      Exit(Lo+i);'                                                                  + LineEnding +
    '    Inc(p, ElSize);'                                                                + LineEnding +
    '  end;'                                                                             + LineEnding +
    ''                                                                                   + LineEnding +
    '  Result := -1;'                                                                    + LineEnding +
    'end;'                                                                               + LineEnding +
    ''                                                                                   + LineEnding +
    'function _IndicesOf(p: Pointer; ElSize, Lo, Hi: SizeInt; Item: Pointer;'            + LineEnding +
    '  Equals: private function(constref A, B): EvalBool): array of Int32; overload;'    + LineEnding +
    'type'                                                                               + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                             + LineEnding +
    'var'                                                                                + LineEnding +
    '  i, Count, Len: SizeInt;'                                                          + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (p = nil) then'                                                                + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  Len := 4;'                                                                        + LineEnding +
    '  SetLength(Result, Len);'                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  if (Hi = -1) then'                                                                + LineEnding +
    '    Hi := PSizeInt(p)[-1]^;'                                                        + LineEnding +
    '  for i := 0 to Hi do'                                                              + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    if Equals(Item^, p^) then'                                                      + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      if (Count = Len) then'                                                        + LineEnding +
    '      begin'                                                                        + LineEnding +
    '        Len := Len * 2;'                                                            + LineEnding +
    '        SetLength(Result, Len);'                                                    + LineEnding +
    '      end;'                                                                         + LineEnding +
    ''                                                                                   + LineEnding +
    '      Result[Count] := Lo+i;'                                                       + LineEnding +
    '      Inc(Count);'                                                                  + LineEnding +
    '    end;'                                                                           + LineEnding +
    '    Inc(p, ElSize);'                                                                + LineEnding +
    '  end;'                                                                             + LineEnding +
    ''                                                                                   + LineEnding +
    '  SetLength(Result, Count);'                                                        + LineEnding +
    'end;';

    _LapeUnique: lpString =
    'procedure _Unique(var p: Pointer; ElSize: SizeInt;'                                 + LineEnding +
    '  Equals: private function(constref A, B): EvalBool;'                               + LineEnding +
    '  Dispose: private procedure(p: Pointer)); overload;'                               + LineEnding +
    'type'                                                                               + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                             + LineEnding +
    'var'                                                                                + LineEnding +
    '  i, j, Len: SizeInt;'                                                              + LineEnding +
    'begin'                                                                              + LineEnding +
    '  if (p = nil) then'                                                                + LineEnding +
    '    Exit;'                                                                          + LineEnding +
    ''                                                                                   + LineEnding +
    '  Len := PSizeInt(p)[-1]^' {$IFDEF FPC}+'+1'{$ENDIF}+';'                            + LineEnding +
    '  while (i < Len) do'                                                               + LineEnding +
    '  begin'                                                                            + LineEnding +
    '    j := i + 1;'                                                                    + LineEnding +
    '    while (j < Len) do'                                                             + LineEnding +
    '    begin'                                                                          + LineEnding +
    '      if Equals(p[i * ElSize]^, p[j * ElSize]^) then'                               + LineEnding +
    '      begin'                                                                        + LineEnding +
    '        Move(p[(Len-1) * ElSize]^, p[j * ElSize]^, ElSize);'                        + LineEnding +
    ''                                                                                   + LineEnding +
    '        Dec(Len);'                                                                  + LineEnding +
    '        Dec(j);'                                                                    + LineEnding +
    '      end;'                                                                         + LineEnding +
    '      Inc(j);'                                                                      + LineEnding +
    '    end;'                                                                           + LineEnding +
    '    Inc(i);'                                                                        + LineEnding +
    '  end;'                                                                             + LineEnding +
    ''                                                                                   + LineEnding +
    '  _ArraySetLength(p, Len, ElSize, Dispose, nil);'                                   + LineEnding +
    'end;';

implementation

uses
  Variants, Math, DateUtils,
  {$IFDEF Lape_NeedAnsiStringsUnit}AnsiStrings,{$ENDIF}
  {$IFDEF FPC}LCLIntf,{$ELSE}{$IFDEF MSWINDOWS}Windows,{$ENDIF}{$ENDIF}
  lpmessages, lpparser;

{$RangeChecks Off}
{$OverflowChecks Off}

type
  PBoolean = ^Boolean; //Make sure it's not ^Byte

procedure _LapeWrite(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Write(PlpString(Params^[0])^);
end;

procedure _LapeWriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  WriteLn('');
end;

procedure _LapeAssigned(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := Assigned(Params^[0]) and Assigned(PPointer(Params^[0])^);
end;

procedure _LapeRaise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  raise Exception(Params^[0]^);
end;

procedure _LapeRaiseString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  LapeException(PlpString(Params^[0])^);
end;

procedure _LapeAssert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (not PEvalBool(Params^[0])^) then
    LapeException(lpeAssertionFailure);
end;

procedure _LapeAssertMsg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (not PEvalBool(Params^[0])^) then
    LapeExceptionFmt(lpeAssertionFailureMsg, [PlpString(Params^[1])^]);
end;

procedure _LapeRangeCheck(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PSizeInt(Params^[0])^ < PSizeInt(Params^[1])^) or (PSizeInt(Params^[0])^ > PSizeInt(Params^[2])^) then
    LapeExceptionFmt(lpeIndexOutOfRange, [PSizeInt(Params^[0])^, PSizeInt(Params^[1])^, PSizeInt(Params^[2])^]);
end;

procedure _LapeGetMem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  GetMem(PPointer(Result)^, PSizeInt(Params^[0])^);
end;

procedure _LapeAllocMem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := AllocMem(PSizeInt(Params^[0])^);
end;

procedure _LapeFreeMem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FreeMem(PPointer(Params^[0])^);
end;

procedure _LapeReallocMem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ReallocMem(PPointer(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeFillMem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FillChar(Params^[0]^, PSizeInt(Params^[1])^, PUInt8(Params^[2])^);
end;

procedure _LapeMove(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Move(Params^[0]^, Params^[1]^, PSizeInt(Params^[2])^);
end;

procedure _LapeCompareMem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := CompareMem(Params^[0], Params^[1], PSizeInt(Params^[2])^);
end;

procedure _LapeSortWeighted_Int32(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Sort(PByte(Params^[0]^), PSizeInt(Params^[1])^, TIntegerArray(Params^[2]^), PEvalBool(Params^[3])^);
end;

procedure _LapeSortWeighted_Int64(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Sort(PByte(Params^[0]^), PSizeInt(Params^[1])^, TInt64Array(Params^[2]^), PEvalBool(Params^[3])^);
end;

procedure _LapeSortWeighted_Extended(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Sort(PByte(Params^[0]^), PSizeInt(Params^[1])^, TExtendedArray(Params^[2]^), PEvalBool(Params^[3])^);
end;

procedure _LapeReverse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Reverse(PByte(Params^[0]^), PSizeInt(Params^[1])^, PSizeInt(Params^[2])^);
end;

procedure _LapeHigh(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := High(PCodeArray(Params^[0])^);
end;

procedure _LapeLength(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Length(PCodeArray(Params^[0])^);
end;

procedure _LapeAStr_GetLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Length(PAnsiString(Params^[0])^);
end;

procedure _LapeWStr_GetLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Length(PWideString(Params^[0])^);
end;

procedure _LapeUStr_GetLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeInt(Result)^ := Length(PUnicodeString(Params^[0])^);
end;

procedure _LapeSStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PUInt8(Params^[1])^ > PUInt8(Params^[2])^) then
    LapeExceptionFmt(lpeOutOfTypeRangeHigh, [PUInt8(Params^[1])^, PUInt8(Params^[2])^]);
  SetLength(PShortString(Params^[0])^, PUInt8(Params^[1])^);
end;

procedure _LapeAStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SetLength(PAnsiString(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeWStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SetLength(PWideString(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeUStr_SetLen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SetLength(PUnicodeString(Params^[0])^, PSizeInt(Params^[1])^);
end;

procedure _LapeSStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Params^[3])^ := Copy(PShortString(Params^[0])^, PUInt8(Params^[1])^, Min(Length(PShortString(Params^[0])^), PUInt8(Params^[2])^));
end;

procedure _LapeAStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Params^[3])^ := Copy(PAnsiString(Params^[0])^, PSizeInt(Params^[1])^, Min(Length(PAnsiString(Params^[0])^), PSizeInt(Params^[2])^));
end;

procedure _LapeWStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Params^[3])^ := Copy(PWideString(Params^[0])^, PSizeInt(Params^[1])^, Min(Length(PWideString(Params^[0])^), PSizeInt(Params^[2])^));
end;

procedure _LapeUStr_Copy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUnicodeString(Params^[3])^ := Copy(PUnicodeString(Params^[0])^, PSizeInt(Params^[1])^, Min(Length(PUnicodeString(Params^[0])^), PSizeInt(Params^[2])^));
end;

procedure _LapeSStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Delete(PShortString(Params^[0])^, PUInt8(Params^[1])^, Min(Length(PShortString(Params^[0])^), PUInt8(Params^[2])^));
end;

procedure _LapeAStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Delete(PAnsiString(Params^[0])^, PSizeInt(Params^[1])^, Min(Length(PAnsiString(Params^[0])^), PSizeInt(Params^[2])^));
end;

procedure _LapeWStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Delete(PWideString(Params^[0])^, PSizeInt(Params^[1])^, Min(Length(PWideString(Params^[0])^), PSizeInt(Params^[2])^));
end;

procedure _LapeUStr_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Delete(PUnicodeString(Params^[0])^, PSizeInt(Params^[1])^, Min(Length(PUnicodeString(Params^[0])^), PSizeInt(Params^[2])^));
end;

procedure _LapeAStr_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PSizeInt(Params^[3])^ > 0) then
    Delete(PAnsiString(Params^[1])^, PSizeInt(Params^[2])^, Min(Length(PAnsiString(Params^[1])^), PSizeInt(Params^[3])^));
  Insert(PAnsiString(Params^[0])^, PAnsiString(Params^[1])^, Min(Length(PAnsiString(Params^[1])^)+1, PSizeInt(Params^[2])^));
end;

procedure _LapeWStr_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PSizeInt(Params^[3])^ > 0) then
    Delete(PWideString(Params^[1])^, PSizeInt(Params^[2])^, Min(Length(PWideString(Params^[1])^), PSizeInt(Params^[3])^));
  Insert(PWideString(Params^[0])^, PWideString(Params^[1])^, Min(Length(PWideString(Params^[1])^)+1, PSizeInt(Params^[2])^));
end;

procedure _LapeUStr_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PSizeInt(Params^[3])^ > 0) then
    Delete(PUnicodeString(Params^[1])^, PSizeInt(Params^[2])^, Min(Length(PUnicodeString(Params^[1])^), PSizeInt(Params^[3])^));
  Insert(PUnicodeString(Params^[0])^, PUnicodeString(Params^[1])^, Min(Length(PUnicodeString(Params^[1])^)+1, PSizeInt(Params^[2])^));
end;

procedure _LapeAStr_Unique(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UniqueString(PAnsiString(Params^[0])^);
end;

procedure _LapeWStr_Unique(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UniqueString(PWideString(Params^[0])^);
end;

procedure _LapeUStr_Unique(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UniqueString(PUnicodeString(Params^[0])^);
end;

procedure _LapeToString_Unknown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := '*UNKNOWN*';
end;

procedure _LapeToString_UInt8(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(UIntToStr(PUInt8(Params^[0])^));
end;

procedure _LapeToString_Int8(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(IntToStr(PInt8(Params^[0])^));
end;

procedure _LapeToString_UInt16(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(UIntToStr(PUInt16(Params^[0])^));
end;

procedure _LapeToString_Int16(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(IntToStr(PInt16(Params^[0])^));
end;

procedure _LapeToString_UInt32(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(UIntToStr(PUInt32(Params^[0])^));
end;

procedure _LapeToString_Int32(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(IntToStr(PInt32(Params^[0])^));
end;

procedure _LapeToString_UInt64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(UIntToStr(PUInt64(Params^[0])^));
end;

procedure _LapeToString_Int64(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(IntToStr(PInt64(Params^[0])^));
end;

procedure _LapeToString_Single(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FloatToStr(PSingle(Params^[0])^));
end;

procedure _LapeToString_Double(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FloatToStr(PDouble(Params^[0])^));
end;

procedure _LapeToString_Currency(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FloatToStr(PCurrency(Params^[0])^));
end;

procedure _LapeToString_Extended(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FloatToStr(PExtended(Params^[0])^));
end;

procedure _LapeToString_Boolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(BoolToStr(PBoolean(Params^[0])^, True));
end;

procedure _LapeToString_ByteBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(BoolToStr(PByteBool(Params^[0])^, True));
end;

procedure _LapeToString_WordBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(BoolToStr(PWordBool(Params^[0])^, True));
end;

procedure _LapeToString_LongBool(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(BoolToStr(PLongBool(Params^[0])^, True));
end;

procedure _LapeToString_AnsiChar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(PAnsiChar(Params^[0])^);
end;

procedure _LapeToString_WideChar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(PWideChar(Params^[0])^);
end;

procedure _LapeToString_ShortString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(PShortString(Params^[0])^);
end;

procedure _LapeToString_AnsiString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(PAnsiString(Params^[0])^);
end;

procedure _LapeToString_WideString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(PWideString(Params^[0])^);
end;

procedure _LapeToString_UnicodeString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(PUnicodeString(Params^[0])^);
end;

procedure _LapeToString_Variant(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  try
    {$IF DEFINED(Lape_Unicode) and DEFINED(FPC)}
    PlpString(Result)^ := lpString(VarToUnicodeStr(PVariant(Params^[0])^));
    {$ELSE}
    PlpString(Result)^ := lpString(VarToStr(PVariant(Params^[0])^));
    {$IFEND}
  except
    PlpString(Result)^ := lpString(VarTypeAsText(VarType(PVariant(Params^[0])^)));
  end;
end;

procedure _LapeToString_Pointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PointerToString(Params^[0]);
end;

type
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

{$I lpeval_wrappers_math.inc}
{$I lpeval_wrappers_string.inc}
{$I lpeval_wrappers_datetime.inc}
{$I lpeval_wrappers_variant.inc}
{$I lpeval_wrappers_file.inc}

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
   Result := (p <> nil) and (p <> Pointer({$IFNDEF FPC}@{$ENDIF}LapeEvalErrorProc));
end;

function ValidEvalFunction(p: TLapeEvalProc): Boolean;
begin
   Result := ValidEvalFunction(Pointer({$IFNDEF FPC}@{$ENDIF}p));
end;

procedure LapeEval_Error(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

{$WARN COMPARING_SIGNED_UNSIGNED OFF}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

{$IFDEF FPC}
  {$WARN 6058 off} // has not been inlined warning
{$ENDIF}

{$I lpeval_functions.inc}

procedure LoadEvalArr(var Arr: TLapeEvalArr);
begin
  {$I lpeval_arr.inc}
end;

procedure lpeAddr(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := Left;
end;

procedure lpeDeref(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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
  
finalization
  ClearToStrArr(LapeToStrArr);
  ClearEvalRes(LapeEvalRes);
  ClearEvalArr(LapeEvalArr);
  
end.



