{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for string related functions.
}
{%MainUnit lpeval.pas}

addGlobalType('set of (rfReplaceAll, rfIgnoreCase)', 'TReplaceFlags');

addGlobalFunc('function UTF8Encode(s: WideString): AnsiString; overload;', @_LapeUTF8EncodeW);
addGlobalFunc('function UTF8Encode(s: UnicodeString): AnsiString; overload;', @_LapeUTF8EncodeU);
addGlobalFunc('function UTF8Decode(s: AnsiString): WideString; overload;', @_LapeUTF8DecodeW);
addGlobalFunc('function UTF8Decode(s: AnsiString): UnicodeString; overload;', @_LapeUTF8DecodeU);

// locale independent
addGlobalFunc('function UpperCase(s: string): string;', @_LapeUpperCase);
addGlobalFunc('function LowerCase(s: string): string;', @_LapeLowerCase);
addGlobalFunc('function UpCase(c: AnsiChar): AnsiChar; overload;', @_LapeUpCaseA);
addGlobalFunc('function UpCase(c: WideChar): WideChar; overload;', @_LapeUpCaseW);

addGlobalFunc('function CompareStr(s1, s2: string): Int32;', @_LapeCompareStr);
addGlobalFunc('function CompareText(s1, s2: string): Int32;', @_LapeCompareText);
addGlobalFunc('function SameText(s1, s2: string): Boolean;', @_LapeSameText);

// Uses current user locale
addGlobalFunc('function AnsiUpperCase(s: string): string;', @_LapeAnsiUpperCase);
addGlobalFunc('function AnsiLowerCase(s: string): string;', @_LapeAnsiLowerCase);
addGlobalFunc('function AnsiCompareStr(s1, s2: string): Int32;', @_LapeAnsiCompareStr);
addGlobalFunc('function AnsiCompareText(s1, s2: string): Int32;', @_LapeAnsiCompareText);
addGlobalFunc('function AnsiSameText(s1,s2: string): Boolean;', @_LapeAnsiSameText);
addGlobalFunc('function AnsiSameStr(s1,s2: string): Boolean;', @_LapeAnsiSameStr);

// Uses current user locale
addGlobalFunc('function WideUpperCase(s: WideString): WideString;', @_LapeWideUpperCase);
addGlobalFunc('function WideLowerCase(s: WideString): WideString;', @_LapeWideLowerCase);
addGlobalFunc('function WideCompareStr(s1, s2: WideString): Int32;', @_LapeWideCompareStr);
addGlobalFunc('function WideCompareText(s1, s2: WideString): Int32;', @_LapeWideCompareText);
addGlobalFunc('function WideSameText(s1,s2: WideString): Boolean;', @_LapeWideSameText);
addGlobalFunc('function WideSameStr(s1,s2: WideString): Boolean;', @_LapeWideSameStr);
addGlobalFunc('function WideFormat(Fmt: WideString; Args: array of Variant): WideString;', @_LapeWideFormat);

addGlobalFunc('function Pos(Substr, Source: AnsiString): SizeInt; overload;', @_LapePosA);
addGlobalFunc('function Pos(Substr, Source: WideString): SizeInt; overload;', @_LapePosW);
addGlobalFunc('function Pos(Substr, Source: UnicodeString): SizeInt; overload;', @_LapePosU);

addGlobalFunc('function StringReplace(S, OldPattern, NewPattern: string; Flags: TReplaceFlags = [rfReplaceAll]): string;', @_LapeStringReplace);
addGlobalFunc('function UnicodeStringReplace(S, OldPattern, NewPattern: UnicodeString; Flags: TReplaceFlags = [rfReplaceAll]): UnicodeString;', @_LapeUnicodeStringReplace);
addGlobalFunc('function WideStringReplace(S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags = [rfReplaceAll]): WideString;', @_LapeWideStringReplace);

addGlobalFunc('function Trim(s: string): string;', @_LapeTrim);
addGlobalFunc('function TrimLeft(s: string): string;', @_LapeTrimLeft);
addGlobalFunc('function TrimRight(s: string): string;', @_LapeTrimRight);
addGlobalFunc('function PadL(s: string; Len: SizeInt; c: Char = '' ''): string;', @_LapePadL);
addGlobalFunc('function PadR(s: string; Len: SizeInt; c: Char = '' ''): string;', @_LapePadR);
addGlobalFunc('function QuotedStr(s: string): string;', @_LapeQuotedStr);

addGlobalFunc('function IntToHex(Value: Int64; Digits: Int32 = 1): string; overload;', @_LapeIntToHex);
addGlobalFunc('function IntToHex(Value: UInt64; Digits: Int32 = 1): string; overload;', @_LapeUIntToHex);
addGlobalFunc('function IntToStr(i: Int64): string; overload;', @_LapeToString_Int64);
addGlobalFunc('function IntToStr(i: UInt64): string; overload;', @_LapeToString_UInt64);
addGlobalFunc('function StrToInt(s: string): Int32; overload;', @_LapeStrToInt);
addGlobalFunc('function StrToInt(s: string; Def: Int32): Int32; overload;', @_LapeStrToIntDef);
addGlobalFunc('function StrToInt64(s: string): Int64; overload;', @_LapeStrToInt64);
addGlobalFunc('function StrToInt64(s: string; Def: Int64): Int64; overload;', @_LapeStrToInt64Def);
addGlobalFunc('function StrToUInt64(s: string): UInt64; overload;', @_LapeStrToUInt64);
addGlobalFunc('function StrToUInt64(s: string; Def: UInt64): UInt64; overload;', @_LapeStrToUInt64Def);
{$IFDEF Lape_NoExtended}
addGlobalFunc('function FloatToStr(f: Double): string;', @_LapeToString_Double);
addGlobalFunc('function StrToFloat(s: string): Double; overload;', @_LapeStrToFloat);
addGlobalFunc('function StrToFloat(s: string; Def: Double): Double; overload;', @_LapeStrToFloatDef);
{$ELSE}
addGlobalFunc('function FloatToStr(f: Extended): string;', @_LapeToString_Extended);
addGlobalFunc('function StrToFloat(s: string): Extended; overload;', @_LapeStrToFloat);
addGlobalFunc('function StrToFloat(s: string; Def: Extended): Extended; overload;', @_LapeStrToFloatDef);
{$ENDIF}
addGlobalFunc('function CurrToStr(Value: Currency): string;', @_LapeToString_Currency);
addGlobalFunc('function StrToCurr(s: string): Currency; overload;', @_LapeStrToCurr);
addGlobalFunc('function StrToCurr(s: string; Def: Currency): Currency; overload;', @_LapeStrToCurrDef);
addGlobalFunc('function StrToBool(s: string): Boolean; overload;', @_LapeStrToBool);
addGlobalFunc('function StrToBool(s: string; Default: Boolean): Boolean; overload;', @_LapeStrToBoolDef);
addGlobalFunc('function BoolToStr(B: Boolean; TrueS: string = ''True''; FalseS: string = ''False''): string;', @_LapeBoolToStr);

addGlobalFunc('function Format(Fmt: string; Args: array of Variant): string;', @_LapeFormat);
addGlobalFunc('function FormatCurr(Format: string; Value: Currency): string;', @_LapeFormatCurr);
{$IFDEF Lape_NoExtended}
addGlobalFunc('function FormatFloat(Format: string; Value: Double): string;', @_LapeFormatFloat);
{$ELSE}
addGlobalFunc('function FormatFloat(Format: string; Value: Extended): string;', @_LapeFormatFloat);
{$ENDIF}

addGlobalFunc('function LastDelimiter(Delimiters, s: string): SizeInt;', @_LapeLastDelimiter);
addGlobalFunc('Function IsDelimiter(Delimiters, s: string; Index: SizeInt): Boolean;', @_LapeIsDelimiter);

addGlobalFunc('function StringOfChar(c: Char; l: SizeInt): string;', @_LapeStringOfChar);

addDelayedCode(LapeDelayedFlags +
  'function Chr(IntValue: UInt8):  AnsiChar; overload; begin Result := AnsiChar(IntValue); end;' + LineEnding +
  'function Chr(IntValue: UInt16): WideChar; overload; begin Result := WideChar(IntValue); end;'
,  '!addDelayedCode::Chr');

