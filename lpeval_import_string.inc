{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the wrappers for string related functions.
}
{%MainUnit lpeval.pas}

addGlobalType('(tlbsLF, tlbsCRLF, tlbsCR)', 'TTextLineBreakStyle');
addGlobalType('set of (rfReplaceAll, rfIgnoreCase)', 'TReplaceFlags');

addGlobalFunc('function UpperCase(s: string): string;', @_LapeUpperCase);
addGlobalFunc('function LowerCase(s: string): string;', @_LapeLowerCase);
addGlobalFunc('function UpCase(c: AnsiChar): AnsiChar; overload;', @_LapeUpCaseA);
addGlobalFunc('function UpCase(c: WideChar): WideChar; overload;', @_LapeUpCaseW);

addGlobalFunc('function CompareStr(s1, s2: string): Int32;', @_LapeCompareStr);
addGlobalFunc('function CompareMem(p1, p2: Pointer; Length: PtrUInt): EvalBool;', @_LapeCompareMem);
addGlobalFunc('function CompareText(s1, s2: string): Int32;', @_LapeCompareText);
addGlobalFunc('function SameText(s1, s2: string): EvalBool;', @_LapeSameText);

addGlobalFunc('function AnsiUpperCase(s: string): string;', @_LapeAnsiUpperCase);
addGlobalFunc('function AnsiLowerCase(s: string): string;', @_LapeAnsiLowerCase);
addGlobalFunc('function AnsiCompareStr(s1, s2: string): Int32;', @_LapeAnsiCompareStr);
addGlobalFunc('function AnsiCompareText(s1, s2: string): Int32;', @_LapeAnsiCompareText);
addGlobalFunc('function AnsiSameText(s1,s2:String): EvalBool;', @_LapeAnsiSameText);
addGlobalFunc('function AnsiSameStr(s1,s2:String): EvalBool;', @_LapeAnsiSameStr);

addGlobalFunc('function Trim(s: string): string;', @_LapeTrim);
addGlobalFunc('function TrimLeft(s: string): string;', @_LapeTrimLeft);
addGlobalFunc('function TrimRight(s: string): string;', @_LapeTrimRight);
addGlobalFunc('function QuotedStr(s: string): string;', @_LapeQuotedStr);
addGlobalFunc('function AnsiQuotedStr(s: string; Quote: Char): string;', @_LapeAnsiQuotedStr);
addGlobalFunc('function AnsiDequotedStr(s: string; AQuote: Char): string;', @_LapeAnsiDequotedStr);

addGlobalFunc('function WrapText(Line, BreakStr: string; BreakChars: set of AnsiChar; MaxCol: Int32): string;', @_LapeWrapText);
addGlobalFunc('function AdjustLineBreaks(s: string; Style: TTextLineBreakStyle): string;', @_LapeAdjustLineBreaks);

addGlobalFunc('function IntToHex(Value: Int64; Digits: Int32): string; overload;', @_LapeIntToHex);
addGlobalFunc('function IntToHex(Value: UInt64; Digits: Int32): string; overload;', @_LapeUIntToHex);
addGlobalFunc('function IntToStr(i: Int64): string; overload;', @_LapeToString_Int64);
addGlobalFunc('function IntToStr(i: UInt64): string; overload;', @_LapeToString_UInt64);
addGlobalFunc('function StrToInt(s: string): Int32;', @_LapeStrToInt);
addGlobalFunc('function StrToIntDef(s: string; Def: Int32): Int32;', @_LapeStrToIntDef);
addGlobalFunc('function StrToInt64(s: string): Int64;', @_LapeStrToInt64);
addGlobalFunc('function StrToInt64Def(s: string; Def: Int64): Int64;', @_LapeStrToInt64Def);
addGlobalFunc('function StrToUInt64(s: string): UInt64;', @_LapeStrToUInt64);
addGlobalFunc('function StrToUInt64Def(s: string; Def: UInt64): UInt64;', @_LapeStrToUInt64Def);
addGlobalFunc('function FloatToStr(f: Extended): string;', @_LapeToString_Extended);
addGlobalFunc('function StrToFloat(s: string): Extended;', @_LapeStrToFloat);
addGlobalFunc('function StrToFloatDef(s: string; Def: Extended): Extended;', @_LapeStrToFloatDef);
addGlobalFunc('function CurrToStr(Value: Currency): string;', @_LapeToString_Currency);
addGlobalFunc('function StrToCurr(s: string): Currency;', @_LapeStrToCurr);
addGlobalFunc('function StrToCurrDef(s: string; Def: Currency): Currency;', @_LapeStrToCurrDef);
addGlobalFunc('function StrToBool(s: string): EvalBool;', @_LapeStrToBool);
addGlobalFunc('function BoolToStr(B: EvalBool; TrueS: string = ''True''; FalseS: string = ''False''): string;', @_LapeBoolToStr);
addGlobalFunc('function StrToBoolDef(s: string; Default: EvalBool): EvalBool;', @_LapeStrToBoolDef);

//addGlobalFunc('function Format(Fmt: string; Args: array of Variant): string;', @_LapeFormat);
addGlobalFunc('function FormatFloat(Format: string; Value: Extended): string;', @_LapeFormatFloat);
addGlobalFunc('function FormatCurr(Format: string; Value: Currency): string;', @_LapeFormatCurr);

addGlobalFunc('function LastDelimiter(Delimiters, s: string): SizeInt;', @_LapeLastDelimiter);
addGlobalFunc('function StringReplace(S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;', @_LapeStringReplace);
addGlobalFunc('Function IsDelimiter(Delimiters, s: string; Index: SizeInt): EvalBool;', @_LapeIsDelimiter);

addGlobalFunc('function Pos(Substr: string; Source: string): SizeInt;', @_LapePos);
addGlobalFunc('function Copy(Source: string; Index, Size: SizeInt): string;', @_LapeCopy);
addGlobalFunc('procedure Insert(Source: string; var s: string; Index: SizeInt);', @_LapeInsert);
addGlobalFunc('procedure Delete(var s: string; Index, Size: SizeInt);', @_LapeDelete);
addGlobalFunc('function StringOfChar(c: Char; l: SizeInt): string;', @_LapeStringOfChar);
