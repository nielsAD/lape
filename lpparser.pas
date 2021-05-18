{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Tokenizer/Parser objects.
}
unit lpparser;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

type
  EParserToken = (
    //General
    tk_NULL,
    tk_Unknown,
    tk_Identifier,
    tk_Comment,
    tk_Directive,
    tk_WhiteSpace,
    tk_NewLine,

    //Keywords
    tk_kw_Array,
    tk_kw_Begin,
    tk_kw_Case,
    tk_kw_Const,
    tk_kw_ConstRef,
    tk_kw_Deprecated,
    tk_kw_Do,
    tk_kw_DownTo,
    tk_kw_Else,
    tk_kw_End,
    tk_kw_Except,
    tk_kw_Experimental,
    tk_kw_External,
    tk_kw_Finally,
    tk_kw_For,
    tk_kw_Forward,
    tk_kw_Function,
    tk_kw_If,
    {$IFDEF Lape_PascalLabels}tk_kw_Label,{$ENDIF}
    tk_kw_Of,
    tk_kw_Object,
    tk_kw_Operator,
    tk_kw_Out,
    tk_kw_Overload,
    tk_kw_Override,
    tk_kw_Packed,
    tk_kw_Private,
    tk_kw_Procedure,
    tk_kw_Program,
    tk_kw_Record,
    tk_kw_Repeat,
    tk_kw_Set,
    tk_kw_Static,
    tk_kw_Then,
    tk_kw_To,
    tk_kw_Try,
    tk_kw_Type,
    tk_kw_Union,
    tk_kw_UnImplemented,
    tk_kw_Until,
    tk_kw_Var,
    tk_kw_While,
    tk_kw_With,

    //Operators
    //Same order as lptypes.EOperator
    tk_cmp_Equal,
    tk_cmp_GreaterThan,
    tk_cmp_GreaterThanOrEqual,
    tk_cmp_LessThan,
    tk_cmp_LessThanOrEqual,
    tk_cmp_NotEqual,

    tk_op_Addr,
    tk_op_AND,
    tk_op_Assign,
    tk_op_AssignDiv,
    tk_op_AssignMinus,
    tk_op_AssignMul,
    tk_op_AssignPlus,
    tk_op_Deref,
    tk_op_DIV,
    tk_op_Divide,
    tk_op_Dot,
    tk_op_IN,
    tk_op_IS,
    tk_op_Index,
    tk_op_Minus,
    tk_op_MOD,
    tk_op_Multiply,
    tk_op_NOT,
    tk_op_OR,
    tk_op_Plus,
    tk_op_Power,
    tk_op_SHL,
    tk_op_SHR,
    tk_op_XOR,

    //Symbols
    tk_sym_BracketClose,
    //tk_sym_BracketOpen,  = tk_op_Index,
    //tk_sym_Caret,        = tk_op_Deref,
    tk_sym_Colon,
    tk_sym_Comma,
    //tk_sym_Dot,          = tk_op_Dot,
    tk_sym_DotDot,
    //tk_sym_Equals,       = tk_cmp_Equal,
    tk_sym_ParenthesisClose,
    tk_sym_ParenthesisOpen,
    tk_sym_SemiColon,

    //Types
    tk_typ_Float,
    tk_typ_Integer,
    tk_typ_Integer_Hex,
    tk_typ_Integer_Bin,
    tk_typ_String,
    tk_typ_HereString,
    tk_typ_Char);
  EParserTokenSet = set of EParserToken;

  PTokenizerState = ^TTokenizerState;
  TTokenizerState = record
    TokStart, Pos: Integer;
    LastTok, Tok: EParserToken;
    InPeek: Boolean;
    DocPos: TDocPos;
  end;

  TLapeTokenizerBase = class;
  TLapeParseDirective = function(Sender: TLapeTokenizerBase): Boolean of object;
  TLapeHandleDirective = function(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean of object;

  TLapeKeyword = record
    Keyword: lpString;
    Token: EParserToken;
  end;

  TLapeKeywordDictionary = specialize TLapeUniqueDictionary<EParserToken>;

  TLapeTokenizerBase = class(TLapeBaseDeclClass)
  protected
    FFileName: lpString;
    FLastTok: EParserToken;
    FTok: EParserToken;
    FTokStart: Integer;
    FPos: Integer;
    FDocPos: TDocPos;
    FLen: Integer;
    FInPeek: Boolean;

    FOnParseDirective: TLapeParseDirective;
    FOnHandleDirective: TLapeHandleDirective;

    FKeywordDictionary: TLapeKeywordDictionary;

    function Compare(Key: lpString): Boolean; virtual; abstract;
    function Identify: EParserToken; virtual;
    function HandleDirective: Boolean; virtual;

    function setTok(ATok: EParserToken): EParserToken; virtual;
    function getTokString: lpString; virtual; abstract;
    function getTokInt: Integer; virtual;
    function getTokUInt64: UInt64; virtual;
    function getTokFloat: Extended; virtual;
    function getTokChar: WideChar; virtual;
    function getTokLen: Integer; virtual;
    function getCurChar: lpChar; virtual;
    procedure setPos(APos: Integer); virtual;
    function getDocPos: TDocPos; override;
  public
    OverridePos: PDocPos;
    NullPos: TDocPos;

    constructor Create(AFileName: lpString = ''); reintroduce; virtual;
    destructor Destroy; override;
    procedure Reset(ClearDoc: Boolean = False); virtual;
    function getState: Pointer; virtual;
    procedure setState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeState(const State: Pointer); virtual;
    function getChar(Offset: Integer = 0): lpChar; virtual; abstract;
    function tempRollBack: EParserToken; virtual;

    function Next: EParserToken; virtual;
    function NextNoWhiteSpace: EParserToken; virtual;
    function NextNoJunk: EParserToken; virtual;
    function Peek: EParserToken; virtual;
    function PeekNoWhiteSpace: EParserToken; virtual;
    function PeekNoJunk: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    property FileName: lpString read FFileName write FFileName;
    property LastTok: EParserToken read FLastTok;
    property Tok: EParserToken read FTok;
    property TokString: lpString read getTokString;
    property TokInteger: Integer read getTokInt;
    property TokUInt64: UInt64 read getTokUInt64;
    property TokFloat: Extended read getTokFloat;
    property TokChar: WideChar read getTokChar;
    property TokStart: Integer read FTokStart;
    property TokLen: Integer read getTokLen;
    property CurChar: lpChar read getCurChar;
    property Pos: Integer read FPos write setPos;
    property Len: Integer read FLen;
    property InPeek: Boolean read FInPeek;
  published
    property OnParseDirective: TLapeParseDirective read FOnParseDirective write FOnParseDirective;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
  end;

  TLapeTokenizerString = class(TLapeTokenizerBase)
  protected
    FDoc: lpString;
    function Compare(Key: lpString): Boolean; override;
    function getTokString: lpString; override;
    procedure setDoc(const ADoc: lpString); virtual;
  public
    procedure Reset(ClearDoc: Boolean = False); override;
    constructor Create(ADoc: lpString; AFileName: lpString = ''); reintroduce; virtual;
    function getChar(Offset: Integer = 0): lpChar; override;
  published
    property Doc: lpString read FDoc write setDoc;
  end;

  TLapeTokenizerFile = class(TLapeTokenizerString)
  public
    constructor Create(AFileName: UnicodeString = ''); reintroduce; overload; virtual;
    constructor Create(AFileName: AnsiString = ''); reintroduce; overload; virtual;
  end;

const
  tk_sym_BracketOpen = tk_op_Index;
  tk_sym_Caret = tk_op_Deref;
  tk_sym_Dot = tk_op_Dot;
  tk_sym_Equals = tk_cmp_Equal;

  TokWhiteSpace = [tk_WhiteSpace, tk_NewLine];
  TokJunk = TokWhiteSpace + [tk_Comment, tk_Directive];

  ParserToken_FirstOperator = tk_cmp_Equal;
  ParserToken_LastOperator = tk_op_XOR;
  ParserToken_Operators = [ParserToken_FirstOperator..ParserToken_LastOperator];
  ParserToken_Keywords = [tk_kw_Array..tk_kw_With];
  ParserToken_Symbols = [tk_sym_BracketClose..tk_sym_SemiColon];
  ParserToken_Types = [tk_typ_Float..tk_typ_Char];

  Lape_Keywords: array[0..51 {$IFDEF Lape_PascalLabels}+1{$ENDIF}] of TLapeKeyword = (
      (Keyword: 'AND';           Token: tk_op_AND),
      (Keyword: 'DIV';           Token: tk_op_DIV),
      (Keyword: 'IN';            Token: tk_op_IN),
      (Keyword: 'IS';            Token: tk_op_IS),
      (Keyword: 'MOD';           Token: tk_op_MOD),
      (Keyword: 'NOT';           Token: tk_op_NOT),
      (Keyword: 'OR';            Token: tk_op_OR),
      (Keyword: 'SHL';           Token: tk_op_SHL),
      (Keyword: 'SHR';           Token: tk_op_SHR),
      (Keyword: 'XOR';           Token: tk_op_XOR),

      (Keyword: 'ARRAY';         Token: tk_kw_Array),
      (Keyword: 'BEGIN';         Token: tk_kw_Begin),
      (Keyword: 'CASE';          Token: tk_kw_Case),
      (Keyword: 'CONST';         Token: tk_kw_Const),
      (Keyword: 'CONSTREF';      Token: tk_kw_ConstRef),
      (Keyword: 'DEPRECATED';    Token: tk_kw_Deprecated),
      (Keyword: 'DO';            Token: tk_kw_Do),
      (Keyword: 'DOWNTO';        Token: tk_kw_DownTo),
      (Keyword: 'ELSE';          Token: tk_kw_Else),
      (Keyword: 'END';           Token: tk_kw_End),
      (Keyword: 'EXCEPT';        Token: tk_kw_Except),
      (Keyword: 'EXPERIMENTAL';  Token: tk_kw_Experimental),
      (Keyword: 'EXTERNAL';      Token: tk_kw_External),
      (Keyword: 'FINALLY';       Token: tk_kw_Finally),
      (Keyword: 'FOR';           Token: tk_kw_For),
      (Keyword: 'FORWARD';       Token: tk_kw_Forward),
      (Keyword: 'FUNCTION';      Token: tk_kw_Function),
      (Keyword: 'IF';            Token: tk_kw_If),
      {$IFDEF Lape_PascalLabels}
      (Keyword: 'LABEL';         Token: tk_kw_Label),
      {$ENDIF}
      (Keyword: 'OBJECT';        Token: tk_kw_Object),
      (Keyword: 'OPERATOR';      Token: tk_kw_Operator),
      (Keyword: 'OF';            Token: tk_kw_Of),
      (Keyword: 'OUT';           Token: tk_kw_Out),
      (Keyword: 'OVERLOAD';      Token: tk_kw_Overload),
      (Keyword: 'OVERRIDE';      Token: tk_kw_Override),
      (Keyword: 'PACKED';        Token: tk_kw_Packed),
      (Keyword: 'PRIVATE';       Token: tk_kw_Private),
      (Keyword: 'PROCEDURE';     Token: tk_kw_Procedure),
      (Keyword: 'PROGRAM';       Token: tk_kw_Program),
      (Keyword: 'RECORD';        Token: tk_kw_Record),
      (Keyword: 'REPEAT';        Token: tk_kw_Repeat),
      (Keyword: 'SET';           Token: tk_kw_Set),
      (Keyword: 'STATIC';        Token: tk_kw_Static),
      (Keyword: 'THEN';          Token: tk_kw_Then),
      (Keyword: 'TO';            Token: tk_kw_To),
      (Keyword: 'TRY';           Token: tk_kw_Try),
      (Keyword: 'TYPE';          Token: tk_kw_Type),
      (Keyword: 'UNION';         Token: tk_kw_Union),
      (Keyword: 'UNIMPLEMENTED'; Token: tk_kw_UnImplemented),
      (Keyword: 'UNTIL';         Token: tk_kw_Until),
      (Keyword: 'VAR';           Token: tk_kw_Var),
      (Keyword: 'WHILE';         Token: tk_kw_While),
      (Keyword: 'WITH';          Token: tk_kw_With)
    );

  OperatorAssociative: array[EOperator] of EOperatorAssociative = (
    assocNone,                          //op_Unkown

    assocLeft,                          //op_cmp_Equal
    assocLeft,                          //op_cmp_GreaterThan
    assocLeft,                          //op_cmp_GreaterThanOrEqual
    assocLeft,                          //op_cmp_LessThan
    assocLeft,                          //op_cmp_LessThanOrEqual
    assocLeft,                          //op_cmp_NotEqual

    assocRight,                         //op_Addr
    assocLeft,                          //op_AND
    assocRight,                         //op_Assign
    assocRight,                         //op_AssignDiv
    assocRight,                         //op_AssignMinus
    assocRight,                         //op_AssignMul
    assocRight,                         //op_AssignPlus
    assocLeft,                          //op_Deref
    assocLeft,                          //op_DIV
    assocLeft,                          //op_Divide
    assocLeft,                          //op_Dot
    assocLeft,                          //op_IN
    assocLeft,                          //op_IS
    assocLeft,                          //op_Index
    assocLeft,                          //op_Minus
    assocLeft,                          //op_MOD
    assocLeft,                          //op_Multiply
    assocRight,                         //op_NOT
    assocLeft,                          //op_OR
    assocLeft,                          //op_Plus
    assocRight,                         //op_Power
    assocLeft,                          //op_SHL
    assocLeft,                          //op_SHR
    assocLeft,                          //op_XOR

    assocRight,                         //op_UnaryMinus
    assocRight                          //op_UnaryPlus
  );

  OperatorPrecedence: array[EOperator] of Byte = (
    0,                                  //op_Unkown

    7,                                  //op_cmp_Equal
    7,                                  //op_cmp_GreaterThan
    7,                                  //op_cmp_GreaterThanOrEqual
    7,                                  //op_cmp_LessThan
    7,                                  //op_cmp_LessThanOrEqual
    7,                                  //op_cmp_NotEqual

    2,                                  //op_Addr
    5,                                  //op_AND
    8,                                  //op_Assign
    8,                                  //op_AssignDiv
    8,                                  //op_AssignMinus
    8,                                  //op_AssignMul
    8,                                  //op_AssignPlus
    1,                                  //op_Deref
    5,                                  //op_DIV
    5,                                  //op_Divide
    1,                                  //op_Dot
    7,                                  //op_IN
    7,                                  //op_IS
    1,                                  //op_Index
    6,                                  //op_Minus
    5,                                  //op_MOD
    5,                                  //op_Multiply
    3,                                  //op_NOT
    6,                                  //op_OR
    6,                                  //op_Plus
    4,                                  //op_Power
    5,                                  //op_SHL
    5,                                  //op_SHR
    6,                                  //op_XOR

    3,                                  //op_UnaryMinus
    3                                   //op_UnaryPlus
  );

{$IF NOT DECLARED(FormatSettings)}
  var FormatSettings: TFormatSettings
    {$IF DECLARED(DefaultFormatSettings)}
    absolute DefaultFormatSettings
    {$ELSE}
    {$DEFINE LoadDefaultFormatSettings}
    {$IFEND};
{$IFEND}

function LapeTokenToString(Token: EParserToken): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ParserTokenToOperator(Token: EParserToken): EOperator; {$IFDEF Lape_Inline}inline;{$ENDIF}
function StrToFloatDot(Str: string): Extended; {$IFDEF Lape_Inline}inline;{$ENDIF}
function StrToFloatDotDef(Str: string; Default: Extended): Extended; {$IFDEF Lape_Inline}inline;{$ENDIF}
function StrToUInt64(Str: string): UInt64;
function StrToUInt64Def(Str: string; const Default: UInt64): UInt64;
function DetermineIntType(IntType: ELapeBaseType; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(Left, Right: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(Str: lpString; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(Str: lpString; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(Str: lpString): ELapeBaseType; overload;
function DetermineIntType(i: Int64; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(i: Int64; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(i: Int64): ELapeBaseType; overload;
function DetermineIntType(i1, i2: Int64; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(i1, i2: Int64; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(i1, i2: Int64): ELapeBaseType; overload;
function DetermineIntType(Size: Integer; Signed: Boolean): ELapeBaseType; overload;

implementation

uses
  typinfo,
  {$IFDEF Lape_NeedAnsiStringsUnit}AnsiStrings,{$ENDIF}
  lpmessages;

{$WARN WIDECHAR_REDUCED OFF}

function LapeTokenToString(Token: EParserToken): lpString;
begin
  Result := lpString(getEnumName(TypeInfo(EParserToken), Ord(Token)));
  if (Token in ParserToken_Symbols + ParserToken_Types) then
    Delete(Result, 1, 7)
  else if (Token in ParserToken_Keywords) then
    Delete(Result, 1, 6)
  else
    Delete(Result, 1, 3);
end;

function ParserTokenToOperator(Token: EParserToken): EOperator;
begin
  if (not (Token in ParserToken_Operators)) then
    Result := op_Unknown
  else
    Result := EOperator(Integer(Token) - Integer(ParserToken_FirstOperator) + 1);
end;

function StrToFloatDot(Str: string): Extended;
begin
  Result := StrToFloat(StringReplace(Str, '.', FormatSettings.DecimalSeparator, []));
end;

function StrToFloatDotDef(Str: string; Default: Extended): Extended;
begin
  Result := StrToFloatDef(StringReplace(Str, '.', FormatSettings.DecimalSeparator, []), Default);
end;

{$IFNDEF FPC}
//Warren P - http://stackoverflow.com/questions/6077258/
function TryStrToUInt64(StrValue: string; var uValue:UInt64): Boolean;
var
  n, Base, Len, Digit: UInt32;
  NextValue: UInt64;
begin
  Result := False;
  uValue := 0;

  StrValue := Trim(UpperCase(StrValue));
  Len := Length(StrValue);
  Base := 10;
  n := 1;

  if (Len < 1) or (Len > 20) or (StrValue[1] = '-') then
    Exit;
  if (StrValue[1] = '$') then
  begin
    Base := 16;
    Inc(n);
    if (Len > 17) then
      Exit;
  end;

  for n := n to Length(StrValue) do
  begin
    if (StrValue[n] in ['0'..'9']) then
      Digit := Ord(StrValue[n]) - Ord('0')
    else if (Base = 16) and (StrValue[n] in ['A'..'F']) then
      Digit := (Ord(StrValue[n]) - Ord('A')) + 10
    else
      Exit;

    NextValue := (uValue * Base) + Digit;
    if (Nextvalue < uValue) then
      Exit;
    uValue := Nextvalue;
  end;

  Result := True;
end;
{$ENDIF}

function StrToUInt64(Str: string): UInt64;
{$IFDEF FPC}
begin Result := StrToQWord(Str); end;
{$ELSE}
begin
  if (not TryStrToUInt64(Str, Result)) then
    raise EConvertError.CreateFmt('"%s" is not a valid UInt64', [Str]);
end;
{$ENDIF}

function StrToUInt64Def(Str: string; const Default: UInt64): UInt64;
{$IFDEF FPC}
begin Result := StrToQWordDef(Str, Default); end;
{$ELSE}
begin
  if (not TryStrToUInt64(Str, Result)) then
    Result := Default;
end;
{$ENDIF}

function DetermineIntType(IntType: ELapeBaseType; MinSize: UInt8): ELapeBaseType; overload;
begin
  Result := IntType;
  while (Result in LapeIntegerTypes) and (LapeTypeSize[Result] < MinSize) do
  begin
    IntType := ELapeBaseType(Ord(Result) + 2);
    if (IntType in LapeIntegerTypes) then
      Result := IntType
    else
      Break;
  end;
end;

function DetermineIntType(Str: lpString; MinSize: UInt8): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(Str), MinSize);
end;

function DetermineIntType(Str: lpString; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(Str), MinType, DoGrow);
end;

function DetermineIntType(Str: lpString): ELapeBaseType;

  function PadZ(Str: lpString; Len: Integer): lpString;
  begin
    if (Length(Str) >= Len) then
      Result := Str
    else
      Result := StringOfChar(lpChar('0'), Len - Length(Str));
  end;

  function PadComp(Str1, Str2: lpString): Integer;
  var
    Len1, Len2: Integer;
  begin
    Len1 := Length(Str1);
    if (Len1 > 0) and (Str1[1] = '-') then
    begin
      Delete(Str1, 1, 1);
      Dec(Len1);
    end;

    Len2 := Length(Str2);
    if (Len2 > 0) and (Str2[1] = '-') then
    begin
      Delete(Str2, 1, 1);
      Dec(Len2);
    end;
    if (Len2 > Len1) then
      Len1 := Len2;

    Result := CompareStr(PadZ(Str1, Len1), PadZ(Str2, Len1));
  end;

var
  Negative: Boolean;
begin
  Str := StringReplace(Str, lpString(' '), lpString(''), [rfReplaceAll]);
  if (Length(Str) < 1) then
    Exit(ltUnknown);
  Negative := (Str[1] = '-');
  if Negative then
    Delete(Str, 1, 1);
  if (Length(Str) < 1) then
    Exit(ltUnknown);

  if (Str[1] = '0') or (((not Negative) and (PadComp(Str, lpString(IntToStr(High(Int8)))) <= 0)) or (Negative and (PadComp(Str, lpString(IntToStr(Low(Int8)))) <= 0))) then
    Result := ltInt8
  else if (not Negative) and (PadComp(Str, lpString(IntToStr(High(UInt8)))) <= 0) then
    Result := ltUInt8
  else if ((not Negative) and (PadComp(Str, lpString(IntToStr(High(Int16)))) <= 0)) or (Negative and (PadComp(Str, lpString(IntToStr(Low(Int16)))) <= 0)) then
    Result := ltInt16
  else if (not Negative) and (PadComp(Str, lpString(IntToStr(High(UInt16)))) <= 0) then
    Result := ltUInt16
  else if ((not Negative) and (PadComp(Str, lpString(IntToStr(High(Int32)))) <= 0)) or (Negative and (PadComp(Str, lpString(IntToStr(Low(Int32)))) <= 0)) then
      Result := ltInt32
  else if (not Negative) and (PadComp(Str, lpString(IntToStr(High(UInt32)))) <= 0) then
    Result := ltUInt32
  else if ((not Negative) and (PadComp(Str, lpString(IntToStr(High(Int64)))) <= 0)) or (Negative {and (PadComp(Str, lpString(IntToStr(Low(Int64)))) <= 0)}) then
    Result := ltInt64
  else {if (not Negative) and (PadComp(Str, lpString(IntToStr(High(UInt64)))) <= 0) then}
    Result := ltUInt64
end;

function DetermineIntType(i: Int64; MinSize: UInt8): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i), MinSize);
end;

function DetermineIntType(i: Int64;  MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i), MinType, DoGrow);
end;

function DetermineIntType(i: Int64): ELapeBaseType;
begin
  Result := DetermineIntType(lpString(IntToStr(i)));
end;

function DetermineIntType(i1, i2: Int64; MinSize: UInt8): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i1, i2), MinSize);
end;

function DetermineIntType(i1, i2: Int64; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i1, i2), MinType, DoGrow);
end;

function DetermineIntType(i1, i2: Int64): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i1), DetermineIntType(i2));
end;

function DetermineIntType(Left, Right: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType;
begin
  if (not (Left in LapeIntegerTypes)) or (not (Right in LapeIntegerTypes)) then
    Exit(ltUnknown);

  Result := Left;
  if (Right > Result) and (LapeTypeSize[Left] <> LapeTypeSize[Right]) then
    Result := Right;

  if DoGrow and (Left <> Right) and ((LapeTypeSize[Left] = LapeTypeSize[Right]) or
     ((Left in LapeSignedIntegerTypes) <> (Right in LapeSignedIntegerTypes)))
  then
    if (Left in LapeSignedIntegerTypes) then
      Result := DetermineIntType(Left, LapeTypeSize[Result] + 1)
    else
      Result := DetermineIntType(Right, LapeTypeSize[Result] + 1);
end;

function DetermineIntType(Size: Integer; Signed: Boolean): ELapeBaseType;
begin
  case Size of
    SizeOf(Int8):  if Signed then Result := ltInt8  else Result := ltUInt8;
    SizeOf(Int16): if Signed then Result := ltInt16 else Result := ltUInt16;
    SizeOf(Int32): if Signed then Result := ltInt32 else Result := ltUInt32;
    SizeOf(Int64): if Signed then Result := ltInt64 else Result := ltUInt64;
    else Result := ltUnknown;
  end;
end;

function TLapeTokenizerBase.Identify: EParserToken;
var
  Char: lpChar;
  tmpDocPos: TDocPos;
  
  procedure NextPos_CountLines; inline;
  begin
    repeat
      case CurChar of
        #10: Inc(FDocPos.Line);
        #13:
          begin
            if (getChar(1) = #10) then
              Inc(FPos);
            Inc(FDocPos.Line);
          end;
      end;
      Inc(FPos);
    until (not (CurChar in [#9, #32, #10, #13]));
  end;

  function Alpha: EParserToken; inline;
  var
    Token: EParserToken;
  begin
    while (getChar(1) in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) do
      Inc(FPos);

    Token := FKeywordDictionary[getTokString()];
    if (Token <> tk_Unknown) then
      Result := setTok(Token)
    else
      Result := setTok(tk_Identifier);
  end;

begin
  FTokStart := FPos;
  Char := CurChar;

  case Char of
    #0: Result := setTok(tk_NULL);
    #9, #32:
      begin
        while (getChar(1) in [#9, #32]) do
          Inc(FPos);
        Result := setTok(tk_WhiteSpace);
      end;
    #10: Result := setTok(tk_NewLine);
    #13:
      begin
        if (getChar(1) = #10) then
          Inc(FPos);
        Result := setTok(tk_NewLine);
      end;

    //Compare Operators
    '=': Result := setTok(tk_sym_Equals);
    '>':
      begin
        if (getChar(1) = '=') then
        begin
          Result := setTok(tk_cmp_GreaterThanOrEqual);
          Inc(FPos);
        end
        else
          Result := setTok(tk_cmp_GreaterThan);
      end;
    '<':
      begin
        case getChar(1) of
          '=':
            begin
              Result := setTok(tk_cmp_LessThanOrEqual);
              Inc(FPos);
            end;
          '>':
            begin
              Result := setTok(tk_cmp_NotEqual);
              Inc(FPos);
            end;
          else
            Result := setTok(tk_cmp_LessThan);
        end;
      end;

    //Operators
    ':':
      begin
        if (getChar(1) = '=') then
        begin
          Result := setTok(tk_op_Assign);
          Inc(FPos);
        end
        else
          Result := setTok(tk_sym_Colon);
      end;
    
    {Divide, Comment, AssignDiv}
    '/':
      case getChar(1) of
        '/': 
          begin
            Inc(FPos);
            while (not (getChar(1) in [#13, #10, #0])) do Inc(FPos);
            Result := setTok(tk_Comment);
          end;
        '=':
          begin
            Result := setTok(tk_op_AssignDiv);
            Inc(FPos);
          end;
        else
          Result := setTok(tk_op_Divide);
      end;

    {Minus, AssignMinus} 
    '-':
      if (getChar(1) = '=') then
      begin
        Result := setTok(tk_op_AssignMinus);
        Inc(FPos);
      end else
        Result := setTok(tk_op_Minus);
    
    {Multiply, Power, AssignMul}      
    '*':
      case getChar(1) of
        '*':
          begin
            Inc(FPos);
            Result := setTok(tk_op_Power);
          end;
        '=': 
          begin
            Result := setTok(tk_op_AssignMul);
            Inc(FPos);
          end;
        else
          Result := setTok(tk_op_Multiply);
      end;

    {Plus, AssignPlus}
    '+':
      if (getChar(1) = '=') then
      begin
        Result := setTok(tk_op_AssignPlus);
        Inc(FPos);
      end else
        Result := setTok(tk_op_Plus);

    '@': Result := setTok(tk_op_Addr);
    '^': Result := setTok(tk_sym_Caret);

    //Symbols
    ']': Result := setTok(tk_sym_BracketClose);
    '[': Result := setTok(tk_sym_BracketOpen);
    ',': Result := setTok(tk_sym_Comma);
    '.':
      if (getChar(1) = '.') then
      begin
        Inc(FPos);
        Result := setTok(tk_sym_DotDot);
      end
      else
        Result := setTok(tk_op_Dot);

    ')': Result := setTok(tk_sym_ParenthesisClose);
    '(':
      if (getChar(1) = '*') then
      begin
        Inc(FPos, 2);
        while (not ((CurChar in ['*', #0]) and (getChar(1) in [')', #0]))) do
          NextPos_CountLines();

        Result := setTok(tk_Comment);
        if (CurChar = '*') then
          Inc(FPos);
      end
      else
        Result := setTok(tk_sym_ParenthesisOpen);

    ';': Result := setTok(tk_sym_SemiColon);
    '{':
      begin
        Inc(FPos);
        if (CurChar = '$') then
        begin
          Result := setTok(tk_Directive);
          if (not HandleDirective()) then
            LapeException(lpeUnknownDirective, DocPos);
        end
        else
        begin
          while (not (CurChar in ['}', #0])) do NextPos_CountLines();
          Result := setTok(tk_Comment);
        end;
      end;

    //Integer and Float
    '0'..'9':
      begin
        Char := getChar(1);
        while (Char in ['0'..'9', '_']) do
        begin
          Inc(FPos);
          Char := getChar(1);
        end;

        if (Char <> '.') or (not (getChar(2) in ['0'..'9'])) then
          Result := setTok(tk_typ_Integer)
        else
        begin
          Inc(FPos, 2);
          Char := getChar(1);
          while (Char in ['0'..'9', '_']) do
          begin
            Inc(FPos);
            Char := getChar(1);
          end;
          if (Char in ['e', 'E']) and (getChar(2) in ['+', '-']) and (getChar(3) in ['0'..'9']) then
          begin
            Inc(FPos, 3);
            while (getChar(1) in ['0'..'9', '_']) do
              Inc(FPos);
          end;

          Result := setTok(tk_typ_Float);
        end;
      end;
    '$':
      if (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) then
      begin
        Inc(FPos);
        while (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f', '_']) do Inc(FPos);
        Result := setTok(tk_typ_Integer_Hex);
      end
      else
        Result := setTok(tk_Unknown);
    '%':
      if (getChar(1) in ['0'..'1']) then
      begin
        Inc(FPos);
        while (getChar(1) in ['0'..'1', '_']) do Inc(FPos);
        Result := setTok(tk_typ_Integer_Bin);
      end
      else
        Result := setTok(tk_Unknown);
    'A'..'Z', '_', 'a'..'z': Result := Alpha();
    #34: //heredoc string
      begin
        Inc(FPos);
        tmpDocPos := DocPos;
        while (not (CurChar in [#34, #0])) do NextPos_CountLines();
        if (CurChar in [#0]) then
          LapeExceptionFmt(lpeErrorScanningString, ['EOF'], tmpDocPos);
        Result := setTok(tk_typ_HereString);
      end;
    #39:
      begin
        Inc(FPos);
        tmpDocPos := DocPos;
        while (not (CurChar in [#39, #0, #13, #10])) do Inc(FPos);
        if not (CurChar in [#39]) then
          LapeExceptionFmt(lpeErrorScanningString, ['EOL'], tmpDocPos);
        Result := setTok(tk_typ_String);
      end;
    '#':
      begin
        Inc(FPos);
        case CurChar of
          '0'..'9':
            begin
              while (getChar(1) in ['0'..'9']) do Inc(FPos);
              Result := setTok(tk_typ_Char);
            end;
          '$':
            begin
              if (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) then
              begin
                Inc(FPos);
                while (getChar(1) in ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(FPos);
                Result := setTok(tk_typ_Char);
              end
              else
                Result := setTok(tk_Unknown);
            end;
          else
            Result := setTok(tk_Unknown);
        end;
      end;
    else
      Result := setTok(tk_Unknown);
  end;
end;

function TLapeTokenizerBase.HandleDirective: Boolean;
var
  Directive, Argument: lpString;
begin
  try
    if ({$IFNDEF FPC}@{$ENDIF}FOnParseDirective <> nil) then
      if FOnParseDirective(Self) then
        Exit(True);

    if ({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil) then
    begin
      Next();
      Expect([tk_Identifier] + ParserToken_Keywords, False, False);
      Directive := TokString;

      NextNoWhiteSpace();
      if (CurChar = '}') then
        Argument := ''
      else
      begin
        while (not (getChar(1) in ['}', #0])) do
        begin
          Inc(FPos);
          if CurChar in [#10,#13] then
          begin
            if (CurChar = #13) and (getChar(1) = #10) then Inc(FPos);
            Inc(FDocPos.Line);
          end;
        end;
        Argument := TokString;
        Inc(FPos);
      end;

      if FOnHandleDirective(Self, Directive, Argument) then
        Exit(True);
    end
    else
      while (not (CurChar in ['}', #0])) do
      begin
        Inc(FPos);
        if CurChar in [#10,#13] then
        begin
          if (CurChar = #13) and (getChar(1) = #10) then Inc(FPos);
          Inc(FDocPos.Line);
        end;
      end;

    Result := False;
  finally
    if (CurChar <> '}') then
      LapeExceptionFmt(lpeExpectedOther, [LapeTokenToString(FTok), '}'], DocPos);
  end;
end;

function TLapeTokenizerBase.setTok(ATok: EParserToken): EParserToken;
begin
  FLastTok := FTok;
  FTok := ATok;
  Result := ATok;
end;

function TLapeTokenizerBase.getTokInt: Integer;
begin
  Result := Integer(getTokUInt64());
end;

function TLapeTokenizerBase.getTokUInt64: UInt64;
  function Bin2Dec(s: lpString): UInt64; inline;
  var
    i: Integer;
  begin
    Result := 0;
    if (Length(s) > 0) and (s[1] <> '%') then
      s := '%' + s;
    for i := 2 to Length(s) do
      Result := (Result shl 1) + UInt64(Ord(s[i])) - UInt64(Ord('0'));
  end;
var
  TokStr: lpString;
begin
  TokStr := StringReplace(getTokString(), lpString('_'), lpString(''), [rfReplaceAll]);
  case FTok of
    tk_typ_Integer_Bin: Result := Bin2Dec(TokStr);
    else Result := StrToUInt64Def(string(TokStr), UInt64(-1));
  end;
end;

function TLapeTokenizerBase.getTokFloat: Extended;
begin
  Result := StrToFloatDotDef(StringReplace(string(getTokString()), '_', '', [rfReplaceAll]), -1);
end;

function TLapeTokenizerBase.getTokChar: WideChar;
var
  Str: lpString;
begin
  Str := getTokString();
  Delete(Str, 1, 1);
  Result := Chr(StrToIntDef(string(Str), 0));
end;

function TLapeTokenizerBase.getTokLen: Integer;
begin
  Result := FPos - FTokStart + 1;
end;

function TLapeTokenizerBase.getCurChar: lpChar;
begin
  Result := getChar(0);
end;

procedure TLapeTokenizerBase.setPos(APos: Integer);
begin
  Assert(APos >= 0);
  Assert(APos < FLen);

  FTokStart := APos;
  FPos := APos - 1;
  Next();
end;

function TLapeTokenizerBase.getDocPos: TDocPos;
begin
  if (OverridePos <> nil) then
    Exit(OverridePos^);

  Result.Line := FDocPos.Line + NullPos.Line;
  if (Integer(FDocPos.Col) > FTokStart) then
    Result.Col := 0
  else
    Result.Col := NullPos.Col + UInt32(FTokStart) - FDocPos.Col;
  if (FFileName <> '') then
    Result.FileName := FFileName
  else
    Result.FileName := NullPos.FileName;
end;

constructor TLapeTokenizerBase.Create(AFileName: lpString = '');
var
  i: Integer;
begin
  inherited Create();

  FFileName := AFileName;
  FOnParseDirective := nil;
  FOnHandleDirective := nil;

  FKeywordDictionary := TLapeKeywordDictionary.Create(tk_Unknown, 512);
  for i := 0 to High(Lape_Keywords) do
    FKeywordDictionary[Lape_Keywords[i].Keyword] := Lape_Keywords[i].Token;

  OverridePos := nil;
  NullPos := NullDocPos;
  with NullPos do
  begin
    Line := 1;
    Col := 1;
  end;

  Reset();
end;

destructor TLapeTokenizerBase.Destroy;
begin
  FreeAndNil(FKeywordDictionary);

  inherited Destroy();
end;

procedure TLapeTokenizerBase.Reset(ClearDoc: Boolean = False);
begin
  FLastTok := tk_NULL;
  FTok := tk_NULL;
  FTokStart := 0;
  FPos := -1;
  FInPeek := False;
  FDocPos := NullDocPos;
  if ClearDoc then
    FLen := 0;
end;

function TLapeTokenizerBase.getState: Pointer;
begin
  New(PTokenizerState(Result));
  with PTokenizerState(Result)^ do
  begin
    TokStart := FTokStart;
    Pos := FPos;
    LastTok := FLastTok;
    Tok := FTok;
    InPeek := FInPeek;
    DocPos := FDocPos;
  end;
end;

procedure TLapeTokenizerBase.setState(const State: Pointer; DoFreeState: Boolean = True);
begin
  with PTokenizerState(State)^ do
  begin
    FTokStart := TokStart;
    FPos := Pos;
    FLastTok := LastTok;
    FTok := Tok;
    FInPeek := InPeek;
    FDocPos := DocPos;
  end;
  if DoFreeState then
    freeState(State);
end;

procedure TLapeTokenizerBase.freeState(const State: Pointer);
begin
  Dispose(PTokenizerState(State));
end;

function TLapeTokenizerBase.tempRollBack: EParserToken;
begin
  Result := FTok;
  FTok := FLastTok;
  FLastTok := tk_NULL;
  FPos := FTokStart - 1;
  FTokStart := FPos;
end;

function TLapeTokenizerBase.Next: EParserToken;
begin
  Inc(FPos);
  if (FTok = tk_NewLine) then
  begin
    FDocPos.Col := FPos;
    Inc(FDocPos.Line);
  end;

  if (FPos < 0) or (FPos >= FLen) then
    Result := setTok(tk_NULL)
  else
    Result := Identify();
end;

function TLapeTokenizerBase.NextNoWhiteSpace: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := FTok;
  repeat
    Result := Next();
  until (not (Result in TokWhiteSpace));
  FLastTok := PrevTok;
end;

function TLapeTokenizerBase.NextNoJunk: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := FTok;
  repeat
    Result := Next();
  until (not (Result in TokJunk));
  FLastTok := PrevTok;
end;

function TLapeTokenizerBase.Peek: EParserToken;
var
  OldSate: Pointer;
begin
  OldSate := getState();
  try
    FInPeek := True;
    Result := Next();
  finally
    setState(OldSate);
  end;
end;

function TLapeTokenizerBase.PeekNoWhiteSpace: EParserToken;
var
  OldState: Pointer;
begin
  OldState := getState();
  try
    FInPeek := True;
    Result := NextNoWhiteSpace();
  finally
    setState(OldState);
  end;
end;

function TLapeTokenizerBase.PeekNoJunk: EParserToken;
var
  OldState: Pointer;
begin
  OldState := getState();
  try
    FInPeek := True;
    Result := NextNoJunk();
  finally
    setState(OldState);
  end;
end;

function TLapeTokenizerBase.Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    NextNoJunk();
  Result := FTok;
  if (FTok <> Token) then
    LapeExceptionFmt(lpeExpectedOther, [TokString, LapeTokenToString(Token)], DocPos);
  if NextAfter then
    NextNoJunk();
end;

function TLapeTokenizerBase.Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    NextNoJunk();
  Result := FTok;
  if (not (FTok in Tokens)) then
    LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(FTok)], DocPos);
  if NextAfter then
    NextNoJunk();
end;

function TLapeTokenizerString.Compare(Key: lpString): Boolean;
var
  KeyLen: Integer;
begin
  KeyLen := Length(Key);
  if (FPos < 0) or (FPos >= FLen) or (FPos + KeyLen - 1 >= FLen) then
    Exit(False);

 Result := SameText(Key, Copy(FDoc, FPos + 1, KeyLen));
 if Result then
   FPos := FPos + KeyLen - 1
end;

function TLapeTokenizerString.getTokString: lpString;
begin
  if (FPos < 0) or (FPos >= FLen) then
    Result := ''
  else
    Result := Copy(FDoc, FTokStart + 1, FPos - FTokStart + 1);
end;

procedure TLapeTokenizerString.setDoc(const ADoc: lpString);
begin
  Reset(True);
  FDoc := ADoc;
  FLen := Length(FDoc);
end;

procedure TLapeTokenizerString.Reset(ClearDoc: Boolean = False);
begin
  inherited;

  if ClearDoc then
    FDoc := '';
end;

constructor TLapeTokenizerString.Create(ADoc: lpString; AFileName: lpString = '');
begin
  inherited Create(AFileName);

  FDoc := ADoc;
  FLen := Length(FDoc);
end;

function TLapeTokenizerString.getChar(Offset: Integer = 0): lpChar;
begin
  if (FPos + Offset < 0) or (FPos + Offset >= FLen) then
    Result := #0
  else
    Result := FDoc[FPos + Offset + 1];
end;

constructor TLapeTokenizerFile.Create(AFileName: UnicodeString = '');
var
  StrList: TStringList;
begin
  StrList := TStringList.Create();
  try
    StrList.LoadFromFile(string(AFileName));
    inherited Create(lpString(StrList.Text), lpString(AFileName));
  finally
    StrList.Free();
  end;
end;

constructor TLapeTokenizerFile.Create(AFileName: AnsiString = '');
begin
  Create(UnicodeString(AFileName));
end;

initialization
  {$IFDEF LoadDefaultFormatSettings}
  GetLocaleFormatSettings(0, FormatSettings);
  {$ENDIF}

end.

