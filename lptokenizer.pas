{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Tokenizer, which turns the text of a script into tokens.
}
unit lptokenizer;

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
    tk_kw_At,
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
    tk_kw_Enum,
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
    tk_kw_Property,
    tk_kw_Record,
    tk_kw_Repeat,
    tk_kw_Set,
    tk_kw_Static,
    tk_kw_Strict,
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
    tk_typ_Char
  );

  EParserTokenSet = set of EParserToken;

  PTokenizerState = ^TTokenizerState;
  TTokenizerState = record
    TokStart, Pos: Integer;
    Tok: EParserToken;
    Line, LineStart: Integer;
    TokLines, TokLineStart: Integer;
  end;

  TLapeTokenizerBase = class;
  TLapeHandleDirective = function(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean of object;

  TLapeKeyword = record
    Keyword: lpString;
    Token: EParserToken;
  end;

  TLapeTokenizerBase = class(TLapeBaseDeclClass)
  protected
    FFileName: lpString;
    FFileNameRelative: lpString;
    FTok: EParserToken;
    FTokStart: Integer;
    FPos: Integer;
    FLine: Integer;
    FLineStart: Integer;
    FTokLines: Integer;
    FTokLineStart: Integer;
    FDoc: lpString;

    FOnHandleDirective: TLapeHandleDirective;

    function Identify: EParserToken; virtual;
    function HandleDirective: Boolean; virtual;
    function LineEnds(p: PlpChar): PlpChar; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function ScanBraceComment(p: PlpChar): PlpChar; virtual;
    function ScanStarComment(p: PlpChar): PlpChar; virtual;
    function ScanHereString(p: PlpChar): PlpChar; virtual;
    function ScanString(p: PlpChar): PlpChar; virtual;
    function ParseKeyword(var Stop: PlpChar): EParserToken; virtual;
    procedure UnknownDirective; virtual;

    function setTok(ATok: EParserToken): EParserToken; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function getTokString: lpString; virtual;
    function getDocPos: TDocPos; override;
  public
    RelativeFileNames: Boolean;
    OverridePos: PDocPos;

    constructor Create(AFileName: lpString = ''); reintroduce; virtual;
    procedure Reset; virtual;
    function getState: Pointer; virtual;

    procedure setState(const State: Pointer); virtual;
    procedure freeState(const State: Pointer); virtual;
    function getChar(Offset: Integer = 0): lpChar; {$IFDEF Lape_Inline}inline;{$ENDIF}
    // Written in digits, in hexadecimal ($FF) or in bits (%1010), with _ anywhere between those.
    function TokInteger(out Value: UInt64): Boolean; virtual;
    function TokFloat(out Value: lpFloat): Boolean; virtual;
    function TokCharCode(out Code: UInt32): Boolean; virtual;

    function tempRollBack(PrevTok: EParserToken): EParserToken; virtual;

    function Next: EParserToken; virtual;
    function NextNoWhiteSpace: EParserToken; virtual;

    property FileName: lpString read FFileName write FFileName;
    property Tok: EParserToken read FTok;
    property TokString: lpString read getTokString;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
  end;

  TLapeTokenizerString = class(TLapeTokenizerBase)
  public
    constructor Create(ADoc: lpString; AFileName: lpString = ''); reintroduce; virtual;
  end;

  TLapeTokenizerFile = class(TLapeTokenizerString)
  public
    constructor Create(AFileName: UnicodeString = ''; AEncoding:TEncoding = nil); reintroduce; virtual;
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
  ParserToken_Hints = [tk_kw_Deprecated, tk_kw_Experimental, tk_kw_UnImplemented];

  // The keywords by the letter that they start with
  Lape_Keywords: array['A'..'Z'] of array of TLapeKeyword = (
    // A
    (
      (Keyword: 'AND'; Token: tk_op_AND),
      (Keyword: 'ARRAY'; Token: tk_kw_Array),
      (Keyword: 'AT'; Token: tk_kw_At)
    ),
    // B
    (
      (Keyword: 'BEGIN'; Token: tk_kw_Begin)
    ),
    // C
    (
      (Keyword: 'CASE'; Token: tk_kw_Case),
      (Keyword: 'CONST'; Token: tk_kw_Const),
      (Keyword: 'CONSTREF'; Token: tk_kw_ConstRef)
    ),
    // D
    (
      (Keyword: 'DEPRECATED'; Token: tk_kw_Deprecated),
      (Keyword: 'DIV'; Token: tk_op_DIV),
      (Keyword: 'DO'; Token: tk_kw_Do),
      (Keyword: 'DOWNTO'; Token: tk_kw_DownTo)
    ),
    // E
    (
      (Keyword: 'ELSE'; Token: tk_kw_Else),
      (Keyword: 'END'; Token: tk_kw_End),
      (Keyword: 'ENUM'; Token: tk_kw_Enum),
      (Keyword: 'EXCEPT'; Token: tk_kw_Except),
      (Keyword: 'EXPERIMENTAL'; Token: tk_kw_Experimental),
      (Keyword: 'EXTERNAL'; Token: tk_kw_External)
    ),
    // F
    (
      (Keyword: 'FINALLY'; Token: tk_kw_Finally),
      (Keyword: 'FOR'; Token: tk_kw_For),
      (Keyword: 'FORWARD'; Token: tk_kw_Forward),
      (Keyword: 'FUNCTION'; Token: tk_kw_Function)
    ),
    // G
    (),
    // H
    (),
    // I
    (
      (Keyword: 'IF'; Token: tk_kw_If),
      (Keyword: 'IN'; Token: tk_op_IN),
      (Keyword: 'IS'; Token: tk_op_IS)
    ),
    // J
    (),
    // K
    (),
    // L
    ({$IFDEF Lape_PascalLabels}
      (Keyword: 'LABEL'; Token: tk_kw_Label)
    {$ENDIF}),
    // M
    (
      (Keyword: 'MOD'; Token: tk_op_MOD)
    ),
    // N
    (
      (Keyword: 'NOT'; Token: tk_op_NOT)
    ),
    // O
    (
      (Keyword: 'OBJECT'; Token: tk_kw_Object),
      (Keyword: 'OF'; Token: tk_kw_Of),
      (Keyword: 'OPERATOR'; Token: tk_kw_Operator),
      (Keyword: 'OR'; Token: tk_op_OR),
      (Keyword: 'OUT'; Token: tk_kw_Out),
      (Keyword: 'OVERLOAD'; Token: tk_kw_Overload),
      (Keyword: 'OVERRIDE'; Token: tk_kw_Override)
    ),
    // P
    (
      (Keyword: 'PACKED'; Token: tk_kw_Packed),
      (Keyword: 'PRIVATE'; Token: tk_kw_Private),
      (Keyword: 'PROCEDURE'; Token: tk_kw_Procedure),
      (Keyword: 'PROGRAM'; Token: tk_kw_Program),
      (Keyword: 'PROPERTY'; Token: tk_kw_Property)
    ),
    // Q
    (),
    // R
    (
      (Keyword: 'RECORD'; Token: tk_kw_Record),
      (Keyword: 'REPEAT'; Token: tk_kw_Repeat)
    ),
    // S
    (
      (Keyword: 'SET'; Token: tk_kw_Set),
      (Keyword: 'SHL'; Token: tk_op_SHL),
      (Keyword: 'SHR'; Token: tk_op_SHR),
      (Keyword: 'STATIC'; Token: tk_kw_Static),
      (Keyword: 'STRICT'; Token: tk_kw_Strict)
    ),
    // T
    (
      (Keyword: 'THEN'; Token: tk_kw_Then),
      (Keyword: 'TO'; Token: tk_kw_To),
      (Keyword: 'TRY'; Token: tk_kw_Try),
      (Keyword: 'TYPE'; Token: tk_kw_Type)
    ),
    // U
    (
      (Keyword: 'UNIMPLEMENTED'; Token: tk_kw_UnImplemented),
      (Keyword: 'UNION'; Token: tk_kw_Union),
      (Keyword: 'UNTIL'; Token: tk_kw_Until)
    ),
    // V
    (
      (Keyword: 'VAR'; Token: tk_kw_Var)
    ),
    // W
    (
      (Keyword: 'WHILE'; Token: tk_kw_While),
      (Keyword: 'WITH'; Token: tk_kw_With)
    ),
    // X
    (
      (Keyword: 'XOR'; Token: tk_op_XOR)
    ),
    // Y
    (),
    // Z
    ()
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
    0,                                  //op_Unknown

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

function LapeTokenToString(Token: EParserToken): lpString;
function ParserTokenToOperator(Token: EParserToken): EOperator;
function FloatToStrDot(Value: Extended): string;
function StrToFloatDot(const Str: string): Extended;
function StrToFloatDotDef(const Str: string; Default: Extended): Extended;
function DetermineIntType(IntType: ELapeBaseType; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(Left, Right: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(const Str: lpString; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(const Str: lpString; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(const Str: lpString): ELapeBaseType; overload;
function DetermineIntType(i: Int64; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(i: Int64; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(i: Int64): ELapeBaseType; overload;
function DetermineIntType(i1, i2: Int64; MinSize: UInt8): ELapeBaseType; overload;
function DetermineIntType(i1, i2: Int64; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType; overload;
function DetermineIntType(i1, i2: Int64): ELapeBaseType; overload;
function DetermineIntType(Size: Integer; Signed: Boolean): ELapeBaseType; overload;

implementation

uses
  typinfo, Math,
  lpmessages;

{$IFDEF Delphi}
  {$WARN WIDECHAR_REDUCED OFF}
{$ENDIF}

var
  LapeFormatSettings: TFormatSettings;

function TLapeTokenizerBase.Identify: EParserToken;
var
  p: PlpChar;
begin
  FTokStart := FPos;
  p := PlpChar(FDoc) + FPos;

  case p^ of
    #0: Result := tk_Unknown;
    #9, #11, #12, #32:
      begin
        while (p[1] in [#9, #11, #12, #32]) do
          Inc(p);
        Result := tk_WhiteSpace;
      end;
    #10: Result := tk_NewLine;
    #13:
      begin
        if (p[1] = #10) then
          Inc(p);
        Result := tk_NewLine;
      end;

    //Compare Operators
    '=': Result := tk_sym_Equals;
    '>':
      if (p[1] = '=') then
      begin
        Inc(p);
        Result := tk_cmp_GreaterThanOrEqual;
      end
      else
        Result := tk_cmp_GreaterThan;
    '<':
      case p[1] of
        '=':
          begin
            Inc(p);
            Result := tk_cmp_LessThanOrEqual;
          end;
        '>':
          begin
            Inc(p);
            Result := tk_cmp_NotEqual;
          end;
        else
          Result := tk_cmp_LessThan;
      end;

    //Operators
    ':':
      if (p[1] = '=') then
      begin
        Inc(p);
        Result := tk_op_Assign;
      end
      else
        Result := tk_sym_Colon;

    {Divide, Comment, AssignDiv}
    '/':
      case p[1] of
        '/':
          begin
            Inc(p);
            while (not (p[1] in [#13, #10, #0])) do
              Inc(p);
            Result := tk_Comment;
          end;
        '=':
          begin
            Inc(p);
            Result := tk_op_AssignDiv;
          end;
        else
          Result := tk_op_Divide;
      end;

    {Minus, AssignMinus}
    '-':
      if (p[1] = '=') then
      begin
        Inc(p);
        Result := tk_op_AssignMinus;
      end
      else
        Result := tk_op_Minus;

    {Multiply, Power, AssignMul}
    '*':
      case p[1] of
        '*':
          begin
            Inc(p);
            Result := tk_op_Power;
          end;
        '=':
          begin
            Inc(p);
            Result := tk_op_AssignMul;
          end;
        else
          Result := tk_op_Multiply;
      end;

    {Plus, AssignPlus}
    '+':
      if (p[1] = '=') then
      begin
        Inc(p);
        Result := tk_op_AssignPlus;
      end
      else
        Result := tk_op_Plus;

    '@': Result := tk_op_Addr;
    '^': Result := tk_sym_Caret;

    //Symbols
    ']': Result := tk_sym_BracketClose;
    '[': Result := tk_sym_BracketOpen;
    ',': Result := tk_sym_Comma;
    '.':
      if (p[1] = '.') then
      begin
        Inc(p);
        Result := tk_sym_DotDot;
      end
      else
        Result := tk_sym_Dot;

    ')': Result := tk_sym_ParenthesisClose;
    '(':
      if (p[1] = '*') then
      begin
        p := ScanStarComment(p);
        Result := tk_Comment;
      end
      else
        Result := tk_sym_ParenthesisOpen;

    ';': Result := tk_sym_SemiColon;
    '{':
      if (p[1] = '$') then
      begin
        Inc(FPos);
        Result := setTok(tk_Directive);
        if (not HandleDirective()) then
          UnknownDirective();
        Exit;
      end
      else
      begin
        p := ScanBraceComment(p);
        Result := tk_Comment;
      end;

    //Integer and Float
    '0'..'9':
      begin
        Result := tk_typ_Integer;
        while (p[1] in ['0'..'9', '_']) do
          Inc(p);

        // a fraction: .5
        if (p[1] = '.') and (p[2] in ['0'..'9']) then
        begin
          Result := tk_typ_Float;
          Inc(p, 2);
          while (p[1] in ['0'..'9', '_']) do
            Inc(p);
        end;

        // an exponent, with a sign or without: e3, E-3
        if (p[1] in ['e', 'E']) and ((p[2] in ['0'..'9']) or ((p[2] in ['+', '-']) and (p[3] in ['0'..'9']))) then
        begin
          Result := tk_typ_Float;
          Inc(p, 2);
          while (p[1] in ['0'..'9', '_']) do
            Inc(p);
        end;
      end;
    '$':
      if (p[1] in ['0'..'9', 'A'..'F', 'a'..'f']) then
      begin
        Inc(p);
        while (p[1] in ['0'..'9', 'A'..'F', 'a'..'f', '_']) do
          Inc(p);
        Result := tk_typ_Integer_Hex;
      end
      else
        Result := tk_Unknown;
    '%':
      if (p[1] in ['0'..'1']) then
      begin
        Inc(p);
        while (p[1] in ['0'..'1', '_']) do
          Inc(p);
        Result := tk_typ_Integer_Bin;
      end
      else
        Result := tk_Unknown;

    // a name, or a keyword
    'A'..'Z', '_', 'a'..'z':
      Result := ParseKeyword(p);

    #34: //heredoc string
      begin
        p := ScanHereString(p);
        Result := tk_typ_HereString;
      end;
    #39:
      begin
        p := ScanString(p);
        Result := tk_typ_String;
      end;

    '#':
      if (p[1] in ['0'..'9']) then
      begin
        Inc(p);
        while (p[1] in ['0'..'9']) do
          Inc(p);
        Result := tk_typ_Char;
      end
      else if (p[1] = '$') and (p[2] in ['0'..'9', 'A'..'F', 'a'..'f']) then
      begin
        Inc(p, 2);
        while (p[1] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(p);
        Result := tk_typ_Char;
      end
      else
        Result := tk_Unknown;
    else
      Result := tk_Unknown;
  end;

  FPos := p - PlpChar(FDoc);
  FTok := Result;
end;

function TLapeTokenizerBase.HandleDirective: Boolean;
var
  Directive, Argument: lpString;
  Depth: Integer;
begin
  Assert({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil);
  try
    if (not (Next() in [tk_Identifier] + ParserToken_Keywords)) then
      LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(FTok)], DocPos);
    Directive := TokString;

    NextNoWhiteSpace();
    if (getChar(0) = '}') then
      Argument := ''
    else
    begin
      Depth := 1;
      while (Depth > 1) or (not (getChar(1) in ['}', #0])) do
      begin
        Inc(FPos);

        case getChar(0) of
          '{': Inc(Depth);
          '}': Dec(Depth);
          #10, #13:
            begin
              if (getChar(0) = #13) and (getChar(1) = #10) then
                Inc(FPos);
              Inc(FTokLines);
              FTokLineStart := FPos + 1;
            end;
        end;
      end;

      Argument := TokString;
      Inc(FPos);
    end;

    Result := FOnHandleDirective(Self, Directive, Argument);
  finally
    if (getChar(0) <> '}') then
      LapeExceptionFmt(lpeExpectedOther, [LapeTokenToString(FTok), '}'], DocPos);
  end;
end;

function TLapeTokenizerBase.LineEnds(p: PlpChar): PlpChar;
begin
  if (p^ = #13) and (p[1] = #10) then
    Inc(p);
  Inc(FTokLines);
  FTokLineStart := (p - PlpChar(FDoc)) + 1;
  Result := p;
end;

function TLapeTokenizerBase.ScanBraceComment(p: PlpChar): PlpChar;
var
  Level: Integer;
begin
  Level := 1;

  repeat
    Inc(p);
    case p^ of
      '{': Inc(Level);
      '}': Dec(Level);
      #10, #13: p := LineEnds(p);
    end;
  until (p^ = #0) or (Level = 0);

  if (Level > 0) then
  begin
    FPos := p - PlpChar(FDoc);
    LapeException(lpeUnClosedComment, DocPos);
  end;
  Result := p;
end;

function TLapeTokenizerBase.ScanStarComment(p: PlpChar): PlpChar;
var
  Level: Integer;
begin
  Level := 1;
  Inc(p, 2);

  while (p^ <> #0) do
  begin
    if (p^ = '(') and (p[1] = '*') then
    begin
      Inc(Level);
      Inc(p);
    end
    else if (p^ = '*') and (p[1] = ')') then
    begin
      Dec(Level);
      Inc(p);
      if (Level = 0) then
        Exit(p);
    end
    else if (p^ in [#10, #13]) then
      p := LineEnds(p);
    Inc(p);
  end;

  FPos := p - PlpChar(FDoc);
  LapeException(lpeUnClosedComment, DocPos);
  Result := p;
end;

function TLapeTokenizerBase.ScanHereString(p: PlpChar): PlpChar;
begin
  Inc(p);
  while (not (p^ in [#34, #0])) do
  begin
    if (p^ in [#10, #13]) then
      p := LineEnds(p);
    Inc(p);
  end;

  if (p^ = #0) then
  begin
    FPos := p - PlpChar(FDoc);
    LapeExceptionFmt(lpeErrorScanningString, ['EOF'], DocPos);
  end;
  Result := p;
end;

function TLapeTokenizerBase.ScanString(p: PlpChar): PlpChar;
begin
  repeat
    Inc(p);
  until (p^ in [#39, #0, #13, #10]);

  if (p^ <> #39) then
  begin
    FPos := p - PlpChar(FDoc);
    LapeExceptionFmt(lpeErrorScanningString, ['EOL'], DocPos);
  end;
  Result := p;
end;

function TLapeTokenizerBase.ParseKeyword(var Stop: PlpChar): EParserToken;
var
  Start: PlpChar;
  Letter: lpChar;
  i: Integer;
begin
  Start := Stop;
  while (Stop[1] in ['0'..'9', 'A'..'Z', '_', 'a'..'z']) do // safe because null termination
    Inc(Stop);

  Letter := LapeUpcaseAlpha(Start^);
  if (Letter >= 'A') and (Letter <= 'Z') then
    for i := 0 to High(Lape_Keywords[AnsiChar(Ord(Letter))]) do
      with Lape_Keywords[AnsiChar(Ord(Letter))][i] do
        if LapeSameStringAlpha(Start, (Stop - Start) + 1, Keyword) then
          Exit(Token);

  Result := tk_Identifier;
end;

procedure TLapeTokenizerBase.UnknownDirective;
begin
  LapeException(lpeUnknownDirective, DocPos);
end;

function TLapeTokenizerBase.setTok(ATok: EParserToken): EParserToken;
begin
  FTok := ATok;
  Result := ATok;
end;

function TLapeTokenizerBase.getTokString: lpString;
begin
  if (FPos < 0) or (FPos >= Length(FDoc)) or (FPos < FTokStart) then
    Result := ''
  else
    SetString(Result, PlpChar(FDoc) + FTokStart, FPos - FTokStart + 1);
end;

function TLapeTokenizerBase.getDocPos: TDocPos;
begin
  if (OverridePos <> nil) then
    Exit(OverridePos^);

  // (lines and columns are counted from 1)
  Result.Line := FLine + 1;
  if (FLineStart > FTokStart) then
    Result.Col := 0
  else
    Result.Col := 1 + UInt32(FTokStart - FLineStart);
  if (FFileName <> '') then
  begin
    if RelativeFileNames then
      Result.FileName := FFileNameRelative
    else
      Result.FileName := FFileName;
  end else
    Result.FileName := '';
end;

constructor TLapeTokenizerBase.Create(AFileName: lpString = '');
begin
  inherited Create();

  FFileName := AFileName;
  FFileNameRelative := ExtractRelativePath(IncludeTrailingPathDelimiter(GetCurrentDir()), FFileName);
  FOnHandleDirective := nil;

  OverridePos := nil;

  Reset();
end;

procedure TLapeTokenizerBase.Reset;
begin
  FTok := tk_NULL;
  FTokStart := 0;
  FPos := -1;
  FLine := 0;
  FLineStart := 0;
  FTokLines := 0;
  FTokLineStart := 0;
end;

function TLapeTokenizerBase.getState: Pointer;
begin
  New(PTokenizerState(Result));
  with PTokenizerState(Result)^ do
  begin
    TokStart := FTokStart;
    Pos := FPos;
    Tok := FTok;
    Line := FLine;
    LineStart := FLineStart;
    TokLines := FTokLines;
    TokLineStart := FTokLineStart;
  end;
end;

procedure TLapeTokenizerBase.setState(const State: Pointer);
begin
  with PTokenizerState(State)^ do
  begin
    FTokStart := TokStart;
    FPos := Pos;
    FTok := Tok;
    FLine := Line;
    FLineStart := LineStart;
    FTokLines := TokLines;
    FTokLineStart := TokLineStart;
  end;
end;

procedure TLapeTokenizerBase.freeState(const State: Pointer);
begin
  Dispose(PTokenizerState(State));
end;

function TLapeTokenizerBase.getChar(Offset: Integer = 0): lpChar;
begin
  if (FPos + Offset < 0) or (FPos + Offset >= Length(FDoc)) then
    Result := #0
  else
    Result := PlpChar(FDoc)[FPos + Offset];
end;

function TLapeTokenizerBase.TokInteger(out Value: UInt64): Boolean;
var
  p, Last: PlpChar;
  Digit: UInt32;
begin
  Value := 0;
  Result := False;
  if (FPos < 0) or (FPos >= Length(FDoc)) or (FPos < FTokStart) then
    Exit;

  p := PlpChar(FDoc) + FTokStart;
  Last := PlpChar(FDoc) + FPos;

  case FTok of
    tk_typ_Integer:
      while (p <= Last) do
      begin
        if (p^ <> '_') then
        begin
          Digit := Ord(p^) - Ord('0');
          // (ten times what there is and this digit: more than there is room for)
          if (Value > (High(UInt64) - Digit) div 10) then
            Exit;
          Value := Value * 10 + Digit;
        end;
        Inc(p);
      end;

    tk_typ_Integer_Hex:
      begin
        Inc(p); // (its $)
        while (p <= Last) do
        begin
          if (p^ <> '_') then
          begin
            case p^ of
              '0'..'9': Digit := Ord(p^) - Ord('0');
              'A'..'F': Digit := Ord(p^) - Ord('A') + 10;
              else      Digit := Ord(p^) - Ord('a') + 10;
            end;
            if (Value shr 60 <> 0) then
              Exit;
            Value := (Value shl 4) or Digit;
          end;
          Inc(p);
        end;
      end;

    tk_typ_Integer_Bin:
      begin
        Inc(p); // (its %)
        while (p <= Last) do
        begin
          if (p^ <> '_') then
          begin
            if (Value shr 63 <> 0) then
              Exit;
            Value := (Value shl 1) or UInt64(Ord(p^) - Ord('0'));
          end;
          Inc(p);
        end;
      end;

    else
      Exit;
  end;

  Result := True;
end;

function TLapeTokenizerBase.TokFloat(out Value: lpFloat): Boolean;
var
  Digits: ShortString;
  p, Last: PlpChar;
  Count, Code: Integer;
  Mask: TFPUExceptionMask;
begin
  Value := 0;
  Result := False;
  if (FTok <> tk_typ_Float) or (FPos < 0) or (FPos >= Length(FDoc)) or (FPos < FTokStart) then
    Exit;

  p := PlpChar(FDoc) + FTokStart;
  Last := PlpChar(FDoc) + FPos;
  Count := 0;
  while (p <= Last) and (Count < High(Digits)) do
  begin
    if (p^ <> '_') then
    begin
      Inc(Count);
      Digits[Count] := AnsiChar(p^);
    end;
    Inc(p);
  end;

  Mask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  try
    if (p <= Last) then
      Val(StringReplace(string(getTokString()), '_', '', [rfReplaceAll]), Value, Code)
    else
    begin
      SetLength(Digits, Count);
      Val(Digits, Value, Code);
    end;
  finally
    // (what was raised while masked is not raised by what comes next)
    ClearExceptions(False);
    SetExceptionMask(Mask);
  end;
  Result := (Code = 0);
end;

function TLapeTokenizerBase.TokCharCode(out Code: UInt32): Boolean;
var
  p, Last: PlpChar;
  Digit: UInt32;
begin
  Code := 0;
  Result := False;
  if (FTok <> tk_typ_Char) or (FPos < 0) or (FPos >= Length(FDoc)) or (FPos < FTokStart) then
    Exit;

  p := PlpChar(FDoc) + FTokStart + 1;
  Last := PlpChar(FDoc) + FPos;

  if (p^ = '$') then
  begin
    Inc(p);
    while (p <= Last) do
    begin
      case p^ of
        '0'..'9': Digit := Ord(p^) - Ord('0');
        'A'..'F': Digit := Ord(p^) - Ord('A') + 10;
        else      Digit := Ord(p^) - Ord('a') + 10;
      end;
      Code := (Code shl 4) or Digit;
      if (Code > High(UInt16)) then
        Exit;
      Inc(p);
    end;
  end
  else
    while (p <= Last) do
    begin
      Code := Code * 10 + UInt32(Ord(p^) - Ord('0'));
      if (Code > High(UInt16)) then
        Exit;
      Inc(p);
    end;

  Result := True;
end;

function TLapeTokenizerBase.tempRollBack(PrevTok: EParserToken): EParserToken;
begin
  Result := FTok;
  FTok := PrevTok;
  FPos := FTokStart - 1;
  FTokStart := FPos;
  FTokLines := 0;
end;

function TLapeTokenizerBase.Next: EParserToken;
begin
  Inc(FPos);

  if (FTok = tk_NewLine) then
  begin
    FLineStart := FPos;
    Inc(FLine);
  end
  else if (FTokLines > 0) then
  begin
    FLineStart := FTokLineStart;
    Inc(FLine, FTokLines);
  end;
  FTokLines := 0;

  if (FPos < 0) or (FPos >= Length(FDoc)) then
    Result := setTok(tk_NULL)
  else
    Result := Identify();
end;

function TLapeTokenizerBase.NextNoWhiteSpace: EParserToken;
begin
  repeat
    Result := Next();
  until (not (Result in TokWhiteSpace));
end;

function WithoutMark(const Doc: lpString): lpString;
begin
  {$IFDEF Lape_Unicode}
  if (Doc <> '') and (Doc[1] = #$FEFF) then
    Exit(Copy(Doc, 2, Length(Doc)));
  {$ELSE}
  if (Length(Doc) >= 3) and (Doc[1] = #$EF) and (Doc[2] = #$BB) and (Doc[3] = #$BF) then
    Exit(Copy(Doc, 4, Length(Doc)));
  {$ENDIF}
  Result := Doc;
end;

constructor TLapeTokenizerString.Create(ADoc: lpString; AFileName: lpString = '');
begin
  inherited Create(AFileName);

  FDoc := WithoutMark(ADoc);
end;

constructor TLapeTokenizerFile.Create(AFileName: UnicodeString = ''; AEncoding:TEncoding = nil);
var
  StrList: TStringList;
  Stream: TFileStream;
  Mark: array[0..2] of Byte;
begin
  StrList := TStringList.Create();
  Stream := TFileStream.Create(string(AFileName), fmOpenRead or fmShareDenyWrite);
  try
    if (AEncoding = nil) then
      if (Stream.Read(Mark, SizeOf(Mark)) = SizeOf(Mark)) and (Mark[0] = $EF) and (Mark[1] = $BB) and (Mark[2] = $BF) then
        AEncoding := TEncoding.UTF8
      else
      begin
        AEncoding := TEncoding.ANSI;
        Stream.Position := 0;
      end;

    StrList.LoadFromStream(Stream, AEncoding);
    inherited Create(lpString(StrList.Text), lpString(AFileName));
  finally
    Stream.Free();
    StrList.Free();
  end;
end;

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

function FloatToStrDot(Value: Extended): string;
begin
  Result := FloatToStr(Value, LapeFormatSettings);
end;

function StrToFloatDot(const Str: string): Extended;
begin
  Result := StrToFloat(Str, LapeFormatSettings);
end;

function StrToFloatDotDef(const Str: string; Default: Extended): Extended;
begin
  Result := StrToFloatDef(Str, Default, LapeFormatSettings);
end;

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

function DetermineIntType(const Str: lpString; MinSize: UInt8): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(Str), MinSize);
end;

function DetermineIntType(const Str: lpString; MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(Str), MinType, DoGrow);
end;

function DetermineIntType(const Str: lpString): ELapeBaseType;
var
  i: Int64;
begin
  if (Length(Str) = 0) then
    Exit(ltUnknown);

  if TryStrToInt64(Str, i) then
    Result := DetermineIntType(i)
  else
    Result := ltUInt64;
end;

function DetermineIntType(i: Int64; MinSize: UInt8): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i), MinSize);
end;

function DetermineIntType(i: Int64;  MinType: ELapeBaseType; DoGrow: Boolean = True): ELapeBaseType;
begin
  Result := DetermineIntType(DetermineIntType(i), MinType, DoGrow);
end;

// The smallest integer type that holds i: a signed one when one of that size does
function DetermineIntType(i: Int64): ELapeBaseType;
begin
  if (i < 0) then
  begin
         if (i >= Low(Int8))  then Result := ltInt8
    else if (i >= Low(Int16)) then Result := ltInt16
    else if (i >= Low(Int32)) then Result := ltInt32
    else                           Result := ltInt64;
  end else
  begin
         if (i <= High(Int8))   then Result := ltInt8
    else if (i <= High(UInt8))  then Result := ltUInt8
    else if (i <= High(Int16))  then Result := ltInt16
    else if (i <= High(UInt16)) then Result := ltUInt16
    else if (i <= High(Int32))  then Result := ltInt32
    else if (i <= High(UInt32)) then Result := ltUInt32
    else                             Result := ltInt64
  end;
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

initialization
  {$IFDEF LoadDefaultFormatSettings}
  GetLocaleFormatSettings(0, FormatSettings);
  {$ENDIF}

  LapeFormatSettings := FormatSettings;
  LapeFormatSettings.DecimalSeparator := '.';

end.

