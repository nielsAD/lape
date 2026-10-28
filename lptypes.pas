{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  General basetypes and objects.
}
unit lptypes;

{$I lape.inc}

interface

uses
  Classes, SysUtils;

const
  LapeSystemCaseSensitive = {$IFDEF Unix}True{$ELSE}False{$ENDIF};

type
  PUInt8 = ^UInt8;
  PInt8 = ^Int8;
  PUInt16 = ^UInt16;
  PInt16 = ^Int16;
  PUInt32 = ^UInt32;
  PInt32 = ^Int32;
  PUInt64 = ^UInt64;
  PInt64 = ^Int64;

  PByteBool = ^ByteBool;

  {$IFDEF FPC}
  NativeInt = PtrInt;
  NativeUInt = PtrUInt;
  PNativeInt = ^NativeInt;
  PNativeUInt = ^NativeUInt;
  {$ELSE}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  PPtrInt = ^PtrInt;
  PPtrUInt = ^PtrUInt;

  SizeInt = {$IFDEF CPUX64}Int64{$ELSE}Int32{$ENDIF};
  SizeUInt = {$IFDEF CPUX64}UInt64{$ELSE}UInt32{$ENDIF};
  PSizeInt = ^SizeInt;
  PSizeUInt = ^SizeUInt;

  PLongBool = ^LongBool;
  {$ENDIF}

  {$IFDEF Lape_Unicode}
  lpString = UnicodeString;
  lpChar = WideChar;
  lpCharInt = UInt16;
  {$ELSE}
  lpString = AnsiString;
  lpChar = AnsiChar;
  lpCharInt = UInt8;
  {$ENDIF}
  PlpString = ^lpString;
  PlpChar = ^lpChar;
  PlpCharInt = ^lpCharInt;

  {$IFDEF Lape_NoExtended}
  lpFloat = Double;
  {$ELSE}
  lpFloat = Extended;
  {$ENDIF}
  PlpFloat = ^lpFloat;

  TVarRecArray = array of TVarRec;
  TVarRecContainer = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    CVar: Variant;
    case SizeInt of
      vtExtended: (CExtended: Extended);
      vtString  : (CShortString: shortstring);
      vtCurrency: (CCurrency: Currency);
      vtInt64   : (CInt64: Int64);
      {$IFDEF FPC}
      vtQWord   : (CQWord: QWord);
      {$ENDIF}
  end;
  TVarRecContainerArray = array of TVarRecContainer;

  TVarRecList = {$IFDEF Lape_SmallCode}packed{$ENDIF} record
    Containers: TVarRecContainerArray;
    VarRecs: TVarRecArray;
  end;

  PStringArray = ^TStringArray;
  TStringArray = array of lpString;
  TByteArray = array of Byte;
  TIntegerArray = array of Integer;
  TInt64Array = array of Int64;
  TSingleArray = array of Single;
  TDoubleArray = array of Double;
  TExtendedArray = array of Extended;
  TInitBool = (bUnknown, bFalse, bTrue);

  TCodeArray = TByteArray;
  PCodeArray = ^TCodeArray;

  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

  PDocPos = ^TDocPos;
  TDocPos = record
    Line, Col: {$IFDEF Lape_SmallCode}UInt16{$ELSE}UInt32{$ENDIF};
    FileName: lpString;
  end;
  TDocPosArray = array of TDocPos;

  TLapeRange = record
    Lo, Hi: Int64;
  end;

  TCodePos = SizeUInt;
  PCodePos = ^TCodePos;

  TCodeOffset = SizeInt;
  PCodeOffset = ^TCodeOffset;

  {$IFDEF Lape_SmallCode}
  //Means Lape can only locate up to 65kb of local variables (per stackframe)
  TStackInc = Int16;
  TStackOffset = UInt16;
  TVarStackOffset = UInt16;
  TPointerOffset = Int16;
  TParamSize = UInt16;
  EvalBool = Boolean;
  {$ELSE}
  TStackInc = NativeInt;
  TStackOffset = NativeUInt;
  TVarStackOffset = NativeUInt;
  TPointerOffset = NativeInt;
  TParamSize = NativeUInt;
  EvalBool = LongBool;
  {$ENDIF}

  PStackInc = ^TStackInc;
  PStackOffset = ^TStackOffset;
  PVarStackOffset = ^TVarStackOffset;
  PPointerOffset = ^TPointerOffset;
  PParamSize = ^TParamSize;
  PEvalBool = ^EvalBool;

  EMemoryPos = (mpNone, mpStack, mpMem, mpVar);
  TLapeEvalProc = procedure(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
  TLapeImportedProc = procedure(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
  TLapeImportedFunc = procedure(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  ELoopType = (loopUp, loopDown, loopOver);
  ELoopOverWhat = (loopOverArray, loopOverEnum, loopOverSet);
  EPropertyType = (ptRead, ptWrite);
  EMethodDef = (mdUnspecified, mdOperator, mdProperty);

  ELapeBaseType = (
    ltUnknown,
    ltUInt8, ltInt8, ltUInt16, ltInt16, ltUInt32, ltInt32, ltUInt64, ltInt64,    //Integer
    ltCurrency, ltSingle, ltDouble, {$IFNDEF Lape_NoExtended}ltExtended,{$ENDIF} //Real
    ltBoolean, ltByteBool, ltWordBool, ltLongBool,                               //Boolean
    ltAnsiChar, ltWideChar,                                                      //Char
    ltShortString, ltAnsiString, ltWideString, ltUnicodeString,                  //String
    ltVariant,                                                                   //Variant
    ltSmallEnum, ltLargeEnum, ltSmallSet, ltLargeSet,                            //Set
    ltPointer,                                                                   //Pointer
    ltRecord, ltUnion,                                                           //Struct
    ltDynArray, ltStaticArray,                                                   //Array
    ltScriptMethod, ltImportedMethod                                             //Methods
  );

  LapeIntegerTypeRange = ltUInt8..ltInt64;

  EOperatorAssociative = (assocNone, assocLeft, assocRight);
  EOperator = (
    op_Unknown,

    //Same order as lptokenizer.EParserToken
    op_cmp_Equal,
    op_cmp_GreaterThan,
    op_cmp_GreaterThanOrEqual,
    op_cmp_LessThan,
    op_cmp_LessThanOrEqual,
    op_cmp_NotEqual,

    op_Addr,
    op_AND,
    op_Assign,
    op_AssignDiv,
    op_AssignMinus,
    op_AssignMul,
    op_AssignPlus,
    op_Deref,
    op_DIV,
    op_Divide,
    op_Dot,
    op_IN,
    op_IS,
    op_Index,
    op_Minus,
    op_MOD,
    op_Multiply,
    op_NOT,
    op_OR,
    op_Plus,
    op_Power,
    op_SHL,
    op_SHR,
    op_XOR,

    //Extra
    op_UnaryMinus,
    op_UnaryPlus
  );

  TLapeSmallSetItem = 0..31;
  TLapeLargeSetItem = 0..255;
  TLapeSmallSet = set of TLapeSmallSetItem;
  TLapeLargeSet = set of TLapeLargeSetItem;

  PLapeSmallSet = ^TLapeSmallSet;
  PLapeLargeSet = ^TLapeLargeSet;

  {$IF DEFINED(FPC) AND (NOT DEFINED(MSWINDOWS)) AND (FPC_FULLVERSION >= 20501)}
    {$DEFINE Interface_CDecl}
  {$IFEND}

  TLapeBaseClass = class(TObject, IUnknown)
  protected
    function _AddRef: Integer; {$IFDEF Interface_CDecl}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFDEF Interface_CDecl}cdecl{$ELSE}stdcall{$ENDIF};
  public
    constructor Create; virtual;
    {$IFDEF Lape_TrackObjects}
    destructor Destroy; override;
    {$ENDIF}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IFDEF Interface_CDecl}cdecl{$ELSE}stdcall{$ENDIF};
    function GetSelf: TLapeBaseClass; inline;
  end;

  TLapeBaseDeclClass = class(TLapeBaseClass)
  protected
    function getDocPos: TDocPos; virtual; abstract;
  public
    Tag: Int64;
    property DocPos: TDocPos read getDocPos;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeList<_T> = class(TLapeBaseClass)
  public type
    TTArray = {$IFDEF Delphi}TArray<_T>{$ELSE}array of _T{$ENDIF};
  protected
    FDuplicates: TDuplicates;
    FSorted: Boolean;
    FItems: TTArray;
    FCount: Integer;

    function getSorted: Boolean; virtual;
    procedure setSorted(Sort: Boolean; DoUpdate: Boolean); overload; virtual;
    procedure setSorted(Sort: Boolean); overload; virtual;
    function getItem(Index: Integer): _T; virtual;
    procedure setItem(Index: Integer; Item: _T); virtual;
  public
    InvalidVal: _T;

    constructor Create(InvalidValue: _T; ADuplicates: TDuplicates; ASort: Boolean); reintroduce; virtual;
    procedure Clear; virtual;

    function Add(Item: _T): Integer; virtual;
    function Insert(Item: _T; Index: Integer): Integer; virtual;

    function Delete(Index: Integer): _T; virtual;
    function DeleteItem(Item: _T): _T; virtual;
    function LastIndexOf(Item: _T): Integer; virtual;

    procedure MoveItem(AFrom, ATo: Integer); virtual;

    function IndexOf(Item: _T; Lo, Hi: Integer): Integer; overload; virtual;
    function IndexOf(Item: _T): Integer; overload; virtual;
    function ExistsItem(Item: _T): Boolean;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem write setItem; default;
    property Count: Integer read FCount;
    property Sorted: Boolean read getSorted write setSorted;
  end;

  // Items sorted by a hash of theirs for a binary search
  {$IFDEF FPC}generic{$ENDIF} TLapeHashList<_T> = class(TLapeBaseClass)
  protected
    FHashes: array of UInt32;
    FItems: {$IFDEF Delphi}TArray<_T>{$ELSE}array of _T{$ENDIF};
    FCount: Integer;

    function getHash(Index: Integer): UInt32; inline;
    function getItem(Index: Integer): _T; inline;
  public
    function Find(Hash: UInt32): Integer; virtual;
    procedure Insert(Hash: UInt32; Item: _T); virtual;
    procedure Clear; virtual;

    property Count: Integer read FCount;
    property Hashes[Index: Integer]: UInt32 read getHash;
    property Items[Index: Integer]: _T read getItem; default;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeStack<_T> = class(TLapeBaseClass)
  public type
    TTArray = {$IFDEF Delphi}TArray<_T>{$ELSE}array of _T{$ENDIF};
  protected
    FItems: TTArray;
    FCount: Integer;

    function getTop: _T; virtual;
    procedure setTop(Item: _T); virtual;
  public
    procedure Clear; virtual;
    function Push(Item: _T): Integer; virtual;
    function Pop: _T; virtual;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Top: _T read getTop write setTop;
    property Count: Integer read FCount;
  end;

  TLapeStringList = class({$IFDEF FPC}specialize{$ENDIF} TLapeList<lpString>)
  public
    constructor Create(InvalidValue: lpString; ADuplicates: TDuplicates); reintroduce; virtual;

    function IndexOf(Item: lpString; Lo, Hi: Integer): Integer; override;
    function LastIndexOf(Item: lpString): Integer; override;
    function Implode(ASep: lpString): lpString; virtual;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeDictionary<_T> = class(TLapeBaseClass)
  public type
    TValues = {$IFDEF Delphi}TArray<_T>{$ELSE}array of _T{$ENDIF};
  protected type
    TItem = record
      Key: lpString;
      Hash: UInt32;
      Prev: Integer;
      Value: _T;
    end;
  protected const
    NoItem = -1;
    Removed = -2;
  protected
    FItems: array of TItem;
    FUsed: Integer;
    FCount: Integer;
    FLast: array of Integer;
    FCaseSensitive: Boolean;

    procedure Rebuild(Grow: Boolean); virtual;
    function SameKey(const A, B: lpString): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function HashOf(const AKey: lpString): UInt32; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function ItemOf(const AKey: lpString; AHash: UInt32): Integer; virtual;
    function ItemAt(Index: Integer): Integer; virtual;
    procedure RemoveItem(i: Integer); virtual;

    function getItem(const AKey: lpString): _T; virtual;
    procedure setItem(const AKey: lpString; const AValue: _T); virtual;
    function getKey(Index: Integer): lpString; virtual;
    function getItemI(Index: Integer): _T; virtual;
  public
    InvalidVal: _T;

    constructor Create(InvalidValue: _T; ACaseSensitive: Boolean = False); reintroduce; virtual;

    procedure Add(const AKey: lpString; const AValue: _T); overload; virtual;
    procedure Add(const AKey: lpString; AHash: UInt32; const AValue: _T); overload; virtual;
    function Find(const AKey: lpString; out AValue: _T): Boolean; overload; virtual;
    function Find(const AKey: lpString; AHash: UInt32; out AValue: _T): Boolean; overload; virtual;
    function FindAll(const AKey: lpString; AHash: UInt32): TValues; virtual;
    function ExistsKey(const AKey: lpString): Boolean; virtual;
    function Remove(const AKey: lpString; AHash: UInt32; const AValue: _T): Boolean; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear(KeepArraysAllocated: Boolean = False); virtual;

    property Items[const AKey: lpString]: _T read getItem write setItem; default;
    property Key[Index: Integer]: lpString read getKey;
    property ItemsI[Index: Integer]: _T read getItemI;
    property Count: Integer read FCount;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeNotifier<_T> = class(TLapeBaseClass)
  public type
    TNotifyProc = procedure(Sender: _T);
    TNotifiers = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TNotifyProc>;
  protected
    FNotifiers: TNotifiers;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    procedure AddProc(const Proc: TNotifyProc); virtual;
    procedure Notify(Sender: _T); virtual;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeSorter<_T> = class
  public type
    TWeightArr = array of _T;
  protected
    class procedure DoQuickSort(const Arr: PByte; const ElSize: SizeInt; var Weights: TWeightArr; iLo, iHi: SizeInt; const SortUp: Boolean);
  public
    class procedure QuickSort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TWeightArr; const SortUp: Boolean);
  end;

  TLapeDeclaration = class;
  TLapeDeclarationClass = class of TLapeDeclaration;
  TLapeDeclArray = array of TLapeDeclaration;

  TLapeDeclarationList = class(TLapeBaseClass)
  protected type
    TDict = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TLapeBaseDeclClass>;
  protected const
    // a list that is no longer than this is looked through for a name
    LookedThrough = 8;
  protected
    FItems: TLapeDeclArray;
    FCount: Integer;
    FNames: TDict;

    function ByName: Boolean; virtual;
    procedure addNames; virtual;
    procedure addItem(ADecl: TLapeDeclaration); virtual;

    function getItem(Index: Integer): TLapeDeclaration; virtual;
  public
    FreeDecls: Boolean;

    constructor Create(ManageDeclarations: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    // Declares it here, at the end: it is taken out of the list that it was in
    procedure addDeclaration(ADecl: TLapeDeclaration); virtual;
    function Has(ADecl: TLapeDeclaration): Boolean; virtual;

    function IndexOf(ADecl: TLapeDeclaration): Integer; virtual;

    procedure Delete(ADecl: TLapeDeclaration; DoFree: Boolean = False); overload; virtual;
    procedure Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False); overload; virtual;

    // The first that was declared with the name
    function Get(AName: lpString; out Decl: TLapeDeclaration): Boolean; overload; virtual;
    function Get(AName: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean; overload; virtual;
    function GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray; virtual;

    function ExportToArray: TLapeDeclArray; virtual;

    property Count: Integer read FCount;
    property Items[Index: Integer]: TLapeDeclaration read getItem; default;
  end;

  TLapeDeclarationHintCallback = procedure(Msg: lpString; Args: array of const; ADocPos: TDocPos) of object;

  ELapeDeclarationHint = (ldhDeprecated, ldhExperimental, ldhUnImplemented);
  ELapeDeclarationHints = set of ELapeDeclarationHint;
  TLapeDeclarationHints = record
    Types: ELapeDeclarationHints;
    Message: lpString;
  end;

  TLapeDeclaration = class(TLapeBaseDeclClass)
  protected
    FList: TLapeDeclarationList;

    FName: lpString;
    FNameHash: UInt32;

    FHints: TLapeDeclarationHints;

    function getDocPos: TDocPos; override;
    procedure setName(const AName: lpString); virtual;
  public
    _DocPos: TDocPos;
    IsUsed: Boolean; // false (default value) var has not been used yet

    constructor Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    destructor Destroy; override;

    procedure AddHint(Typ: ELapeDeclarationHint; Msg: lpString = ''); virtual;
    procedure CopyHints(From: TLapeDeclaration); virtual;
    procedure WriteHints(Callback: TLapeDeclarationHintCallback; ADocPos: TDocPos); virtual;
    function HasHints: Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function HasDocPos: Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
    // no name, or one the compiler made (script cannot write it)
    function isHidden: Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
    // What is declared in it goes (of a type: what the type declares)
    procedure ClearDeclarations; virtual;

    // The list that it is declared in (a list adds it, and deletes it)
    property DeclarationList: TLapeDeclarationList read FList;
    property Name: lpString read FName write setName;
    property NameHash: UInt32 read FNameHash;
    property Hints: TLapeDeclarationHints read FHints;
  end;

const
  op_Invoke = op_Index;
  op_Label = op_Addr;

  {$IFNDEF FPC}
  LineEnding = {$IFDEF MSWINDOWS}#13#10{$ELSE}#10{$ENDIF};
  DirectorySeparator = PathDelim;
  {$ENDIF}

  {$IFDEF Lape_Unicode}
  ltString = ltUnicodeString;
  ltChar = ltWideChar;
  ltCharInt = ltUInt16;
  {$ELSE}
  ltString = ltAnsiString;
  ltChar = ltAnsiChar;
  ltCharInt = ltUInt8;
  {$ENDIF}

  {$IF SizeOf(NativeInt) = SizeOf(Int64)}
  ltNativeInt = ltInt64;
  ltNativeUInt = ltUInt64;
  {$ELSE}
  ltNativeInt = ltInt32;
  ltNativeUInt = ltUInt32;
  {$IFEND}

  ltSizeInt = ltNativeInt;
  ltSizeUInt = ltNativeUInt;

  {$IFDEF Lape_SmallCode}
  ltEvalBool = ltBoolean;
  {$ELSE}
  ltEvalBool = ltLongBool;
  {$ENDIF}

  {$IFDEF Lape_NoExtended}
  ltFloat = ltDouble;
  {$ELSE}
  ltFloat = ltExtended;
  {$ENDIF}

  LapeTypeSize: array[ELapeBaseType] of Integer = (
    -1,
    SizeOf(UInt8), SizeOf(Int8), SizeOf(UInt16), SizeOf(Int16), SizeOf(UInt32),
    SizeOf(Int32), SizeOf(UInt64), SizeOf(Int64),
    SizeOf(Currency), SizeOf(Single), SizeOf(Double), {$IFNDEF Lape_NoExtended}SizeOf(Extended),{$ENDIF}
    SizeOf(Boolean), SizeOf(ByteBool), SizeOf(WordBool), SizeOf(LongBool),
    SizeOf(AnsiChar), SizeOf(WideChar),
    SizeOf(ShortString), SizeOf(AnsiString), SizeOf(WideString), SizeOf(UnicodeString),
    SizeOf(Variant),
    SizeOf(Int32), SizeOf(Int32), SizeOf(TLapeSmallSet), SizeOf(TLapeLargeSet),
    SizeOf(Pointer),
    -1, -1,
    SizeOf(Pointer), -1,
    SizeOf(TCodePos), SizeOf(Pointer)
  );

  {$IF SizeOf(TCodePos) > SizeOf(Pointer)}
    {$MESSAGE Fatal 'TCodePos should be <= Pointer for universal methods'}
  {$IFEND}

  LapeBaseTypes = [Low(ELapeBaseType)..High(ELapeBaseType)];
  LapeIntegerTypes = [Low(LapeIntegerTypeRange)..High(LapeIntegerTypeRange)];
  LapeSignedIntegerTypes = [ltInt8, ltInt16, ltInt32, ltInt64];
  LapeSignedOrdinalTypes = LapeSignedIntegerTypes + [ltSmallEnum, ltLargeEnum];
  LapeUnsignedIntegerTypes = [ltUInt8, ltUInt16, ltUInt32, ltUInt64];

  LapeRealTypes = [ltCurrency..{$IFDEF Lape_NoExtended}ltDouble{$ELSE}ltExtended{$ENDIF}];
  LapeBoolTypes = [ltBoolean..ltLongBool];
  LapeStringTypes = [ltShortString..ltUnicodeString];
  LapeCharTypes = [ltAnsiChar..ltWideChar];
  LapeEnumTypes = [ltSmallEnum..ltLargeEnum] + LapeBoolTypes;
  LapeSetTypes = [ltSmallSet..ltLargeSet];
  LapeArrayTypes = [ltDynArray..ltStaticArray] + LapeStringTypes;
  LapeStructTypes = [ltRecord..ltUnion];
  LapeProcTypes = [ltScriptMethod..ltImportedMethod];
  LapeOrdinalTypes = LapeIntegerTypes + LapeBoolTypes + LapeCharTypes + LapeEnumTypes;
  LapeRefTypes = LapeArrayTypes - [ltShortString, ltStaticArray];
  LapePointerTypes = [ltPointer] + LapeProcTypes + LapeRefTypes;
  LapeStackTypes = LapeOrdinalTypes + LapeRealTypes + LapeSetTypes;
  LapeIfTypes = LapeOrdinalTypes + LapeStringTypes + LapePointerTypes + LapeRealTypes + [ltVariant, ltRecord];
  LapeNoInitTypes = LapeOrdinalTypes + LapeRealTypes + [ltUnknown, ltPointer, ltScriptMethod, ltImportedMethod, ltShortString];

  NullDocPos: TDocPos = (Line: 0; Col: 0; FileName: '');
  NullRange: TLapeRange = (Lo: 0; Hi: -1);

  UnaryOperators = [op_Addr, op_Deref, op_NOT, op_UnaryMinus, op_UnaryPlus];
  BinaryOperators = [op_AND, op_OR, op_XOR];
  CompareOperators = [op_cmp_Equal, op_cmp_GreaterThan, op_cmp_GreaterThanOrEqual, op_cmp_LessThan, op_cmp_LessThanOrEqual, op_cmp_NotEqual];

  LabelOperators = CompareOperators;

  CompoundOperators = [op_AssignPlus, op_AssignMinus, op_AssignDiv, op_AssignMul];
  AssignOperators = [op_Assign] + CompoundOperators;

  OverloadableOperators = [op_Assign, op_Plus, op_Minus, op_Multiply, op_Divide, op_DIV, op_Power, op_MOD, op_IN, op_IS, op_SHL, op_SHR] + CompareOperators + BinaryOperators + CompoundOperators;

  op_str: array[EOperator] of lpString = ('',
    '=', '>', '>=', '<', '<=', '<>', '@', 'and', ':=', '/=', '-=', '*=', '+=',
    '^', 'div', '/', '.' , 'in', 'is', '[', '-', 'mod', '*', 'not', 'or', '+',
    '**', 'shl', 'shr', 'xor', '-', '+'
  );
  op_name: array[EOperator] of lpString = ('',
    'EQ', 'GT', 'GTEQ', 'LT', 'LTEQ', 'NEQ', 'ADDR', 'AND', 'ASGN', 'DIVASGN', 'SUBASGN', 'MULASGN', 'ADDASGN',
    'DEREF', 'IDIV', 'DIV', 'DOT', 'IN', 'IS', 'IDX', 'SUB', 'MOD', 'MUL', 'NOT', 'OR', 'ADD',
    'POW', 'SHL', 'SHR', 'XOR', 'UMIN', 'UPOS'
  );

var
  lowUInt8: UInt8 = Low(UInt8);     highUInt8: UInt8 = High(UInt8);
  lowInt8: Int8 = Low(Int8);        highInt8: Int8 = High(Int8);
  lowUInt16: UInt16 = Low(UInt16);  highUInt16: UInt16 = High(UInt16);
  lowInt16: Int16 = Low(Int16);     highInt16: Int16 = High(Int16);
  lowUInt32: UInt32 = Low(UInt32);  highUInt32: UInt32 = High(UInt32);
  lowInt32: Int32 = Low(Int32);     highInt32: Int32 = High(Int32);
  lowUInt64: UInt64 = Low(UInt64);  highUInt64: UInt64 = High(UInt64);
  lowInt64: Int64 = Low(Int64);     highInt64: Int64 = High(Int64);

  LapeTypeLow: array[LapeIntegerTypeRange] of Pointer = (
    @lowUInt8, @lowInt8, @lowUInt16, @lowInt16, @lowUInt32, @lowInt32, @lowUInt64, @lowInt64
  );

  LapeTypeHigh: array[LapeIntegerTypeRange] of Pointer = (
    @highUInt8, @highInt8, @highUInt16, @highInt16, @highUInt32, @highInt32, @highUInt64, @highInt64
  );

// Alpha suffix meaning a-z and A-Z are treated as the same (case insensitive)
function LapeUpcaseAlpha(c: lpChar): lpChar; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeLowercaseAlpha(c: lpChar): lpChar; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeSameStringAlpha(P: PlpChar; Len: Integer; const Str: lpString): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeHashAlpha(const Str: lpString): UInt32; {$IFDEF Lape_Inline}inline;{$ENDIF}

// regular
function LapeUpperCase(const Str: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeLowerCase(const Str: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeSameString(const A, B: lpString): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeHash(const Str: lpString): UInt32; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeHashBytes(Data: PByte; Size: Integer): UInt32;
function LapeSameValue(BaseType: ELapeBaseType; Ptr, Value: Pointer): Boolean;

function LapeTypeToString(Token: ELapeBaseType): lpString;
function LapeOperatorToString(Token: EOperator): lpString;
function LapeMethodDefToString(Value: EMethodDef): lpString;

function PointerToString(const p: Pointer): lpString;

function VarTypeToVType(v: TVarType): SizeInt;
function VariantToVarRec(const v: Variant): TVarRec; overload;
function VariantToVarRec(const v: Variant; out Container: TVarRecContainer): TVarRec; overload;
function VariantArrToConstArr(v: array of Variant): TVarRecList;

procedure Swap(var A, B: Pointer); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
procedure Swap(var A, B: Boolean); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

function _BCompare(Arr: PUInt8; const Item: PUInt8; const Size: Integer; const Lo, Hi: Integer): Integer; {$IFDEF Lape_Inline}inline;{$ENDIF}

function _BSearch8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer;
function _BSearch16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer;
function _BSearch32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer;
function _BSearch64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer;

procedure _Insert8(Arr: PUInt8; var Index: Integer);
procedure _Insert16(Arr: PUInt16; var Index: Integer);
procedure _Insert32(Arr: PUInt32; var Index: Integer);
procedure _Insert64(Arr: PUInt64; var Index: Integer);

procedure _Reverse32(const Arr: PByte; const Len: SizeInt);
procedure _Reverse64(const Arr: PByte; const Len: SizeInt);
procedure _Reverse(const Arr: PByte; const ElSize, Len: SizeInt);

{$IFDEF Lape_TrackObjects}
var
  lpgCounter: Integer;
  lpgList: TList;
{$ENDIF}

implementation

uses
  typinfo, variants, {$IFNDEF FPC}Generics.Defaults,{$ENDIF}
  lpmessages;

function LapeUpcaseAlpha(c: lpChar): lpChar;
begin
  if (c >= 'a') and (c <= 'z') then
    Result := lpChar(Ord(c) - 32)
  else
    Result := c;
end;

function LapeLowercaseAlpha(c: lpChar): lpChar;
begin
  if (c >= 'A') and (c <= 'Z') then
    Result := lpChar(Ord(c) + 32)
  else
    Result := c;
end;

function LapeSameStringAlpha(P: PlpChar; Len: Integer; const Str: lpString): Boolean;
var
  S, Last: PlpChar;
  c: UInt32;
begin
  if (Len <> Length(Str)) then
    Exit(False);

  S := PlpChar(Str);
  Last := P + Len;
  while (P < Last) do
  begin
    if (P^ <> S^) then
    begin
      c := Ord(P^) or $20;
      if (c <> (Ord(S^) or $20)) or (c < Ord('a')) or (c > Ord('z')) then
        Exit(False);
    end;
    Inc(P);
    Inc(S);
  end;
  Result := True;
end;

// FNV-1a
function LapeHash(const Str: lpString): UInt32;
var
  P, Last: PlpChar;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Result := 2166136261;
  P := PlpChar(Str);
  Last := P + Length(Str);
  while (P < Last) do
  begin
    Result := (Result xor UInt32(Ord(P^))) * 16777619;
    Inc(P);
  end;
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

// FNV-1a, of Size bytes
function LapeHashBytes(Data: PByte; Size: Integer): UInt32;
var
  i: Integer;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Result := 2166136261;
  for i := 0 to Size - 1 do
    Result := (Result xor Data[i]) * 16777619;
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

function LapeSameValue(BaseType: ELapeBaseType; Ptr, Value: Pointer): Boolean;
begin
  case BaseType of
    // 1 byte
    ltUInt8, ltInt8, ltBoolean, ltByteBool, ltAnsiChar:
      Result := (PUInt8(Ptr)^ = PUInt8(Value)^);
    // 2 byte
    ltUInt16, ltInt16, ltWordBool, ltWideChar:
      Result := (PUInt16(Ptr)^ = PUInt16(Value)^);
    // 4 byte
    ltUInt32, ltInt32, ltLongBool:
      Result := (PUInt32(Ptr)^ = PUInt32(Value)^);
    // 8 byte
    ltUInt64, ltInt64:
      Result := (PUInt64(Ptr)^ = PUInt64(Value)^);

    else
      LapeException(lpeImpossible);
  end;
end;

// FNV-1a, of the name with a-z as A-Z: the same of every way that a name can be written
function LapeHashAlpha(const Str: lpString): UInt32;
var
  P, Last: PlpChar;
  c: UInt32;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Result := 2166136261;
  P := PlpChar(Str);
  Last := P + Length(Str);
  while (P < Last) do
  begin
    c := Ord(P^);
    if (c >= Ord('a')) and (c <= Ord('z')) then
      Dec(c, Ord('a') - Ord('A'));
    Result := (Result xor c) * 16777619;
    Inc(P);
  end;
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

function LapeUpperCase(const Str: lpString): lpString;
var
  P, Last: PlpChar;
begin
  P := PlpChar(Str);
  Last := P + Length(Str);
  while (P < Last) and ((P^ < 'a') or (P^ > 'z')) do
    Inc(P);
  if (P = Last) then
    Exit(Str);

  SetString(Result, PlpChar(Str), Length(Str));
  P := PlpChar(Result);
  Last := P + Length(Result);
  while (P < Last) do
  begin
    if (P^ >= 'a') and (P^ <= 'z') then
      P^ := lpChar(Ord(P^) - (Ord('a') - Ord('A')));
    Inc(P);
  end;
end;

function LapeLowerCase(const Str: lpString): lpString;
var
  P, Last: PlpChar;
begin
  P := PlpChar(Str);
  Last := P + Length(Str);
  while (P < Last) and ((P^ < 'A') or (P^ > 'Z')) do
    Inc(P);
  if (P = Last) then
    Exit(Str);

  SetString(Result, PlpChar(Str), Length(Str));
  P := PlpChar(Result);
  Last := P + Length(Result);
  while (P < Last) do
  begin
    if (P^ >= 'A') and (P^ <= 'Z') then
      P^ := lpChar(Ord(P^) + (Ord('a') - Ord('A')));
    Inc(P);
  end;
end;

function LapeSameString(const A, B: lpString): Boolean;
var
  PA, PB, Last: PlpChar;
  c: UInt32;
begin
  if (Pointer(A) = Pointer(B)) then
    Exit(True);
  if (Length(A) <> Length(B)) then
    Exit(False);

  PA := PlpChar(A);
  PB := PlpChar(B);
  Last := PA + Length(A);
  while (PA < Last) do
  begin
    if (PA^ <> PB^) then
    begin
      c := Ord(PA^) or $20;
      if (c <> (Ord(PB^) or $20)) or (c < Ord('a')) or (c > Ord('z')) then
        Exit(False);
    end;
    Inc(PA);
    Inc(PB);
  end;
  Result := True;
end;

function LapeTypeToString(Token: ELapeBaseType): lpString;
begin
  Result := lpString(getEnumName(TypeInfo(ELapeBaseType), Ord(Token)));
  Delete(Result, 1, 2);
end;

function LapeOperatorToString(Token: EOperator): lpString;
begin
  Result := lpString(getEnumName(TypeInfo(EOperator), Ord(Token)));
  Delete(Result, 1, 3);
end;

function LapeMethodDefToString(Value: EMethodDef): lpString;
begin
  if Value = mdUnspecified then
    Result := 'Method'
  else
  begin
    Result := lpString(getEnumName(TypeInfo(EMethodDef), Ord(Value)));
    Delete(Result, 1, 2);
  end;
end;

function PointerToString(const p: Pointer): lpString;
begin
  if ((p = nil) or (PPointer(p)^ = nil)) then
    Result := 'nil'
  else
    Result := lpString('0x'+IntToHex(PtrUInt(p^), 1));
end;

function VarTypeToVType(v: TVarType): SizeInt;
begin
  case (v and VarTypeMask) of
    varSingle,
    varDouble:   Result := vtExtended;
    varCurrency: Result := vtCurrency;
    varOleStr:   Result := vtWideString;
    varUString:  Result := vtUnicodeString;
    varDispatch: Result := vtInterface;
    varBoolean:  Result := vtBoolean;
    varVariant:  Result := vtVariant;
    varSmallint,
    varShortInt,
    varByte,
    varWord,
    varLongWord,
    varInteger:  Result := vtInteger;
    varInt64:    Result := vtInt64;
    varString:   Result := vtString;
    {$IFDEF FPC}
    varDecimal:  Result := vtInteger;
    varQWord:    Result := vtQWord;
    {$ENDIF}
    else
      Result := vtVariant;
  end;
end;

function VariantToVarRec(const v: Variant): TVarRec;
begin
  Result.VType := VarTypeToVType(VarType(v));
  case Result.VType of
    vtInteger:       Result.VInteger := v;
    vtBoolean:       Result.VBoolean := v;
    vtAnsiString:    Result.VAnsiString := TVarData(v).VString;
    vtCurrency:      Result.VCurrency := @TVarData(v).VCurrency;
    vtVariant:       Result.VVariant := @v;
    vtInterface:     Result.VInterface := TVarData(v).VDispatch;
    vtWideString:    Result.VWideString := TVarData(v).VOleStr;
    vtUnicodeString: Result.VUnicodeString := TVarData(v).VUString;
    vtInt64:         Result.VInt64 := @TVarData(v).VInt64;
    {$IFDEF FPC}
    vtChar:          Result.VChar := v;
    vtWideChar:      Result.VWideChar := v;
    vtQWord:         Result.VQWord := @TVarData(v).VQWord;
    {$ENDIF}
    else VarCastError();
  end;
end;

function VariantToVarRec(const v: Variant; out Container: TVarRecContainer): TVarRec;
begin
  Container.CVar := v;
  Result.VType := VarTypeToVType(VarType(v));

  case Result.VType of
    vtExtended:
      begin
        Container.CExtended := Extended(Container.CVar);
        Result.VExtended := @Container.CExtended;
      end;
    vtString:
      begin
        Container.CShortString := ShortString(Container.CVar);
        Result.VString := @Container.CShortString;
      end;
    vtCurrency:
      begin
        Container.CCurrency := Currency(Container.CVar);
        Result.VCurrency := @Container.CCurrency;
      end;
    vtInt64:
      begin
        Container.CInt64 := Int64(Container.CVar);
        Result.VInt64 := @Container.CInt64;
      end;
    {$IFDEF FPC}
    vtQWord:
      begin
        Container.CQWord := QWord(Container.CVar);
        Result.VQWord := @Container.CQWord;
      end;
    {$ENDIF}
    else Result := VariantToVarRec(Container.CVar);
  end;
end;

function VariantArrToConstArr(v: array of Variant): TVarRecList;
var
  i: Integer;
begin
  SetLength(Result.VarRecs, Length(v));
  SetLength(Result.Containers, Length(v));

  for i := 0 to High(v) do
    Result.VarRecs[i] := VariantToVarRec(v[i], Result.Containers[i]);
end;

procedure Swap(var A, B: Pointer);
var
  C: Pointer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Swap(var A, B: Boolean);
var
  C: Boolean;
begin
  C := A;
  A := B;
  B := C;
end;

function _BCompare(Arr: PUInt8; const Item: PUInt8; const Size: Integer; const Lo, Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := Lo to Hi do
  begin
    if CompareMem(Arr, Item, Size) then
      Exit(i);

    Inc(Arr, Size);
  end;

  Result := -1;
end;

function _BSearch8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer;
var
  mVal: UInt8;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

function _BSearch16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer;
var
  mVal: UInt16;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

function _BSearch32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer;
var
  mVal: UInt32;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

function _BSearch64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer;
var
  mVal: UInt64;
  mIndex: Integer;
begin
  while (Lo <= Hi) do
  begin
    mIndex := (Lo + Hi) div 2;
    mVal := Arr[mIndex];
    if (Item = mVal) then
    begin
      while (mIndex > Lo) and (Arr[mIndex - 1] = Item) do
        Dec(mIndex);
      Exit(mIndex);
    end
    else if (Item < mVal) then
      Hi := mIndex - 1
    else
      Lo := mIndex + 1;
  end;
  Result := -(Lo + 1);
end;

procedure _Insert8(Arr: PUInt8; var Index: Integer);
var
  Item: UInt8;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch8(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt8));
    Arr[Index] := Item;
  end;
end;

procedure _Insert16(Arr: PUInt16; var Index: Integer);
var
  Item: UInt16;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch16(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt16));
    Arr[Index] := Item;
  end;
end;

procedure _Insert32(Arr: PUInt32; var Index: Integer);
var
  Item: UInt32;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch32(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt32));
    Arr[Index] := Item;
  end;
end;

procedure _Insert64(Arr: PUInt64; var Index: Integer);
var
  Item: UInt64;
  Hi: Integer;
begin
  Item := Arr[Index];
  Hi := Index;

  if (Index > 0) and (Item < Arr[Index - 1]) then
  begin
    Index := _BSearch64(Arr, Item, 0, Hi);
    if (Index < 0) then
      Index := -(Index + 1);

    Move(Arr[Index], Arr[Index + 1], (Hi - Index) * SizeOf(UInt64));
    Arr[Index] := Item;
  end;
end;

class procedure TLapeSorter{$IFNDEF FPC}<_T>{$ENDIF}.DoQuickSort(const Arr: PByte; const ElSize: SizeInt; var Weights: TWeightArr; iLo, iHi: SizeInt; const SortUp: Boolean);
var
  Lo, Hi: SizeInt;
  Mid, T: _T;
  Item: PByte;
  {$IFNDEF FPC}
  Comparer: IComparer<_T>;
  {$ENDIF}
begin
  Item := GetMemory(ElSize);

  {$IFNDEF FPC}
  Comparer := TComparer<_T>.Default();
  {$ENDIF}

  repeat
    Lo := iLo;
    Hi := iHi;
    Mid := Weights[(Lo + Hi) shr 1];
    repeat
      {$IFDEF FPC}
      if SortUp then
      begin
        while (Weights[Lo] < Mid) do Inc(Lo);
        while (Weights[Hi] > Mid) do Dec(Hi);
      end else
      begin
        while (Weights[Lo] > Mid) do Inc(Lo);
        while (Weights[Hi] < Mid) do Dec(Hi);
      end;
      {$ELSE}
      if SortUp then
      begin
        while Comparer.Compare(Weights[Lo], Mid) < 0 do Inc(Lo);
        while Comparer.Compare(Weights[Hi], Mid) > 0 do Dec(Hi);
      end else
      begin
        while Comparer.Compare(Weights[Lo], Mid) > 0 do Inc(Lo);
        while Comparer.Compare(Weights[Hi], Mid) < 0 do Dec(Hi);
      end;
      {$ENDIF}

      if (Lo <= Hi) then
      begin
        if (Lo <> Hi) then
        begin
          T := Weights[Lo];
          Weights[Lo] := Weights[Hi];
          Weights[Hi] := T;

          Move(Arr[Lo * ElSize], Item^, ElSize);
          Move(Arr[Hi * ElSize], Arr[Lo * ElSize], ElSize);
          Move(Item^, Arr[Hi * ElSize], ElSize);
        end;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;

    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if Hi - iLo < iHi - Lo then
    begin
      if iLo < Hi then
        DoQuickSort(Arr, ElSize, Weights, iLo, Hi, SortUp);
      iLo := Lo;
    end else
    begin
      if Lo < iHi then
        DoQuickSort(Arr, ElSize, Weights, Lo, iHi, SortUp);
      iHi := Hi;
    end;
  until iLo >= iHi;

  FreeMem(Item);
end;

class procedure TLapeSorter{$IFNDEF FPC}<_T>{$ENDIF}.QuickSort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TWeightArr; const SortUp: Boolean);
begin
  if (Len <> Length(Weights)) then
    LapeExceptionFmt(lpeArrayLengthsDontMatch, [Format('%d,%d', [Len, Length(Weights)])]);

  if (Length(Weights) > 0) then
    DoQuickSort(Arr, ElSize, Weights, Low(Weights), High(Weights), SortUp);
end;

procedure _Reverse32(const Arr: PByte; const Len: SizeInt);
var
  Temp: Int32;
  Lo, Hi: PInt32;
begin
  if (Arr = nil) then
    Exit;

  Lo := PInt32(Arr);
  Hi := PInt32(Arr + (Len * SizeOf(Int32)));
  while (PtrUInt(Lo) < PtrUInt(Hi)) do
  begin
    Temp := Hi^;
    Hi^ := Lo^;
    Lo^ := Temp;

    Dec(Hi);
    Inc(Lo);
  end;
end;

procedure _Reverse64(const Arr: PByte; const Len: SizeInt);
var
  Temp: Int64;
  Lo, Hi: PInt64;
begin
  if (Arr = nil) then
    Exit;

  Lo := PInt64(Arr);
  Hi := PInt64(Arr + (Len * SizeOf(Int64)));
  while (PtrUInt(Lo) < PtrUInt(Hi)) do
  begin
    Temp := Hi^;
    Hi^ := Lo^;
    Lo^ := Temp;

    Dec(Hi);
    Inc(Lo);
  end;
end;

procedure _Reverse(const Arr: PByte; const ElSize, Len: SizeInt);
var
  T: PByte;
  Lo, Hi: PByte;
begin
  if (Arr <> nil) then
    case ElSize of
      SizeOf(Int32): _Reverse32(Arr, Len);
      SizeOf(Int64): _Reverse64(Arr, Len);
      else
      begin
        T := GetMemory(ElSize);

        Lo := Arr;
        Hi := Arr + (Len * ElSize);
        while (PtrUInt(Lo) < PtrUInt(Hi)) do
        begin
          Move(Hi^, T^, ElSize);
          Move(Lo^, Hi^, ElSize);
          Move(T^, Lo^, ElSize);

          Dec(Hi, ElSize);
          Inc(Lo, ElSize);
        end;

        FreeMem(T);
      end;
    end;
end;

function TLapeBaseClass._AddRef: Integer; {$IFDEF Interface_CDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TLapeBaseClass._Release: Integer; {$IFDEF Interface_CDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

constructor TLapeBaseClass.Create;
begin
  inherited;
  {$IFDEF Lape_TrackObjects}
  Inc(lpgCounter);
  lpgList.Add(Pointer(Self));
  if (Self is TLapeDeclaration) and (TLapeDeclaration(Self).Name <> '') then
    WriteLn('New(', ClassName, '::', TLapeDeclaration(Self).Name, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']')
  else
    WriteLn('New(', ClassName, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']');
  {$ENDIF}
end;

{$IFDEF Lape_TrackObjects}
destructor TLapeBaseClass.Destroy;
begin
  Dec(lpgCounter);
  lpgList.Delete(lpgList.IndexOf(Pointer(Self)));
  if (Self is TLapeDeclaration) and (TLapeDeclaration(Self).Name <> '') then
    WriteLn('Free(', ClassName, '::', TLapeDeclaration(Self).Name, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']')
  else
    WriteLn('Free(', ClassName, ') ', lpgCounter, ' -- [',  PtrInt(Self), ']');
  inherited;
end;
{$ENDIF}

function TLapeBaseClass.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IFDEF Interface_CDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TLapeBaseClass.GetSelf: TLapeBaseClass;
begin
  Result := Self;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.getSorted: Boolean;
begin
  Result := FSorted;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.setSorted(Sort: Boolean; DoUpdate: Boolean);
var
  a: TTArray;
  i: Integer;
begin
  Sort := Sort and (Byte(SizeOf(_T)) in [SizeOf(UInt8), SizeOf(UInt16), SizeOf(UInt32), SizeOf(UInt64)]);
  if (FSorted <> Sort) then
  begin
    FSorted := Sort;

    if DoUpdate and Sort and (FCount > 0) then
    begin
      a := ExportToArray();
      Clear();
      for i := 0 to High(a) do
        Add(a[i]);
    end;
  end;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.setSorted(Sort: Boolean);
begin
  setSorted(Sort, True);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Index: Integer): _T;
begin
  if (Index > -1) and (Index < FCount) then
    Result := FItems[Index]
  else
    Result := InvalidVal;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.setItem(Index: Integer; Item: _T);
begin
  if (Index > -1) and (Index < FCount) then
  begin
    if (not FSorted) then
      FItems[Index] := Item
    else
    begin
      Delete(Index);
      Add(Item);
    end;
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [lpString(IntToStr(Index))]);
end;

constructor TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Create(InvalidValue: _T; ADuplicates: TDuplicates; ASort: Boolean);
begin
  inherited Create();

  InvalidVal := InvalidValue;
  FDuplicates := ADuplicates;
  FSorted := False;

  Clear();
  Sorted := ASort;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Add(Item: _T): Integer;
var
  Len: Integer;
begin
  if (FDuplicates in [dupIgnore, dupError]) and ExistsItem(Item) then
    if (FDuplicates = dupError) then
      LapeExceptionFmt(lpeDuplicateDeclaration, ['_T'])
    else
      Exit(-1);

  Result := FCount;
  Inc(FCount);

  Len := Length(FItems);
  if (FCount > Len) then
  begin
    Len := Len + 2 + (Len div 2) + ((Len div 2) mod 2);
    SetLength(FItems, Len);
  end;

  FItems[Result] := Item;
  if FSorted then
    case SizeOf(_T) of
      SizeOf(UInt8):  _Insert8 (@FItems[0], Result);
      SizeOf(UInt16): _Insert16(@FItems[0], Result);
      SizeOf(UInt32): _Insert32(@FItems[0], Result);
      SizeOf(UInt64): _Insert64(@FItems[0], Result);
    end;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Insert(Item: _T; Index: Integer): Integer;
begin
  Result := Add(Item);

  if (Result <> Index) and (Result > -1) and (not Sorted) then
  begin
    MoveItem(Result, Index);
    Result := Index;
  end;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Index: Integer): _T;
var
  Sort: Boolean;
begin
  Result := InvalidVal;
  if (Index > -1) and (Index < FCount) then
  begin
    Result := FItems[Index];

    Sort := Sorted;
    try
      setSorted(False, False);
      MoveItem(Index, FCount - 1);
    finally
      setSorted(Sort, False);
      Dec(FCount);
    end;
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [lpString(IntToStr(Index))]);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.DeleteItem(Item: _T): _T;
var
  Index: Integer;
begin
  // (what goes is mostly what was added last: what is in a list is freed from the last to the first)
  Index := LastIndexOf(Item);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := InvalidVal;
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.MoveItem(AFrom, ATo: Integer);
var
  Item: _T;
begin
  if (AFrom = ATo) or
     (ATo   < 0) or (ATo   >= FCount)  or
     (AFrom < 0) or (AFrom >= FCount)
  then
    Exit;

  if FSorted then
    LapeException(lpeImpossible);

  Item := FItems[AFrom];
  if (ATo > AFrom) then
    Move(FItems[AFrom + 1], FItems[AFrom], (ATo - AFrom) * SizeOf(_T))
  else
    Move(FItems[ATo], FItems[ATo + 1], (AFrom - ATo) * SizeOf(_T));
  Move(Item, FItems[ATo], SizeOf(_T));
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.IndexOf(Item: _T; Lo, Hi: Integer): Integer;
begin
  if (Lo < 0) or (Lo > Hi) or (Hi >= FCount) then
    Exit(-1);

  if FSorted then
    case SizeOf(_T) of
      SizeOf(UInt8) : Result := _BSearch8 (@FItems[0], PUInt8 (@Item)^, Lo, Hi);
      SizeOf(UInt16): Result := _BSearch16(@FItems[0], PUInt16(@Item)^, Lo, Hi);
      SizeOf(UInt32): Result := _BSearch32(@FItems[0], PUInt32(@Item)^, Lo, Hi);
      SizeOf(UInt64): Result := _BSearch64(@FItems[0], PUInt64(@Item)^, Lo, Hi);
    end
  else
    Result := _BCompare(@FItems[Lo], @Item, SizeOf(_T), Lo, Hi);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.LastIndexOf(Item: _T): Integer;
begin
  for Result := FCount - 1 downto 0 do
    if CompareMem(@FItems[Result], @Item, SizeOf(_T)) then
      Exit;
  Result := -1;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.IndexOf(Item: _T): Integer;
begin
  Result := IndexOf(Item, 0, FCount - 1);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.ExistsItem(Item: _T): Boolean;
begin
  Result := (IndexOf(Item) > -1);
end;

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.ImportFromArray(Arr: TTArray);
begin
  // (a copy: what is written to the list is not written to Arr, which who gives it keeps)
  FItems := Copy(Arr);
  FCount := Length(Arr);

  if Sorted then
  begin
    Sorted := False;
    Sorted := True;
  end;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArray: TTArray;
var
  i: Integer;
begin
  SetLength(Result, FCount);
  for i := 0 to FCount - 1 do
    Result[i] := FItems[i];
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getTop: _T;
begin
  if (FCount < 1) then
    LapeException(lpeOutOfStackRange);
  Result := FItems[FCount - 1];
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.setTop(Item: _T);
begin
  if (FCount < 1) then
    LapeException(lpeOutOfStackRange);
  FItems[FCount - 1] := Item;
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Push(Item: _T): Integer;
begin
  Result := FCount;
  if (FCount >= Length(FItems)) then
    SetLength(FItems, 4 + FCount * 2);

  FItems[FCount] := Item;
  Inc(FCount);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Pop: _T;
begin
  Result := getTop();
  Dec(FCount);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.ImportFromArray(Arr: TTArray);
begin
  FItems := Copy(Arr);
  FCount := Length(Arr);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArray: TTArray;
begin
  Result := Copy(FItems, 0, FCount);
end;

constructor TLapeStringList.Create(InvalidValue: lpString; ADuplicates: TDuplicates);
begin
  inherited Create(InvalidValue, ADuplicates, False);
end;

function TLapeStringList.IndexOf(Item: lpString; Lo, Hi: Integer): Integer;
var
  i: Integer;
begin
  if (Lo < 0) or (Lo > Hi) or (Hi >= FCount) then
    Exit(-1);

  for i := Lo to Hi do
    if (FItems[i] = Item) then
      Exit(i);
  Result := -1;
end;

function TLapeStringList.LastIndexOf(Item: lpString): Integer;
begin
  for Result := FCount - 1 downto 0 do
    if (FItems[Result] = Item) then
      Exit;
  Result := -1;
end;

function TLapeStringList.Implode(ASep: lpString): lpString;
var
  i, Len: Integer;
begin
  Result := '';
  if (FCount < 1) then
    Exit;

  Len := Length(ASep) * (FCount - 1);
  for i := 0 to FCount - 1 do
    Len := Len + Length(FItems[i]);

  SetLength(Result, Len);
  Len := 1;

  for i := 0 to FCount - 1 do
  begin
    if (i > 0) and (ASep <> '') then
    begin
      Move(ASep[1], Result[Len], Length(ASep) * SizeOf(ASep[1]));
      Len := Len + Length(ASep);
    end;
    if (FItems[i] <> '') then
    begin
      Move(FItems[i][1], Result[Len], Length(FItems[i]) * SizeOf(FItems[i][1]));
      Len := Len + Length(FItems[i]);
    end;
  end;
end;

function TLapeHashList{$IFNDEF FPC}<_T>{$ENDIF}.getHash(Index: Integer): UInt32;
begin
  Result := FHashes[Index];
end;

function TLapeHashList{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Index: Integer): _T;
begin
  Result := FItems[Index];
end;

function TLapeHashList{$IFNDEF FPC}<_T>{$ENDIF}.Find(Hash: UInt32): Integer;
begin
  Result := _BSearch32(PUInt32(FHashes), Hash, 0, FCount - 1);
  if (Result < 0) then
    Result := -(Result + 1);
end;

procedure TLapeHashList{$IFNDEF FPC}<_T>{$ENDIF}.Insert(Hash: UInt32; Item: _T);
var
  Index, i: Integer;
begin
  if (FCount >= Length(FItems)) then
  begin
    SetLength(FHashes, 8 + (Length(FItems) * 2));
    SetLength(FItems, Length(FHashes));
  end;

  Index := FCount;
  FHashes[Index] := Hash;
  _Insert32(PUInt32(FHashes), Index);

  for i := FCount downto Index + 1 do
    FItems[i] := FItems[i - 1];
  FItems[Index] := Item;
  Inc(FCount);
end;

procedure TLapeHashList{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FHashes := nil;
  FItems := nil;
  FCount := 0;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Rebuild(Grow: Boolean);
var
  i, Used, Place: Integer;
begin
  Used := 0;
  for i := 0 to FUsed - 1 do
    if (FItems[i].Prev <> Removed) then
    begin
      if (Used < i) then
      begin
        FItems[Used] := FItems[i];
        FItems[i].Key := '';
        FItems[i].Value := Default(_T);
      end;
      Inc(Used);
    end;
  FUsed := Used;
  Assert(FUsed = FCount);

  if Grow then
    if (Length(FItems) = 0) then
      SetLength(FItems, 16)
    else if (FUsed * 2 >= Length(FItems)) then
      SetLength(FItems, Length(FItems) * 2);
  if (Length(FLast) <> Length(FItems) * 2) then
    SetLength(FLast, Length(FItems) * 2);

  for i := 0 to High(FLast) do
    FLast[i] := NoItem;
  for i := 0 to FUsed - 1 do
  begin
    Place := Integer(FItems[i].Hash and UInt32(High(FLast)));
    FItems[i].Prev := FLast[Place];
    FLast[Place] := i;
  end;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.SameKey(const A, B: lpString): Boolean;
begin
  if FCaseSensitive then
    Result := (A = B)
  else
    Result := LapeSameString(A, B);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.HashOf(const AKey: lpString): UInt32;
begin
  if FCaseSensitive then
    Result := LapeHash(AKey)
  else
    Result := LapeHashAlpha(AKey);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.ItemOf(const AKey: lpString; AHash: UInt32): Integer;
var
  i: Integer;
begin
  Result := NoItem;
  if (FCount = 0) then
    Exit;

  // (they are met from the one that was added last to the first: the first of the key is the last that is met)
  i := FLast[AHash and UInt32(High(FLast))];
  while (i <> NoItem) do
  begin
    if (FItems[i].Hash = AHash) and SameKey(FItems[i].Key, AKey) then
      Result := i;
    i := FItems[i].Prev;
  end;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.ItemAt(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    Exit(NoItem);
  if (FUsed <> FCount) then
    Rebuild(False);
  Result := Index;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.getItem(const AKey: lpString): _T;
var
  i: Integer;
begin
  i := ItemOf(AKey, HashOf(AKey));
  if (i = NoItem) then
    Result := InvalidVal
  else
    Result := FItems[i].Value;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.setItem(const AKey: lpString; const AValue: _T);
var
  Hash: UInt32;
  i: Integer;
begin
  Hash := HashOf(AKey);
  i := ItemOf(AKey, Hash);
  if (i = NoItem) then
    Add(AKey, Hash, AValue)
  else
    FItems[i].Value := AValue;
end;

// (of one that is not there: nothing, as a list gives)
function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.getKey(Index: Integer): lpString;
begin
  Index := ItemAt(Index);
  if (Index = NoItem) then
    Result := ''
  else
    Result := FItems[Index].Key;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.getItemI(Index: Integer): _T;
begin
  Index := ItemAt(Index);
  if (Index = NoItem) then
    Result := InvalidVal
  else
    Result := FItems[Index].Value;
end;

constructor TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Create(InvalidValue: _T; ACaseSensitive: Boolean = False);
begin
  inherited Create();

  InvalidVal := InvalidValue;
  FCaseSensitive := ACaseSensitive;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Clear(KeepArraysAllocated: Boolean = False);
var
  i: Integer;
begin
  if KeepArraysAllocated then
  begin
    for i := 0 to FUsed - 1 do
    begin
      FItems[i].Key := '';
      FItems[i].Value := Default(_T);
    end;
    for i := 0 to High(FLast) do
      FLast[i] := NoItem;
  end
  else
  begin
    FItems := nil;
    FLast := nil;
  end;

  FUsed := 0;
  FCount := 0;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Add(const AKey: lpString; const AValue: _T);
begin
  Add(AKey, HashOf(AKey), AValue);
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Add(const AKey: lpString; AHash: UInt32; const AValue: _T);
var
  Place: Integer;
begin
  Assert(AHash = HashOf(AKey));
  if (FUsed = Length(FItems)) then
    Rebuild(True);

  Place := Integer(AHash and UInt32(High(FLast)));
  FItems[FUsed].Key := AKey;
  FItems[FUsed].Hash := AHash;
  FItems[FUsed].Prev := FLast[Place];
  FItems[FUsed].Value := AValue;
  FLast[Place] := FUsed;

  Inc(FUsed);
  Inc(FCount);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Find(const AKey: lpString; out AValue: _T): Boolean;
begin
  Result := Find(AKey, HashOf(AKey), AValue);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Find(const AKey: lpString; AHash: UInt32; out AValue: _T): Boolean;
var
  i: Integer;
begin
  Assert(AHash = HashOf(AKey));
  i := ItemOf(AKey, AHash);
  Result := (i <> NoItem);
  if Result then
    AValue := FItems[i].Value
  else
    AValue := InvalidVal;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.FindAll(const AKey: lpString; AHash: UInt32): TValues;
var
  i, Found: Integer;
begin
  Assert(AHash = HashOf(AKey));
  Result := nil;
  if (FCount = 0) then
    Exit;

  Found := 0;
  i := FLast[AHash and UInt32(High(FLast))];
  while (i <> NoItem) do
  begin
    if (FItems[i].Hash = AHash) and SameKey(FItems[i].Key, AKey) then
      Inc(Found);
    i := FItems[i].Prev;
  end;

  // (they are met from the one that was added last to the first)
  SetLength(Result, Found);
  i := FLast[AHash and UInt32(High(FLast))];
  while (Found > 0) do
  begin
    if (FItems[i].Hash = AHash) and SameKey(FItems[i].Key, AKey) then
    begin
      Dec(Found);
      Result[Found] := FItems[i].Value;
    end;
    i := FItems[i].Prev;
  end;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.ExistsKey(const AKey: lpString): Boolean;
begin
  Result := (ItemOf(AKey, HashOf(AKey)) <> NoItem);
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.RemoveItem(i: Integer);
var
  After, Place: Integer;
begin
  // (After: the item that was added after it in the same place of the table, which has it as the one before)
  Place := Integer(FItems[i].Hash and UInt32(High(FLast)));
  After := NoItem;
  if (FLast[Place] <> i) then
  begin
    After := FLast[Place];
    while (FItems[After].Prev <> i) do
      After := FItems[After].Prev;
  end;

  if (After = NoItem) then
    FLast[Place] := FItems[i].Prev
  else
    FItems[After].Prev := FItems[i].Prev;

  FItems[i].Key := '';
  FItems[i].Value := Default(_T);
  FItems[i].Prev := Removed;
  Dec(FCount);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Remove(const AKey: lpString; AHash: UInt32; const AValue: _T): Boolean;
var
  i: Integer;
begin
  Assert(AHash = HashOf(AKey));
  Result := False;
  if (FCount = 0) then
    Exit;

  i := FLast[AHash and UInt32(High(FLast))];
  while (i <> NoItem) do
  begin
    if (FItems[i].Hash = AHash) and SameKey(FItems[i].Key, AKey) and CompareMem(@FItems[i].Value, @AValue, SizeOf(_T)) then
    begin
      RemoveItem(i);
      Exit(True);
    end;
    i := FItems[i].Prev;
  end;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Index: Integer);
var
  i: Integer;
begin
  i := ItemAt(Index);
  if (i = NoItem) then
    LapeExceptionFmt(lpeInvalidIndex, [lpString(IntToStr(Index))]);
  RemoveItem(i);
end;

constructor TLapeNotifier{$IFNDEF FPC}<_T>{$ENDIF}.Create;
begin
  inherited;

  FNotifiers := TNotifiers.Create(nil, dupIgnore, False);
end;

destructor TLapeNotifier{$IFNDEF FPC}<_T>{$ENDIF}.Destroy;
begin
  FNotifiers.Free();

  inherited;
end;

procedure TLapeNotifier{$IFNDEF FPC}<_T>{$ENDIF}.AddProc(const Proc: TNotifyProc);
begin
  FNotifiers.Add(Proc);
end;

procedure TLapeNotifier{$IFNDEF FPC}<_T>{$ENDIF}.Notify(Sender: _T);
var
  i: Integer;
  p: TNotifyProc;
begin
  for i := FNotifiers.Count - 1 downto 0 do
  begin
    p := FNotifiers[i];
    p(Sender);
  end;
end;

function TLapeDeclarationList.ByName: Boolean;
begin
  if (FNames = nil) and (FCount > LookedThrough) then
  begin
    FNames := TDict.Create(nil);
    addNames();
  end;
  Result := (FNames <> nil);
end;

procedure TLapeDeclarationList.addNames;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if (FItems[i].FName <> '') then
      FNames.Add(FItems[i].FName, FItems[i].FNameHash, FItems[i]);
end;

procedure TLapeDeclarationList.addItem(ADecl: TLapeDeclaration);
begin
  if (FCount >= Length(FItems)) then
    SetLength(FItems, 4 + (Length(FItems) * 2));
  FItems[FCount] := ADecl;
  Inc(FCount);

  if (FNames <> nil) and (ADecl.FName <> '') then
    FNames.Add(ADecl.FName, ADecl.FNameHash, ADecl);
end;

function TLapeDeclarationList.getItem(Index: Integer): TLapeDeclaration;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems[Index]
  else
    Result := nil;
end;

constructor TLapeDeclarationList.Create(ManageDeclarations: Boolean = True);
begin
  inherited Create();
  FreeDecls := ManageDeclarations;
end;

destructor TLapeDeclarationList.Destroy;
begin
  Clear();
  inherited;
end;

procedure TLapeDeclarationList.Clear;
var
  Decls: TLapeDeclArray;
  i: Integer;
begin
  if FreeDecls then
    for i := FCount - 1 downto 0 do
      FItems[i].ClearDeclarations();

  Decls := ExportToArray();
  FCount := 0;
  FreeAndNil(FNames);

  for i := High(Decls) downto 0 do
  begin
    if (Decls[i].FList = Self) then
      Decls[i].FList := nil;
    if FreeDecls then
      Decls[i].Free();
  end;
end;

procedure TLapeDeclarationList.addDeclaration(ADecl: TLapeDeclaration);
begin
  if (ADecl = nil) or Has(ADecl) then
    Exit;

  if (ADecl.FList <> nil) then
    ADecl.FList.Delete(ADecl);
  addItem(ADecl);
  ADecl.FList := Self;
end;

function TLapeDeclarationList.Has(ADecl: TLapeDeclaration): Boolean;
begin
  Result := (ADecl <> nil) and (ADecl.FList = Self);
end;

function TLapeDeclarationList.IndexOf(ADecl: TLapeDeclaration): Integer;
begin
  for Result := 0 to FCount - 1 do
    if (FItems[Result] = ADecl) then
      Exit;
  Result := -1;
end;

procedure TLapeDeclarationList.Delete(ADecl: TLapeDeclaration; DoFree: Boolean);
var
  i: Integer;
begin
  if (ADecl = nil) then
    Exit;

  // (what was declared last is often what goes)
  i := FCount - 1;
  while (i > -1) and (FItems[i] <> ADecl) do
    Dec(i);
  if (i < 0) then
    Exit;

  if (i < FCount - 1) then
    Move(FItems[i + 1], FItems[i], (FCount - (i + 1)) * SizeOf(TLapeDeclaration));
  Dec(FCount);
  if (FNames <> nil) and (ADecl.FName <> '') then
    FNames.Remove(ADecl.FName, ADecl.FNameHash, ADecl);

  if (ADecl.FList = Self) then
    ADecl.FList := nil;
  if DoFree then
    ADecl.Free();
end;

procedure TLapeDeclarationList.Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False);
var
  ClassItems: TLapeDeclArray;
  i, Found, Kept: Integer;
begin
  // (in one go: what is not of the class is kept in its order, and what is goes from the last to the first)
  SetLength(ClassItems, FCount);
  Found := 0;
  Kept := 0;
  for i := 0 to FCount - 1 do
    if (FItems[i] is AClass) then
    begin
      ClassItems[Found] := FItems[i];
      Inc(Found);
    end
    else
    begin
      FItems[Kept] := FItems[i];
      Inc(Kept);
    end;
  if (Found = 0) then
    Exit;

  FCount := Kept;
  FreeAndNil(FNames);
  for i := Found - 1 downto 0 do
  begin
    if (ClassItems[i].FList = Self) then
      ClassItems[i].FList := nil;
    if DoFree then
      ClassItems[i].Free();
  end;
end;

function TLapeDeclarationList.Get(AName: lpString; out Decl: TLapeDeclaration): Boolean;
var
  Hash: UInt32;
  i: Integer;
begin
  Decl := nil;
  Result := False;
  if (AName = '') then
    Exit;

  Hash := LapeHashAlpha(AName);
  if ByName() then
    Result := FNames.Find(AName, Hash, TLapeBaseDeclClass(Decl))
  else
    for i := 0 to FCount - 1 do
      if (FItems[i].FNameHash = Hash) and LapeSameString(FItems[i].FName, AName) then
      begin
        Decl := FItems[i];
        Result := True;
        Break;
      end;
end;

function TLapeDeclarationList.Get(AName: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean;
var
  Hash: UInt32;
  Found: TLapeDeclArray;
  i: Integer;
begin
  Decl := nil;
  Result := False;
  if (AName = '') then
    Exit;

  Hash := LapeHashAlpha(AName);
  if ByName() then
  begin
    Found := TLapeDeclArray(FNames.FindAll(AName, Hash));
    for i := 0 to High(Found) do
      if (Found[i] is AClass) then
      begin
        Decl := Found[i];
        Result := True;
        Break;
      end;
  end
  else
    for i := 0 to FCount - 1 do
      if (FItems[i].FNameHash = Hash) and LapeSameString(FItems[i].FName, AName) and (FItems[i] is AClass) then
      begin
        Decl := FItems[i];
        Result := True;
        Break;
      end;
end;

function TLapeDeclarationList.GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray;
var
  i, Found: Integer;
begin
  SetLength(Result, FCount);
  Found := 0;
  for i := 0 to FCount - 1 do
    if (FItems[i] is AClass) then
    begin
      Result[Found] := FItems[i];
      Inc(Found);
    end;
  SetLength(Result, Found);
end;

function TLapeDeclarationList.ExportToArray: TLapeDeclArray;
begin
  Result := Copy(FItems, 0, FCount);
end;

function TLapeDeclaration.getDocPos: TDocPos;
begin
  Result := _DocPos;
end;

procedure TLapeDeclaration.setName(const AName: lpString);
begin
  if (FName = AName) then
    Exit;
  if (FName <> '') or (FList <> nil) then
    LapeExceptionFmt(lpeCannotRename, [FName, AName]);

  FName := AName;
  FNameHash := LapeHashAlpha(FName);
end;

constructor TLapeDeclaration.Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create();
  IsUsed := True;

  if (AName <> '') then
    Name := AName;

  if (ADocPos <> nil) then
    _DocPos := ADocPos^
  else
    _DocPos := NullDocPos;

  if (AList <> nil) then
    AList.addDeclaration(Self);
end;

destructor TLapeDeclaration.Destroy;
begin
  if (FList <> nil) then
    FList.Delete(Self);
  inherited;
end;

procedure TLapeDeclaration.AddHint(Typ: ELapeDeclarationHint; Msg: lpString);
begin
  Include(FHints.Types, Typ);
  if (Msg <> '') then
    FHints.Message := Msg;
end;

procedure TLapeDeclaration.CopyHints(From: TLapeDeclaration);
begin
  FHints := From.Hints;
end;

procedure TLapeDeclaration.WriteHints(Callback: TLapeDeclarationHintCallback; ADocPos: TDocPos);
begin
  Assert(HasHints());

  if (ldhDeprecated in FHints.Types) then
    if (FHints.Message <> '') then
      Callback('"%s" is deprecated: "%s"', [FName, FHints.Message], ADocPos)
    else
      Callback('"%s" is deprecated', [FName], ADocPos);

  if (ldhExperimental in FHints.Types) then
    Callback('"%s" is experimental', [FName], ADocPos);

  if (ldhUnImplemented  in FHints.Types) then
    Callback('"%s" is unimplemented', [FName], ADocPos);
end;

function TLapeDeclaration.HasHints: Boolean;
begin
  Result := FHints.Types <> [];
end;

function TLapeDeclaration.isHidden: Boolean;
begin
  Result := (FName = '') or (FName[1] = '!');
end;

function TLapeDeclaration.HasDocPos: Boolean;
begin
  Result := (DocPos.Line <> NullDocPos.Line) and (DocPos.Col <> NullDocPos.Col);
end;

procedure TLapeDeclaration.ClearDeclarations;
begin
end;

{$IFDEF Lape_TrackObjects}
initialization
  lpgList := TList.Create();
finalization
  lpgList.Free();
{$ENDIF}

end.

