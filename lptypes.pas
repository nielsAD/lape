{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	General basetypes and objects.
}
unit lptypes;

{$I lape.inc}

interface

uses
  Classes, SysUtils;

const
  LapeCaseSensitive = {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  LapeSystemCaseSensitive = {$IFDEF Unix}True{$ELSE}False{$ENDIF};
type
  UInt8 = Byte;
  Int8 = ShortInt;
  UInt16 = Word;
  Int16 = SmallInt;
  UInt32 = LongWord;
  Int32 = LongInt;
  //UInt64 = QWord;    Already defined
  //Int64 = Int64;     Already defined

  PUInt8 = ^UInt8;
  PInt8 = ^Int8;
  PUInt16 = ^UInt16;
  PInt16 = ^Int16;
  PUInt32 = ^UInt32;
  PInt32 = ^Int32;
  PUInt64 = ^UInt64;
  //PInt64 = ^Int64;   Already defined

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
  lpString = ansistring;
  lpChar = AnsiChar;
  lpCharInt = UInt8;
  {$ENDIF}
  PlpString = ^lpString;
  PlpChar = ^lpChar;
  PlpCharInt = ^lpCharInt;

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

  TStringArray = array of lpString;
  TByteArray = array of Byte;
  TIntegerArray = array of Integer;
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

  TLapeRange = record
    Lo, Hi: Int64;
  end;

  TCodePos = NativeUInt;
  PCodePos = ^TCodePos;

  TCodeOffset = NativeInt;
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
  TLapeEvalProc = procedure(const Dest, Left, Right: Pointer);
  TLapeImportedProc = procedure(const Params: PParamArray);
  TLapeImportedFunc = procedure(const Params: PParamArray; const Result: Pointer);

  ELapeBaseType = (
    ltUnknown,
    ltUInt8, ltInt8, ltUInt16, ltInt16, ltUInt32, ltInt32, ltUInt64, ltInt64, //Integer
    ltSingle, ltDouble, ltCurrency, ltExtended,                               //Real
    ltBoolean, ltByteBool, ltWordBool, ltLongBool,                            //Boolean
    ltAnsiChar, ltWideChar,                                                   //Char
    ltShortString, ltAnsiString, ltWideString, ltUnicodeString,               //String
    ltVariant,                                                                //Variant
    ltSmallEnum, ltLargeEnum, ltSmallSet, ltLargeSet,                         //Set
    ltPointer,                                                                //Pointer
    ltRecord, ltUnion,                                                        //Struct
    ltDynArray, ltStaticArray,                                                //Array
    ltScriptMethod, ltImportedMethod                                          //Methods
  );
  LapeIntegerTypeRange = ltUInt8..ltInt64;

  EOperatorAssociative = (assocNone, assocLeft, assocRight);
  EOperator = (
    op_Unknown,

    //Same order as lpparser.EParserToken
    op_cmp_Equal,
    op_cmp_GreaterThan,
    op_cmp_GreaterThanOrEqual,
    op_cmp_LessThan,
    op_cmp_LessThanOrEqual,
    op_cmp_NotEqual,

    op_Addr,
    op_AND,
    op_Assign,
    op_Deref,
    op_DIV,
    op_Divide,
    op_Dot,
    op_IN,
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

  ELapeSmallEnum = (__LapeSmallEnum1,__LapeSmallEnum2,__LapeSmallEnum3,__LapeSmallEnum4,__LapeSmallEnum5,__LapeSmallEnum6,__LapeSmallEnum7,__LapeSmallEnum8,__LapeSmallEnum9,__LapeSmallEnum10,__LapeSmallEnum11,__LapeSmallEnum12,__LapeSmallEnum13,__LapeSmallEnum14,__LapeSmallEnum15,__LapeSmallEnum16,__LapeSmallEnum17,__LapeSmallEnum18,__LapeSmallEnum19,__LapeSmallEnum20,__LapeSmallEnum21,__LapeSmallEnum22,__LapeSmallEnum23,__LapeSmallEnum24,__LapeSmallEnum25,__LapeSmallEnum26,__LapeSmallEnum27,__LapeSmallEnum28,__LapeSmallEnum29,__LapeSmallEnum30,__LapeSmallEnum31,__LapeSmallEnum32);
  ELapeLargeEnum = (__LapeLargeEnum1,__LapeLargeEnum2,__LapeLargeEnum3,__LapeLargeEnum4,__LapeLargeEnum5,__LapeLargeEnum6,__LapeLargeEnum7,__LapeLargeEnum8,__LapeLargeEnum9,__LapeLargeEnum10,__LapeLargeEnum11,__LapeLargeEnum12,__LapeLargeEnum13,__LapeLargeEnum14,__LapeLargeEnum15,__LapeLargeEnum16,__LapeLargeEnum17,__LapeLargeEnum18,__LapeLargeEnum19,__LapeLargeEnum20,__LapeLargeEnum21,__LapeLargeEnum22,__LapeLargeEnum23,__LapeLargeEnum24,__LapeLargeEnum25,__LapeLargeEnum26,__LapeLargeEnum27,__LapeLargeEnum28,__LapeLargeEnum29,__LapeLargeEnum30,__LapeLargeEnum31,__LapeLargeEnum32,__LapeLargeEnum33,__LapeLargeEnum34,__LapeLargeEnum35,__LapeLargeEnum36,__LapeLargeEnum37,__LapeLargeEnum38,__LapeLargeEnum39,__LapeLargeEnum40,__LapeLargeEnum41,__LapeLargeEnum42,__LapeLargeEnum43,__LapeLargeEnum44,__LapeLargeEnum45,__LapeLargeEnum46,__LapeLargeEnum47,__LapeLargeEnum48,__LapeLargeEnum49,__LapeLargeEnum50,
                    __LapeLargeEnum51,__LapeLargeEnum52,__LapeLargeEnum53,__LapeLargeEnum54,__LapeLargeEnum55,__LapeLargeEnum56,__LapeLargeEnum57,__LapeLargeEnum58,__LapeLargeEnum59,__LapeLargeEnum60,__LapeLargeEnum61,__LapeLargeEnum62,__LapeLargeEnum63,__LapeLargeEnum64,__LapeLargeEnum65,__LapeLargeEnum66,__LapeLargeEnum67,__LapeLargeEnum68,__LapeLargeEnum69,__LapeLargeEnum70,__LapeLargeEnum71,__LapeLargeEnum72,__LapeLargeEnum73,__LapeLargeEnum74,__LapeLargeEnum75,__LapeLargeEnum76,__LapeLargeEnum77,__LapeLargeEnum78,__LapeLargeEnum79,__LapeLargeEnum80,__LapeLargeEnum81,__LapeLargeEnum82,__LapeLargeEnum83,__LapeLargeEnum84,__LapeLargeEnum85,__LapeLargeEnum86,__LapeLargeEnum87,__LapeLargeEnum88,__LapeLargeEnum89,__LapeLargeEnum90,__LapeLargeEnum91,__LapeLargeEnum92,__LapeLargeEnum93,__LapeLargeEnum94,__LapeLargeEnum95,__LapeLargeEnum96,__LapeLargeEnum97,__LapeLargeEnum98,__LapeLargeEnum99,__LapeLargeEnum100,
                    __LapeLargeEnum101,__LapeLargeEnum102,__LapeLargeEnum103,__LapeLargeEnum104,__LapeLargeEnum105,__LapeLargeEnum106,__LapeLargeEnum107,__LapeLargeEnum108,__LapeLargeEnum109,__LapeLargeEnum110,__LapeLargeEnum111,__LapeLargeEnum112,__LapeLargeEnum113,__LapeLargeEnum114,__LapeLargeEnum115,__LapeLargeEnum116,__LapeLargeEnum117,__LapeLargeEnum118,__LapeLargeEnum119,__LapeLargeEnum120,__LapeLargeEnum121,__LapeLargeEnum122,__LapeLargeEnum123,__LapeLargeEnum124,__LapeLargeEnum125,__LapeLargeEnum126,__LapeLargeEnum127,__LapeLargeEnum128,__LapeLargeEnum129,__LapeLargeEnum130,__LapeLargeEnum131,__LapeLargeEnum132,__LapeLargeEnum133,__LapeLargeEnum134,__LapeLargeEnum135,__LapeLargeEnum136,__LapeLargeEnum137,__LapeLargeEnum138,__LapeLargeEnum139,__LapeLargeEnum140,__LapeLargeEnum141,__LapeLargeEnum142,__LapeLargeEnum143,__LapeLargeEnum144,__LapeLargeEnum145,__LapeLargeEnum146,__LapeLargeEnum147,__LapeLargeEnum148,__LapeLargeEnum149,__LapeLargeEnum150,
                    __LapeLargeEnum151,__LapeLargeEnum152,__LapeLargeEnum153,__LapeLargeEnum154,__LapeLargeEnum155,__LapeLargeEnum156,__LapeLargeEnum157,__LapeLargeEnum158,__LapeLargeEnum159,__LapeLargeEnum160,__LapeLargeEnum161,__LapeLargeEnum162,__LapeLargeEnum163,__LapeLargeEnum164,__LapeLargeEnum165,__LapeLargeEnum166,__LapeLargeEnum167,__LapeLargeEnum168,__LapeLargeEnum169,__LapeLargeEnum170,__LapeLargeEnum171,__LapeLargeEnum172,__LapeLargeEnum173,__LapeLargeEnum174,__LapeLargeEnum175,__LapeLargeEnum176,__LapeLargeEnum177,__LapeLargeEnum178,__LapeLargeEnum179,__LapeLargeEnum180,__LapeLargeEnum181,__LapeLargeEnum182,__LapeLargeEnum183,__LapeLargeEnum184,__LapeLargeEnum185,__LapeLargeEnum186,__LapeLargeEnum187,__LapeLargeEnum188,__LapeLargeEnum189,__LapeLargeEnum190,__LapeLargeEnum191,__LapeLargeEnum192,__LapeLargeEnum193,__LapeLargeEnum194,__LapeLargeEnum195,__LapeLargeEnum196,__LapeLargeEnum197,__LapeLargeEnum198,__LapeLargeEnum199,__LapeLargeEnum200,
                    __LapeLargeEnum201,__LapeLargeEnum202,__LapeLargeEnum203,__LapeLargeEnum204,__LapeLargeEnum205,__LapeLargeEnum206,__LapeLargeEnum207,__LapeLargeEnum208,__LapeLargeEnum209,__LapeLargeEnum210,__LapeLargeEnum211,__LapeLargeEnum212,__LapeLargeEnum213,__LapeLargeEnum214,__LapeLargeEnum215,__LapeLargeEnum216,__LapeLargeEnum217,__LapeLargeEnum218,__LapeLargeEnum219,__LapeLargeEnum220,__LapeLargeEnum221,__LapeLargeEnum222,__LapeLargeEnum223,__LapeLargeEnum224,__LapeLargeEnum225,__LapeLargeEnum226,__LapeLargeEnum227,__LapeLargeEnum228,__LapeLargeEnum229,__LapeLargeEnum230,__LapeLargeEnum231,__LapeLargeEnum232,__LapeLargeEnum233,__LapeLargeEnum234,__LapeLargeEnum235,__LapeLargeEnum236,__LapeLargeEnum237,__LapeLargeEnum238,__LapeLargeEnum239,__LapeLargeEnum240,__LapeLargeEnum241,__LapeLargeEnum242,__LapeLargeEnum243,__LapeLargeEnum244,__LapeLargeEnum245,__LapeLargeEnum246,__LapeLargeEnum247,__LapeLargeEnum248,__LapeLargeEnum249,__LapeLargeEnum250,
                    __LapeLargeEnum251,__LapeLargeEnum252,__LapeLargeEnum253,__LapeLargeEnum254,__LapeLargeEnum255,__LapeLargeEnum256);
  TLapeSmallSet = set of ELapeSmallEnum;
  TLapeLargeSet = set of ELapeLargeEnum;

  PLapeSmallEnum = ^ELapeSmallEnum;
  PLapeLargeEnum = ^ELapeLargeEnum;
  PLapeSmallSet = ^TLapeSmallSet;
  PLapeLargeSet = ^TLapeLargeSet;

  TLapeBaseClass = class(TObject, IUnknown)
  protected
    function _AddRef: Integer; {$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    constructor Create; virtual;
    {$IFDEF Lape_TrackObjects}
    destructor Destroy; override;
    {$ENDIF}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function GetSelf: TLapeBaseClass; inline;
  end;

  TLapeBaseDeclClass = class(TLapeBaseClass)
  protected
    function getDocPos: TDocPos; virtual; abstract;
  public
    Tag: Int64;
    property DocPos: TDocPos read getDocPos;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeStack<_T> = class(TLapeBaseClass)
  public type
    TTArray = array of _T;
  var protected
    FArr: TTArray;
    FLen: Integer;
    FCur: Integer;

    procedure Grow(AGrowSize: Integer); virtual;
    procedure CheckIndex(Index: Integer; GrowIfNeeded: Boolean = False); virtual;
    function getItem(Index: Integer): _T; virtual;
    function getCurItem: _T; virtual;
    procedure setCurItem(Item: _T); virtual;
    function getCount: Integer; virtual;
  public
    GrowSize: Word;

    constructor Create(StartBufLen: Cardinal = 32); reintroduce; virtual;
    procedure Reset; virtual;
    function Pop: _T; virtual;
    function Push(Item: _T): Integer; virtual;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem; default;
    property Top: _T read getCurItem write setCurItem;
    property Size: Integer read FLen;
    property Count: Integer read getCount;
    property Cur: Integer read FCur;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeList<_T> = class(TLapeBaseClass)
  public type
    TTArray = {$IFDEF Delphi}TArray<_T>{$ELSE}array of _T{$ENDIF};
  var protected
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

    function add(Item: _T): Integer; virtual;
    function Insert(Item: _T; Index: Integer): Boolean; virtual;

    function Delete(Index: Integer): _T; virtual;
    function DeleteItem(Item: _T): _T; virtual;

    procedure MoveItem(AFrom, ATo: Integer); virtual;
    procedure SwapItem(AFrom, ATo: Integer); virtual;

    function IndicesOf(Item: _T): TIntegerArray; virtual;
    function IndexOf(Item: _T; Lo, Hi: Integer): Integer; overload; virtual;
    function IndexOf(Item: _T): Integer; overload; virtual;
    function ExistsItem(Item: _T): Boolean;

    procedure ImportFromArray(Arr: TTArray); virtual;
    function ExportToArray: TTArray; virtual;

    property Items[Index: Integer]: _T read getItem write setItem; default;
    property Count: Integer read FCount;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read getSorted write setSorted;
  end;

  TLapeList_String = {$IFDEF FPC}specialize{$ENDIF} TLapeList<lpString>;
  TLapeList_UInt32 = {$IFDEF FPC}specialize{$ENDIF} TLapeList<UInt32>;

  TLapeStringList = class(TLapeList_String)
  protected
    FHashList: TLapeList_UInt32;
    FCaseSensitive: Boolean;

    function getSorted: Boolean; override;
    procedure setSorted(Sort: Boolean; DoUpdate: Boolean = True); override;
  public
    constructor Create(InvalidValue: lpString; ADuplicates: TDuplicates; ACaseSensitive, ASort: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    function CaseSens(const Item: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function add(Item: lpString): Integer; override;
    function Delete(Index: Integer): lpString; override;

    procedure MoveItem(AFrom, ATo: Integer); override;
    procedure SwapItem(AFrom, ATo: Integer); override;

    function IndexOf(Item: lpString; Lo, Hi: Integer): Integer; override;
    procedure ImportFromArray(Arr: TLapeList_String.TTArray); override;

    property CaseSensitive: Boolean read FCaseSensitive;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeStringMap<_T> = class(TLapeBaseClass)
  public type
    TTItems = {$IFDEF FPC}specialize{$ENDIF} TLapeList<_T>;
    TTArrays = record
      Keys: {$IFDEF Delphi}TArray<lpString>{$ELSE}TLapeStringList.TTArray{$ENDIF};
      Items: {$IFDEF Delphi}TTItems.TTArray{$ELSE}array of _T{$ENDIF};
    end;
  var protected
    FStringList: TLapeStringList;
    FItems: TTItems;
    FCount: Integer;

    function getItem(Key: lpString): _T; virtual;
    procedure setItem(Key: lpString; Item: _T); virtual;
    function getItemI(Index: Integer): _T; virtual;
    procedure setItemI(Index: Integer; Item: _T); virtual;
    function getKey(Index: Integer): lpString; virtual;

    function getSorted: Boolean; virtual;
    procedure setSorted(Sort: Boolean); virtual;
  public
    constructor Create(InvalidValue: _T; Duplicates: TDuplicates; Sort: Boolean; InvalidKey: lpString = ''; ACaseSensitive: Boolean = LapeCaseSensitive); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    function add(Key: lpString; Item: _T): Integer; virtual;
    function Delete(Key: lpString): _T; overload; virtual;
    function Delete(Index: Integer): _T; overload; virtual;
    function DeleteItem(Item: _T): _T; virtual;

    function IndicesOfItemI(Item: _T): TIntegerArray; virtual;
    function IndicesOfItem(Item: _T): TStringArray; virtual;
    function IndicesOfKey(Key: lpString): TIntegerArray; virtual;

    function IndexOfItemI(Item: _T): Integer; virtual;
    function IndexOfItem(Item: _T): lpString; virtual;
    function IndexOfKey(Key: lpString): Integer; virtual;
    function ExistsItem(Item: _T): Boolean;
    function ExistsKey(Key: lpString): Boolean;

    procedure setKey(Index: Integer; NewKey: lpString); virtual;
    procedure ImportFromArrays(Arr: TTArrays); virtual;
    function ExportToArrays: TTArrays; virtual;

    property Items[Index: lpString]: _T read getItem write setItem; default;
    property ItemsI[Index: Integer]: _T read getItemI write setItemI;
    property Key[Index: Integer]: lpString read getKey write setKey;
    property Count: Integer read FCount;
    property Sorted: Boolean read getSorted write setSorted;
  end;

  TLapeDeclaration = class;
  TLapeDeclarationClass = class of TLapeDeclaration;
  TLapeDeclArray = array of TLapeDeclaration;
  TLapeDeclCollection = class({$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeDeclaration>); //Needs class() for Delphi support!!

  TLapeDeclarationList = class(TLapeBaseClass)
  protected
    FList: TLapeDeclCollection;
  public
    FreeDecls: Boolean;

    constructor Create(AList: TLapeDeclCollection; ManageDeclarations: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure ClearSubDeclarations; virtual;
    procedure Assign(Other: TLapeDeclarationList); virtual;

    function addDeclaration(d: TLapeDeclaration): Integer; virtual;
    function HasSubDeclaration(AName: lpString): Boolean; overload; virtual;
    function HasSubDeclaration(ADecl: TLapeDeclaration): Boolean; overload; virtual;

    procedure Delete(i: Integer; DoFree: Boolean = False); overload; virtual;
    procedure Delete(d: TLapeDeclaration; DoFree: Boolean = False); overload; virtual;
    procedure Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False); overload; virtual;

    function getByName(AName: lpString): TLapeDeclArray; virtual;
    function getByClass(AClass: TLapeDeclarationClass; FullClassMatch: Boolean = False): TLapeDeclArray; virtual;
    function getByClassAndName(AName: lpString; AClass: TLapeDeclarationClass; FullClassMatch: Boolean = False): TLapeDeclArray; virtual;

    property Items: TLapeDeclCollection read FList;
  end;

  TLapeDeclaration = class(TLapeBaseDeclClass)
  protected
    FList: TLapeDeclarationList;
    FName: lpString;
    FNameHash: UInt32;
    function getDocPos: TDocPos; override;
    procedure setList(AList: TLapeDeclarationList); virtual;
    procedure setName(AName: lpString); virtual;
  public
    _DocPos: TDocPos;
    Used: Boolean;
    constructor Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    destructor Destroy; override;

    property DeclarationList: TLapeDeclarationList read FList write setList;
    property Name: lpString read FName write setName;
    property NameHash: UInt32 read FNameHash;
  end;

  TLapeManagingDeclaration = class(TLapeDeclaration)
  protected
    FManagedDecls: TLapeDeclarationList;
  public
    FreeDecls: Boolean;

    constructor Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); override;
    destructor Destroy; override;

    procedure setManagedDecls(ADecls: TLapeDeclarationList; DoManage: Boolean); overload; virtual;
    procedure setManagedDecls(ADecls: TLapeDeclarationList); overload; virtual;
    procedure copyManagedDecls(ADecls: TLapeDeclarationList; ReferenceOnly: Boolean = False); virtual;

    function addSubDeclaration(ADecl: TLapeDeclaration): Integer; virtual;
    function HasSubDeclaration(AName: lpString): Boolean; overload; virtual;
    function HasSubDeclaration(ADecl: TLapeDeclaration): Boolean; overload; virtual;
    procedure ClearSubDeclarations; virtual;

    property ManagedDecls: TLapeDeclarationList read FManagedDecls write setManagedDecls;
  end;

const
  op_Invoke = op_Index;
  op_Label = op_Addr;

  {$IFNDEF FPC}
  LineEnding = #13#10;
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


  {$IFDEF Lape_SmallCode}
  ltEvalBool = ltBoolean;
  {$ELSE}
  ltEvalBool = ltLongBool;
  {$ENDIF}

  LapeTypeSize: array[ELapeBaseType] of Integer = (
    -1,
    SizeOf(UInt8), SizeOf(Int8), SizeOf(UInt16), SizeOf(Int16), SizeOf(UInt32),
    SizeOf(Int32), SizeOf(UInt64), SizeOf(Int64),
    SizeOf(Single), SizeOf(Double), SizeOf(Currency), SizeOf(Extended),
    SizeOf(Boolean), SizeOf(ByteBool), SizeOf(WordBool), SizeOf(LongBool),
    SizeOf(AnsiChar), SizeOf(WideChar),
    SizeOf(ShortString), SizeOf(AnsiString), SizeOf(WideString), SizeOf(UnicodeString),
    SizeOf(Variant),
    SizeOf(ELapeSmallEnum), SizeOf(ELapeLargeEnum), SizeOf(TLapeSmallSet), SizeOf(TLapeLargeSet),
    SizeOf(Pointer),
    -1, -1,
    SizeOf(Pointer), -1,
    SizeOf(TCodePos), SizeOf(Pointer)
  );

  {$IF SizeOf(TCodePos) > SizeOf(Pointer)}
    {$MESSAGE Fatal 'TCodePos should be <= Pointer for universal methods'}
  {$IFEND}

  LapeIntegerTypes = [Low(LapeIntegerTypeRange)..High(LapeIntegerTypeRange)];
  LapeSignedIntegerTypes = [ltInt8, ltInt16, ltInt32, ltInt64];
  LapeUnsignedIntegerTypes = [ltUInt8, ltUInt16, ltUInt32, ltUInt64];
  _LapeIntegerTypes = LapeSignedIntegerTypes + LapeUnsignedIntegerTypes;

  {$IF _LapeIntegerTypes <> LapeIntegerTypes}
    {$MESSAGE Fatal 'Integer types aren''t correctly split'}
  {$IFEND}

  LapeRealTypes = [ltSingle..ltExtended];
  LapeBoolTypes = [ltBoolean..ltLongBool];
  LapeStringTypes = [ltShortString..ltUnicodeString];
  LapeCharTypes = [ltAnsiChar..ltWideChar];
  LapeEnumTypes = [ltSmallEnum..ltLargeEnum] + LapeBoolTypes;
  LapeSetTypes = [ltSmallSet..ltLargeSet];
  LapeArrayTypes = [ltDynArray..ltStaticArray] + LapeStringTypes;
  LapeStructTypes = [ltRecord..ltUnion];
  LapeProcTypes = [ltScriptMethod..ltImportedMethod];
  LapeOrdinalTypes = LapeIntegerTypes + LapeBoolTypes + LapeCharTypes + LapeEnumTypes;
  LapePointerTypes = ([ltPointer] + LapeProcTypes + LapeArrayTypes) - [ltShortString, ltStaticArray];
  LapeStackTypes = LapeOrdinalTypes + LapeRealTypes + LapeSetTypes;
  LapeIfTypes = LapeOrdinalTypes + LapeStringTypes + LapePointerTypes + LapeRealTypes + [ltVariant];
  LapeNoInitTypes = LapeOrdinalTypes + LapeRealTypes + [ltPointer, ltScriptMethod, ltImportedMethod, ltShortString];

  NullDocPos: TDocPos = (Line: 0; Col: 0; FileName: '');
  NullRange: TLapeRange = (Lo: 0; Hi: 0);

  UnaryOperators = [op_Addr, op_Deref, op_NOT, op_UnaryMinus, op_UnaryPlus];
  BinaryOperators = [op_AND, op_OR, op_XOR];
  CompareOperators = [op_cmp_Equal, op_cmp_GreaterThan, op_cmp_GreaterThanOrEqual, op_cmp_LessThan, op_cmp_LessThanOrEqual, op_cmp_NotEqual];

  LabelOperators = CompareOperators;
  EnumOperators = [op_Plus, op_Minus, op_Assign] + CompareOperators;

  op_str: array[EOperator] of lpString = ('',
    '=', '>', '>=', '<', '<=', '<>', '@', 'and', ':=', '^', 'div', '/', '.' , 'in',
    '[', '-', 'mod', '*', 'not', 'or', '+', '**', 'shl', 'shr', 'xor', '-', '+');
  op_name: array[EOperator] of lpString = ('',
    'EQ', 'GT', 'GTEQ', 'LT', 'LTEQ', 'NEQ', {'ADDR'}'', 'AND', 'ASGN', {'DREF'}'', 'IDIV', 'DIV', {'dot'}'', 'IN',
    {'index'}'', 'SUB', 'MOD', 'MUL', 'NOT', 'OR', 'ADD', {'power'}'', 'SHL', 'SHR', 'XOR', 'UMIN', {'UPOS'}'');

var
  lowUInt8: UInt8 = Low(UInt8);    highUInt8: UInt8 = High(UInt8);
  lowInt8: Int8 = Low(Int8);       highInt8: Int8 = High(Int8);
  lowUInt16: UInt16 = Low(UInt16); highUInt16: UInt16 = High(UInt16);
  lowInt16: Int16 = Low(Int16);    highInt16: Int16 = High(Int16);
  lowUInt32: UInt32 = Low(UInt32); highUInt32: UInt32 = High(UInt32);
  lowInt32: Int32 = Low(Int32);    highInt32: Int32 = High(Int32);
  lowUInt64: UInt64 = Low(UInt64); highUInt64: UInt64 = High(UInt64);
  lowInt64: Int64 = Low(Int64);    highInt64: Int64 = High(Int64);

  LapeTypeLow: array[LapeIntegerTypeRange] of Pointer = (
    @lowUInt8, @lowInt8, @lowUInt16, @lowInt16, @lowUInt32, @lowInt32, @lowUInt64, @lowInt64
  );

  LapeTypeHigh: array[LapeIntegerTypeRange] of Pointer = (
    @highUInt8, @highInt8, @highUInt16, @highInt16, @highUInt32, @highInt32, @highUInt64, @highInt64
  );

function LapeCase(const Str: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeHash(const Value: lpString): UInt32;
function LapeTypeToString(Token: ELapeBaseType): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeOperatorToString(Token: EOperator): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}

function VarTypeToVType(v: TVarType): SizeInt;
function VariantToVarRec(const v: Variant): TVarRec; overload;
function VariantToVarRec(const v: Variant; out Container: TVarRecContainer): TVarRec; overload;
function VariantArrToConstArr(v: array of Variant): TVarRecList;

procedure Swap(var A, B: Pointer); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
procedure Swap(var A, B: Boolean); overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

function _Compare8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer;
function _Compare16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer;
function _Compare32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer;
function _Compare64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer;
function _BSearch8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer;
function _BSearch16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer;
function _BSearch32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer;
function _BSearch64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer;
procedure _Insert8(Arr: PUInt8; var Index: Integer);
procedure _Insert16(Arr: PUInt16; var Index: Integer);
procedure _Insert32(Arr: PUInt32; var Index: Integer);
procedure _Insert64(Arr: PUInt64; var Index: Integer);

{$IFDEF Lape_TrackObjects}
var
  lpgCounter: Integer;
  lpgList: TList;
{$ENDIF}

implementation

uses
  typinfo, variants,
  lpexceptions;

function LapeCase(const Str: lpString): lpString;
begin
  {$IFDEF Lape_CaseSensitive}
  Result := Str;
  {$ELSE}
  Result := LowerCase(Str);
  {$ENDIF}
end;

//MurMurHas2 by Tommi Prami & optimizations by Patrick van Logchem
function LapeHash(const Value: lpString): UInt32;
const
  Seed: UInt32 = $c58f1a7b;
  cM:   UInt32 = $5bd1e995;
  cR:   UInt32 = 24;
var
  Data: Pointer;
  i, k, Len: UInt32;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
  Data := @Value[1];
  Len := Length(Value) * SizeOf(lpChar);

  Result := Seed xor Len;
  for i := 1 to Len div SizeOf(UInt32) do
  begin
    k := PUInt32(Data)^ * cM;
    Result := (Result * cM) xor ((k xor (k shr cR)) * cM);
    Inc(PtrUInt(Data), SizeOf(UInt32));
  end;

  Len := Len and 3;
  if (Len > 0) then
  begin
    if (Len >= SizeOf(UInt16)) then
    begin
      k := PUInt16(Data)^;
      if (Len > SizeOf(UInt16)) then
        k := k + (UInt32(PUInt8(PtrUInt(Data) + SizeOf(UInt16))^) shl 16);
    end
    else
      k := PUInt8(Data)^;
    Result := (Result xor k) * cM;
  end;

  Result := (Result xor (Result shr 13)) * cM;
  Result := (Result xor (Result shr 15));
  {$IFDEF REDO_Q}{$Q+}{$ENDIF}
  {$IFDEF REDO_R}{$R+}{$ENDIF}
end;

function LapeTypeToString(Token: ELapeBaseType): lpString;
begin
  Result := getEnumName(TypeInfo(ELapeBaseType), Ord(Token));
  Delete(Result, 1, 2);
end;

function LapeOperatorToString(Token: EOperator): lpString;
begin
  Result := getEnumName(TypeInfo(EOperator), Ord(Token));
  Delete(Result, 1, 3);
end;

function VarTypeToVType(v: TVarType): SizeInt;
begin
  Result := vtVariant;
  v := v and VarTypeMask;

  case v of
    varSingle,
    varDouble:   Result := vtExtended;
    varCurrency: Result := vtCurrency;
    varOleStr:   Result := vtWideString;
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
  end;
end;

function VariantToVarRec(const v: Variant): TVarRec;
begin
  Result.VType := VarTypeToVType(VarType(v));
  case Result.VType of
    vtInteger:    Result.VInteger := v;
    vtBoolean:    Result.VBoolean := v;
    vtAnsiString: Result.VAnsiString := TVarData(v).VString;
    vtCurrency:   Result.VCurrency := @TVarData(v).VCurrency;
    vtVariant:    Result.VVariant := @v;
    vtInterface:  Result.VInterface := TVarData(v).VDispatch;
    vtWideString: Result.VWideString := TVarData(v).VOleStr;
    vtInt64:      Result.VInt64 := @TVarData(v).VInt64;
    {$IFDEF FPC}
    vtChar:       Result.VChar := v;
    vtWideChar:   Result.VWideChar := v;
    vtQWord:      Result.VQWord := @TVarData(v).VQWord;
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
        Container.CExtended := Container.CVar;
        Result.VExtended := @Container.CExtended;
      end;
    vtString:
      begin
        Container.CShortString := Container.CVar;
        Result.VString := @Container.CShortString;
      end;
    vtCurrency:
      begin
        Container.CCurrency := Container.CVar;
        Result.VCurrency := @Container.CCurrency;
      end;
    vtInt64:
      begin
        Container.CInt64 := Container.CVar;
        Result.VInt64 := @Container.CInt64;
      end;
    {$IFDEF FPC}
    vtQWord:
      begin
        Container.CQWord := Container.CVar;
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

function _Compare8(Arr: PUInt8; Item: UInt8; Lo, Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare16(Arr: PUInt16; Item: UInt16; Lo, Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare32(Arr: PUInt32; Item: UInt32; Lo, Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
  Result := -1;
end;

function _Compare64(Arr: PUInt64; Item: UInt64; Lo, Hi: Integer): Integer;
var
  i: Integer;
begin
  for i := Lo to Hi do
    if (Arr^ = Item) then
      Exit(i)
    else
      Inc(Arr);
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

function TLapeBaseClass._AddRef: Integer; {$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TLapeBaseClass._Release: Integer; {$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

constructor TLapeBaseClass.Create;
begin
  inherited;
  {$IFDEF Lape_TrackObjects}
  Inc(lpgCounter);
  lpgList.add(Pointer(Self));
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

function TLapeBaseClass.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getInterface(IId, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TLapeBaseClass.GetSelf: TLapeBaseClass;
begin
  Result := Self;
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Grow(AGrowSize: Integer);
begin
  FLen := FLen + AGrowSize;
  if (FLen < 0) then
    FLen := 0;
  SetLength(FArr, FLen);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.CheckIndex(Index: Integer; GrowIfNeeded: Boolean = False);
var
  NeedGrow: Boolean;
begin
  NeedGrow := (Index >= FLen);
  if (NeedGrow and (not GrowIfNeeded)) or (Index < 0) then
    LapeException(lpeOutOfStackRange)
  else if NeedGrow then
    Grow(GrowSize);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Index: Integer): _T;
begin
  CheckIndex(Index);
  Result := FArr[Index];
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getCurItem: _T;
begin
  if (FCur >= 0) then
    Result := FArr[FCur]
  else
    CheckIndex(FCur);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.setCurItem(Item: _T);
begin
  if (FCur >= 0) then
    FArr[FCur] := Item
  else
    CheckIndex(FCur);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.getCount: Integer;
begin
  if (FCur < 0) then
    Result := 0
  else
    Result := FCur + 1;
end;

constructor TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Create(StartBufLen: Cardinal = 32);
begin
  inherited Create();

  GrowSize := StartBufLen div 2;
  if (GrowSize < 1) then
    GrowSize := 1;

  Reset();
  Grow(StartBufLen);
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Reset;
begin
  FArr := nil;
  FLen := 0;
  FCur := -1;
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Pop: _T;
begin
  Result := getCurItem;
  Dec(FCur);
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.Push(Item: _T): Integer;
begin
  Inc(FCur);
  CheckIndex(FCur, True);
  FArr[FCur] := Item;
  Result := FCur;
end;

procedure TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.ImportFromArray(Arr: TTArray);
begin
  FArr := Arr;
  FLen := Length(Arr);
  FCur := FLen - 1;
end;

function TLapeStack{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArray: TTArray;
var
  i: Integer;
begin
  SetLength(Result, FCur + 1);
  for i := 0 to FCur do
    Result[i] := FArr[i];
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
  Sort := Sort and (SizeOf(_T) in [SizeOf(UInt8), SizeOf(UInt16), SizeOf(UInt32), SizeOf(UInt64)]);
  if (FSorted <> Sort) then
  begin
    FSorted := Sort;

    if DoUpdate and Sort and (FCount > 0) then
    begin
      a := ExportToArray();
      Clear();
      for i := 0 to High(a) do
        add(a[i]);
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
      Insert(Item, Index);
    end;
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [IntToStr(Index)]);
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

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.add(Item: _T): Integer;
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

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.Insert(Item: _T; Index: Integer): Boolean;
var
  OldIndex: Integer;
begin
  OldIndex := add(Item);
  Result := OldIndex > -1;

  if Result then
    MoveItem(OldIndex, Index);
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
      Sorted := False;
      MoveItem(Index, FCount - 1);
    finally
      setSorted(Sort, False);
      Dec(FCount);
    end;
  end
  else
    LapeExceptionFmt(lpeInvalidIndex, [IntToStr(Index)]);
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.DeleteItem(Item: _T): _T;
var
  Index: Integer;
begin
  Index := IndexOf(Item);
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

procedure TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.SwapItem(AFrom, ATo: Integer);
var
  c: _T;
begin
  if (AFrom = ATo) or
     (ATo   < 0) or (ATo   >= FCount)  or
     (AFrom < 0) or (AFrom >= FCount)
  then
    Exit;

  if FSorted then
    LapeException(lpeImpossible);

  c := FItems[AFrom];
  FItems[AFrom] := FItems[ATo];
  FItems[ATo] := c;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.IndicesOf(Item: _T): TIntegerArray;
var
  Index, Lo, Hi: Integer;
begin
  Result := nil;
  Lo := 0;
  Hi := FCount - 1;

  Index := IndexOf(Item, Lo, Hi);
  while (Index > -1) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Index;

    Lo := Index + 1;
    Index := IndexOf(Item, Lo, Hi);
  end;
end;

function TLapeList{$IFNDEF FPC}<_T>{$ENDIF}.IndexOf(Item: _T; Lo, Hi: Integer): Integer;
var
  i, ii: Integer;
  ItemA, ItemB: PByteArray;
  Match: Boolean;
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
    case SizeOf(_T) of
      SizeOf(UInt8) : Result := _Compare8 (@FItems[Lo], PUInt8 (@Item)^, Lo, Hi);
      SizeOf(UInt16): Result := _Compare16(@FItems[Lo], PUInt16(@Item)^, Lo, Hi);
      SizeOf(UInt32): Result := _Compare32(@FItems[Lo], PUInt32(@Item)^, Lo, Hi);
      SizeOf(UInt64): Result := _Compare64(@FItems[Lo], PUInt64(@Item)^, Lo, Hi);
      else
      begin
        ItemB := PByteArray(@Item);
        for i := Lo to Hi do
        begin
          ItemA := PByteArray(@FItems[i]);
          Match := True;
          for ii := 0 to SizeOf(_T) - 1 do
            if (ItemA^[ii] <> ItemB^[ii]) then
            begin
              Match := False;
              Break;
            end;
          if Match then
            Exit(i);
        end;
        Result := -1;
      end;
    end;
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
  FItems := Arr;
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

function TLapeStringList.getSorted: Boolean;
begin
  Result := FHashList.Sorted;
end;

procedure TLapeStringList.setSorted(Sort: Boolean; DoUpdate: Boolean = True);
begin
  FHashList.setSorted(Sort, DoUpdate);
  inherited;
  FSorted := False;
end;

constructor TLapeStringList.Create(InvalidValue: lpString; ADuplicates: TDuplicates; ACaseSensitive, ASort: Boolean);
begin
  FHashList := TLapeList_UInt32.Create(0, dupAccept, True);
  inherited Create(InvalidValue, ADuplicates, ASort);

  FCaseSensitive := ACaseSensitive;
  Sorted := ASort;
end;

destructor TLapeStringList.Destroy;
begin
  inherited;
  FHashList.Free();
end;

procedure TLapeStringList.Clear;
begin
  inherited;
  FHashList.Clear();
end;

function TLapeStringList.CaseSens(const Item: lpString): lpString;
begin
  if FCaseSensitive then
    Result := Item
  else
    Result := UpperCase(Item);
end;

function TLapeStringList.add(Item: lpString): Integer;
var
  dup: TDuplicates;
begin
  if (FDuplicates in [dupIgnore, dupError]) and ExistsItem(Item) then
    if (FDuplicates = dupError) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Item])
    else
      Exit(-1);

  Item := CaseSens(Item);
  Result := FHashList.add(LapeHash(Item));

  if (Result > -1) then
  try
    dup := FDuplicates;
    FDuplicates := dupAccept;
    inherited MoveItem(inherited add(Item), Result);
  finally
    FDuplicates := dup;
  end;
end;

function TLapeStringList.Delete(Index: Integer): lpString;
begin
  Result := inherited;
  if (Result <> InvalidVal) then
    FHashList.Delete(FCount);
end;

procedure TLapeStringList.MoveItem(AFrom, ATo: Integer);
begin
  if FHashList.Sorted then
    LapeException(lpeImpossible)
  else
    FHashList.MoveItem(AFrom, ATo);
  inherited;
end;

procedure TLapeStringList.SwapItem(AFrom, ATo: Integer);
begin
  if FHashList.Sorted then
    LapeException(lpeImpossible)
  else
    FHashList.MoveItem(AFrom, ATo);
  inherited;
end;

function TLapeStringList.IndexOf(Item: lpString; Lo, Hi: Integer): Integer;
begin
  if (Lo < 0) or (Lo > Hi) or (Hi >= FCount) then
    Exit(-1);

  Item := CaseSens(Item);
  Result := FHashList.IndexOf(LapeHash(Item), Lo, Hi);

  if (Result > -1) and (FItems[Result] <> Item) then
    Result := IndexOf(Item, Result + 1, Hi);
end;

procedure TLapeStringList.ImportFromArray(Arr: TLapeList_String.TTArray);
var
  Sort: Boolean;
begin
  Sort := Sorted;
  try
    Sorted := False;
    inherited;
  finally
    Sorted := Sort;
  end;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getItem(Key: lpString): _T;
begin
  Result := FItems[IndexOfKey(Key)];
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setItem(Key: lpString; Item: _T);
var
  Index: Integer;
begin
  Index := IndexOfKey(Key);
  if (Index > -1) and (Index < FCount) then
    FItems[Index] := Item
  else
    add(Key, Item);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getItemI(Index: Integer): _T;
begin
  Result := FItems[Index];
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setItemI(Index: Integer; Item: _T);
begin
  FItems[Index] := Item;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getKey(Index: Integer): lpString;
begin
  Result := FStringList[Index];
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.getSorted: Boolean;
begin
  Result := FStringList.Sorted;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setSorted(Sort: Boolean);
begin
  FStringList.Sorted := Sort;
end;

constructor TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Create(InvalidValue: _T; Duplicates: TDuplicates; Sort: Boolean; InvalidKey: lpString = ''; ACaseSensitive: Boolean = LapeCaseSensitive);
begin
  inherited Create();

  FStringList := TLapeStringList.Create(InvalidKey, Duplicates, ACaseSensitive, Sort);
  FItems := TTItems.Create(InvalidValue, dupAccept, False);

  Clear();
end;

destructor TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Destroy;
begin
  FStringList.Free();
  FItems.Free;
  inherited;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FStringList.Clear();
  FItems.Clear;
  FCount := 0;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.add(Key: lpString; Item: _T): Integer;
begin
  Result := FStringList.add(Key);
  if (Result > -1) then
    if FItems.Insert(Item, Result) then
      Inc(FCount)
    else
      FStringList.Delete(Result);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Key: lpString): _T;
var
  Index: Integer;
begin
  Index := IndexOfKey(Key);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := FItems.InvalidVal;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Delete(Index: Integer): _T;
begin
  Result := FItems.Delete(Index);
  FStringList.Delete(Index);
  Dec(FCount);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.DeleteItem(Item: _T): _T;
var
  Index: Integer;
begin
  Index := IndexOfItemI(Item);
  if (Index > -1) then
    Result := Delete(Index)
  else
    Result := FItems.InvalidVal;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndicesOfItemI(Item: _T): TIntegerArray;
begin
  Result := FItems.IndicesOf(Item);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndicesOfItem(Item: _T): TStringArray;
var
  Indices: TIntegerArray;
  i: Integer;
begin
  Indices := IndicesOfItemI(Item);

  SetLength(Result, Length(Indices));
  for i := 0 to High(Indices) do
    Result[i] := FStringList[Indices[i]];
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndicesOfKey(Key: lpString): TIntegerArray;
begin
  Result := FStringList.IndicesOf(Key);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndexOfItemI(Item: _T): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndexOfItem(Item: _T): lpString;
begin
  Result := FStringList[IndexOfItemI(Item)];
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.IndexOfKey(Key: lpString): Integer;
begin
  Result := FStringList.IndexOf(Key);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.ExistsItem(Item: _T): Boolean;
begin
  Result := FItems.ExistsItem(Item);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.ExistsKey(Key: lpString): Boolean;
begin
  Result := FStringList.ExistsItem(Key);
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setKey(Index: Integer; NewKey: lpString);
begin
  if (Index > -1) and (Index < FCount) then
    FStringList[Index] := NewKey;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.ImportFromArrays(Arr: TTArrays);
begin
  FStringList.ImportFromArray(Arr.Keys);
  FItems.ImportFromArray(Arr.Items);

  FCount := FItems.Count;
  Assert(FStringList.Count = FCount);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArrays: TTArrays;
begin
  Result.Keys := FStringList.ExportToArray();
  Result.Items := FItems.ExportToArray;
end;

constructor TLapeDeclarationList.Create(AList: TLapeDeclCollection; ManageDeclarations: Boolean = True);
begin
  inherited Create();

  if (AList = nil) then
    AList := TLapeDeclCollection.Create(nil, dupAccept, True);

  FList := AList;
  FreeDecls := ManageDeclarations;
end;

destructor TLapeDeclarationList.Destroy;
begin
  Clear();
  if (FList <> nil) then
    FList.Free();
  inherited;
end;

procedure TLapeDeclarationList.Clear;
begin
  if (FList <> nil) then
  begin
    if FreeDecls then
      ClearSubDeclarations();
    while (FList.Count > 0) do
      if (FList[0] = nil) or (not FreeDecls) then
        FList.Delete(0)
      else
        FList[0].Free();
    FList.Clear();
  end;
end;

procedure TLapeDeclarationList.ClearSubDeclarations;
var
  ClassItems: TLapeDeclArray;
  i: Integer;
begin
  ClassItems := getByClass(TLapeManagingDeclaration);
  for i := High(ClassItems) downto 0 do
    TLapeManagingDeclaration(ClassItems[i]).ClearSubDeclarations();
end;

procedure TLapeDeclarationList.Assign(Other: TLapeDeclarationList);
begin
  if (Other = nil) then
    Clear()
  else
    FList.ImportFromArray(Other.FList.ExportToArray());
end;

function TLapeDeclarationList.addDeclaration(d: TLapeDeclaration): Integer;
begin
  if (FList <> nil) and (d <> nil) and (not HasSubDeclaration(d)) then
  begin
    Result := FList.add(d);

    if (d.DeclarationList <> nil) then
      d.DeclarationList := Self
    else
      d.FList := Self;
  end
  else
    Result := -1;
end;

function TLapeDeclarationList.HasSubDeclaration(AName: lpString): Boolean;
begin
  Result := Length(getByName(AName)) > 0;
end;

function TLapeDeclarationList.HasSubDeclaration(ADecl: TLapeDeclaration): Boolean;
begin
  Result := FList.ExistsItem(ADecl);
end;

procedure TLapeDeclarationList.Delete(i: Integer; DoFree: Boolean = False);
begin
  if (FList <> nil) and (FList[i] <> nil) then
    with FList.Delete(i) do
      if DoFree then
        Free()
      else
        FList := nil;
end;

procedure TLapeDeclarationList.Delete(d: TLapeDeclaration; DoFree: Boolean = False);
begin
  if (FList <> nil) and (FList.DeleteItem(d) <> nil) then
    if DoFree then
      d.Free()
    else
      d.FList := nil;
end;

procedure TLapeDeclarationList.Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False);
var
  ClassItems: TLapeDeclArray;
  i: Integer;
begin
  ClassItems := getByClass(AClass);
  for i := High(ClassItems) downto 0 do
    Delete(ClassItems[i], DoFree);
end;

function TLapeDeclarationList.getByName(AName: lpString): TLapeDeclArray;
var
  i: Integer;
  Hash: UInt32;
begin
  Result := nil;
  AName := LapeCase(AName);
  Hash := LapeHash(AName);

  if (FList <> nil) then
    for i := 0 to FList.Count - 1 do
      if (FList[i] <> nil) and (FList[i].NameHash = Hash) and (LapeCase(FList[i].Name) = AName) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FList[i];
      end;
end;

function TLapeDeclarationList.getByClass(AClass: TLapeDeclarationClass; FullClassMatch: Boolean = False): TLapeDeclArray;
var
  i, Current, GrowSize, Len: Integer;
begin
  Result := nil;
  if (FList <> nil) and (FList.Count > 0) then
  begin
    GrowSize := (FList.Count div 4) + 1;
    Len := GrowSize;

    SetLength(Result, Len);
    Current := 0;
    for i := 0 to FList.Count - 1 do
      if (FList[i] <> nil) and ((FList[i].ClassType = AClass) or ((not FullClassMatch) and (FList[i] is AClass))) then
      begin
        if (Current = Len) then
        begin
          Inc(Len, GrowSize);
          SetLength(Result, Len);
        end;

        Result[Current] := FList[i];
        Inc(Current);
      end;
    SetLength(Result, Current);
  end;
end;

function TLapeDeclarationList.getByClassAndName(AName: lpString; AClass: TLapeDeclarationClass; FullClassMatch: Boolean = False): TLapeDeclArray;
var
  i: Integer;
  Hash: UInt32;
begin
  Result := nil;
  AName := LapeCase(AName);
  Hash := LapeHash(AName);

  if (FList <> nil) then
    for i := 0 to FList.Count - 1 do
      if (FList[i] <> nil) and
         ((FList[i].ClassType = AClass) or ((not FullClassMatch) and (FList[i] is AClass))) and
         (FList[i].NameHash = Hash) and (LapeCase(FList[i].Name) = AName)
      then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := FList[i];
      end;
end;

function TLapeDeclaration.getDocPos: TDocPos;
begin
  Result := _DocPos;
end;

procedure TLapeDeclaration.setList(AList: TLapeDeclarationList);
begin
  if (AList <> FList) then
  begin
    if (FList <> nil) then
      FList.Delete(Self);
    FList := AList;
    if (AList <> nil) then
      FList.addDeclaration(Self);
  end;
end;

procedure TLapeDeclaration.setName(AName: lpString);
begin
  if (FName <> AName) then
  begin
    FName := AName;
    FNameHash := LapeHash(LapeCase(AName));
  end;
end;

constructor TLapeDeclaration.Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create();
  Name := AName;
  Used := False;
  if (ADocPos <> nil) then
    _DocPos := ADocPos^
  else
    _DocPos := NullDocPos;
  setList(AList);
end;

destructor TLapeDeclaration.Destroy;
begin
  setList(nil);
  inherited;
end;

constructor TLapeManagingDeclaration.Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited;
  FreeDecls := True;
  FManagedDecls := TLapeDeclarationList.Create(nil);
end;

destructor TLapeManagingDeclaration.Destroy;
begin
  if FreeDecls then
    FManagedDecls.Free();
  inherited;
end;

procedure TLapeManagingDeclaration.setManagedDecls(ADecls: TLapeDeclarationList; DoManage: Boolean);
begin
  Assert(ADecls <> nil);
  if FreeDecls then
    FManagedDecls.Free();
  FManagedDecls := ADecls;
  FreeDecls := DoManage;
end;

procedure TLapeManagingDeclaration.setManagedDecls(ADecls: TLapeDeclarationList);
begin
  setManagedDecls(ADecls, False);
end;

procedure TLapeManagingDeclaration.copyManagedDecls(ADecls: TLapeDeclarationList; ReferenceOnly: Boolean = False);
begin
  Assert(ADecls <> nil);
  if ReferenceOnly then
    setManagedDecls(ADecls)
  else
    FManagedDecls.Assign(ADecls);
end;

function TLapeManagingDeclaration.addSubDeclaration(ADecl: TLapeDeclaration): Integer;
begin
  Result := FManagedDecls.addDeclaration(ADecl);
end;

function TLapeManagingDeclaration.HasSubDeclaration(AName: lpString): Boolean;
begin
  Result := FManagedDecls.HasSubDeclaration(AName);
end;

function TLapeManagingDeclaration.HasSubDeclaration(ADecl: TLapeDeclaration): Boolean;
begin
  Result := FManagedDecls.HasSubDeclaration(ADecl);
end;

procedure TLapeManagingDeclaration.ClearSubDeclarations;
begin
  FManagedDecls.Clear();
end;

{$IFDEF Lape_TrackObjects}
initialization
  lpgList := TList.Create();
finalization
  lpgList.Free();
{$ENDIF}

end.

