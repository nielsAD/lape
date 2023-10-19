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
  LapeCaseSensitive = {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
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

  ELoopType = (loopUp, loopDown, loopOver, loopOverEnum, loopOverSet);

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

  {$IFDEF FPC}generic{$ENDIF} TLapeStack<_T> = class(TLapeBaseClass)
  public type
    TTArray = array of _T;
  protected
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

  {$IFDEF FPC}generic{$ENDIF} TLapeKeyValueList<_K, _V> = class(TLapeBaseClass)
  public type
    TTKeyList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<_K>;
    TTValueList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<_V>;
  protected
    FKeyList: TTKeyList;
    FValueList: TTValueList;

    procedure setValue(Key: _K; Value: _V); virtual;
    function getValue(Key: _K): _V; virtual;
  public
    constructor Create(InvalidKey: _K; InvalidValue: _V; ADuplicates: TDuplicates); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    property Value[Key: _K]: _V read getValue write setValue; default;
  end;

  TLapeList_String = {$IFDEF FPC}specialize{$ENDIF} TLapeList<lpString>;
  TLapeList_UInt32 = {$IFDEF FPC}specialize{$ENDIF} TLapeList<UInt32>;

  TLapeStringList = class(TLapeList_String)
  protected
    FHashList: TLapeList_UInt32;
    FCaseSensitive: Boolean;

    function getSorted: Boolean; override;
    procedure setSorted(Sort: Boolean; DoUpdate: Boolean = True); override;
    procedure setItem(Index: Integer; Item: lpString); override;
  public
    constructor Create(InvalidValue: lpString; ADuplicates: TDuplicates; ACaseSensitive, ASort: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    function CaseSens(const Item: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function Add(Item: lpString): Integer; override;
    function Delete(Index: Integer): lpString; override;

    procedure MoveItem(AFrom, ATo: Integer); override;
    procedure SwapItem(AFrom, ATo: Integer); override;

    function IndexOf(Item: lpString; Lo, Hi: Integer): Integer; override;
    procedure ImportFromArray(Arr: TLapeList_String.TTArray); override;

    function Implode(ASep: lpString): lpString; virtual;

    property CaseSensitive: Boolean read FCaseSensitive;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeStringMap<_T> = class(TLapeBaseClass)
  public type
    TTItems = {$IFDEF FPC}specialize{$ENDIF} TLapeList<_T>;
    TTArrays = record
      Keys: {$IFDEF Delphi}TArray<lpString>{$ELSE}TLapeStringList.TTArray{$ENDIF};
      Items: {$IFDEF Delphi}TTItems.TTArray{$ELSE}array of _T{$ENDIF};
    end;
  protected
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

    function Insert(Key: lpString; Item: _T; Index: Integer): Integer; virtual;
    function Add(Key: lpString; Item: _T): Integer; virtual;

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

    procedure setKeyI(Index: Integer; NewKey: lpString); virtual;
    procedure setKey(Item: _T; NewKey: lpString); virtual;

    procedure ImportFromArrays(Arr: TTArrays); virtual;
    function ExportToArrays: TTArrays; virtual;

    property Items[Index: lpString]: _T read getItem write setItem; default;
    property ItemsI[Index: Integer]: _T read getItemI write setItemI;
    property Key[Index: Integer]: lpString read getKey write setKeyI;
    property Count: Integer read FCount;
    property Sorted: Boolean read getSorted write setSorted;
  end;

  // "Perfect Hashing" - Each entry does not share a bucket.
  {$IFDEF FPC}generic{$ENDIF} TLapeUniqueStringDictionary<_T> = class(TLapeBaseClass)
  protected type
    TBucket = record Key: lpString; Value: _T; end;
    TBuckets = array of TBucket;
  protected
    FBuckets: TBuckets;
    FSize: UInt32;
    FTag: UInt32;
    FMinLength: Integer;
    FMaxLength: Integer;
    FCount: Integer;
    FItems: TBuckets;

    function Hash(const S: PChar; const Len: Integer): UInt32;

    procedure Build;

    procedure setValue(Key: lpString; Value: _T);
    function getValue(Key: lpString): _T;
  public
    InvalidVal: _T;

    constructor Create(InvalidValue: _T; Size: Integer = 1024); reintroduce; virtual;

    property Value[Key: lpString]: _T read getValue write setValue; default;
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
    procedure DeleteProc(const Proc: TNotifyProc); virtual;

    procedure Notify(Sender: _T); virtual;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeNotifierOfObject<_T> = class(TLapeBaseClass)
  public type
    TNotifyProcOfObject = procedure(Sender: _T) of object;
    TNotifiers = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TNotifyProcOfObject>;
  protected
    FNotifiers: TNotifiers;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    procedure AddProc(const Proc: TNotifyProcOfObject); virtual;
    procedure DeleteProc(const Proc: TNotifyProcOfObject); virtual;

    procedure Notify(Sender: _T); virtual;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeDictionary<_T> = class(TLapeBaseClass)
  public
  type
    THashBucketItems = array of record
      Item: _T;
      Hash: UInt32;
    end;

    PHashBucket = ^THashBucket;
    THashBucket = record
      Count: Integer;
      Items: THashBucketItems;
    end;
    THashBuckets = array of THashBucket;

    TItemArray = array of _T;
  protected
    FInitialSize: Integer;
    FGrowth: Integer;
    FBuckets: THashBuckets;
    FCount: Integer;
    FSize: Integer;

    procedure Grow;
  public
    constructor Create(InitialSize: Integer = 32; Growth: Integer = 4); reintroduce;

    function GetBucket(const Hash: UInt32): PHashBucket; inline;

    function Remove(const Hash: UInt32; const Item: _T): Boolean;
    procedure Add(const Hash: UInt32; const Item: _T; const CanGrow: Boolean = True);
    procedure Clear;

    function ExportToArray: TItemArray;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeSorter<_T> = class
  protected
    class procedure DoQuickSort(const Arr: PByte; const ElSize: SizeInt; var Weights: array of _T; iLo, iHi: SizeInt; const SortUp: Boolean);
  public
    class procedure QuickSort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: array of _T; const SortUp: Boolean);
  end;

  TLapeDeclaration = class;
  TLapeDeclarationClass = class of TLapeDeclaration;
  TLapeDeclArray = array of TLapeDeclaration;

  // Abstract class
  TLapeDeclCollection = class(TLapeBaseClass)
  protected
    function getItem(Index: Integer): TLapeDeclaration; virtual; abstract;
    function getCount: Integer; virtual; abstract;
  public
    procedure Clear; virtual; abstract;

    procedure Add(Decl: TLapeDeclaration); virtual; abstract;
    function Delete(Decl: TLapeDeclaration): Boolean; virtual; abstract;

    function Get(Name: lpString; out Decl: TLapeDeclaration): Boolean; overload; virtual; abstract;
    function Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean; overload; virtual; abstract;
    function GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray; virtual; abstract;

    function IndexOf(Decl: TLapeDeclaration): Integer; virtual; abstract;
    function Exists(Decl: TLapeDeclaration): Boolean; virtual; abstract;

    function ExportToArray: TLapeDeclArray; virtual; abstract;

    property Count: Integer read getCount;
    property Items[Index: Integer]: TLapeDeclaration read getItem; default;
  end;

  TLapeDeclCollection_Dictionary = class(TLapeDeclCollection)
  protected
  type
    TDictionary = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TLapeDeclaration>;
    TList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeDeclaration>;
  protected
    FDictionary: TDictionary;
    FUnnamedBucket: TList;

    procedure NameChanged(Decl: TLapeDeclaration); virtual;

    function getItem(Index: Integer): TLapeDeclaration; override;
    function getCount: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;

    procedure Add(Decl: TLapeDeclaration); override;
    function Delete(Decl: TLapeDeclaration): Boolean; override;

    function Get(Name: lpString; out Decl: TLapeDeclaration): Boolean; override;
    function Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean; override;
    function GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray; override;

    function IndexOf(Decl: TLapeDeclaration): Integer; override;
    function Exists(Decl: TLapeDeclaration): Boolean; override;

    function ExportToArray: TLapeDeclArray; override;
  end;

  TLapeDeclCollection_List = class(TLapeDeclCollection)
  protected type
    TList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeDeclaration>;
  protected
    FList: TList;

    function getItem(Index: Integer): TLapeDeclaration; override;
    function getCount: Integer; override;
  public
    constructor Create(Sorted: Boolean); reintroduce; virtual;
    destructor Destroy; override;

    procedure Clear; override;

    procedure Add(Decl: TLapeDeclaration); override;
    function Delete(Decl: TLapeDeclaration): Boolean; override;

    function Get(Name: lpString; out Decl: TLapeDeclaration): Boolean; override;
    function Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean; override;
    function GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray; override;

    function IndexOf(Decl: TLapeDeclaration): Integer; override;
    function Exists(Decl: TLapeDeclaration): Boolean; override;

    function ExportToArray: TLapeDeclArray; override;
  end;

  TLapeDeclarationList = class(TLapeBaseClass)
  protected
    FList: TLapeDeclCollection;

    function getItem(Index: Integer): TLapeDeclaration; virtual;
    function getItemCount: Integer; virtual;
    function getCount: Integer; virtual;
  public
    Parent: TLapeDeclarationList;
    FreeDecls: Boolean;

    constructor Create(AList: TLapeDeclCollection; ManageDeclarations: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;

    function HasParent: Boolean; virtual;
    procedure Clear; virtual;
    procedure ClearSubDeclarations; virtual;

    procedure addDeclaration(Decl: TLapeDeclaration); virtual;
    function HasSubDeclaration(Name: lpString; CheckParent: TInitBool): Boolean; overload; virtual;
    function HasSubDeclaration(Decl: TLapeDeclaration; CheckParent: TInitBool): Boolean; overload; virtual;

    function IndexOf(Decl: TLapeDeclaration): Integer; virtual;

    procedure Delete(Decl: TLapeDeclaration; DoFree: Boolean = False); overload; virtual;
    procedure Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False); overload; virtual;

    function Get(Name: lpString; out Decl: TLapeDeclaration; CheckParent: TInitBool): Boolean; overload; virtual;
    function Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration; CheckParent: TInitBool): Boolean; overload; virtual;
    function GetByClass(AClass: TLapeDeclarationClass; CheckParent: TInitBool): TLapeDeclArray; virtual;

    function ExportToArray: TLapeDeclArray; virtual;

    property Count: Integer read getCount;
    property Items[Index: Integer]: TLapeDeclaration read getItem; default;
    property ItemCount: Integer read getItemCount;
  end;

  EDeclarationUsed = (duFalse, duTrue, duIgnore);
  ELapeDeclarationHint = (ldhDeprecated, ldhExperimental, ldhUnImplemented);
  ELapeDeclarationHints = set of ELapeDeclarationHint;
  TLapeDeclarationHints = record
    Types: ELapeDeclarationHints;
    Message: lpString;
  end;

  TLapeDeclaration = class(TLapeBaseDeclClass)
  protected type
    TNameChangeNotifier = {$IFDEF FPC}specialize{$ENDIF} TLapeNotifierOfObject<TLapeDeclaration>;
  protected
    FList: TLapeDeclarationList;

    FName: lpString;
    FNameHash: UInt32;
    FNameHashPrevious: UInt32;
    FNameLapeCase: lpString;
    FNameChangeNotifier: TNameChangeNotifier;

    FHints: TLapeDeclarationHints;

    function getDocPos: TDocPos; override;
    procedure setList(AList: TLapeDeclarationList); virtual;
    procedure setName(AName: lpString); virtual;
  public
    _DocPos: TDocPos;
    Used: EDeclarationUsed;

    constructor Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    destructor Destroy; override;

    procedure AddHint(Typ: ELapeDeclarationHint; Msg: lpString = ''); virtual;
    procedure CopyHints(From: TLapeDeclaration);

    property DeclarationList: TLapeDeclarationList read FList write setList;
    property Name: lpString read FName write setName;
    property Hints: TLapeDeclarationHints read FHints;
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
    procedure inheritManagedDecls(ADecls: TLapeManagingDeclaration; AReplace: Boolean = False); virtual;

    procedure addSubDeclaration(ADecl: TLapeDeclaration); virtual;
    function HasSubDeclaration(AName: lpString; CheckParent: TInitBool): Boolean; overload; virtual;
    function HasSubDeclaration(ADecl: TLapeDeclaration; CheckParent: TInitBool): Boolean; overload; virtual;
    procedure ClearSubDeclarations; virtual;

    property ManagedDeclarations: TLapeDeclarationList read FManagedDecls write setManagedDecls;
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
    SizeOf(ELapeSmallEnum), SizeOf(ELapeLargeEnum), SizeOf(TLapeSmallSet), SizeOf(TLapeLargeSet),
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
  LapeIfTypes = LapeOrdinalTypes + LapeStringTypes + LapePointerTypes + LapeRealTypes + [ltVariant];
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

function LapeCase(const Str: lpString): lpString; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeHash(Data: Pointer; Len: UInt32; const Seed: UInt32 = 0): UInt32; overload;
function LapeHash(const Str: lpString): UInt32; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
function LapeTypeToString(Token: ELapeBaseType): lpString;
function LapeOperatorToString(Token: EOperator): lpString;

function PointerToString(const p: Pointer): lpString;
{$IF NOT(DECLARED(UIntToStr))}
function UIntToStr(i: UInt32): lpString; inline; overload;
function UIntToStr(i: UInt64): lpString; inline; overload;
{$DEFINE DoUIntToStr}
{$IFEND}

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

procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TIntegerArray; const SortUp: Boolean); overload;
procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TInt64Array; const SortUp: Boolean); overload;
procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TSingleArray; const SortUp: Boolean); overload;
procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TDoubleArray; const SortUp: Boolean); overload;
procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TExtendedArray; const SortUp: Boolean); overload;

procedure _Reverse(const Arr: PByte; const ElSize: SizeInt; Len: SizeInt);

{$IFDEF Lape_TrackObjects}
var
  lpgCounter: Integer;
  lpgList: TList;
{$ENDIF}

implementation

uses
  typinfo, variants, {$IFNDEF FPC}Generics.Defaults,{$ENDIF}
  lpmessages;

function LapeCase(const Str: lpString): lpString;
begin
  {$IFDEF Lape_CaseSensitive}
  Result := Str;
  {$ELSE}
  Result := LowerCase(Str);
  {$ENDIF}
end;

//MurMurHas2 by Tommi Prami & optimizations by Patrick van Logchem
function LapeHash(Data: Pointer; Len: UInt32; const Seed: UInt32 = 0): UInt32;
const
  cM: UInt32 = $5BD1E995;
  cR: UInt32 = 24;
var
  i, k: UInt32;
begin
  {$UNDEF REDO_Q}{$IFOPT Q+}{$Q-}{$DEFINE REDO_Q}{$ENDIF}
  {$UNDEF REDO_R}{$IFOPT R+}{$R-}{$DEFINE REDO_R}{$ENDIF}
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

function LapeHash(const Str: lpString): UInt32;
begin
  Result := LapeHash(PChar(Str), Length(Str) * SizeOf(lpChar), 0);
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

function PointerToString(const p: Pointer): lpString;
begin
  if ((p = nil) or (PPointer(p)^ = nil)) then
    Result := 'nil'
  else
    Result := lpString('0x'+IntToHex(PtrUInt(p^), 1));
end;

{$IFDEF DoUIntToStr}
function UIntToStr(i: UInt32): lpString; inline; overload;
begin
  Result := lpString(IntToStr(i));
end;

function UIntToStr(i: UInt64): lpString; inline; overload;
begin
  Result := lpString(IntToStr(i));
end;
{$ENDIF}

function VarTypeToVType(v: TVarType): SizeInt;
begin
  case (v and VarTypeMask) of
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
    else
      Result := vtVariant;
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

class procedure TLapeSorter{$IFNDEF FPC}<_T>{$ENDIF}.DoQuickSort(const Arr: PByte; const ElSize: SizeInt; var Weights: array of _T; iLo, iHi: SizeInt; const SortUp: Boolean);
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

class procedure TLapeSorter{$IFNDEF FPC}<_T>{$ENDIF}.QuickSort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: array of _T; const SortUp: Boolean);
begin
  if (Len <> Length(Weights)) then
    LapeExceptionFmt(lpeArrayLengthsDontMatch, [Format('%d,%d', [Len, Length(Weights)])]);

  if (Length(Weights) > 0) then
    DoQuickSort(Arr, ElSize, Weights, Low(Weights), High(Weights), SortUp);
end;

procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TIntegerArray; const SortUp: Boolean);
begin
  {$IFDEF FPC}specialize{$ENDIF} TLapeSorter<Integer>.QuickSort(Arr, ElSize, Len, Weights, SortUp);
end;

procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TInt64Array; const SortUp: Boolean);
begin
  {$IFDEF FPC}specialize{$ENDIF} TLapeSorter<Int64>.QuickSort(Arr, ElSize, Len, Weights, SortUp);
end;

procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TSingleArray; const SortUp: Boolean);
begin
  {$IFDEF FPC}specialize{$ENDIF} TLapeSorter<Single>.QuickSort(Arr, ElSize, Len, Weights, SortUp);
end;

procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TDoubleArray; const SortUp: Boolean);
begin
  {$IFDEF FPC}specialize{$ENDIF} TLapeSorter<Double>.QuickSort(Arr, ElSize, Len, Weights, SortUp);
end;

procedure _Sort(const Arr: PByte; const ElSize, Len: SizeInt; var Weights: TExtendedArray; const SortUp: Boolean);
begin
  {$IFDEF FPC}specialize{$ENDIF} TLapeSorter<Extended>.QuickSort(Arr, ElSize, Len, Weights, SortUp);
end;

procedure _Reverse(const Arr: PByte; const ElSize: SizeInt; Len: SizeInt);
type
  PSizeInt = ^SizeInt;
var
  T: PByte;
  Lo, Hi: PByte;
begin
  if (Arr = nil) then
    Exit;

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

function TLapeKeyValueList{$IFNDEF FPC}<_K, _V>{$ENDIF}.getValue(Key: _K): _V;
var
  i: Integer;
begin
  i := FKeyList.IndexOf(Key);
  if (i > -1) then
    Result := FValueList[i]
  else
    Result := FValueList.InvalidVal;
end;

procedure TLapeKeyValueList{$IFNDEF FPC}<_K, _V>{$ENDIF}.setValue(Key: _K; Value: _V);
begin
  FValueList.Insert(Value, FKeyList.Add(Key));
end;

constructor TLapeKeyValueList{$IFNDEF FPC}<_K, _V>{$ENDIF}.Create(InvalidKey: _K; InvalidValue: _V; ADuplicates: TDuplicates);
begin
  inherited Create();

  FKeyList := TTKeyList.Create(InvalidKey, ADuplicates, True);
  FValueList := TTValueList.Create(InvalidValue, ADuplicates, False);
end;

destructor TLapeKeyValueList{$IFNDEF FPC}<_K, _V>{$ENDIF}.Destroy;
begin
  FKeyList.Free();
  FValueList.Free();

  inherited Destroy();
end;

procedure TLapeKeyValueList{$IFNDEF FPC}<_K, _V>{$ENDIF}.Clear;
begin
  FKeyList.Clear();
  FValueList.Clear();
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

procedure TLapeStringList.setItem(Index: Integer; Item: lpString);
begin
  if (Index > -1) and (Index < FCount) then
  begin
    Delete(Index);
    Insert(Item, Index);
  end
  else
    inherited;
end;

constructor TLapeStringList.Create(InvalidValue: lpString; ADuplicates: TDuplicates; ACaseSensitive, ASort: Boolean);
begin
  FHashList := TLapeList_UInt32.Create(0, dupAccept, ASort);
  inherited Create(InvalidValue, ADuplicates, False);

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

function TLapeStringList.Add(Item: lpString): Integer;
var
  dup: TDuplicates;
begin
  if (FDuplicates in [dupIgnore, dupError]) and ExistsItem(Item) then
    if (FDuplicates = dupError) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Item])
    else
      Exit(-1);

  Assert(FHashList.Count = Count);

  Item := CaseSens(Item);
  Result := FHashList.Add(LapeHash(Item));

  if (Result > -1) then
  try
    dup := FDuplicates;
    FDuplicates := dupAccept;
    inherited MoveItem(inherited Add(Item), Result);
  finally
    FDuplicates := dup;
  end;
end;

function TLapeStringList.Delete(Index: Integer): lpString;
begin
  Result := inherited;
  FHashList.Delete(FCount);
  Assert(FHashList.Count = Count);
end;

procedure TLapeStringList.MoveItem(AFrom, ATo: Integer);
begin
  FHashList.MoveItem(AFrom, ATo);
  inherited;
end;

procedure TLapeStringList.SwapItem(AFrom, ATo: Integer);
begin
  FHashList.SwapItem(AFrom, ATo);
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
  i: Integer;
begin
  Clear();
  for i := 0 to High(Arr) do
    Add(Arr[i]);
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
    Add(Key, Item);
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
  FItems.Free();
  inherited;
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FStringList.Clear();
  FItems.Clear();
  FCount := 0;
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Insert(Key: lpString; Item: _T; Index: Integer): Integer;
begin
  Result := FStringList.Insert(Key, Index);
  if (Result > -1) then
    if (FItems.Insert(Item, Result) = Result) then
      Inc(FCount)
    else
      FStringList.Delete(Result);
end;

function TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.Add(Key: lpString; Item: _T): Integer;
begin
  Result := Insert(Key, Item, FCount);
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

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setKeyI(Index: Integer; NewKey: lpString);
begin
  if (Index > -1) and (Index < FCount) then
    Insert(NewKey, Delete(Index), Index);
end;

procedure TLapeStringMap{$IFNDEF FPC}<_T>{$ENDIF}.setKey(Item: _T; NewKey: lpString);
begin
  setKeyI(IndexOfItemI(Item), NewKey);
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

function TLapeUniqueStringDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Hash(const S: PChar; const Len: Integer): UInt32;
begin
  Result := LapeHash(S, Len * SizeOf(lpChar), FTag) and FSize;
end;

procedure TLapeUniqueStringDictionary{$IFNDEF FPC}<_T>{$ENDIF}.setValue(Key: lpString; Value: _T);
begin
  SetLength(FItems, Length(FItems) + 1);

  FItems[High(FItems)].Value := Value;
  FItems[High(FItems)].Key := Key;
end;

function TLapeUniqueStringDictionary{$IFNDEF FPC}<_T>{$ENDIF}.getValue(Key: lpString): _T;
var
  Bucket, Len: Integer;
begin
  if (Length(FItems) <> FCount) then
    Build();

  Len := Length(Key);
  if (Len >= FMinLength) and (Len <= FMaxLength) then
  begin
    Key := LowerCase(Key);
    Bucket := Self.Hash(PChar(Key), Len);

    if (FBuckets[Bucket].Key = Key) then
    begin
      Result := FBuckets[Bucket].Value;
      Exit;
    end;
  end;

  Result := InvalidVal;
end;

// Bruteforce a hashtable with no collisions
procedure TLapeUniqueStringDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Build;
var
  HashList: TStringList;
  i, Len, Bucket: Integer;
  Key: lpString;
  HashValue: String;
begin
  FCount := Length(FItems);
  FTag := 8;

  HashList := TStringList.Create();
  HashList.Sorted := True;

  while (HashList.Count < FCount) do
  begin
    HashList.Clear();

    for i := 0 to High(FItems) do
    begin
      Key := LowerCase(FItems[I].Key);
      Len := Length(Key);

      HashValue := IntToStr(Self.Hash(PChar(Key), Len));
      if (HashList.IndexOf(HashValue) > -1) then
      begin
        FTag := FTag * 2;

        Break;
      end;

      HashList.Add(HashValue);
    end;

    if (FTag >= High(Int32)) then
      LapeException(lpeImpossible); // Increase bucket size!
  end;

  HashList.Free();

  FMinLength := $FFFFFF;
  FMaxLength := 0;

  for i := 0 to High(FItems) do
  begin
    Key := LowerCase(FItems[I].Key);
    Len := Length(Key);

    if (Len < FMinLength) then
      FMinLength := Len;
    if (Len > FMaxLength) then
      FMaxLength := Len;

    Bucket := Self.Hash(PChar(Key), Len);

    FBuckets[Bucket].Key := Key;
    FBuckets[Bucket].Value := FItems[I].Value;
  end;
end;

constructor TLapeUniqueStringDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Create(InvalidValue: _T; Size: Integer);
begin
  inherited Create();

  FSize := Size - 1;
  SetLength(FBuckets, FSize + 1);

  InvalidVal := InvalidValue;
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

procedure TLapeNotifier{$IFNDEF FPC}<_T>{$ENDIF}.DeleteProc(const Proc: TNotifyProc);
var
  p: TNotifyProc;
begin
  p := FNotifiers.DeleteItem(Proc);
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

constructor TLapeNotifierOfObject{$IFNDEF FPC}<_T>{$ENDIF}.Create;
begin
  inherited;

  FNotifiers := TNotifiers.Create(nil, dupIgnore, False);
end;

destructor TLapeNotifierOfObject{$IFNDEF FPC}<_T>{$ENDIF}.Destroy;
begin
  FNotifiers.Free;

  inherited;
end;

procedure TLapeNotifierOfObject{$IFNDEF FPC}<_T>{$ENDIF}.AddProc(const Proc: TNotifyProcOfObject);
begin
  FNotifiers.Add(Proc);
end;

procedure TLapeNotifierOfObject{$IFNDEF FPC}<_T>{$ENDIF}.DeleteProc(const Proc: TNotifyProcOfObject);
var
  p: TNotifyProcOfObject;
begin
  p := FNotifiers.DeleteItem(Proc);
end;

procedure TLapeNotifierOfObject{$IFNDEF FPC}<_T>{$ENDIF}.Notify(Sender: _T);
var
  i: Integer;
  p: TNotifyProcOfObject;
begin
  for i := FNotifiers.Count - 1 downto 0 do
  begin
    p := FNotifiers[i];
    p(Sender);
  end;
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.GetBucket(const Hash: UInt32): PHashBucket;
begin
  Assert(FSize > 0);

  Result := @FBuckets[Hash and FSize];
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Clear;
begin
  FSize := 0;
  FCount := 0;
  FBuckets := nil;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Grow;
var
  i, j: Integer;
  OldBuckets: THashBuckets;
begin
  FCount := 0;
  if (FSize = 0) then
    FSize := FInitialSize - 1
  else
    FSize := (FSize + 1) * FGrowth - 1;

  OldBuckets := FBuckets;

  SetLength(FBuckets, 0);
  SetLength(FBuckets, FSize + 1);

  for i := 0 to High(OldBuckets) do
    for j := 0 to OldBuckets[i].Count - 1 do
      Add(OldBuckets[i].Items[j].Hash, OldBuckets[i].Items[j].Item, False);
end;

constructor TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Create(InitialSize: Integer; Growth: Integer);
begin
  inherited Create();

  FInitialSize := InitialSize;
  FGrowth := Growth;
end;

procedure TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Add(const Hash: UInt32; const Item: _T; const CanGrow: Boolean);
begin
  if CanGrow and ((FSize = 0) or (FCount > FSize div 2)) then
    Grow();

  with GetBucket(Hash)^ do
  begin
    if (Count = Length(Items)) then
      SetLength(Items, 4 + (Count * 4));

    Items[Count].Item := Item;
    Items[Count].Hash := Hash;

    Inc(Count);
  end;

  Inc(FCount);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.Remove(const Hash: UInt32; const Item: _T): Boolean;
var
  i: Integer;
begin
  Result := False;

  with GetBucket(Hash)^ do
  begin
    for i := 0 to Count - 1 do
      if CompareMem(@Items[i].Item, @Item, SizeOf(_T)) then
      begin
        Dec(Count);
        if (i < Count) then
          Items[i] := Items[Count];

        Result := True;
        Break;
      end;
  end;

  if Result then
    Dec(FCount);
end;

function TLapeDictionary{$IFNDEF FPC}<_T>{$ENDIF}.ExportToArray: TItemArray;
var
  i, j: Integer;
  Count: Integer;
begin
  SetLength(Result, FCount);

  if (FSize > 0) then
  begin
    Count := 0;

    for i := 0 to FSize do
      for j := 0 to FBuckets[i].Count - 1 do
      begin
        Result[Count] := FBuckets[i].Items[j].Item;

        Inc(Count);
      end;

    Assert(Count = FCount);
  end;
end;

procedure TLapeDeclCollection_Dictionary.NameChanged(Decl: TLapeDeclaration);
begin
  Assert(Decl <> nil);

  if (Decl.FNameHashPrevious = 0) then
    FUnnamedBucket.DeleteItem(Decl)
  else
    FDictionary.Remove(Decl.FNameHashPrevious, Decl);

  if (Decl.FNameHash = 0) then
    FUnnamedBucket.Add(Decl)
  else
    FDictionary.Add(Decl.FNameHash, Decl);
end;

function TLapeDeclCollection_Dictionary.getItem(Index: Integer): TLapeDeclaration;
begin
  Result := nil;

  LapeExceptionFmt(lpeInvalidDictionaryOperation, ['getItem']);
end;

function TLapeDeclCollection_Dictionary.getCount: Integer;
begin
  Result := FDictionary.FCount + FUnnamedBucket.Count;
end;

constructor TLapeDeclCollection_Dictionary.Create;
begin
  inherited Create;

  FUnnamedBucket := TList.Create(nil, dupAccept, True);
  FDictionary := TDictionary.Create();
end;

destructor TLapeDeclCollection_Dictionary.Destroy;
begin
  Clear();

  FreeAndNil(FUnnamedBucket);
  FreeAndNil(FDictionary);

  inherited;
end;

procedure TLapeDeclCollection_Dictionary.Clear;
begin
  FUnnamedBucket.Clear();
  FDictionary.Clear();
end;

procedure TLapeDeclCollection_Dictionary.Add(Decl: TLapeDeclaration);
begin
  Assert(Decl <> nil);

  if (Decl.FNameHash = 0) then
    FUnnamedBucket.Add(Decl)
  else
    FDictionary.Add(Decl.FNameHash, Decl);

  Decl.FNameChangeNotifier.AddProc({$IFDEF FPC}@{$ENDIF}NameChanged);
end;

function TLapeDeclCollection_Dictionary.Delete(Decl: TLapeDeclaration): Boolean;
begin
  Assert(Decl <> nil);

  if (Decl.FNameHash = 0) then
    Result := FUnnamedBucket.DeleteItem(Decl) <> nil
  else
    Result := FDictionary.Remove(Decl.FNameHash, Decl);

  Decl.FNameChangeNotifier.DeleteProc({$IFDEF FPC}@{$ENDIF}NameChanged);
end;

function TLapeDeclCollection_Dictionary.Get(Name: lpString; out Decl: TLapeDeclaration): Boolean;
var
  i: Integer;
begin
  Result := False;

  if (FDictionary.FSize > 0) then
    with FDictionary.GetBucket(LapeHash(Name))^ do
    begin
      for i := 0 to Count - 1 do
        if (Items[i].Item.FNameLapeCase = Name) then
        begin
          Decl := Items[i].Item;

          Result := True;
          Break;
        end;
    end;
end;

function TLapeDeclCollection_Dictionary.Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean;
var
  i: Integer;
begin
  Result := False;

  if (FDictionary.FSize > 0) then
    with FDictionary.GetBucket(LapeHash(Name))^ do
    begin
      for i := 0 to Count - 1 do
        if (Items[i].Item.FNameLapeCase = Name) and (Items[i].Item is AClass) then
        begin
          Decl := Items[i].Item;

          Result := True;
          Break;
        end;
    end;
end;

function TLapeDeclCollection_Dictionary.GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray;
var
  i, Len: Integer;
  Decls: TLapeDeclArray;
begin
  Len := 0;
  Decls := ExportToArray();

  SetLength(Result, Length(Decls));

  for i := 0 to High(Decls) do
    if (Decls[i] is AClass) then
    begin
      Result[Len] := Decls[i];
      Inc(Len);
    end;

  SetLength(Result, Len);
end;

function TLapeDeclCollection_Dictionary.IndexOf(Decl: TLapeDeclaration): Integer;
begin
  Result := -1;

  LapeExceptionFmt(lpeInvalidDictionaryOperation, ['IndexOf']);
end;

function TLapeDeclCollection_Dictionary.Exists(Decl: TLapeDeclaration): Boolean;
var
  i: Integer;
begin
  Assert(Decl <> nil);

  if (Decl.FNameHash = 0) then
  begin
    Result := FUnnamedBucket.ExistsItem(Decl);
    Exit;
  end;

  if (FDictionary.FSize > 0) then
    with FDictionary.GetBucket(Decl.FNameHash)^ do
    begin
      for i := 0 to Count - 1 do
        if (Items[i].Item = Decl) then
        begin
          Result := True;
          Exit;
        end;
    end;

  Result := False;
end;

function TLapeDeclCollection_Dictionary.ExportToArray: TLapeDeclArray;
begin
  Result := TLapeDeclArray(Concat(FDictionary.ExportToArray(), FUnnamedBucket.ExportToArray()));
end;

function TLapeDeclCollection_List.getItem(Index: Integer): TLapeDeclaration;
begin
  Result := FList[Index];
end;

function TLapeDeclCollection_List.getCount: Integer;
begin
  Result := FList.Count;
end;

constructor TLapeDeclCollection_List.Create(Sorted: Boolean);
begin
  inherited Create;

  FList := TList.Create(nil, dupAccept, Sorted);
end;

destructor TLapeDeclCollection_List.Destroy;
begin
  FList.Free();

  inherited Destroy;
end;

procedure TLapeDeclCollection_List.Clear;
begin
  FList.Clear();
end;

procedure TLapeDeclCollection_List.Add(Decl: TLapeDeclaration);
begin
  FList.Add(Decl);
end;

function TLapeDeclCollection_List.Delete(Decl: TLapeDeclaration): Boolean;
begin
  Result := FList.DeleteItem(Decl) <> nil;
end;

function TLapeDeclCollection_List.Get(Name: lpString; out Decl: TLapeDeclaration): Boolean;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if (FList[i].FNameLapeCase = Name) then
    begin
      Decl := FList[i];

      Result := True;
      Exit;
    end;

  Result := False;
end;

function TLapeDeclCollection_List.Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration): Boolean;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if (FList[i].FNameLapeCase = Name) and (FList[i] is AClass) then
    begin
      Decl := FList[i];

      Result := True;
      Exit;
    end;

  Result := False;
end;

function TLapeDeclCollection_List.GetByClass(AClass: TLapeDeclarationClass): TLapeDeclArray;
var
  Current: Integer;
  Decl: TLapeDeclaration;
begin
  SetLength(Result, FList.Count);

  if (FList.Count > 0) then
  begin
    Current := 0;

    for Decl in ExportToArray() do
      if (Decl is AClass) then
      begin
        Result[Current] := Decl;
        Inc(Current);
      end;

    SetLength(Result, Current);
  end;
end;

function TLapeDeclCollection_List.IndexOf(Decl: TLapeDeclaration): Integer;
begin
  Result := FList.IndexOf(Decl);
end;

function TLapeDeclCollection_List.Exists(Decl: TLapeDeclaration): Boolean;
begin
  Result := FList.ExistsItem(Decl);
end;

function TLapeDeclCollection_List.ExportToArray: TLapeDeclArray;
begin
  Result := TLapeDeclArray(FList.ExportToArray());
end;

constructor TLapeDeclarationList.Create(AList: TLapeDeclCollection; ManageDeclarations: Boolean);
begin
  inherited Create();

  if (AList = nil) then
    AList := TLapeDeclCollection_Dictionary.Create();

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

function TLapeDeclarationList.HasParent: Boolean;
begin
  Result := Parent <> nil;
end;

function TLapeDeclarationList.getItemCount: Integer;
begin
  Result := Count;
  if HasParent() then
    Inc(Result, Parent.ItemCount);
end;

function TLapeDeclarationList.getCount: Integer;
begin
  if (FList <> nil) then
    Result := FList.Count
  else
    Result := 0;
end;

function TLapeDeclarationList.IndexOf(Decl: TLapeDeclaration): Integer;
begin
  if (FList <> nil) then
    Result := FList.IndexOf(Decl)
  else
    Result := -1;
end;

function TLapeDeclarationList.getItem(Index: Integer): TLapeDeclaration;
begin
  if (FList <> nil) then
    Result := FList[Index]
  else
    Result := nil;
end;

procedure TLapeDeclarationList.Clear;
var
  Decl: TLapeDeclaration;
begin
  if (FList <> nil) then
  begin
    if FreeDecls then
    begin
      ClearSubDeclarations();

      for Decl in FList.ExportToArray() do
      begin
        Decl.FList := nil;
        Decl.Free();
      end;
    end;

    FList.Clear();
  end;
end;

procedure TLapeDeclarationList.ClearSubDeclarations;
var
  Decls: TLapeDeclArray;
  i: Integer;
begin
  Decls := GetByClass(TLapeManagingDeclaration, bFalse);
  for i := High(Decls) downto 0 do
    TLapeManagingDeclaration(Decls[i]).ClearSubDeclarations();
end;

procedure TLapeDeclarationList.addDeclaration(Decl: TLapeDeclaration);
begin
  if (FList <> nil) and (Decl <> nil) and (not HasSubDeclaration(Decl, bFalse)) then
  begin
    FList.Add(Decl);
    if (Decl.DeclarationList <> nil) then
      Decl.DeclarationList := Self
    else
      Decl.FList := Self;
  end;
end;

function TLapeDeclarationList.HasSubDeclaration(Name: lpString; CheckParent: TInitBool): Boolean;
var
  Decl: TLapeDeclaration;
begin
  Result := Get(Name, Decl, CheckParent);;
end;

function TLapeDeclarationList.HasSubDeclaration(Decl: TLapeDeclaration; CheckParent: TInitBool): Boolean;
begin
  if (FList = nil) then
    Exit(False);

  Result := FList.Exists(Decl);
  if (not Result) and HasParent() then
    if (CheckParent = bUnknown) then
      Result := Parent.HasSubDeclaration(Decl, bFalse)
    else if (CheckParent = bTrue) then
      Result := Parent.HasSubDeclaration(Decl, bTrue);
end;

function TLapeDeclarationList.ExportToArray: TLapeDeclArray;
begin
  Result := FList.ExportToArray;
end;

procedure TLapeDeclarationList.Delete(Decl: TLapeDeclaration; DoFree: Boolean);
begin
  if (FList <> nil) and FList.Delete(Decl) then
    if DoFree then
      Decl.Free()
    else
      Decl.FList := nil;
end;

procedure TLapeDeclarationList.Delete(AClass: TLapeDeclarationClass; DoFree: Boolean = False);
var
  ClassItems: TLapeDeclArray;
  i: Integer;
begin
  ClassItems := GetByClass(AClass, bFalse);
  for i := High(ClassItems) downto 0 do
    Delete(ClassItems[i], DoFree);
end;

function TLapeDeclarationList.Get(Name: lpString; out Decl: TLapeDeclaration; CheckParent: TInitBool): Boolean;
begin
  Result := False;

  Name := LapeCase(Name);
  if (FList <> nil) then
    Result := FList.Get(Name, Decl);

  if (not Result) and HasParent() then
    if (CheckParent = bUnknown) then
      Result := Parent.Get(Name, Decl, bFalse)
    else if (CheckParent = bTrue) then
      Result := Parent.Get(Name, Decl, bTrue);
end;

function TLapeDeclarationList.Get(Name: lpString; AClass: TLapeDeclarationClass; out Decl: TLapeDeclaration; CheckParent: TInitBool): Boolean;
begin
  Result := False;

  Name := LapeCase(Name);
  if (FList <> nil) then
    Result := FList.Get(Name, AClass, Decl);

  if (not Result) and HasParent() then
    if (CheckParent = bUnknown) then
      Result := Parent.Get(Name, AClass, Decl, bFalse)
    else if (CheckParent = bTrue) then
      Result := Parent.Get(Name, AClass, Decl, bTrue);
end;

function TLapeDeclarationList.GetByClass(AClass: TLapeDeclarationClass; CheckParent: TInitBool): TLapeDeclArray;
begin
  Result := nil;

  if (FList <> nil) then
    Result := FList.GetByClass(AClass);

  if (Result = nil) and HasParent() then
    if (CheckParent = bUnknown) then
      Result := Parent.GetByClass(AClass, bFalse)
    else if (CheckParent = bTrue) then
      Result := Parent.GetByClass(AClass, bTrue);
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
    FNameHashPrevious := FNameHash;

    FName := AName;
    FNameLapeCase := LapeCase(FName);
    FNameHash := LapeHash(FNameLapeCase);

    FNameChangeNotifier.Notify(Self);
  end;
end;

constructor TLapeDeclaration.Create(AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create();

  FNameChangeNotifier := TNameChangeNotifier.Create();
  FNameHash := 0;
  if (AName <> '') then
    Name := AName;

  Used := duIgnore;
  if (ADocPos <> nil) then
    _DocPos := ADocPos^
  else
    _DocPos := NullDocPos;

  setList(AList);
end;

destructor TLapeDeclaration.Destroy;
begin
  setList(nil);
  FNameChangeNotifier.Free();
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

procedure TLapeManagingDeclaration.inheritManagedDecls(ADecls: TLapeManagingDeclaration; AReplace: Boolean = False);
begin
  Assert(ADecls <> nil);
  Assert(ADecls.ManagedDeclarations <> nil);

  if AReplace then
    setManagedDecls(ADecls.ManagedDeclarations)
  else
    FManagedDecls.Parent := ADecls.ManagedDeclarations;
end;

procedure TLapeManagingDeclaration.addSubDeclaration(ADecl: TLapeDeclaration);
begin
  FManagedDecls.addDeclaration(ADecl);
end;

function TLapeManagingDeclaration.HasSubDeclaration(AName: lpString; CheckParent: TInitBool): Boolean;
begin
  Result := FManagedDecls.HasSubDeclaration(AName, CheckParent);
end;

function TLapeManagingDeclaration.HasSubDeclaration(ADecl: TLapeDeclaration; CheckParent: TInitBool): Boolean;
begin
  Result := FManagedDecls.HasSubDeclaration(ADecl, CheckParent);
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

