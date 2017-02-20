{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All (script)type and (script)variable classes, including corresponding evaluation functions (runtime/compile time).
}
unit lpvartypes;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpparser, lpcodeemitter;

type
  ECompilerOption = (
    lcoAssertions,                     // {$C} {$ASSERTIONS}
    lcoRangeCheck,                     // {$R} {$RANGECHECKS}      TODO
    lcoShortCircuit,                   // {$B} {$BOOLEVAL}
    lcoAlwaysInitialize,               // {$M} {$MEMORYINIT}
    lcoFullDisposal,                   // {$D} {$FULLDISPOSAL}
    lcoLooseSemicolon,                 // {$L} {$LOOSESEMICOLON}
    lcoLooseSyntax,                    // {$X} {$EXTENDEDSYNTAX}
    lcoAutoInvoke,                     // {$F} {$AUTOINVOKE}
    lcoAutoProperties,                 // {$P} {$AUTOPROPERTIES}
    lcoScopedEnums,                    // {$S} {$SCOPEDENUMS}
    lcoConstAddress,                   // {$J} {$CONSTADDRESS}
    lcoHints,                          // {$H} {$HINTS}
    lcoContinueCase,                   //      {$CONTINUECASE}
    lcoCOperators,                     //      {$COPERATORS}
    lcoInitExternalResult              // Ensure empty result for external calls (useful for ffi)
  );
  ECompilerOptionsSet = set of ECompilerOption;
  PCompilerOptionsSet = ^ECompilerOptionsSet;

const
  Lape_OptionsDef = [lcoCOperators, lcoRangeCheck, lcoHints, lcoShortCircuit, lcoAlwaysInitialize, lcoAutoInvoke, lcoConstAddress];
  Lape_PackRecordsDef = 8;

type
  TLapeType = class;
  TLapeVar = class;
  TLapeStackVar = class;
  TLapeStackInfo = class;
  TLapeGlobalVar = class;
  TLapeType_OverloadedMethod = class;
  TLapeCompilerBase = class;

  TLapeBaseTypes = array[ELapeBaseType] of TLapeType;
  TLapeTypeArray = array of TLapeType;
  TLapeVarStack = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeStackVar>;
  TLapeVarMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapeGlobalVar>;

  TVarPos = record
    isPointer: Boolean;
    Offset: Integer;
    case MemPos: EMemoryPos of
      mpVar: (StackVar: TLapeStackVar);
      mpMem: (GlobalVar: TLapeGlobalVar);
      mpStack: (ForceVariable: Boolean);
  end;

  PResVar = ^TResVar;
  TResVar = {$IFDEF FPC}object{$ELSE}record{$ENDIF}
  private
    function getReadable: Boolean;
    function getWriteable: Boolean;
    function getConstant: Boolean;
  public
    VarType: TLapeType;
    VarPos: TVarPos;

    class function New(AVar: TLapeVar): TResVar; {$IFNDEF FPC}static;{$ENDIF}
    function HasType: Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}

    procedure Spill(Unlock: Integer = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
    function IncLock(Count: Integer = 1): TResVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
    function DecLock(Count: Integer = 1): TResVar; {$IFDEF Lape_Inline}inline;{$ENDIF}

    function InScope(AStack: TLapeStackInfo; Pos: PDocPos = nil): TResVar; {$IFDEF Lape_Inline}inline;{$ENDIF}

    procedure IncOffset(Offset: Integer); {$IFDEF Lape_Inline}inline;{$ENDIF}
    procedure DecOffset(Offset: Integer); {$IFDEF Lape_Inline}inline;{$ENDIF}
    procedure setOffset(Offset: Integer); {$IFDEF Lape_Inline}inline;{$ENDIF}

    procedure setReadable(AReadable: Boolean; ChangeStack: Boolean); overload;
    procedure setReadable(AReadable: Boolean); overload;
    procedure setWriteable(AWriteable: Boolean; ChangeStack: Boolean); overload;
    procedure setWriteable(AWriteable: Boolean); overload;
    procedure setConstant(AConst: Boolean; ChangeStack: Boolean); overload;
    procedure setConstant(AConst: Boolean); overload;

    procedure setReadWrite(AReadable, AWriteable: Boolean);
    procedure CopyFlags(Other: TResVar);

    property Readable: Boolean read getReadable write setReadable;
    property Writeable: Boolean read getWriteable write setWriteable;
    property isConstant: Boolean read getConstant write setConstant;
  end;

  ELapeParameterType = (lptNormal, lptConst, lptConstRef, lptVar, lptOut);
  TLapeParameter = record
    ParType: ELapeParameterType;
    VarType: TLapeType;
    Default: TLapeVar;
  end;
  TLapeParameterList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeParameter>;

  ELapeVarFlag = (lvfReadable, lvfWriteable);
  ELapeVarFlags = set of ELapeVarFlag;

  TLapeVar = class(TLapeDeclaration)
  protected
    FVarFlags: ELapeVarFlags;

    function getBaseType: ELapeBaseType; virtual;
    function getSize: SizeInt; virtual;
    function getLo: TLapeGlobalVar; virtual;
    function getHi: TLapeGlobalVar; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;

    function getReadable: Boolean; virtual;
    procedure setReadable(AReadable: Boolean); virtual;
    function getWriteable: Boolean; virtual;
    procedure setWriteable(AWriteable: Boolean); virtual;
    function getConstant: Boolean;
    procedure setConstant(AConst: Boolean);
  public
    VarType: TLapeType;

    constructor Create(AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    function HasType: Boolean;

    procedure setReadWrite(AReadable, AWriteable: Boolean);
    procedure CopyFlags(Other: TLapeVar);

    property BaseType: ELapeBaseType read getBaseType;
    property Size: SizeInt read getSize;
    property Lo: TLapeGlobalVar read getLo;
    property Hi: TLapeGlobalVar read getHi;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;

    property Readable: Boolean read getReadable write setReadable;
    property Writeable: Boolean read getWriteable write setWriteable;
    property isConstant: Boolean read getConstant write setConstant;
  end;

  TLapeStackVar = class(TLapeVar)
  protected
    FStack: TLapeVarStack;
    procedure setStack(Stack: TLapeVarStack); virtual;
    function getOffset: Integer; virtual;
  public
    constructor Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    destructor Destroy; override;

    property Stack: TLapeVarStack read FStack write setStack;
    property Offset: Integer read getOffset;
  end;

  TLapeStackTempVar = class(TLapeStackVar)
  protected
    FLock: Integer;
    function getLocked: Boolean;
    procedure setLocked(DoLock: Boolean);
  public
    constructor Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); override;
    function IncLock(Count: Integer = 1): Integer; virtual;
    function Declock(Count: Integer = 1): Integer; virtual;
    property Locked: Boolean read getLocked write setLocked;
  end;

  TLapeParameterVar = class(TLapeStackVar)
  protected
    FParType: ELapeParameterType;
    function getSize: SizeInt; override;
    function getInitialization: Boolean; override;
    function getFinalization: Boolean; override;
  public
    constructor Create(AParType: ELapeParameterType; AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    property ParType: ELapeParameterType read FParType;
  end;

  TLapeStackInheritedVar = class(TLapeParameterVar)
  protected
    FParent: TLapeStackVar;
  public
    constructor Create(AParent: TLapeStackVar; AStack: TLapeVarStack; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    property Parent: TLapeStackVar read FParent;
  end;

  TLapeGlobalVar = class(TLapeVar)
  protected
    FBasePtr: Pointer;
    FPtr: Pointer;
    function getAsString: lpString; virtual;
    function getAsInt: Int64; virtual;
  public
    DoManage: Boolean;
    constructor Create(AVarType: TLapeType; Initialize: Boolean = True; ManagePtr: Boolean = True; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; overload; virtual;
    constructor Create(AVarType: TLapeType; Ptr: Pointer; ManagePtr: Boolean = False; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; overload; virtual;
    destructor Destroy; override;

    function CreateCopy(CopyContent: Boolean = True): TLapeGlobalVar; virtual;
    function CompatibleWith(Other: TLapeGlobalVar): Boolean; virtual;

    function isNull: Boolean; virtual;
    function Equals(Other: TLapeGlobalVar): Boolean; reintroduce; virtual;

    property Ptr: Pointer read FPtr;
    property AsString: lpString read getAsString;
    property AsInteger: Int64 read getAsInt;
  end;

  ELapeEvalFlag = (lefAssigning, lefInvoking, lefConstAddress, lefConstRangeCheck, lefRangeCheck);
  ELapeEvalFlags = set of ELapeEvalFlag;

  TLapeType = class(TLapeManagingDeclaration)
  protected
    FBaseType: ELapeBaseType;
    FCompiler: TLapeCompilerBase;
    FSize: SizeInt;
    FInit: TInitBool;
    FStatic: Boolean;

    FLo: TLapeGlobalVar;
    FHi: TLapeGlobalVar;
    FAsString: lpString;

    function getEvalRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType; virtual;
    function getEvalProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc; virtual;

    procedure setBaseType(ABaseType: ELapeBaseType); virtual;
    function getBaseIntType: ELapeBaseType; virtual;
    function getPadding: SizeInt; virtual;
    function getSize: SizeInt; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
    function getAsString: lpString; virtual;
  public
    TypeID: Integer;

    constructor Create(ABaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; virtual;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; reintroduce; virtual;
    function CompatibleWith(Other: TLapeType): Boolean; virtual;
    procedure ClearCache; virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; virtual;
    function VarToString(AVar: Pointer): lpString; virtual;
    function VarToInt(AVar: Pointer): Int64; virtual;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; virtual;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; virtual;

    function NewGlobalVarP(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;

    function addSubDeclaration(ADecl: TLapeDeclaration): Integer; override;
    function CanHaveChild: Boolean; virtual;
    function IsOrdinal(OrPointer: Boolean = False): Boolean; virtual;

    function HasChild(AName: lpString): Boolean; overload; virtual;
    function HasChild(ADecl: TLapeDeclaration): Boolean; overload; virtual;
    function HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; overload; virtual;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; overload; virtual;
    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; virtual;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; virtual;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; overload; virtual;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; Pos: PDocPos = nil): TResVar; overload; virtual;

    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); overload; virtual;
    procedure Finalize(AVar: TResVar; UseCompiler: Boolean = True; Pos: PDocPos = nil); overload; virtual;
    procedure Finalize(AVar: TLapeVar; var Offset: Integer; UseCompiler: Boolean = False; Pos: PDocPos = nil); overload; virtual;
    procedure Finalize(AVar: TLapeVar; UseCompiler: Boolean = False; Pos: PDocPos = nil); overload; virtual;

    property Compiler: TLapeCompilerBase read FCompiler;
    property BaseType: ELapeBaseType read FBaseType write setBaseType;
    property BaseIntType: ELapeBaseType read getBaseIntType;
    property Padding: SizeInt read getPadding;
    property Size: SizeInt read getSize;
    property IsStatic: Boolean read FStatic;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
    property AsString: lpString read getAsString;
  end;

  TLapeTTypeClass = class of TLapeType_Type;
  TLapeType_Type = class(TLapeType)
  protected
    FTType: TLapeType;
    function getAsString: lpString; override;
  public
    constructor Create(AType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    property TType: TLapeType read FTType;
  end;

  TLapeType_TypeEnum = class(TLapeType_Type)
  public
    function CanHaveChild: Boolean; override;
    function HasChild(AName: lpString): Boolean; override;
    function HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean; override;

    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
  end;

  TLapeType_Pointer = class(TLapeType)
  protected
    FPType: TLapeType;
    FPConst: Boolean;
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; PointerType: TLapeType = nil; ConstPointer: Boolean = True; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;
    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil; AsValue: Boolean = True): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function HasType: Boolean;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property PType: TLapeType read FPType;
    property PConst: Boolean read FPConst;
  end;

  TLapeType_Label = class(TLapeType_Pointer)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_Method = class(TLapeType)
  protected
    FParams: TLapeParameterList;
    procedure setBaseType(ABaseType: ELapeBaseType); override;
    function getSize: SizeInt; override;
    function getAsString: lpString; override;
    function getParamSize: SizeInt; virtual;
    function getParamInitialization: Boolean; virtual;
  public
    FreeParams: Boolean;
    ImplicitParams: Integer;
    Res: TLapeType;
    IsOperator: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(ACompiler: TLapeCompilerBase; AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AMethod: TLapeType_Method); overload; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;
    destructor Destroy; override;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;
    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;

    function EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True; IgnoreDefault: Boolean = False): Boolean; virtual;
    procedure addParam(Param: TLapeParameter); virtual;
    procedure setImported(AVar: TLapeGlobalVar; isImported: Boolean); virtual;

    function NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;
    function NewGlobalVar(CodePos: TCodePos; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property Params: TLapeParameterList read FParams;
    property ParamSize: SizeInt read getParamSize;
    property ParamInitialization: Boolean read getParamInitialization;
  end;

  TLapeType_MethodOfObject = class(TLapeType_Method)
  protected
    FMethodRecord: TLapeType;
    function getSize: SizeInt; override;
    function getAsString: lpString; override;
    function getParamSize: SizeInt; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); override;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;
    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;

    function NewGlobalVar(AMethod: TMethod; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_MethodOfType = class(TLapeType_MethodOfObject)
  protected
    FObjectType: TLapeType;
    function getAsString: lpString; override;
  public
    SelfParam: ELapeParameterType;

    constructor Create(ACompiler: TLapeCompilerBase; AObjType: TLapeType; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AMethod: TLapeType_Method; AObjType: TLapeType); overload; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;
    function EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True; IgnoreDefault: Boolean = False): Boolean; override;

    property ObjectType: TLapeType read FObjectType;
  end;

  TLapeGetOverloadedMethod = function(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method;
    AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar of object;

  TLapeType_OverloadedMethod = class(TLapeType)
  protected
    FOfObject: TInitBool;
    function getAsString: lpString; override;
  public
    OnFunctionNotFound: TLapeGetOverloadedMethod;
    NeedFullMatch: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function addSubDeclaration(ADecl: TLapeDeclaration): Integer; override;
    procedure addMethod(AMethod: TLapeGlobalVar; DoOverride: Boolean = False); virtual;
    function overrideMethod(AMethod: TLapeGlobalVar): TLapeGlobalVar; virtual;

    function getMethodIndex(AType: TLapeType_Method): Integer; overload; virtual;
    function getMethodIndex(AParams: TLapeTypeArray; AResult: TLapeType = nil): Integer; overload; virtual;
    function getMethod(AType: TLapeType_Method): TLapeGlobalVar; overload; virtual;
    function getMethod(AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar; overload; virtual;

    function NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; override;

    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property MethodsOfObject: TInitBool read FOfObject;
  end;

  TLapeWithDeclRec = record
    WithVar: PResVar;
    WithType: TLapeType;
  end;
  TLapeWithDeclarationList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeWithDeclRec>;

  TLapeWithDeclaration = class(TLapeDeclaration)
  protected
    FWithDeclRec: TLapeWithDeclRec;
  public
    constructor Create(AWithDeclRec: TLapeWithDeclRec); reintroduce; virtual;
    property WithDeclRec: TLapeWithDeclRec read FWithDeclRec;
  end;

  TLapeVarRef = record
    Lock: Integer;
    ResVar: TResVar;
    RefVar: TLapeVar;
  end;
  TLapeVarRefMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapeVarRef>;

  TLapeType_VarRefMap = class(TLapeType)
  protected
    FVarMap: TLapeVarRefMap;
  public
    constructor Create(ACompiler: TLapeCompilerBase); reintroduce; virtual;
    destructor Destroy; override;

    function CanHaveChild: Boolean; override;
    function HasChild(AName: lpString): Boolean; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; override;

    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    procedure addVar(RefVar: TLapeVar; AName: lpString); overload; virtual;
    procedure addVar(RefVar: TResVar; AName: lpString); overload; virtual;
    property VarMap: TLapeVarRefMap read FVarMap;
  end;

  TLapeStackInfo = class(TLapeDeclarationList)
  protected
    FVarStack: TLapeVarStack;
    FWithStack: TLapeWithDeclarationList;

    FOldStackPos: Integer;
    FOldMaxStack: Integer;

    function getVar(Index: Integer): TLapeStackVar; virtual;
    function getVarCount: Integer; virtual;
    function getTotalSize: SizeInt; virtual;
    function getTotalParamSize: SizeInt; virtual;
    function getTotalNoParamSize: SizeInt; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
  public
    Owner: TLapeStackInfo;
    FreeVars: Boolean;
    CodePos: Integer;

    ForceInitialization: Boolean;
    FullDisposal: Boolean;

    constructor Create(AlwaysInitialize: Boolean = True; ForceDisposal: Boolean = False; AOwner: TLapeStackInfo = nil; ManageVars: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    function getDeclaration(Name: lpString; CheckParent: TInitBool; CheckWith: Boolean): TLapeDeclaration; virtual;
    function hasDeclaration(Name: lpString; CheckParent: TInitBool; CheckWith: Boolean): Boolean; overload; virtual;
    function hasDeclaration(Decl: TLapeDeclaration; CheckParent: TInitBool; CheckWith: Boolean): Boolean; overload; virtual;

    function getTempVar(VarType: TLapeType; Lock: Integer = 1): TLapeStackTempVar; virtual;
    function addDeclaration(Decl: TLapeDeclaration): Integer; override;
    function addVar(StackVar: TLapeStackVar): TLapeStackVar; overload; virtual;
    function addVar(VarType: TLapeType; Name: lpString = ''): TLapeStackVar; overload; virtual;
    function addVar(ParType: ELapeParameterType; VarType: TLapeType; Name: lpString = ''): TLapeStackVar; overload; virtual;
    function addSelfVar(ParType: ELapeParameterType; VarType: TLapeType): TLapeStackVar; overload; virtual;

    function inheritVar(StackVar: TLapeStackVar): TLapeStackVar; virtual;
    function getInheritedVar(StackVar: TLapeStackVar): TLapeStackVar; virtual;

    function addWith(AWith: TLapeWithDeclRec): Integer; virtual;
    procedure delWith(ACount: Integer); virtual;

    property VarStack: TLapeVarStack read FVarStack;
    property WithStack: TLapeWithDeclarationList read FWithStack;
    property Vars[Index: Integer]: TLapeStackVar read getVar; default;
    property VarCount: Integer read getVarCount;
    property TotalSize: SizeInt read getTotalSize;
    property TotalParamSize: SizeInt read getTotalParamSize;
    property TotalNoParamSize: SizeInt read getTotalNoParamSize;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
    property OldStackPos: Integer read FOldStackPos;
    property OldMaxStack: Integer read FOldMaxStack;
  end;

  TLapeDeclStack = class(TLapeStackInfo)
  protected
    FManagingList: TLapeManagingDeclaration;
  public
    //constructor Create(AList: TLapeDeclarationList; AOwner: TLapeStackInfo = nil); reintroduce; overload; virtual;
    constructor Create(AList: TLapeManagingDeclaration; AOwner: TLapeStackInfo = nil); reintroduce; overload; virtual;

    function addDeclaration(Decl: TLapeDeclaration): Integer; override;
    function addVar(StackVar: TLapeStackVar): TLapeStackVar; override;
    property ManagingList: TLapeManagingDeclaration read FManagingList;
  end;

  TLapeEmptyStack = class(TLapeStackInfo)
  public
    function addDeclaration(Decl: TLapeDeclaration): Integer; override;
  end;

  TLapeCodeEmitter = class(TLapeCodeEmitterBase)
  public
    function _IncCall(ACodePos: TResVar; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _IncCall(ACodePos: TResVar; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;

    function _InvokeImportedProc(AMemPos: TResVar; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InvokeImportedProc(AMemPos: TResVar; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;
    function _InvokeImportedFunc(AMemPos, AResPos: TResVar; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InvokeImportedFunc(AMemPos, AResPos: TResVar; AParamSize: UInt16; Pos: PDocPos = nil): Integer; overload;

    function _JmpRIf(Jmp: TCodeOffset; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIf(Jmp: TCodeOffset; Cond: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;

    function _Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    function _Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;
  end;

  TLapeHint = procedure(Sender: TLapeCompilerBase; Msg: lpString) of object;

  TLapeCompilerBase = class(TLapeBaseDeclClass)
  protected
    FEmitter: TLapeCodeEmitter;
    FStackInfo: TLapeStackInfo;
    FBaseTypes: TLapeBaseTypes;

    FGlobalDeclarations: TLapeDeclarationList;
    FManagedDeclarations: TLapeDeclarationList;
    FCachedDeclarations: TLapeVarMap;

    FBaseOptions: ECompilerOptionsSet;
    FBaseOptions_PackRecords: UInt8;
    FOptions: ECompilerOptionsSet;
    FOptions_PackRecords: UInt8;

    FOnHint: TLapeHint;

    procedure Reset; virtual;
    procedure setEmitter(AEmitter: TLapeCodeEmitter); virtual;
  public
    FreeEmitter: Boolean;

    constructor Create(AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure VarToDefault(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil); overload; virtual;
    procedure VarToDefault(AVar: TResVar; Pos: PDocPos = nil); overload; virtual;
    procedure FinalizeVar(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil); overload; virtual;
    procedure FinalizeVar(AVar: TResVar; Pos: PDocPos = nil); overload; virtual;
    function PopVarToStack(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; virtual;

    function IncStackInfo(AStackInfo: TLapeStackInfo; var Offset: Integer; Emit: Boolean = True; Pos: PDocPos = nil): TLapeStackInfo; overload; virtual;
    function IncStackInfo(var Offset: Integer; Emit: Boolean = True; Pos: PDocPos = nil): TLapeStackInfo; overload; virtual;
    function IncStackInfo(Emit: Boolean = False): TLapeStackInfo; overload; virtual;
    function DecStackInfo(var Offset: Integer; InFunction: Boolean = False; Emit: Boolean = True; DoFree: Boolean = False; Pos: PDocPos = nil): TLapeStackInfo; overload; virtual;
    function DecStackInfo(InFunction: Boolean = False; Emit: Boolean = False; DoFree: Boolean = False): TLapeStackInfo; overload; virtual;

    procedure EmitCode(ACode: lpString; var Offset: Integer; Pos: PDocPos = nil); overload; virtual; abstract;
    procedure EmitCode(ACode: lpString; AVarNames: array of lpString; AVars: array of TLapeVar; AResVars: array of TResVar; var Offset: Integer; Pos: PDocPos = nil); overload; virtual;

    function getBaseType(Name: lpString): TLapeType; overload; virtual;
    function getBaseType(BaseType: ELapeBaseType): TLapeType; overload; virtual;
    function addLocalDecl(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration; overload; virtual;
    function addLocalDecl(ADecl: TLapeDeclaration): TLapeDeclaration; overload; virtual;
    function addGlobalDecl(ADecl: TLapeDeclaration): TLapeDeclaration; virtual;
    function addManagedDecl(ADecl: TLapeDeclaration): TLapeDeclaration; virtual;
    function addManagedVar(AVar: TLapeVar; PtrCheckOnly: Boolean = False): TLapeVar; virtual;
    function addManagedType(AType: TLapeType): TLapeType; virtual;
    function addStackVar(VarType: TLapeType; Name: lpString): TLapeStackVar; virtual;

    function getCachedConstant(Str: lpString; BaseType: ELapeBaseType = ltUnknown): TLapeGlobalVar; virtual;
    function getConstant(Str: lpString; BaseType: ELapeBaseType = ltString; DoGrow: Boolean = False; ForceType: Boolean = False): TLapeGlobalVar; overload; virtual;
    function getConstant(i: Int64; IntType: ELapeBaseType = ltNativeInt; DoGrow: Boolean = False; ForceType: Boolean = False): TLapeGlobalVar; overload; virtual;
    function getLabel(CodePos: Integer): TLapeGlobalVar; virtual;

    procedure getDestVar(var Dest, Res: TResVar; Op: EOperator); virtual;
    function getTempVar(VarType: ELapeBaseType; Lock: Integer = 1): TLapeStackTempVar; overload; virtual;
    function getTempVar(VarType: TLapeType; Lock: Integer = 1): TLapeStackTempVar; overload; virtual;
    function getTempStackVar(VarType: ELapeBaseType): TResVar; overload; virtual;
    function getTempStackVar(VarType: TLapeType): TResVar; overload; virtual;

    function getPointerType(PType: ELapeBaseType; PConst: Boolean): TLapeType_Pointer; overload; virtual;
    function getPointerType(PType: TLapeType; PConst: Boolean): TLapeType_Pointer; overload; virtual;
    function getTypeVar(AType: ELapeBaseType): TLapeGlobalVar; overload; virtual;
    function getTypeVar(AType: TLapeType): TLapeGlobalVar; overload; virtual;
    function getGlobalVar(AName: lpString): TLapeGlobalVar; virtual;
    function getGlobalType(AName: lpString): TLapeType; virtual;

    function getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration; overload; virtual;
    function getDeclaration(AName: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration; overload; virtual;
    function hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(AName: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(ADecl: TLapeDeclaration; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;

    procedure Hint(Msg: lpString; Args: array of const; ADocPos: TDocPos);

    property StackInfo: TLapeStackInfo read FStackInfo;
    property BaseTypes: TLapeBaseTypes read FBaseTypes;

    property GlobalDeclarations: TLapeDeclarationList read FGlobalDeclarations;
    property ManagedDeclarations: TLapeDeclarationList read FManagedDeclarations;
    property Globals[AName: lpString]: TLapeGlobalVar read getGlobalVar; default;
  published
    property Emitter: TLapeCodeEmitter read FEmitter write setEmitter;
    property Options: ECompilerOptionsSet read FOptions write FBaseOptions default Lape_OptionsDef;
    property Options_PackRecords: UInt8 read FOptions_PackRecords write FBaseOptions_PackRecords default Lape_PackRecordsDef;
    property OnHint: TLapeHint read FOnHint write FOnHint;
  end;

function ResolveCompoundOp(op:EOperator; typ:TLapeType): EOperator; {$IFDEF Lape_Inline}inline;{$ENDIF}
function getTypeArray(Arr: array of TLapeType): TLapeTypeArray;
procedure ClearBaseTypes(var Arr: TLapeBaseTypes; DoFree: Boolean);
procedure LoadBaseTypes(var Arr: TLapeBaseTypes; Compiler: TLapeCompilerBase);

function MethodOfObject(VarType: TLapeType): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ValidFieldName(Field: TLapeGlobalVar): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}
function ValidFieldName(Field: TResVar): Boolean; overload; {$IFDEF Lape_Inline}inline;{$ENDIF}

const
  BigLock = 256;

  TypeID_Unknown = Ord(Low(ELapeBaseType)) - 1;
  TypeID_User = Ord(High(ELapeBaseType)) + 1;

  NullResVar: TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpNone;  GlobalVar: nil));
  VarResVar:  TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpVar;   StackVar : nil));
  StackResVar:TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpStack; ForceVariable: False));

  NullParameter: TLapeParameter = (ParType: lptNormal; VarType: nil; Default: nil);
  NullWithDecl: TLapeWithDeclRec = (WithVar: nil; WithType: nil);
  NullVarRef: TLapeVarRef = (Lock: -1; ResVar: (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpNone; GlobalVar: nil)); RefVar: nil);

  Lape_RefParams   = [lptConstRef, lptOut, lptVar];
  Lape_ConstParams = [lptConst, lptConstRef];
  Lape_ValParams   = Lape_ConstParams + [lptNormal];
  Lape_SelfParam   = lptVar;

var
  LapeReservedLocals: lpString = '|System|';
  EmptyStackInfo: TLapeEmptyStack = nil;
  _ResVar: TResVar;

implementation

uses
  {$IFDEF Lape_NeedAnsiStringsUnit}AnsiStrings,{$ENDIF}
  lpvartypes_ord, lpvartypes_array,
  lpmessages, lpeval, lpinterpreter;


function ResolveCompoundOp(op:EOperator; typ:TLapeType): EOperator;
begin
  case op of
    op_AssignDiv:
      if (typ.BaseType in LapeRealTypes) then
        Result := op_divide
      else
        Result := op_DIV;
    op_AssignMinus: Result := op_Minus;
    op_AssignMul:   Result := op_Multiply;
    op_AssignPlus:  Result := op_Plus;
    else            Result := op_Unknown;
  end;
end;

function getTypeArray(Arr: array of TLapeType): TLapeTypeArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Arr));
  for i := 0 to High(Arr) do
    Result[i] := Arr[i];
end;

procedure ClearBaseTypes(var Arr: TLapeBaseTypes; DoFree: Boolean);
var
  BaseType: ELapeBaseType;
begin
  for BaseType := Low(ELapeBaseType) to High(ELapeBaseType) do
    if (Arr[BaseType] <> nil) then
      if DoFree then
        FreeAndNil(Arr[BaseType])
      else
        Arr[BaseType].ClearSubDeclarations();
end;

procedure LoadBaseTypes(var Arr: TLapeBaseTypes; Compiler: TLapeCompilerBase);
begin
  Arr[ltUInt8] := TLapeType_UInt8.Create(Compiler, LapeTypeToString(ltUInt8));
  Arr[ltInt8] := TLapeType_Int8.Create(Compiler, LapeTypeToString(ltInt8));
  Arr[ltUInt16] := TLapeType_UInt16.Create(Compiler, LapeTypeToString(ltUInt16));
  Arr[ltInt16] := TLapeType_Int16.Create(Compiler, LapeTypeToString(ltInt16));
  Arr[ltUInt32] := TLapeType_UInt32.Create(Compiler, LapeTypeToString(ltUInt32));
  Arr[ltInt32] := TLapeType_Int32.Create(Compiler, LapeTypeToString(ltInt32));
  Arr[ltUInt64] := TLapeType_UInt64.Create(Compiler, LapeTypeToString(ltUInt64));
  Arr[ltInt64] := TLapeType_Int64.Create(Compiler, LapeTypeToString(ltInt64));
  Arr[ltSingle] := TLapeType_Single.Create(Compiler, LapeTypeToString(ltSingle));
  Arr[ltDouble] := TLapeType_Double.Create(Compiler, LapeTypeToString(ltDouble));
  Arr[ltCurrency] := TLapeType_Currency.Create(Compiler, LapeTypeToString(ltCurrency));
  Arr[ltExtended] := TLapeType_Extended.Create(Compiler, LapeTypeToString(ltExtended));
  Arr[ltBoolean] := TLapeType_Boolean.Create(Compiler, LapeTypeToString(ltBoolean));
  Arr[ltByteBool] := TLapeType_ByteBool.Create(Compiler, LapeTypeToString(ltByteBool));
  Arr[ltWordBool] := TLapeType_WordBool.Create(Compiler, LapeTypeToString(ltWordBool));
  Arr[ltLongBool] := TLapeType_LongBool.Create(Compiler, LapeTypeToString(ltLongBool));
  Arr[ltAnsiChar] := TLapeType_AnsiChar.Create(Compiler, LapeTypeToString(ltAnsiChar));
  Arr[ltWideChar] := TLapeType_WideChar.Create(Compiler, LapeTypeToString(ltWideChar));
  Arr[ltShortString] := TLapeType_ShortString.Create(Compiler, High(UInt8), LapeTypeToString(ltShortString));
  Arr[ltAnsiString] := TLapeType_AnsiString.Create(Compiler, LapeTypeToString(ltAnsiString));
  Arr[ltWideString] := TLapeType_WideString.Create(Compiler, LapeTypeToString(ltWideString));
  Arr[ltUnicodeString] := TLapeType_UnicodeString.Create(Compiler, LapeTypeToString(ltUnicodeString));
  Arr[ltVariant] := TLapeType_Variant.Create(Compiler, LapeTypeToString(ltVariant));
  Arr[ltPointer] := TLapeType_Pointer.Create(Compiler, nil, False, LapeTypeToString(ltPointer));
end;

function MethodOfObject(VarType: TLapeType): Boolean;
begin
  Result := (VarType is TLapeType_MethodOfObject) or
           ((VarType is TLapeType_OverloadedMethod) and
            (TLapeType_OverloadedMethod(VarType).MethodsOfObject <> bFalse));
end;

function ValidFieldName(Field: TLapeGlobalVar): Boolean; overload;
begin
  Result := (Field <> nil) and Field.isConstant and (Field.BaseType = ltString) and (Field.Ptr <> nil);
end;

function ValidFieldName(Field: TResVar): Boolean; overload;
begin
  Result := (Field.VarPos.MemPos = mpMem) and Field.isConstant and Field.HasType() and (Field.VarType.BaseType = ltString) and (Field.VarPos.GlobalVar.Ptr <> nil);
end;

function TResVar.getReadable: Boolean;
begin
  if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    Result := VarPos.GlobalVar.Readable
  else if (VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) then
    Result := VarPos.StackVar.Readable
  else if (VarPos.MemPos = mpStack) then
    Result := not VarPos.ForceVariable
  else
    Result := True;
end;

function TResVar.getWriteable: Boolean;
begin
  Result := ((VarPos.MemPos = mpStack) and (VarPos.isPointer or VarPos.ForceVariable)) or
    ((VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) and VarPos.GlobalVar.Writeable) or
    ((VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) and VarPos.StackVar.Writeable);
end;

function TResVar.getConstant: Boolean;
begin
  Result := Readable and (not Writeable);
end;

class function TResVar.New(AVar: TLapeVar): TResVar;
begin
  Result := NullResVar;
  if (AVar <> nil) then
  begin
    Result.VarType := AVar.VarType;
    if (AVar is TLapeStackVar) then
    begin
      Result.VarPos.MemPos := mpVar;
      Result.VarPos.StackVar := AVar as TLapeStackVar;
    end
    else if (AVar is TLapeGlobalVar) then
    begin
      Result.VarPos.MemPos := mpMem;
      Result.VarPos.GlobalVar := AVar as TLapeGlobalVar;
    end;
    if (AVar is TLapeParameterVar) then
      Result.VarPos.isPointer := (TLapeParameterVar(AVar).ParType in Lape_RefParams);
  end;
end;

function TResVar.HasType: Boolean;
begin
  Result := VarType <> nil;
end;

procedure TResVar.Spill(Unlock: Integer = 0);
begin
  DecLock(Unlock);
  Self := NullResVar;
end;

function TResVar.IncLock(Count: Integer = 1): TResVar;
begin
  Result := Self;
  if (Count > 0) and (VarPos.MemPos = mpVar) and
     (VarPos.StackVar <> nil) and (VarPos.StackVar is TLapeStackTempVar)
  then
    TLapeStackTempVar(VarPos.StackVar).IncLock(Count);
end;

function TResVar.DecLock(Count: Integer = 1): TResVar;
begin
  Result := Self;
  if (Count > 0) and (VarPos.MemPos = mpVar) and
     (VarPos.StackVar <> nil) and (VarPos.StackVar is TLapeStackTempVar)
  then
    TLapeStackTempVar(VarPos.StackVar).DecLock(Count);
end;

function TResVar.InScope(AStack: TLapeStackInfo; Pos: PDocPos = nil): TResVar;
begin
  Result := Self;
  if (VarPos.MemPos = mpVar) and (AStack <> nil) and (VarPos.StackVar.Stack <> AStack.VarStack) then
  begin
    Result.VarPos.StackVar := AStack.getInheritedVar(VarPos.StackVar);
    Result.VarPos.isPointer := True;
    if (Result.VarPos.StackVar = nil) then
      if (Pos <> nil) then
        LapeException(lpeParentOutOfScope, Pos^)
      else
        LapeException(lpeParentOutOfScope, VarPos.StackVar.DocPos);
  end;
end;

procedure TResVar.IncOffset(Offset: Integer);
begin
  if VarPos.isPointer or ((VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil)) then
    Inc(VarPos.Offset, Offset)
  else if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    Inc(PtrInt(VarPos.GlobalVar.FPtr), Offset)
  else if (VarPos.MemPos = mpStack) then
    Dec(VarPos.Offset, Offset);
end;

procedure TResVar.DecOffset(Offset: Integer);
begin
  IncOffset(-Offset);
end;

procedure TResVar.setOffset(Offset: Integer);
begin
  VarPos.Offset := 0;
  IncOffset(Offset);
end;

procedure TResVar.setReadable(AReadable: Boolean; ChangeStack: Boolean);
begin
  if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    VarPos.GlobalVar.Readable := AReadable
  else if (VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) then
    VarPos.StackVar.Readable := AReadable
  else if ChangeStack and (VarPos.MemPos = mpStack) then
    VarPos.ForceVariable := not AReadable;
end;

procedure TResVar.setReadable(AReadable: Boolean);
begin
  setReadable(AReadable, True);
end;

procedure TResVar.setWriteable(AWriteable: Boolean; ChangeStack: Boolean);
begin
  if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    VarPos.GlobalVar.Writeable := AWriteable
  else if (VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) then
    VarPos.StackVar.Writeable := AWriteable
  else if ChangeStack and (VarPos.MemPos = mpStack) then
    VarPos.ForceVariable := AWriteable;
end;

procedure TResVar.setWriteable(AWriteable: Boolean);
begin
  setWriteable(AWriteable, True);
end;

procedure TResVar.setConstant(AConst: Boolean; ChangeStack: Boolean);
begin
  setReadable(AConst, ChangeStack);
  setWriteable(not AConst, ChangeStack);
end;

procedure TResVar.setConstant(AConst: Boolean);
begin
  setConstant(AConst, True);
end;

procedure TResVar.setReadWrite(AReadable, AWriteable: Boolean);
begin
  Readable := AReadable;
  Writeable := AWriteable;
end;

procedure TResVar.CopyFlags(Other: TResVar);
begin
  Readable := Other.Readable;
  Writeable := Other.Writeable;
end;

function TLapeVar.getBaseType: ELapeBaseType;
begin
  if HasType() then
    Result := VarType.BaseType
  else
    Result := ltUnknown;
end;

function TLapeVar.getSize: SizeInt;
begin
  if HasType() then
    Result := VarType.Size
  else
    Result := -1;
end;

function TLapeVar.getLo: TLapeGlobalVar;
begin
  if HasType() then
    Result := VarType.VarLo(nil)
  else
    Result := nil;
end;

function TLapeVar.getHi: TLapeGlobalVar;
begin
  if HasType() then
    Result := VarType.VarHi(nil)
  else
    Result := nil;
end;

function TLapeVar.getInitialization: Boolean;
begin
  if HasType() then
    Result := VarType.NeedInitialization
  else
    Result := False;
end;

function TLapeVar.getFinalization: Boolean;
begin
  if HasType() then
    Result := VarType.NeedFinalization
  else
    Result := False;
end;

function TLapeVar.getReadable: Boolean;
begin
  Result := lvfReadable in FVarFlags;
end;

procedure TLapeVar.setReadable(AReadable: Boolean);
begin
  if AReadable then
    Include(FVarFlags, lvfReadable)
  else
    Exclude(FVarFlags, lvfReadable);
end;

function TLapeVar.getWriteable: Boolean;
begin
  Result := lvfWriteable in FVarFlags;
end;

procedure TLapeVar.setWriteable(AWriteable: Boolean);
begin
  if AWriteable then
    Include(FVarFlags, lvfWriteable)
  else
    Exclude(FVarFlags, lvfWriteable);
end;

function TLapeVar.getConstant: Boolean;
begin
  Result := Readable and not Writeable;
end;

procedure TLapeVar.setConstant(AConst: Boolean);
begin
  Readable := AConst;
  Writeable := not AConst;
end;

constructor TLapeVar.Create(AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AName, ADocPos, AList);

  isConstant := (AName = '');
  VarType := AVarType;
end;

function TLapeVar.HasType: Boolean;
begin
  Result := VarType <> nil;
end;

procedure TLapeVar.setReadWrite(AReadable, AWriteable: Boolean);
begin
  Readable := AReadable;
  Writeable := AWriteable;
end;

procedure TLapeVar.CopyFlags(Other: TLapeVar);
begin
  if (Other = nil) then
    Exit;

  Readable  := Other.Readable;
  Writeable := Other.Writeable;
end;

procedure TLapeStackVar.setStack(Stack: TLapeVarStack);
begin
  if (Stack <> FStack) then
  begin
    if (FStack <> nil) then
      FStack.DeleteItem(Self);
    FStack := Stack;
    if (FStack <> nil) then
      FStack.Add(Self);
  end;
end;

function TLapeStackVar.getOffset: Integer;
var
  i: Integer;
begin
  Result := 0;
  if (FStack = nil) then
    Exit;
  for i := 0 to FStack.Count - 1 do
    if (FStack[i] = Self) then
      Exit
    else
    begin
      Assert(FStack[i].Size > 0);
      Result := Result + FStack[i].Size;
    end;
  Result := -1;
end;

constructor TLapeStackVar.Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AName, ADocPos, AList);
  setStack(AStack);
end;

destructor TLapeStackVar.Destroy;
begin
  setStack(nil);
  inherited;
end;

constructor TLapeStackTempVar.Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited;
  FLock := 0;
  isConstant := False;
end;

function TLapeStackTempVar.getLocked: Boolean;
begin
  Result := (FLock > 0);
end;

procedure TLapeStackTempVar.setLocked(DoLock: Boolean);
begin
  if DoLock then
    FLock := 1
  else
    FLock := 0;
end;

function TLapeStackTempVar.IncLock(Count: Integer = 1): Integer;
begin
  Inc(FLock, Count);
  Result := FLock;
end;

function TLapeStackTempVar.DecLock(Count: Integer = 1): Integer;
begin
  Dec(FLock, Count);
  if (FLock < 0) then
    FLock := 0;
  Result := FLock;
end;

function TLapeParameterVar.getSize: SizeInt;
begin
  if (FParType in Lape_RefParams) then
    Result := SizeOf(Pointer)
  else
    Result := inherited;
end;

function TLapeParameterVar.getInitialization: Boolean;
begin
  Result := False;
end;

function TLapeParameterVar.getFinalization: Boolean;
begin
  Result := (not (FParType in Lape_RefParams)) and inherited;
end;

constructor TLapeParameterVar.Create(AParType: ELapeParameterType; AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AStack, AName, ADocPos, AList);
  FParType := AParType;
  isConstant := (FParType in Lape_ConstParams);
end;

constructor TLapeStackInheritedVar.Create(AParent: TLapeStackVar; AStack: TLapeVarStack; AList: TLapeDeclarationList = nil);
begin
  Assert(AParent <> nil);
  Assert(not (AParent is TLapeStackTempVar));

  if (AParent is TLapeStackInheritedVar) then
  begin
    Create(TLapeStackInheritedVar(AParent).Parent, AStack, AList);
    FName := '!' + FName;
    Exit;
  end
  else if AParent.isConstant then
    inherited Create(lptConstRef, AParent.VarType, AStack, '!' + AParent.Name, @AParent._DocPos, AList)
  else
    inherited Create(lptVar, AParent.VarType, AStack, '!' + AParent.Name, @AParent._DocPos, AList);

  FParent := AParent;
end;

function TLapeGlobalVar.getAsString: lpString;
begin
  if (Ptr <> nil) and HasType() then
    Result := VarType.VarToString(FPtr)
  else
    Result := Name;
end;

function TLapeGlobalVar.getAsInt: Int64;
begin
  if (FPtr <> nil) and HasType() then
    Result := VarType.VarToInt(FPtr)
  else
    Result := -1;
end;

constructor TLapeGlobalVar.Create(AVarType: TLapeType; Initialize: Boolean = True; ManagePtr: Boolean = True; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AName, ADocPos, AList);

  FBasePtr := nil;
  FPtr := nil;
  DoManage := ManagePtr;

  if Initialize and (Size > 0) then
  begin
    {$IFDEF Lape_SmallCode}
    //FPtr := AllocMem(Size);
    FBasePtr := AllocMem(Size + 4);
    FPtr := {$IFDEF FPC}Align(FBasePtr, 4){$ELSE}Pointer((PtrUInt(FBasePtr) + 3) and not 3){$ENDIF};
    {$ELSE}
    //Assure aligned memory
    FBasePtr := AllocMem(Size + 16);
    FPtr := {$IFDEF FPC}Align(FBasePtr, 16){$ELSE}Pointer((PtrUInt(FBasePtr) + 15) and not 15){$ENDIF};
    {$ENDIF}
  end;
end;

constructor TLapeGlobalVar.Create(AVarType: TLapeType; Ptr: Pointer; ManagePtr: Boolean = False; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil);
begin
  Create(AVarType, False, ManagePtr, AName, ADocPos, AList);
  FPtr := Ptr;
end;

destructor TLapeGlobalVar.Destroy;
begin
  if (FPtr <> nil) and DoManage then
  begin
    if HasType() then
      VarType.Finalize(Self, False);

    if (FBasePtr <> nil) then
      FreeMem(FBaseptr)
    else
      FreeMem(FPtr);
  end;

  inherited;
end;

function TLapeGlobalVar.CreateCopy(CopyContent: Boolean = True): TLapeGlobalVar;
type
  TLapeClassType = class of TLapeGlobalVar;
begin
  if (not CopyContent) then
    Result := TLapeClassType(Self.ClassType).Create(VarType, FPtr, False, Name, @_DocPos)
  else
  begin
    Result := TLapeClassType(Self.ClassType).Create(VarType, True, True, Name, @_DocPos);
    if HasType() then
    begin
      Result.Writeable := True;
      VarType.EvalConst(op_Assign, Result, Self, []);
    end;
  end;
  Result.CopyFlags(Self);
end;

function TLapeGlobalVar.Equals(Other: TLapeGlobalVar): Boolean;
var
  Res: TLapeGlobalVar;
begin
  if (Other <> nil) and HasType() then
    Res := VarType.EvalConst(op_cmp_Equal, Self, Other, [])
  else
    Exit(False);

  try
    Result := (Res.BaseType in LapeBoolTypes) and (Res.AsInteger <> 0);
  finally
    Res.Free();
  end;
end;

function TLapeGlobalVar.isNull: Boolean;
var
  i: Integer;
  Int: Int64;
  Item: PByteArray;
begin
  Int := AsInteger;
  Result := (Int = 0);

  if (not Result) and (Int = -1) and HasType() then
  begin
    Item := Ptr;
    for i := 0 to Size do
      if (Item^[i] <> 0) then
        Exit(False);

    Result := True;
  end;
end;

function TLapeGlobalVar.CompatibleWith(Other: TLapeGlobalVar): Boolean;
begin
  Result := (Other <> nil) and HasType() and VarType.CompatibleWith(Other.VarType);
end;

function TLapeType.getEvalRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType;
begin
  Result := lpeval.getEvalRes(Op, Left, Right);
end;

function TLapeType.getEvalProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc;
begin
  Result := lpeval.getEvalProc(Op, Left, Right);
end;

function TLapeType.getPadding: SizeInt;
begin
  Result := 0;
end;

function TLapeType.getSize: SizeInt;
begin
  if (FSize = 0) then
  begin
    FSize := LapeTypeSize[FBaseType];
    if (FSize = 0) then
      FSize := -1;
  end;
  Result := FSize;
end;

procedure TLapeType.setBaseType(ABaseType: ELapeBaseType);
begin
  Assert(FBaseType = ltUnknown);
  FBaseType := ABaseType;
end;

function TLapeType.getBaseIntType: ELapeBaseType;
begin
  if (not (FBaseType in LapeOrdinalTypes{ + [ltPointer]})) then
    Result := ltUnknown
  else if (FBaseType in LapeIntegerTypes) then
    Result := FBaseType
  else
    Result := DetermineIntType(Size, False);
end;

function TLapeType.getInitialization: Boolean;
begin
  if (FInit = bUnknown) then
    if (FBaseType in LapeNoInitTypes) then
      FInit := bFalse
    else
      FInit := bTrue;
  Result := (FInit = bTrue) and (Size > 0);
end;

function TLapeType.getFinalization: Boolean;
begin
  Result := (not (FBaseType in LapeSetTypes)) and NeedInitialization;
end;

function TLapeType.getAsString: lpString;
begin
  if (FAsString = '') then
    FAsString := LapeTypeToString(BaseType);
  Result := FAsString;
end;

constructor TLapeType.Create(ABaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(AName, ADocPos);
  TypeID := TypeID_Unknown;

  FBaseType := ABaseType;
  FCompiler := ACompiler;
  FStatic := False;
  ClearCache();
end;

procedure TLapeType.ClearCache;
begin
  FSize := 0;
  FLo := nil;
  FHi := nil;
  FAsString := '';
end;

function TLapeType.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
begin
  Result := '';
end;

function TLapeType.VarToString(AVar: Pointer): lpString;
begin
  if (AVar <> nil) and ({$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType] <> nil) then
    LapeToStrArr[BaseType](@AVar, @Result)
  else
    Result := AsString + ' (' + PointerToString(AVar) + ')';
end;

function TLapeType.VarToInt(AVar: Pointer): Int64;
begin
  if (AVar = nil) then
    Result := 0
  else
    case BaseIntType of
      ltInt8: Result := PInt8(AVar)^;
      ltUInt8: Result := PUInt8(AVar)^;
      ltInt16: Result := PInt16(AVar)^;
      ltUInt16: Result := PUInt16(AVar)^;
      ltInt32: Result := PInt32(AVar)^;
      ltUInt32: Result := PUInt32(AVar)^;
      ltInt64: Result := PInt64(AVar)^;
      ltUInt64: UInt64(Result) := PUInt64(AVar)^;
      else Result := -1;
    end;
end;

function TLapeType.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FLo = nil) and (FCompiler <> nil) and (BaseIntType <> ltUnknown) then
    with FCompiler, getBaseType(BaseIntType) do
      Self.FLo := addManagedVar(NewGlobalVarP(LapeTypeLow[BaseType])) as TLapeGlobalVar;
  Result := FLo;
end;

function TLapeType.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FHi = nil) and (FCompiler <> nil) and (BaseIntType <> ltUnknown) then
    with FCompiler, getBaseType(BaseIntType) do
      Self.FHi := addManagedVar(NewGlobalVarP(LapeTypeHigh[BaseType])) as TLapeGlobalVar;
  Result := FHi;
end;

function TLapeType.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result := (Other = Self) or (
    (Other <> nil) and
    (TypeID = Other.TypeID) and
    (ContextOnly or (ClassType = Other.ClassType)) and
    (Other.BaseType = BaseType) and
    (Other.Size = Size) and
    (Other.AsString = AsString)
  );
end;

function TLapeType.CompatibleWith(Other: TLapeType): Boolean;
begin
  Result := (EvalRes(op_Assign, Other) <> nil);
end;

function TLapeType.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType;
begin
  Result := TLapeClassType(Self.ClassType).Create(FBaseType, FCompiler, Name, @_DocPos);
  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;
end;

function TLapeType.NewGlobalVarP(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Ptr = nil) then
    Result := TLapeGlobalVar.Create(Self, True, True, AName, ADocPos)
  else
    Result := TLapeGlobalVar.Create(Self, Ptr, False, AName, ADocPos);
end;

function TLapeType.NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(UnicodeString(Str), AName, ADocPos);
end;

function TLapeType.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeImpossible);
end;

function TLapeType.addSubDeclaration(ADecl: TLapeDeclaration): Integer;
begin
  if (ADecl.Name <> '') then
    if (not (ADecl is TLapeGlobalVar))  then
      LapeException(lpeImpossible)
    else if HasChild(ADecl.Name) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [ADecl.Name]);
  Result := inherited;
end;

function TLapeType.CanHaveChild: Boolean;
begin
  Result := (FBaseType in LapeStructTypes) or (FManagedDecls.ItemCount > 0);
end;

function TLapeType.IsOrdinal(OrPointer: Boolean = False): Boolean;
begin
  Result := (BaseIntType <> ltUnknown) or (OrPointer and (BaseType = ltPointer));
end;

function TLapeType.HasChild(AName: lpString): Boolean;
var
  DotName: TLapeGlobalVar;
begin
  if (not CanHaveChild()) or (FCompiler = nil) then
    Exit(False);

  Result := HasSubDeclaration(AName, bTrue);
  if (not Result) then
  begin
    DotName := FCompiler.getBaseType(ltString).NewGlobalVarStr(AName);
    try
      Result := EvalRes(op_Dot, DotName) <> nil;
    finally
      DotName.Free();
    end;
  end;
end;

function TLapeType.HasChild(ADecl: TLapeDeclaration): Boolean;
begin
  Result := HasSubDeclaration(ADecl, bTrue);
end;

function TLapeType.HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean;
var
  Decls: TLapeDeclArray;
begin
  Decls := FManagedDecls.getByClassAndName(AName, TLapeGlobalVar, bTrue);
  if (Length(Decls) <= 0) then
    Result := HasChild(AName)
  else with TLapeGlobalVar(Decls[0]) do
    Result := Readable and (BaseType = ltImportedMethod);
end;

function TLapeType.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Assert(FCompiler <> nil);

  if (op in CompoundOperators) and (lcoCOperators in Compiler.FOptions) then
    Exit(EvalRes(op_Assign, EvalRes(ResolveCompoundOp(op, Self), Right, flags), flags));

  if (Op = op_Addr) then
    Result := FCompiler.getPointerType(Self, False)
  else if (Op in AssignOperators) and (Right <> nil) and (getEvalRes(Op, FBaseType, Right.BaseType) <> ltUnknown) then
    Result := Self
  else if (Right = nil) then
    Result := FCompiler.getBaseType(getEvalRes(Op, FBaseType, ltUnknown))
  else
    Result := FCompiler.getBaseType(getEvalRes(Op, FBaseType, Right.BaseType));

  if (not (op in UnaryOperators + [op_Dot, op_Assign])) and
     (Result = nil) and
     (Right <> nil) and
     (not Equals(Right, False))
  then
  begin
    if Right.CompatibleWith(Self) then
      Result := Right.EvalRes(Op, Right, Flags);

    if ((Result = nil) or (Size >= Right.Size)) and
        CompatibleWith(Right) and
        (EvalRes(Op, Self, Flags) <> nil)
    then
      Result := EvalRes(Op, Self, Flags);
  end;
end;

function TLapeType.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType;
var
  d: TLapeDeclArray;
begin
  if (Right = nil) then
    Result := EvalRes(Op, TLapeType(nil), Flags)
  else
  begin
    Result := EvalRes(Op, Right.VarType, Flags);

    if (Result = nil) and (op = op_Dot) and CanHaveChild() and ValidFieldName(Right) then
    begin
      d := FManagedDecls.getByName(PlpString(Right.Ptr)^, bTrue);
      if (Length(d) = 1) then
        if (d[0] is TLapeType) then
          Result := TLapeType(d[0])
        else if (d[0] is TLapeVar) then
          Result := TLapeVar(d[0]).VarType;
    end;
  end;

  if (not (op in UnaryOperators + [op_Dot, op_Assign])) and
     (Result = nil) and
     (Right <> nil) and
     Right.HasType()
  then
    Result := EvalRes(Op, Right.VarType, Flags);
end;

function TLapeType.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  Assert((Left = nil) or (Left.VarType = Self));

  Result := (op <> op_dot) and ((Left = nil) or Left.Readable) and ((Right = nil) or Right.Readable);
  if (not Result) and (Right <> nil) and Right.Readable then
    if (op = op_Dot) and CanHaveChild() and ValidFieldName(Right) then
      Result := HasConstantChild(Left, PlpString(Right.Ptr)^)
    else if (op = op_Index) and (BaseType in [ltUnknown{overloaded method}, ltShortString, ltStaticArray]) then
      Result := Right.HasType() and Right.VarType.IsOrdinal();
end;

function TLapeType.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;

  function TryCast(DoRight: Boolean; out Res: TLapeGlobalVar): Boolean;
  var
    CastVar: TLapeGlobalVar;
  begin
    if ((not Left.HasType()) and (not Right.HasType())) or
       (Left.HasType() and Left.VarType.Equals(Right.VarType, False)) or
       (DoRight       and ((not Left.HasType())  or (Right.HasType() and (not Left.VarType.CompatibleWith(Right.VarType))))) or
       ((not DoRight) and ((not Right.HasType()) or (Left.HasType()  and (not Right.VarType.CompatibleWith(Left.VarType)))))
    then
      Exit(False);

    try
      if ((not Left.HasType()) or (not Right.HasType())) then
        if DoRight then
          CastVar := Left.VarType.NewGlobalVarP(Right.Ptr)
        else
          CastVar := Right.VarType.NewGlobalVarP(Left.Ptr)
      else
        if DoRight then
          CastVar := Left.VarType.EvalConst(op_Assign, Left.VarType.NewGlobalVarP(), Right, [])
        else
          CastVar := Right.VarType.EvalConst(op_Assign, Right.VarType.NewGlobalVarP(), Left, []);

      try
        if DoRight then
          Res := CastVar.VarType.EvalConst(op, Left, CastVar, Flags)
        else
          Res := CastVar.VarType.EvalConst(op, CastVar, Right, Flags);
        Result := True;
      finally
        CastVar.Free();
      end;
    except
      Result := False;
    end;
  end;

  function EvalDot(Field: lpString): TLapeGlobalVar;
  var
    d: TLapeDeclArray;
    Res: TLapeGlobalVar;
  begin
    if (Left = nil) or (not HasSubDeclaration(Field, bTrue)) then
      LapeExceptionFmt(lpeUnknownDeclaration, [Field]);

    d := FManagedDecls.getByName(Field, bTrue);
    Assert(Length(d) = 1);

    if (d[0] is TLapeType) then
      d[0] := FCompiler.getTypeVar(TLapeType(d[0]));
    Result := d[0] as TLapeGlobalVar;

    if (Result <> nil) and MethodOfObject(Result.VarType) and (not Result.Readable) and (not Result.Writeable) then
    begin
      if (not (lefInvoking in Flags)) and (not Left.Writeable) then
        if InheritsFrom(TLapeType_Type) then
          Exit()
        else
          LapeException(lpeVariableExpected);

      Res := FCompiler.getGlobalType('TMethod').NewGlobalVarP(nil);
      Res.VarType := Result.VarType;

      with TMethod(Res.Ptr^) do
      begin
        if (Result.Ptr <> nil) then
          Code := PPointer(Result.Ptr)^;
        Data := Left.Ptr;
      end;

      Result := Res;
      Result.Readable := Left.Writeable;
    end;
  end;

var
  EvalProc: TLapeEvalProc;
  ResType: TLapeType;
begin
  Result := nil;
  Assert(FCompiler <> nil);

  if (Left = nil) then
  begin
    if (Right = nil) then
      LapeExceptionFmt(lpeIncompatibleOperator, [LapeOperatorToString(op)]);
    //Left := Right;
    //Right := nil;
  end;
  if (Op = op_UnaryPlus) then
    Exit(Left)
  else if (op in CompoundOperators) and (lcoCOperators in Compiler.FOptions) then
    Exit(EvalConst(op_Assign, Left, EvalConst(ResolveCompoundOp(op, Left.VarType), Left, Right, Flags), Flags));

  try
    if (Right = nil) then
    begin
      ResType := EvalRes(Op, TLapeType(nil), Flags);
      if (op = op_Addr) and (not Left.Writeable) then
        if (lefConstAddress in Flags) and (Left.Name <> '') then
          ResType := FCompiler.getPointerType(Self, True)
        else
          LapeException(lpeVariableExpected);

      if (ResType <> nil) or ((op = op_Deref) and ((not Left.HasType()) or (Left.BaseType = ltPointer))) then
        if (op = op_Addr) then
          Exit(TLapeGlobalVar.Create(ResType, @Left.Ptr))
        else if (op = op_Deref) then
          Exit(TLapeGlobalVar.Create(ResType, PPointer(Left.Ptr)^));

      EvalProc := getEvalProc(Op, FBaseType, ltUnknown);
    end
    else if (op <> op_Assign) or CompatibleWith(Right.VarType) then
    begin
      EvalProc := getEvalProc(Op, FBaseType, Right.BaseType);
      ResType := EvalRes(Op, Right, Flags);
    end
    else
      EvalProc := nil;

    if (ResType = nil) or (not ValidEvalFunction(EvalProc)) then
      if (op = op_Dot) and ValidFieldName(Right) then
        Exit(EvalDot(PlpString(Right.Ptr)^))
      else if (op = op_Assign) and (Right <> nil) and Right.HasType() then
        LapeExceptionFmt(lpeIncompatibleAssignment, [Right.VarType.AsString, AsString])
      else if (not (op in UnaryOperators)) and (Right <> nil) and ((not Left.HasType()) or (not Right.HasType()) or (not Left.VarType.Equals(Right.VarType, False))) then
        if (Left.HasType()       and     Right.HasType()  and Left.VarType.Equals(Right.VarType, False)) or
          (((not Left.HasType()) or (not Right.HasType()) or  (Left.VarType.Size >= Right.VarType.Size)) and (not TryCast(True, Result)) and (not TryCast(False, Result))) or
          ((Left.HasType()       and     Right.HasType()) and (Left.VarType.Size <  Right.VarType.Size)  and (not TryCast(False, Result)) and (not TryCast(True, Result)))
        then
          if Right.HasType() then
            LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op), AsString, Right.VarType.AsString ])
          else
            LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op), AsString, LapeTypeToString(ltUnknown)])
        else
          Exit
      else if (op in UnaryOperators) or (Left.HasType() and (Right <> nil) and Left.VarType.Equals(Right.VarType, False)) then
        LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), AsString])
      else
        LapeExceptionFmt(lpeIncompatibleOperator, [LapeOperatorToString(op)]);

    if (Op in AssignOperators) then
    begin
      if (Right = nil) then
        LapeException(lpeInvalidAssignment);
      Result := Left;
      EvalProc(Result.Ptr, Right.Ptr, nil);
    end
    else
    begin
      Result := ResType.NewGlobalVarP();
      if (Right = nil) then
        EvalProc(Result.Ptr, Left.Ptr, nil)
      else
        EvalProc(Result.Ptr, Left.Ptr, Right.Ptr);
    end;
  finally
    if (not (op in AssignOperators + [op_Dot])) and (Result <> nil) and (Left <> nil) then
    begin
      Result.Readable := Left.Readable and ((Right = nil) or Right.Readable);
      Result.Writeable := False;
    end;
  end;
end;

function TLapeType.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;

  function TryCast(DoRight: Boolean; out Res: TResVar): Boolean;
  var
    tmpVar, CastVar: TResVar;
  begin
    if ((not Left.HasType()) and (not Right.HasType())) or
       (Left.HasType() and Left.VarType.Equals(Right.VarType, False)) or
       (DoRight       and ((Left.VarType  = nil) or (Right.HasType() and (not Left.VarType.CompatibleWith(Right.VarType))))) or
       ((not DoRight) and ((not Right.HasType()) or (Left.HasType()  and (not Right.VarType.CompatibleWith(Left.VarType)))))
    then
      Exit(False);

    if ((not Left.HasType()) or (not Right.HasType())) then
    try
      if (     DoRight  and (Right.VarPos.MemPos = NullResVar.VarPos.MemPos)) or
         ((not DoRight) and (Left.VarPos.MemPos  = NullResVar.VarPos.MemPos))
      then
        Exit(False);

      if DoRight then
        Right.VarType := Left.VarType
      else
        Left.VarType := Right.VarType;
      Res := Left.VarType.Eval(op, Dest, Left, Right, Flags, Offset, Pos);
      Exit(True);
    except
      Result := False;
      Exit;
    end;

    tmpVar := NullResVar;
    CastVar := NullResVar;
    CastVar.VarPos.MemPos := mpVar;
    if DoRight then
      CastVar.VarType := Left.VarType
    else
      CastVar.VarType := Right.VarType;

    try
      CastVar.VarPos.StackVar := FCompiler.getTempVar(CastVar.VarType, 1);
      try
        if DoRight then
          Res := Left.VarType.Eval(op, Dest, Left, CastVar.VarType.Eval(op_Assign, tmpVar, CastVar, Right, [], Offset, Pos), Flags, Offset, Pos)
        else
          Res := CastVar.VarType.Eval(op, Dest, CastVar.VarType.Eval(op_Assign, tmpVar, CastVar, Left, [], Offset, Pos), Right, Flags, Offset, Pos);

        Result := True;
      finally
        CastVar.Spill(1);
      end;
    except
      Result := False;
    end;
  end;

  function EvalDot(Field: lpString): TResVar;
  var
    d: TLapeDeclArray;
    Res: TResVar;
  begin
    if (not HasSubDeclaration(Field, bTrue)) then
      LapeExceptionFmt(lpeUnknownDeclaration, [Field]);

    d := FManagedDecls.getByName(Field, bTrue);
    Assert(Length(d) = 1);

    if (d[0] is TLapeType) then
      d[0] := FCompiler.getTypeVar(TLapeType(d[0]));

    Dest := NullResVar;
    Result := _ResVar.New(d[0] as TLapeGlobalVar);

    if MethodOfObject(Result.VarType) and (not Result.Readable) and (not Result.Writeable) then
    begin
      Res := _ResVar.New(FCompiler.getTempVar(FCompiler.getGlobalType('TMethod')));
      Res.VarType := Result.VarType;

      if (not (lefInvoking in Flags)) and (not Left.Writeable) then
        LapeException(lpeVariableExpected);

      FCompiler.Emitter._Eval(getEvalProc(op_Dot, ltUnknown, ltPointer), Res, Result, Left.IncLock(), Offset, Pos);
      Result := Res;
      Result.Readable := Left.Writeable;
    end;
  end;

var
  EvalProc: TLapeEvalProc;
begin
  Result := NullResVar;
  Assert(FCompiler <> nil);

  if (Left.VarPos.MemPos = NullResVar.VarPos.MemPos) then
  begin
    if (Right.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      LapeExceptionFmt(lpeIncompatibleOperator, [LapeOperatorToString(op)]);
    //Left := Right;
    //Right := NullResVar;
  end;
  if (op = op_UnaryPlus) then
  begin
    Dest := NullResVar;
    Exit(Left);
  end
  else if (op in CompoundOperators) and (lcoCOperators in Compiler.FOptions) then
  begin
    Dest := Left;
    Exit(Eval(op_Assign, Dest, Left, Eval(ResolveCompoundOp(op, Left.VarType), Dest, Left, Right, Flags, Offset, Pos), Flags, Offset, Pos));
  end; 

  Result.VarType := EvalRes(Op, Right.VarType, Flags);
  if (op = op_Addr) and (not Left.Writeable) then
    if (lefConstAddress in Flags) and (
       ((Left.VarPos.MemPos = mpVar) and (not (Left.VarPos.StackVar is TLapeStackTempVar))) or
       ((Left.VarPos.MemPos = mpMem) and (Left.VarPos.GlobalVar.Name <> '')))
    then
      Result.VarType := FCompiler.getPointerType(Self, True)
    else
      LapeException(lpeVariableExpected)
  else if (not Result.HasType()) and (op = op_Deref) and ((not Left.HasType()) or (Left.VarType.BaseType = ltPointer)) then
    Result.VarType := TLapeType.Create(ltUnknown, FCompiler);

  try
    if (not Right.HasType()) then
      EvalProc := getEvalProc(Op, FBaseType, ltUnknown)
    else if (op <> op_Assign) or CompatibleWith(Right.VarType) then
      EvalProc := getEvalProc(Op, FBaseType, Right.VarType.BaseType)
    else
      EvalProc := nil;

    if (not Result.HasType()) or (not ValidEvalFunction(EvalProc)) then
      if (op = op_Dot) and ValidFieldName(Right) then
        Exit(EvalDot(PlpString(Right.VarPos.GlobalVar.Ptr)^))
      else if (op = op_Assign) and Right.HasType() then
        LapeExceptionFmt(lpeIncompatibleAssignment, [Right.VarType.AsString, AsString])
      else if (not (op in UnaryOperators)) and ((not Left.HasType()) or (not Right.HasType()) or (not Left.VarType.Equals(Right.VarType, False))) then
        if (Left.HasType() and Right.HasType() and Left.VarType.Equals(Right.VarType, False)) or
          (((not Left.HasType()) or (not Right.HasType()) or  (Left.VarType.Size >= Right.VarType.Size)) and (not TryCast(True, Result))  and (not TryCast(False, Result))) or
          ((     Left.HasType() and      Right.HasType()) and (Left.VarType.Size  < Right.VarType.Size)  and (not TryCast(False, Result)) and (not TryCast(True, Result)))
        then
          if Right.HasType() then
            LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op), AsString, Right.VarType.AsString ])
          else
            LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op), AsString, LapeTypeToString(ltUnknown)])
        else
          Exit
      else if (op in UnaryOperators) or (Left.HasType() and Left.VarType.Equals(Right.VarType, False)) then
        LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), AsString])
      else
        LapeExceptionFmt(lpeIncompatibleOperator, [LapeOperatorToString(op)]);

    FCompiler.getDestVar(Dest, Result, op);

    if (op in AssignOperators) then
    begin
      if (not Left.HasType()) or (not Right.HasType()) or (Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
        LapeException(lpeInvalidAssignment);

      FCompiler.Emitter._Eval(EvalProc, Left, Right, NullResVar, Offset, Pos);
      Result := Left;
    end
    else
      FCompiler.Emitter._Eval(EvalProc, Result, Left, Right, Offset, Pos);

    if (op = op_Deref) then
      Result.VarPos.isPointer := (Result.VarPos.MemPos = mpVar);
  finally
    if Result.HasType() and (Result.VarType.ClassType = TLapeType) and (Result.VarType.BaseType = ltUnknown) then
      FreeAndNil(Result.VarType);
  end;
end;

function TLapeType.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; Pos: PDocPos = nil): TResVar;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := Eval(op, Dest, Left, Right, Flags, Offset, Pos);
end;

procedure TLapeType.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);

  function FullNil(p: Pointer; Size: Integer): Boolean;
  var
    i: Integer;
  begin
    if (p = nil) then
      Exit(True);
    case Size of
      SizeOf(UInt8):  Result := PUInt8(p)^  = 0;
      SizeOf(UInt16): Result := PUInt16(p)^ = 0;
      SizeOf(UInt32): Result := PUInt32(p)^ = 0;
      SizeOf(UInt64): Result := PUInt64(p)^ = 0;
      else
      begin
        for i := 0 to Size - 1 do
          if (PByteArray(p)^[i] <> 0) then
            Exit(False);
        Result := True;
      end;
    end;
  end;

var
  EmptyVar, tmpVar: TResVar;
  wasConstant: Boolean;
begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) or (not NeedFinalization) then
    Exit;
  if (AVar.VarPos.MemPos = mpMem) and (AVar.VarPos.GlobalVar <> nil) and FullNil(AVar.VarPos.GlobalVar.Ptr, Size) then
    Exit;

  wasConstant := not AVar.Writeable;
  if wasConstant then
    AVar.Writeable := True;

  tmpVar := NullResVar;
  EmptyVar := NullResVar;
  EmptyVar.VarType := Self;
  EmptyVar.VarPos.MemPos := mpMem;
  if UseCompiler and (FCompiler <> nil) then
    EmptyVar.VarPos.GlobalVar := FCompiler.addManagedVar(NewGlobalVarP()) as TLapeGlobalVar
  else
    EmptyVar.VarPos.GlobalVar := NewGlobalVarP();

  try
    if UseCompiler and (FCompiler <> nil) then
      Eval(op_Assign, tmpVar, AVar, EmptyVar, [], Offset, Pos)
    else if (AVar.VarPos.MemPos = mpMem) and (AVar.VarPos.GlobalVar <> nil) then
      EvalConst(op_Assign, AVar.VarPos.GlobalVar, EmptyVar.VarPos.GlobalVar, []);
  finally
    if (not UseCompiler) or (FCompiler = nil) then
      FreeAndNil(EmptyVar.VarPos.GlobalVar);
    if wasConstant then
      AVar.Writeable := False;
  end;
end;

procedure TLapeType.Finalize(AVar: TResVar; UseCompiler: Boolean = True; Pos: PDocPos = nil);
var
  Offset: Integer;
begin
  Offset := -1;
  Finalize(AVar, Offset, UseCompiler, Pos);
end;

procedure TLapeType.Finalize(AVar: TLapeVar; var Offset: Integer; UseCompiler: Boolean = False; Pos: PDocPos = nil);
begin
  Finalize(_ResVar.New(AVar), Offset, UseCompiler, Pos);
end;

procedure TLapeType.Finalize(AVar: TLapeVar; UseCompiler: Boolean = False; Pos: PDocPos = nil);
var
  Offset: Integer;
begin
  Offset := -1;
  Finalize(AVar, Offset, UseCompiler, Pos);
end;

function TLapeType_Type.getAsString: lpString;
begin
  if (FTType <> nil) then
    Result := FTType.AsString
  else
    Result := '';
end;

constructor TLapeType_Type.Create(AType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUnknown, ACompiler, AName, ADocPos);

  FTType := AType;
  if (FTType <> nil) then
    inheritManagedDecls(FTType);
end;

function TLapeType_Type.CreateCopy(DeepCopy: Boolean = False): TLapeType;
begin
  Result := TLapeTTypeClass(Self.ClassType).Create(FTType, FCompiler, Name, @_DocPos);
  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;
end;

function TLapeType_Type.HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean;
begin
  Result := ((Left <> nil) and Left.isConstant and HasChild(AName)) or inherited;
end;

function TLapeType_Type.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
end;

function TLapeType_TypeEnum.CanHaveChild: Boolean;
begin
  Result := (FTType is TLapeType_Enum) or inherited;
end;

function TLapeType_TypeEnum.HasChild(AName: lpString): Boolean;
begin
  Result := HasConstantChild(nil, AName) or inherited;
end;

function TLapeType_TypeEnum.HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean;
begin
  Result := ((FTType is TLapeType_Enum) and TLapeType_Enum(FTType).hasMember(AName)) or inherited;
end;

function TLapeType_TypeEnum.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (Op = op_Dot) and (FTType <> nil) and ValidFieldName(Right) and
     (FTType is TLapeType_Enum) and TLapeType_Enum(FTType).hasMember(PlpString(Right.Ptr)^)
  then
    Result := FTType
  else
    Result := inherited EvalRes(Op, Right, Flags);
end;

function TLapeType_TypeEnum.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Dot) and ValidFieldName(Right) and (FTType is TLapeType_Enum) and TLapeType_Enum(FTType).hasMember(PlpString(Right.Ptr)^) then
    Result := FTType.NewGlobalVarStr(PlpString(Right.Ptr)^)
  else
    Result := inherited;
end;

function TLapeType_Pointer.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltPointer) then
    if HasType() then
      if FPConst then
        FAsString := '^const '+FPType.AsString
      else
        FAsString := '^'+FPType.AsString
    else if FPConst then
      FAsString := 'Const' + inherited;
  Result := inherited;
end;

constructor TLapeType_Pointer.Create(ACompiler: TLapeCompilerBase; PointerType: TLapeType = nil; ConstPointer: Boolean = True; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltPointer, ACompiler, AName, ADocPos);
  FPType := PointerType;
  FPConst := ConstPointer;
end;

function TLapeType_Pointer.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  if ContextOnly and (Other <> nil) and (Other.TypeID = TypeID) and (ClassType = Other.ClassType) and (Other.BaseType = BaseType) then
    Result := (not HasType()) or (not TLapeType_Pointer(Other).HasType()) or inherited
  else
    Result := inherited;
end;

function TLapeType_Pointer.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
begin
  Result := 'begin Result := System.ToString(Pointer(Param0));';
  if HasType() and (ToStr <> nil) and (ToStr.getMethod(getTypeArray([PType])) <> nil) then
  begin
    Result := Result                   +
      'if (Param0 <> nil) then '       +
      'try'                            +
      '  Result := Result + '#39' ('#39' + System.ToString(Param0^) + '#39')'#39';' +
      'except end;';
  end;
  Result := Result + 'end;';
end;

function TLapeType_Pointer.VarToString(AVar: Pointer): lpString;
begin
  Result := PointerToString(AVar);
  if (AVar <> nil) and HasType() then
  try
    Result := Result + '(' + FPType.VarToString(PPointer(AVar)^) + ')';
  except end;
end;

function TLapeType_Pointer.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_Pointer;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FPType, FPConst, Name, @_DocPos);
  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;
end;

function TLapeType_Pointer.NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil; AsValue: Boolean = True): TLapeGlobalVar;
begin
  if AsValue then
  begin
    Result := NewGlobalVarP(nil, AName, ADocPos);
    PPointer(Result.FPtr)^ := Ptr;
  end
  else if (Ptr = nil) then
  begin
    Result := NewGlobalVarP(Pointer(-1), AName, ADocPos);
    Result.FPtr := nil;
  end
  else
    Result := NewGlobalVarP(Ptr, AName, ADocPos);
end;

function TLapeType_Pointer.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (LapeCase(lpString(Str)) = 'nil') then
    Result := NewGlobalVar(nil, AName, ADocPos)
  else
    Result := NewGlobalVar(Pointer(StrToInt64(string(Str))), AName, ADocPos);
end;

function TLapeType_Pointer.HasType: Boolean;
begin
  Result := (FPType <> nil);
end;

function TLapeType_Pointer.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (op = op_Deref) then
    Result := FPType
  else if (op = op_Index) then
    Result := Self
  else if (op = op_Assign) and (Right <> nil) and (Right is TLapeType_Pointer) and (BaseType = Right.BaseType) then
    if ((not HasType()) or (not TLapeType_Pointer(Right).HasType()) or FPType.Equals(TLapeType_Pointer(Right).FPType)) and
       (FPConst or (not TLapeType_Pointer(Right).PConst))
    then
      Result := Self
    else
      Result := nil
  else
  begin
    Result := inherited;
    if (Result <> nil) and (Result <> Self) and (Result.BaseType = ltPointer) and (not TLapeType_Pointer(Result).HasType()) and (FCompiler <> nil) then
      Result := FCompiler.getPointerType(FPType, PConst);
  end;
end;

function TLapeType_Pointer.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  tmpType: TLapeType;
  TypeSize, IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Right <> nil) then
  begin
    tmpType := Right.VarType;
    if (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
      if (Right <> nil) and Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else
      Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    if HasType() then
      TypeSize := FCompiler.getConstant(FPType.Size)
    else
      TypeSize := FCompiler.getConstant(1);
    IndexVar := nil;
    try
      if (TypeSize <> nil) and (TypeSize.AsInteger > 1) then
        IndexVar := Right.VarType.EvalConst(op_Multiply, Right, TypeSize, [])
      else
        IndexVar := Right;

      Result := //Result := (Pointer + Index * PSize)
        EvalConst(
          op_Plus,
          Left,
          IndexVar,
          []
        );
      Result.CopyFlags(Left);
    finally
      if (IndexVar <> nil) and (IndexVar <> Right) then
        IndexVar.Free();
      Right.VarType := tmpType;
    end;
  end
  else
  begin
    Result := inherited;

    if (not (op in AssignOperators)) and (Result <> nil) then
    begin
      Result.Readable := (op <> op_Deref) and Result.Readable;
      Result.Writeable := ((op = op_Deref) and (not PConst)) or Result.Writeable;
    end;
  end;
end;

function TLapeType_Pointer.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpVar, IndexVar: TResVar;
  wasConstant: Boolean;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  tmpVar := NullResVar;

  if (op = op_Index) then
  begin
    wasConstant := Left.isConstant;

    if (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else
      Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    IndexVar := Right;
    if HasType() and (FPType.Size <> 1) then
      if (Right.VarPos.MemPos = mpMem) and Right.Readable then
        IndexVar := _ResVar.New(FCompiler.getConstant(Right.VarPos.GlobalVar.AsInteger * FPType.Size))
      else
        IndexVar :=
          Right.VarType.Eval(
            op_Multiply,
            tmpVar,
            Right,
            _ResVar.New(FCompiler.getConstant(FPType.Size)),
            [],
            Offset,
            Pos
          );

    Result := //Result := (Pointer + Index * PSize)
      Eval(
        op_Plus,
        Dest,
        Left,
        IndexVar,
        [],
        Offset,
        Pos
      );

    IndexVar.Spill(1);
    Result.setConstant(wasConstant, False);
  end
  else
  begin
    Result := inherited;
    if (not (op in AssignOperators)) then
    begin
      Result.Readable := (op <> op_Deref) and Result.Readable;
      Result.Writeable := ((op = op_Deref) and (not PConst)) or Result.Writeable;
    end;
  end;
end;

constructor TLapeType_Label.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, nil, True, AName, ADocPos);
  FStatic := True;
end;

function TLapeType_Label.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (op in LabelOperators) then
    Result := inherited
  else
    Result := nil;
end;

function TLapeType_Label.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  if (op in LabelOperators) then
    Result := inherited
  else
    LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), 'label']);
end;

function TLapeType_Label.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  if (op in LabelOperators) then
    Result := inherited
  else
    LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), 'label']);
end;

procedure TLapeType_Method.setBaseType(ABaseType: ELapeBaseType);
begin
  Assert(ABaseType in [ltPointer, ltScriptMethod, ltImportedMethod]);
  FBaseType := ABaseType;
end;

function TLapeType_Method.getSize: SizeInt;
begin
  if (FSize = 0) then
    FSize := LapeTypeSize[ltImportedMethod];
  Result := inherited;
end;

function TLapeType_Method.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    if (Res <> nil) then
      FAsString := 'function('
    else
      FAsString := 'procedure(';

    for i := 0 to FParams.Count - 1 do
    begin
      if (i > 0) then
        FAsString := FAsString + ',';
      if (FParams[i].ParType in Lape_RefParams) and (not (FParams[i].ParType in Lape_ValParams)) then
        FAsString := FAsString + '<';
      if (FParams[i].Default <> nil) then
        FAsString := FAsString + '[';
      if (FParams[i].VarType = nil) then
        FAsString := FAsString + '*unknown*'
      else
        FAsString := FAsString + FParams[i].VarType.AsString;
      if (FParams[i].Default <> nil) then
        FAsString := FAsString + ']';
      if (FParams[i].ParType in Lape_RefParams) and (not (FParams[i].ParType in Lape_ValParams)) then
        FAsString := FAsString + '>';
    end;

    FAsString := FAsString + ')';
    if (Res <> nil) then
      FAsString := FAsString + ':' + Res.AsString;
  end;
  Result := inherited;
end;

function TLapeType_Method.getParamSize: SizeInt;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FParams.Count - 1 do
    if (FParams[i].ParType in Lape_RefParams) then
      Result := Result + SizeOf(Pointer)
    else if (FParams[i].VarType <> nil) then
      Result := Result + FParams[i].VarType.Size;
  if (Res <> nil) then
    Result := Result + SizeOf(Pointer);
end;

function TLapeType_Method.getParamInitialization: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FParams.Count - 1 do
    if (not (FParams[i].ParType in Lape_RefParams)) and (FParams[i].VarType <> nil) and FParams[i].VarType.NeedInitialization then
      Exit(True);
end;

constructor TLapeType_Method.Create(ACompiler: TLapeCompilerBase; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltPointer, ACompiler, AName, ADocPos);

  FreeParams := (AParams = nil);
  if (AParams = nil) then
    AParams := TLapeParameterList.Create(NullParameter, dupAccept, False);
  FParams := AParams;
  Res := ARes;
end;

constructor TLapeType_Method.Create(ACompiler: TLapeCompilerBase; AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil);
var
  i: Integer;
  Param: TLapeParameter;
begin
  if (Length(AParams) <> Length(AParTypes)) or (Length(AParams) <> Length(AParDefaults)) then
    LapeExceptionFmt(lpeArrayLengthsDontMatch, [Format('%d, %d, %d', [Length(AParams), Length(AParTypes), Length(AParDefaults)])]);

  Create(ACompiler, nil, ARes, AName, ADocPos);
  for i := 0 to High(AParams) do
  begin
    Param.ParType := AParTypes[i];
    Param.VarType := AParams[i];
    Param.Default := AParDefaults[i];
    FParams.Add(Param);
  end;
end;

constructor TLapeType_Method.Create(AMethod: TLapeType_Method);
begin
  Assert(AMethod <> nil);
  Create(AMethod.Compiler, nil, AMethod.Res, AMethod.Name, @AMethod._DocPos);
  Params.ImportFromArray(AMethod.Params.ExportToArray());

  ImplicitParams := AMethod.ImplicitParams;

  inheritManagedDecls(AMethod);
  TypeID := AMethod.TypeID;
  FBaseType := AMethod.FBaseType;
end;

function TLapeType_Method.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_Method;
begin
  if DeepCopy then
  begin
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, nil, Res, Name, @_DocPos);
    TLapeType_Method(Result).Params.ImportFromArray(FParams.ExportToArray());
  end
  else
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, FParams, Res, Name, @_DocPos);

  TLapeType_Method(Result).ImplicitParams := ImplicitParams;

  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;
  Result.FBaseType := FBaseType;
end;

destructor TLapeType_Method.Destroy;
begin
  if FreeParams then
    FParams.Free();
  inherited;
end;

function TLapeType_Method.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result := (Other <> nil) and (Other.TypeID = TypeID) and (Size = Other.Size) and (Other is TLapeType_Method) and EqualParams(Other as TLapeType_Method, False, ContextOnly);
  if Result and (not ContextOnly) then
    Result := (Other.BaseType = BaseType);
end;

function TLapeType_Method.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
begin
  Result := 'begin Result := '#39 + AsString + ' ('#39' + System.ToString(Pointer(Param0)) + '#39')'#39'; end;';
end;

function TLapeType_Method.EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True; IgnoreDefault: Boolean = False): Boolean;

  function _EqualTypes(const Left, Right: TLapeType): Boolean;
  begin
    Result := (Left = Right) or ((Left <> nil) and Left.Equals(Right, False));
  end;

  function _EqualVals(const Left, Right: TLapeVar): Boolean;
  begin
    if IgnoreDefault then
      Result := True
    else if ContextOnly or (Left = nil) or (Right = nil) or (Left = Right) then
      Result := (Left <> nil) = (Right <> nil)
    else
      Result := Left.isConstant and Right.isConstant and
        (Left is TLapeGlobalVar) and (Right is TLapeGlobalVar) and
        (TLapeGlobalVar(Left).AsString = TLapeGlobalVar(Right).AsString);
  end;

  function _EqualParams(const Left, Right: TLapeParameter): Boolean;
  begin
    Result := (ContextOnly or (Left.ParType = Right.ParType)) and
      _EqualTypes(Left.VarType, Right.VarType) and
      _EqualVals(Left.Default, Right.Default)
  end;

var
  i, ImplicitLeft, ImplicitRight: Integer;
begin
  Result := False;
  if (Other = nil) or (not _EqualTypes(Res, Other.Res)) then
    Exit;

  if ContextOnly then
  begin
    ImplicitLeft  := ImplicitParams;
    ImplicitRight := Other.ImplicitParams;
  end
  else
  begin
    ImplicitLeft := 0;
    ImplicitRight := 0;
  end;

  if (Params.Count - ImplicitLeft <> Other.Params.Count - ImplicitRight) then
    Exit;

  for i := 0 to Params.Count - ImplicitLeft - 1 do
    if (not _EqualParams(Params[ImplicitLeft + i], Other.Params[ImplicitRight + i])) then
      Exit;

  Result := True;
end;

procedure TLapeType_Method.addParam(Param: TLapeParameter);
begin
  if (Param.VarType = nil) and (not (Param.ParType in Lape_RefParams)) then
    LapeException(lpeImpossible);
  FParams.Add(Param);
end;

procedure TLapeType_Method.setImported(AVar: TLapeGlobalVar; isImported: Boolean);
var
  MethodType: ELapeBaseType;
begin
  if isImported then
    MethodType := ltImportedMethod
  else
    MethodType := ltScriptMethod;

  if (AVar = nil) or (AVar.BaseType = MethodType) then
    Exit;

  AVar.VarType := CreateCopy();
  AVar.VarType.FBaseType := MethodType;
  if (FCompiler <> nil) then
    AVar.VarType := FCompiler.addManagedType(AVar.VarType);
end;

function TLapeType_Method.NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  setImported(Result, True);
  PPointer(Result.Ptr)^ := Ptr;
end;

function TLapeType_Method.NewGlobalVar(CodePos: TCodePos; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  setImported(Result, False);
  PCodePos(Result.Ptr)^ := CodePos;
end;

function TLapeType_Method.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
var
  m: TLapeGlobalVar;
begin
  if (Right <> nil) and (Right is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right).getMethod(Self);
    if (m <> nil) then
      Right := m.VarType;
  end;

  if (Op = op_Assign) and (Right <> nil) then
    if (Right is TLapeType_Method) then
      if Equals(Right) and (FBaseType in [ltPointer, Right.BaseType]) then
        Result := Self
      else
        Result := nil
    else if (Right is TLapeType_Pointer) then
      if (TLapeType_Pointer(Right).HasType()) then
        Result := nil
      else
        Result := Self
    else
      Result := inherited
  else
    Result := inherited;
end;

function TLapeType_Method.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  m: TLapeGlobalVar;
begin
  if (Right <> nil) and Right.HasType() and (Right.VarType is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right.VarType).getMethod(Self);
    if (m <> nil) then
      Right := m;
  end;
  Result := inherited;
end;

function TLapeType_Method.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  m: TLapeGlobalVar;
begin
  if Right.HasType() and (Right.VarType is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right.VarType).getMethod(Self);
    if (m <> nil) then
      Right := _ResVar.New(m);
  end;
  Result := inherited;
end;

function TLapeType_MethodOfObject.getSize: SizeInt;
begin
  Result := inherited;
  Result := Result + SizeOf(Pointer);
  Assert(Result = SizeOf(TMethod));
end;

function TLapeType_MethodOfObject.getAsString: lpString;
begin
  Result := inherited;
  Result := Result + ' of object';
end;

function TLapeType_MethodOfObject.getParamSize: SizeInt;
begin
  Result := inherited;
  Result := Result + SizeOf(Pointer);
end;

constructor TLapeType_MethodOfObject.Create(ACompiler: TLapeCompilerBase; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited;
  FMethodRecord := FCompiler.getGlobalType('TMethod');
  Assert(FMethodRecord <> nil);
end;

function TLapeType_MethodOfObject.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result := (Other is TLapeType_MethodOfObject) and inherited;
end;

function TLapeType_MethodOfObject.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
begin
  Result := 'begin Result := '#39 + AsString + ' '#39' + System.ToString(System.TMethod(Param0)); end;';
end;

function TLapeType_MethodOfObject.NewGlobalVar(AMethod: TMethod; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVar(nil, AName, ADocPos);
  TMethod(Result.Ptr^) := AMethod
end;

function TLapeType_MethodOfObject.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
var
  m: TLapeGlobalVar;
begin
  if (Right <> nil) and (Right is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right).getMethod(Self);
    if (m <> nil) then
      Right := m.VarType;
  end;

  if (Op in CompareOperators + [op_Assign]) then
    Result := inherited
  else if CompatibleWith(Right) then
    Result := FMethodRecord.EvalRes(Op, FMethodRecord, Flags)
  else
    Result := FMethodRecord.EvalRes(Op, Right, Flags);
  if (Result = FMethodRecord) then
    Result := Self;
end;

function TLapeType_MethodOfObject.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  VarType: TLapeType;
  MethodIndex: Integer;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Left <> nil) then
    Left.VarType := FMethodRecord;

  if (Right <> nil) and Right.HasType() and (Right.VarType is TLapeType_OverloadedMethod) then
  begin
    MethodIndex := TLapeType_OverloadedMethod(Right.VarType).getMethodIndex(Self);
    if (MethodIndex >= 0) then
      Right := Right.VarType.EvalConst(op_Index, Right, FCompiler.getConstant(MethodIndex), Flags);
  end;

  try
    if (Op in CompareOperators) then
      Result := inherited
    else
    try
      if CompatibleWith(Right.VarType) then
      try
        VarType := Right.VarType;
        Right.VarType := FMethodRecord;
        Result := FMethodRecord.EvalConst(Op, Left, Right, Flags);
      finally
        Right.VarType := VarType;
      end
      else
        Result := FMethodRecord.EvalConst(Op, Left, Right, Flags);
    except
      Result := inherited;
    end;
  finally
    if (Left <> nil) then
      Left.VarType := Self;
  end;
end;

function TLapeType_MethodOfObject.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpRes: TResVar;
  MethodIndex: Integer;
begin
  Assert(Left.VarType = Self);
  Left.VarType := FMethodRecord;

  if Right.HasType() and (Right.VarType is TLapeType_OverloadedMethod) then
  begin
    MethodIndex := TLapeType_OverloadedMethod(Right.VarType).getMethodIndex(Self);
    if (MethodIndex >= 0) then
    begin
      tmpRes := NullResVar;
      Right := Right.VarType.Eval(op_Index, tmpRes, Right, _ResVar.New(FCompiler.getConstant(MethodIndex)), Flags, Offset, Pos);
    end;
  end;

  if (Op in CompareOperators) then
    Result := inherited
  else
  try
    if CompatibleWith(Right.VarType) then
      Right.VarType := FMethodRecord;
    Result := FMethodRecord.Eval(Op, Dest, Left, Right, Flags, Offset, Pos);
    if (Result.VarType = FMethodRecord) then
      Result.VarType := Self;
  except
    Result := inherited;
  end;
end;

function TLapeType_MethodOfType.getAsString: lpString;
begin
  Result := '(' + FObjectType.AsString + ').' + inherited;
end;

constructor TLapeType_MethodOfType.Create(ACompiler: TLapeCompilerBase; AObjType: TLapeType; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(AObjType <> nil);
  inherited Create(ACompiler, AParams, ARes, AName, ADocPos);
  FObjectType := AObjType;
  SelfParam := Lape_SelfParam;
end;

constructor TLapeType_MethodOfType.Create(AMethod: TLapeType_Method; AObjType: TLapeType);
begin
  Assert(AObjType <> nil);
  inherited Create(AMethod);
  FObjectType := AObjType;
end;

function TLapeType_MethodOfType.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_MethodOfType;
begin
  if DeepCopy then
  begin
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, FObjectType, nil, Res, Name, @_DocPos);
    TLapeType_MethodOfType(Result).Params.ImportFromArray(FParams.ExportToArray());
  end
  else
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, FObjectType, FParams, Res, Name, @_DocPos);

  TLapeType_MethodOfType(Result).SelfParam := SelfParam;
  TLapeType_MethodOfType(Result).ImplicitParams := ImplicitParams;

  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;
  Result.FBaseType := FBaseType;
end;

function TLapeType_MethodOfType.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result :=
    (Other is TLapeType_MethodOfType) and
    (SelfParam = TLapeType_MethodOfType(Other).SelfParam) and
    ObjectType.Equals(TLapeType_MethodOfType(Other).ObjectType, ContextOnly) and
    inherited;
end;

function TLapeType_MethodOfType.EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True; IgnoreDefault: Boolean = False): Boolean;
begin
  Result := inherited;
  if Result and (not ContextOnly) and (Other is TLapeType_MethodOfType) then
    Result := SelfParam = TLapeType_MethodOfType(Other).SelfParam;
end;

function TLapeType_OverloadedMethod.getAsString: lpString;
begin
  if (FAsString = '') then
  begin
    if (Name <> '') then
      FAsString := Name
    else
      FAsString := 'Overloaded Method';
    FAsString := FAsString + ' [' + lpString(IntToStr(FManagedDecls.Count)) + ']'
  end;
  Result := inherited;
end;

constructor TLapeType_OverloadedMethod.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUnknown, ACompiler, AName, ADocPos);

  FOfObject := bUnknown;
  OnFunctionNotFound := nil;
  NeedFullMatch := False;
  FManagedDecls.Sorted := False;
end;

function TLapeType_OverloadedMethod.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_OverloadedMethod;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, Name, @_DocPos);
  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;

  TLapeType_OverloadedMethod(Result).FOfObject := FOfObject;
end;

function TLapeType_OverloadedMethod.addSubDeclaration(ADecl: TLapeDeclaration): Integer;
begin
  Result := -1;
  LapeException(lpeImpossible);
end;

procedure TLapeType_OverloadedMethod.addMethod(AMethod: TLapeGlobalVar; DoOverride: Boolean = False);
var
  i: Integer;
begin
  if (AMethod = nil) or (not AMethod.HasType()) or
     (not ((AMethod.VarType is TLapeType_Method) or (AMethod.VarType is TLapeType_OverloadedMethod)))
  then
    LapeException(lpeImpossible);

  AMethod.setReadWrite(False, False);

  if MethodOfObject(AMethod.VarType) then
    case FOfObject of
      bUnknown: FOfObject := bTrue;
      bFalse: LapeException(lpeCannotMixStaticOverload);
    end
  else
    case FOfObject of
      bUnknown: FOfObject := bFalse;
      bTrue: LapeException(lpeCannotMixStaticOverload);
    end;

  if (AMethod.VarType is TLapeType_OverloadedMethod) then
    for i := 0 to AMethod.VarType.FManagedDecls.Count - 1 do
      addMethod(TLapeGlobalVar(AMethod.VarType.FManagedDecls[i]).CreateCopy(False))
  else if DoOverride then
  begin
    AMethod := overrideMethod(AMethod);
    if FList.FreeDecls and (AMethod <> nil) then
      AMethod.Free();
  end
  else
  begin
    for i := 0 to FManagedDecls.Count - 1 do
      if TLapeType_Method(TLapeGlobalVar(FManagedDecls[i]).VarType).EqualParams(AMethod.VarType as TLapeType_Method) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [AMethod.VarType.AsString]);

    AMethod.Name := Name;
    FManagedDecls.addDeclaration(AMethod);
  end;
end;

function TLapeType_OverloadedMethod.overrideMethod(AMethod: TLapeGlobalVar): TLapeGlobalVar;
var
  i: Integer;
  OldMethod: TLapeGlobalVar;
begin
  if (AMethod = nil) or (not AMethod.HasType()) or
     (not ((AMethod.VarType is TLapeType_Method) or (AMethod.VarType is TLapeType_OverloadedMethod)))
  then
    LapeException(lpeImpossible);

  Result := nil;
  AMethod.setReadWrite(False, False);

  for i := 0 to FManagedDecls.Count - 1 do
  begin
    OldMethod := TLapeGlobalVar(FManagedDecls[i]);
    if TLapeType_Method(OldMethod.VarType).EqualParams(AMethod.VarType as TLapeType_Method) then
    begin
      Result := OldMethod;
      FManagedDecls.Delete(i);
      Break;
    end;
  end;

  FManagedDecls.addDeclaration(AMethod);
end;

function TLapeType_OverloadedMethod.getMethodIndex(AType: TLapeType_Method): Integer;
var
  i: Integer;
begin
  if (AType = nil) then
    Exit(-1);

  for i := 0 to FManagedDecls.Count - 1 do
    if TLapeType_Method(TLapeGlobalVar(FManagedDecls[i]).VarType).EqualParams(AType) then
      Exit(i);

  if ({$IFNDEF FPC}@{$ENDIF}OnFunctionNotFound <> nil) then
    Result := FManagedDecls.IndexOf(OnFunctionNotFound(Self, AType, nil, nil))
  else
    Result := -1;
end;

function TLapeType_OverloadedMethod.getMethodIndex(AParams: TLapeTypeArray; AResult: TLapeType = nil): Integer;

  function SizeWeight(a, b: TLapeType): SizeInt; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := Abs(a.Size - b.Size);
    if (a.BaseType <> b.BaseType) then
      Result := Result + 1;

    if (a.BaseType in LapeRealTypes) and (b.BaseType in LapeRealTypes+LapeIntegerTypes) then
      if (a.BaseType >= b.BaseType) then
        if (b.BaseType in LapeIntegerTypes) then
          Result := (Ord(a.BaseType) - Ord(b.BaseType)) + 12
        else
          Result := Ord(a.BaseType) - Ord(b.BaseType)
      else
        Result := (Ord(b.BaseType) - Ord(a.BaseType)) + 4
    else if (a.BaseType in LapeBoolTypes) and (b.BaseType in LapeBoolTypes) then
      Result := Ord(a.BaseType) - Ord(b.BaseType)
    else if (a.BaseType in LapeStringTypes) and (b.BaseType in LapeStringTypes) then
      if (a.BaseType >= b.BaseType) then
        Result := Ord(a.BaseType) - Ord(b.BaseType)
      else
        Result := (Ord(b.BaseType) - Ord(a.BaseType)) * 4
    else if (a.BaseType in LapeArrayTypes) and (b.BaseType in LapeArrayTypes) then
      {nothing}
    else if (a.BaseType in LapePointerTypes) and (b.BaseType in LapePointerTypes) then
      if (TLapeType_Pointer(a).PConst <> TLapeType_Pointer(b).PConst) then
        Result := Result + 1
      else
        {nothing}
    else if (a.BaseType in LapeOrdinalTypes) and (b.BaseType in LapeOrdinalTypes) then
    begin
      if (a.VarLo().AsInteger <= b.VarLo().AsInteger) and (UInt64(a.VarHi().AsInteger) >= UInt64(b.VarHi().AsInteger)) then
          Result := Result - 1
      else if (a.Size < b.Size) then
        Result := Result + 24;
      if ((a.BaseIntType in LapeSignedIntegerTypes) = (b.BaseIntType in LapeSignedIntegerTypes)) then
        Result := Result - 1;
    end
    else
    begin
      Result := Result * 2;
      if (a.Size < b.Size) then
        Result := Result * 2;
    end;
  end;

var
  MethodIndex, ParamsCount, AIndx, i: Integer;
  Weight, MinWeight: SizeInt;
  Match: Boolean;
begin
  Result := -1;
  MinWeight := High(Integer);

  for MethodIndex := 0 to FManagedDecls.Count - 1 do
    with TLapeType_Method(TLapeGlobalVar(FManagedDecls[MethodIndex]).VarType) do
    begin
      ParamsCount := Params.Count - ImplicitParams;
      if (Length(AParams) > ParamsCount) or ((AResult <> nil) and (Res = nil)) then
        Continue;

      if (AResult = nil) or AResult.Equals(Res, False) then
        Weight := (ParamsCount * 4)
      else if AResult.Equals(Res) then
        Weight := (ParamsCount * 4) + 3
      else if (not AResult.CompatibleWith(Res)) then
        Continue
      else
        Weight := SizeWeight(AResult, Res) + (ParamsCount + 1) * 4;

      Match := True;
      for i := ImplicitParams to Params.Count - 1 do
      begin
        AIndx := i - ImplicitParams;
        Match := False;

        if ((AIndx >= Length(AParams)) or (AParams[AIndx] = nil)) and (Params[i].Default = nil) then
          Break
        else if (AIndx >= Length(AParams)) or (AParams[AIndx] = nil) or (Params[i].VarType = nil) or Params[i].VarType.Equals(AParams[AIndx]) then
          if (AIndx >= Length(AParams)) or (AParams[AIndx] = nil) or ((Params[i].VarType <> nil) and Params[i].VarType.Equals(AParams[AIndx], False)) then
            Weight := Weight - 4
          else if NeedFullMatch then
            if (Params[i].VarType = nil) then
              Weight := Weight - 1
            else
              Break
          else
            Weight := Weight - 3
        else if (not (Params[i].ParType in Lape_ValParams)) or NeedFullMatch then
          Break
        else if (not Params[i].VarType.CompatibleWith(AParams[AIndx])) then
          Break
        else
          Weight := Weight + SizeWeight(Params[i].VarType, AParams[AIndx]);
        Match := True;
      end;

      if Match then
        if (Weight = MinWeight) then
          Result := -1
        else if (Weight < MinWeight) then
        begin
          Result := MethodIndex;
          MinWeight := Weight;
        end;
    end;

  if (Result < 0) and ({$IFNDEF FPC}@{$ENDIF}OnFunctionNotFound <> nil) then
    Result := FManagedDecls.IndexOf(OnFunctionNotFound(Self, nil, AParams, AResult));
end;

function TLapeType_OverloadedMethod.getMethod(AType: TLapeType_Method): TLapeGlobalVar;
begin
  Result := FManagedDecls[getMethodIndex(AType)] as TLapeGlobalVar;
end;

function TLapeType_OverloadedMethod.getMethod(AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;
begin
  Result := FManagedDecls[getMethodIndex(AParams, AResult)] as TLapeGlobalVar;
end;

function TLapeType_OverloadedMethod.NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  Result.setReadWrite(False, False);
end;

function TLapeType_OverloadedMethod.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
end;

function TLapeType_OverloadedMethod.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType;
var
  Method: TLapeGlobalVar;
begin
  if (Op = op_Index) and (Right <> nil) and (Right.BaseType in LapeIntegerTypes) then
  begin
    Method := FManagedDecls[Right.AsInteger] as TLapeGlobalVar;
    if (Method = nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [Right.AsInteger,0,FManagedDecls.Count-1]);

    Result := Method.VarType
  end
  else
    Result := inherited;
end;

function TLapeType_OverloadedMethod.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Index) and (Left <> nil) and (Right <> nil) and (Right.BaseType in LapeIntegerTypes) then
    Result := Right.AsInteger < FManagedDecls.Count
  else
    Result := inherited;
end;

function TLapeType_OverloadedMethod.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Index) and (Left <> nil) and (Right <> nil) and (Right.BaseType in LapeIntegerTypes) then
  begin
    Result := FManagedDecls[Right.AsInteger] as TLapeGlobalVar;
    if (Result = nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [Right.AsInteger,0,FManagedDecls.Count-1]);

    if MethodOfObject(Result.VarType) and (Left.Ptr <> nil) and (TMethod(Left.Ptr^).Data <> nil) then
    begin
      Result := Result.CreateCopy();
      TMethod(Result.Ptr^).Data := TMethod(Left.Ptr^).Data;
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_OverloadedMethod.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  Res: TResVar;
  Method: TLapeGlobalVar;
begin
  Assert(Left.VarType = Self);
  if (Op = op_Index) and (Right.VarPos.MemPos = mpMem) and Right.HasType() and
      Right.isConstant and (Right.VarType.BaseType in LapeIntegerTypes)
  then
  begin
    Method := FManagedDecls[Right.VarPos.GlobalVar.AsInteger] as TLapeGlobalVar;
    if (Method = nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [Right.VarPos.GlobalVar.AsInteger,0,FManagedDecls.Count-1]);

    Result := _ResVar.New(Method);

    if MethodOfObject(Result.VarType) then
    begin
      Dest := NullResVar;

      if Left.Writeable then
        Res := Left.IncLock()
      else
        Res := _ResVar.New(FCompiler.getTempVar(Result.VarType));

      Assert(Left.VarPos.MemPos <> mpStack);

      FCompiler.Emitter._Eval(getEvalProc(op_Index, ltUnknown, ltPointer), Res, Result, Left, Offset, Pos);
      Res.VarType := Result.VarType;
      Result := Res;
    end;
  end
  else
    Result := inherited;
end;

constructor TLapeWithDeclaration.Create(AWithDeclRec: TLapeWithDeclRec);
begin
  inherited Create();
  FWithDeclRec := AWithDeclRec;
end;

constructor TLapeType_VarRefMap.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ltUnknown, ACompiler);
  FVarMap := TLapeVarRefMap.Create(NullVarRef, dupError, True);
end;

destructor TLapeType_VarRefMap.Destroy;
var
  i: Integer;
begin
  for i := 0 to FVarMap.Count - 1 do
    with FVarMap.ItemsI[i] do
      if (Lock <> NullVarRef.Lock) then
        if (RefVar <> nil) then
          TLapeStackTempVar(RefVar).FLock := Lock
        else
          TLapeStackTempVar(ResVar.VarPos.StackVar).FLock := Lock;

  FreeAndNil(FVarMap);
  inherited;
end;

function TLapeType_VarRefMap.CanHaveChild: Boolean;
begin
  Result := True;
end;

function TLapeType_VarRefMap.HasChild(AName: lpString): Boolean;
begin
  Result := FVarMap.ExistsKey(AName) or HasSubDeclaration(AName, bTrue);
end;

function TLapeType_VarRefMap.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
end;

function TLapeType_VarRefMap.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (Op = op_Dot) and ValidFieldName(Right) and FVarMap.ExistsKey(PlpString(Right.Ptr)^) then
    with FVarMap[PlpString(Right.Ptr)^] do
      if (RefVar <> nil) then
        Result := RefVar.VarType
      else
        Result := ResVar.VarType
  else
    Result := inherited;
end;

function TLapeType_VarRefMap.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  Result := inherited;
  if Result and (op = op_Dot) and ValidFieldName(Right) and FVarMap.ExistsKey(PlpString(Right.Ptr)^) then
    Result := (FVarMap[PlpString(Right.Ptr)^].RefVar is TLapeGlobalVar);
end;

function TLapeType_VarRefMap.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Dot) and ValidFieldName(Right) and FVarMap.ExistsKey(PlpString(Right.Ptr)^) then
    Result := FVarMap[PlpString(Right.Ptr)^].RefVar as TLapeGlobalVar
  else
    Result := inherited;
end;

function TLapeType_VarRefMap.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  Assert(Left.VarType = Self);
  if (Op = op_Dot) and ValidFieldName(Right) and FVarMap.ExistsKey(PlpString(Right.VarPos.GlobalVar.Ptr)^) then
    with FVarMap[PlpString(Right.VarPos.GlobalVar.Ptr)^] do
    begin
      Dest := NullResVar;
      if (RefVar <> nil) then
        Result := _ResVar.New(RefVar)
      else
        Result := ResVar;
    end
  else
    Result := inherited;
end;

procedure TLapeType_VarRefMap.addVar(RefVar: TLapeVar; AName: lpString);
var
  Rec: TLapeVarRef;
begin
  Rec := NullVarRef;
  Rec.RefVar := RefVar;
  if (RefVar is TLapeStackTempVar) then
    with TLapeStackTempVar(RefVar) do
      if (FLock <> BigLock * BigLock) then
      begin
        Rec.Lock := FLock;
        FLock := BigLock * BigLock;
      end;

  FVarMap.Add(AName, Rec);
end;

procedure TLapeType_VarRefMap.addVar(RefVar: TResVar; AName: lpString);
var
  Rec: TLapeVarRef;
begin
  Rec := NullVarRef;
  Rec.ResVar := RefVar;
  if (RefVar.VarPos.MemPos = mpVar) and (RefVar.VarPos.StackVar is TLapeStackTempVar) then
    with TLapeStackTempVar(RefVar.VarPos.StackVar) do
      if (FLock <> BigLock * BigLock) then
      begin
        Rec.Lock := FLock;
        FLock := BigLock * BigLock;
      end;

  FVarMap.Add(AName, Rec);
end;

function TLapeStackInfo.getVar(Index: Integer): TLapeStackVar;
begin
  Result := FVarStack[Index];
end;

function TLapeStackInfo.getVarCount: Integer;
begin
  Result := FVarStack.Count;
end;

function TLapeStackInfo.getTotalSize: SizeInt;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FVarStack.Count - 1 do
    Result := Result + FVarStack[i].Size;
end;

function TLapeStackInfo.getTotalParamSize: SizeInt;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FVarStack.Count - 1 do
    if (FVarStack[i] is TLapeParameterVar) then
      Result := Result + FVarStack[i].Size;
end;

function TLapeStackInfo.getTotalNoParamSize: SizeInt;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FVarStack.Count - 1 do
    if (not (FVarStack[i] is TLapeParameterVar)) then
      Result := Result + FVarStack[i].Size;
end;

function TLapeStackInfo.getInitialization: Boolean;
var
  i: Integer;
begin
  if ForceInitialization then
    Exit(True);

  for i := 0 to FVarStack.Count - 1 do
    if FVarStack[i].NeedInitialization then
      Exit(True);
  Result := False;
end;

function TLapeStackInfo.getFinalization: Boolean;
var
  i: Integer;
begin
  for i := 0 to FVarStack.Count - 1 do
    if FVarStack[i].NeedFinalization then
      Exit(True);
  Result := False;
end;

constructor TLapeStackInfo.Create(AlwaysInitialize: Boolean = True; ForceDisposal: Boolean = False; AOwner: TLapeStackInfo = nil; ManageVars: Boolean = True);
begin
  inherited Create(nil, False);

  Owner := AOwner;
  FVarStack := TLapeVarStack.Create(nil, dupIgnore, False);
  FWithStack := TLapeWithDeclarationList.Create(NullWithDecl, dupIgnore, False);
  FreeVars := ManageVars;
  CodePos := -1;

  ForceInitialization := AlwaysInitialize;
  FullDisposal := ForceDisposal;
end;

destructor TLapeStackInfo.Destroy;
var
  i: Integer;
begin
  for i := FVarStack.Count - 1 downto 0 do
    if FreeVars then
      FVarStack[i].Free()
    else
      FVarStack[i].Stack := nil;
  FVarStack.Free();
  FWithStack.Free();

  inherited;
end;

procedure TLapeStackInfo.Clear;
begin
  if FreeDecls then
    Delete(TLapeVar, True);
  inherited;
end;

function TLapeStackInfo.getDeclaration(Name: lpString; CheckParent: TInitBool; CheckWith: Boolean): TLapeDeclaration;
var
  i: Integer;
  Declarations: TLapeDeclArray;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if (FWithStack[i].WithType <> nil) and FWithStack[i].WithType.hasChild(Name) then
        Exit(TLapeWithDeclaration.Create(FWithStack[i]));

  Declarations := getByName(Name, CheckParent);
  if (Length(Declarations) > 1) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Name])
  else if (Length(Declarations) > 0) and (Declarations[0] <> nil) then
    Result := Declarations[0]
  else
    Result := nil;
end;

function TLapeStackInfo.hasDeclaration(Name: lpString; CheckParent: TInitBool; CheckWith: Boolean): Boolean;
var
  i: Integer;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if (FWithStack[i].WithType <> nil) and FWithStack[i].WithType.hasChild(Name) then
        Exit(True);

  Result := HasSubDeclaration(Name, CheckParent);
end;

function TLapeStackInfo.hasDeclaration(Decl: TLapeDeclaration; CheckParent: TInitBool; CheckWith: Boolean): Boolean;
var
  i: Integer;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if (FWithStack[i].WithType <> nil) and FWithStack[i].WithType.hasChild(Decl) then
        Exit(True);

  Result := HasSubDeclaration(Decl, CheckParent);
end;

function TLapeStackInfo.getTempVar(VarType: TLapeType; Lock: Integer = 1): TLapeStackTempVar;
var
  i: Integer;
begin
  Result := nil;
  if (VarType = nil) then
    Exit;

  try
    for i := 0 to FVarStack.Count - 1 do
      if (FVarStack[i] is TLapeStackTempVar) and (not TLapeStackTempVar(FVarStack[i]).Locked) and FVarStack[i].VarType.Equals(VarType) then
      begin
        Result := FVarStack[i] as TLapeStackTempVar;
        Result.VarType := VarType;
        Result.isConstant := False;
        Exit;
      end;
    Result := addVar(VarType) as TLapeStackTempVar;
  finally
    if (Result <> nil) then
      TLapeStackTempVar(Result).IncLock(Lock);
  end;
end;

function TLapeStackInfo.addDeclaration(Decl: TLapeDeclaration): Integer;
begin
  if (Decl = nil) then
    Result := -1
  else if FList.ExistsItem(Decl) or ((Decl.Name <> '') and hasDeclaration(Decl.Name, bTrue, True)) or
         (Pos(LapeCase('|'+Decl.Name+'|'), LapeReservedLocals) > 0)
  then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Decl.Name]);
  Result := FList.Add(Decl);
end;

function TLapeStackInfo.addVar(StackVar: TLapeStackVar): TLapeStackVar;
begin
  if (StackVar = nil) then
    Exit(nil);
  Assert(StackVar.Size > 0);

  StackVar.Stack := FVarStack;
  Result := StackVar;
  addDeclaration(StackVar);
end;

function TLapeStackInfo.addVar(VarType: TLapeType; Name: lpString = ''): TLapeStackVar;
begin
  if (Name = '') then
    Result := addVar(TLapeStackTempVar.Create(VarType, nil, Name))
  else
    Result := addVar(TLapeStackVar.Create(VarType, nil, Name));
end;

function TLapeStackInfo.addVar(ParType: ELapeParameterType; VarType: TLapeType; Name: lpString): TLapeStackVar;
begin
  if (VarType = nil) then
    Result := addVar(TLapeParameterVar.Create(ParType, VarType, nil, Name))
  else
    Result := addVar(TLapeParameterVar.Create(ParType, VarType, nil, Name, @VarType._DocPos));
end;

function TLapeStackInfo.addSelfVar(ParType: ELapeParameterType; VarType: TLapeType): TLapeStackVar;
begin
  Result := addVar(ParType, VarType, 'Self');
end;

function TLapeStackInfo.inheritVar(StackVar: TLapeStackVar): TLapeStackVar;
begin
  Assert(StackVar <> nil);
  Assert(StackVar.Stack <> VarStack);
  Result := addVar(TLapeStackInheritedVar.Create(StackVar, nil));
end;

function TLapeStackInfo.getInheritedVar(StackVar: TLapeStackVar): TLapeStackVar;
var
  i: Integer;
begin
  if (StackVar = nil) then
    Exit(nil)
  else if (StackVar.Stack = VarStack) then
    Exit(StackVar);

  for i := 0 to FVarStack.Count - 1 do
    if (FVarStack[i] is TLapeStackInheritedVar) and (TLapeStackInheritedVar(FVarStack[i]).Parent = StackVar) then
      Exit(FVarStack[i]);

  Result := nil;
end;

function TLapeStackInfo.addWith(AWith: TLapeWithDeclRec): Integer;
begin
  Result := FWithStack.Add(AWith);
end;

procedure TLapeStackInfo.delWith(ACount: Integer);
var
  i: Integer;
begin
  for i := FWithStack.Count - 1 downto FWithStack.Count - ACount do
    FWithStack.Delete(i);
end;

constructor TLapeDeclStack.Create(AList: TLapeManagingDeclaration; AOwner: TLapeStackInfo = nil);
begin
  Assert(AList <> nil);
  inherited Create(False, False, AOwner, False);

  Parent := AList.ManagedDeclarations;
  FManagingList := AList;
end;

function TLapeDeclStack.addDeclaration(Decl: TLapeDeclaration): Integer;
begin
  Result := FManagingList.addSubDeclaration(Decl);
end;

function TLapeDeclStack.addVar(StackVar: TLapeStackVar): TLapeStackVar;
begin
  Result := nil;
  LapeException(lpeImpossible);
end;

function TLapeEmptyStack.addDeclaration(Decl: TLapeDeclaration): Integer;
begin
  Result := -1;
  LapeException(lpeImpossible);
end;

type
  EMyMemoryPos = (mmpNone, mmpPtr, mmpVar, mmpStk, mmpPVar, mmpPStk);
function getMemoryPos(AVar: TVarPos): EMyMemoryPos; inline;
begin
  Result := mmpNone;
  case AVar.MemPos of
    mpMem:
      if AVar.isPointer then
        LapeException(lpeImpossible)
      else
        Result := mmpPtr;
    mpVar:
      if AVar.isPointer then
        Result := mmpPVar
      else
        Result := mmpVar;
    mpStack:
      if AVar.isPointer then
        Result := mmpPStk
      else
        Result := mmpStk;
  end;
end;

function TLapeCodeEmitter._IncCall(ACodePos: TResVar; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  e: Boolean;
begin
  Assert(ACodePos.HasType());
  e := False;

  case getMemoryPos(ACodePos.VarPos) of
    mmpVar: Result := _IncCall_Var(ACodePos.VarPos.StackVar.Offset + ACodePos.VarPos.Offset, AParamSize, Offset, Pos);
    mmpPVar: Result := _IncCall_PVar(ACodePos.VarPos.StackVar.Offset, ACodePos.VarPos.Offset, AParamSize, Offset, Pos);
    mmpPtr: Result := _IncCall_Ptr(ACodePos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._IncCall(ACodePos: TResVar; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _IncCall(ACodePos, AParamSize, Offset, Pos);
end;

function TLapeCodeEmitter._InvokeImportedProc(AMemPos: TResVar; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  e: Boolean;
begin
  Assert(AMemPos.HasType());
  e := False;

  case getMemoryPos(AMemPos.VarPos) of
    mmpVar: Result := _InvokeImported_Var(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AParamSize, Offset, Pos);
    mmpPVar: Result := _InvokeImported_PVar(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AParamSize, Offset, Pos);
    mmpPtr: Result := _InvokeImported_Ptr(AMemPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._InvokeImportedProc(AMemPos: TResVar; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _InvokeImportedProc(AMemPos, AParamSize, Offset, Pos);
end;

function TLapeCodeEmitter._InvokeImportedFunc(AMemPos, AResPos: TResVar; AParamSize: UInt16; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  e: Boolean;
begin
  Assert(AMemPos.HasType());
  Assert(AResPos.HasType());
  e := False;

  case getMemoryPos(AMemPos.VarPos) of
    mmpVar: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Var_Stk(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AParamSize, AResPos.VarType.Size, Offset, Pos);
      mmpVar: Result := _InvokeImported_Var_Var(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPVar: Result := _InvokeImported_Var_PVar(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.StackVar.Offset, AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPtr: Result := _InvokeImported_Var_Ptr(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      else e := True;
    end;
    mmpPVar: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_PVar_Stk(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AParamSize, AResPos.VarType.Size, Offset, Pos);
      mmpVar: Result := _InvokeImported_PVar_Var(AMemPos.VarPos.StackVar.Offset, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AMemPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPVar: Result := _InvokeImported_PVar_PVar(AMemPos.VarPos.StackVar.Offset, AResPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPtr: Result := _InvokeImported_PVar_Ptr(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AResPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      else e := True;
    end;
    mmpPtr: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Ptr_Stk(AMemPos.VarPos.GlobalVar.Ptr, AParamSize, AResPos.VarType.Size, Offset, Pos);
      mmpVar: Result := _InvokeImported_Ptr_Var(AMemPos.VarPos.GlobalVar.Ptr, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPVar: Result := _InvokeImported_Ptr_PVar(AMemPos.VarPos.GlobalVar.Ptr, AResPos.VarPos.StackVar.Offset, AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPtr: Result := _InvokeImported_Ptr_Ptr(AMemPos.VarPos.GlobalVar.Ptr, AResPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      else e := True;
    end;
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._InvokeImportedFunc(AMemPos, AResPos: TResVar; AParamSize: UInt16; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _InvokeImportedFunc(AMemPos, AResPos, AParamSize, Offset, Pos);
end;

function TLapeCodeEmitter._JmpRIf(Jmp: TCodeOffset; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  e: Boolean;
begin
  Assert(Cond.HasType());
  e := False;

  case getMemoryPos(Cond.VarPos) of
    mmpStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_Stk(Jmp, Offset, Pos);
        2: Result := _JmpRIf16_Stk(Jmp, Offset, Pos);
        4: Result := _JmpRIf32_Stk(Jmp, Offset, Pos);
        8: Result := _JmpRIf64_Stk(Jmp, Offset, Pos);
        else e := True;
      end;
    mmpVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        2: Result := _JmpRIf16_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        4: Result := _JmpRIf32_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        8: Result := _JmpRIf64_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        else e := True;
      end;
    mmpPtr:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        2: Result := _JmpRIf16_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        4: Result := _JmpRIf32_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        8: Result := _JmpRIf64_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        else e := True;
      end;
    mmpPStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_PStk(Jmp, Offset, Pos);
        2: Result := _JmpRIf16_PStk(Jmp, Offset, Pos);
        4: Result := _JmpRIf32_PStk(Jmp, Offset, Pos);
        8: Result := _JmpRIf64_PStk(Jmp, Offset, Pos);
        else e := True;
      end;
    mmpPVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        2: Result := _JmpRIf16_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        4: Result := _JmpRIf32_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        8: Result := _JmpRIf64_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        else e := True;
      end;
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._JmpRIf(Jmp: TCodeOffset; Cond: TResVar; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _JmpRIf(Jmp, Cond, Offset, Pos);
end;

function TLapeCodeEmitter._JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  e: Boolean;
begin
  Assert(Cond.HasType());
  e := False;

  case getMemoryPos(Cond.VarPos) of
    mmpStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_Stk(Jmp, Offset, Pos);
        2: Result := _JmpRIfNot16_Stk(Jmp, Offset, Pos);
        4: Result := _JmpRIfNot32_Stk(Jmp, Offset, Pos);
        8: Result := _JmpRIfNot64_Stk(Jmp, Offset, Pos);
        else e := True;
      end;
    mmpVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        2: Result := _JmpRIfNot16_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        4: Result := _JmpRIfNot32_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        8: Result := _JmpRIfNot64_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        else e := True;
      end;
    mmpPtr:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        2: Result := _JmpRIfNot16_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        4: Result := _JmpRIfNot32_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        8: Result := _JmpRIfNot64_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        else e := True;
      end;
    mmpPStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_PStk(Jmp, Offset, Pos);
        2: Result := _JmpRIfNot16_PStk(Jmp, Offset, Pos);
        4: Result := _JmpRIfNot32_PStk(Jmp, Offset, Pos);
        8: Result := _JmpRIfNot64_PStk(Jmp, Offset, Pos);
        else e := True;
      end;
    mmpPVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        2: Result := _JmpRIfNot16_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        4: Result := _JmpRIfNot32_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        8: Result := _JmpRIfNot64_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        else e := True;
      end;
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _JmpRIfNot(Jmp, Cond, Offset, Pos);
end;

function TLapeCodeEmitter._Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  d, l, r: EMyMemoryPos;
  StackIncD, StackIncL, StackIncR: TStackInc;
  e: Boolean;
begin
  Assert({$IFNDEF FPC}@{$ENDIF}AProc <> nil);
  Assert(Dest.HasType());
  if (not Left.HasType()) then
    Left.VarType := Dest.VarType;

  d := getMemoryPos(Dest.VarPos);
  l := getMemoryPos(Left.VarPos);
  r := getMemoryPos(Right.VarPos);
  e := False;

  StackIncD := Dest.VarType.Size;
  if (Left.VarPos.MemPos = mpStack) and Left.VarPos.ForceVariable then
    StackIncL := 0
  else if Left.VarPos.isPointer then
    StackIncL := SizeOf(Pointer)
  else
    StackIncL := Left.VarType.Size;
  if (not Right.HasType()) or ((Right.VarPos.MemPos = mpStack) and Right.VarPos.ForceVariable) then
    StackIncR := 0
  else if Right.VarPos.isPointer then
    StackIncR := SizeOf(Pointer)
  else
    StackIncR := Right.VarType.Size;

  {$I lpcodeemitter_evalcase.inc}

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _Eval(AProc, Dest, Left, Right, Offset, Pos);
end;

procedure TLapeCompilerBase.setEmitter(AEmitter: TLapeCodeEmitter);
begin
  if FreeEmitter and (FEmitter <> nil) then
    FEmitter.Free();
  FEmitter := AEmitter;
  if (FEmitter <> nil) then
    FEmitter.Reset();
end;

procedure TLapeCompilerBase.Reset;
begin
  FOptions := FBaseOptions;
  FOptions_PackRecords := FBaseOptions_PackRecords;

  if (FEmitter <> nil) then
    FEmitter.Reset();
  while (DecStackInfo(False, False, (FStackInfo <> nil) and (FStackInfo.Owner = nil)) <> nil) do ;
end;

constructor TLapeCompilerBase.Create(AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True);
begin
  inherited Create();

  FBaseOptions := Lape_OptionsDef;
  FBaseOptions_PackRecords := Lape_PackRecordsDef;

  FOnHint := nil;

  FreeEmitter := ManageEmitter;
  if (AEmitter = nil) then
    AEmitter := TLapeCodeEmitter.Create();
  FEmitter := AEmitter;

  LoadBaseTypes(FBaseTypes, Self);
  FStackInfo := nil;

  FGlobalDeclarations := TLapeDeclarationList.Create(nil);
  FManagedDeclarations := TLapeDeclarationList.Create(nil);
  FCachedDeclarations := TLapeVarMap.Create(nil, dupIgnore, True, '', True);
end;

destructor TLapeCompilerBase.Destroy;
begin
  Clear();
  setEmitter(nil);

  FreeAndNil(FGlobalDeclarations);
  FreeAndNil(FManagedDeclarations);
  FreeAndNil(FCachedDeclarations);
  ClearBaseTypes(FBaseTypes, True);

  inherited;
end;

procedure TLapeCompilerBase.Clear;
begin
  ClearBaseTypes(FBaseTypes, False);
  FGlobalDeclarations.Delete(TLapeVar, True);
  FManagedDeclarations.Delete(TLapeVar, True);
  FGlobalDeclarations.Clear();
  FManagedDeclarations.Clear();
  FCachedDeclarations.Clear();
  Reset();
end;

procedure TLapeCompilerBase.VarToDefault(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil);
begin
  FinalizeVar(AVar, Offset, Pos);
end;

procedure TLapeCompilerBase.VarToDefault(AVar: TResVar; Pos: PDocPos = nil);
var
  Offset: Integer;
begin
  Offset := -1;
  VarToDefault(AVar, Offset, Pos);
end;

procedure TLapeCompilerBase.FinalizeVar(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil);
begin
  if (AVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) and (AVar.VarType <> nil) then
    AVar.VarType.Finalize(AVar, Offset, True, Pos);
end;

procedure TLapeCompilerBase.FinalizeVar(AVar: TResVar; Pos: PDocPos = nil);
var
  Offset: Integer;
begin
  Offset := -1;
  FinalizeVar(AVar, Offset, Pos);
end;

function TLapeCompilerBase.PopVarToStack(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  Result := AVar;
  if (Result.VarPos.MemPos = mpVar) and Result.HasType() and (Result.DecLock().VarType.Size > 0) then
  begin
    Emitter._PopVarToStack(Result.VarType.Size, Result.VarPos.StackVar.Offset + Result.VarPos.Offset, Offset, Pos);
    Result.VarPos.MemPos := mpStack;
    Result.VarPos.ForceVariable := False;
  end;
end;

function TLapeCompilerBase.IncStackInfo(AStackInfo: TLapeStackInfo; var Offset: Integer; Emit: Boolean = True; Pos: PDocPos = nil): TLapeStackInfo;
begin
  if (AStackInfo <> nil) and (AStackInfo <> EmptyStackInfo) then
  begin
    if (AStackInfo <> FStackInfo) then
      AStackInfo.Owner := FStackInfo;

    AStackInfo.FOldMaxStack := Emitter.MaxStack;
    AStackInfo.FOldStackPos := Emitter.NewStack();

    if Emit then
    begin
      Emitter.FullEmit := False;
      AStackInfo.CodePos := Emitter._IncTry(0, Try_NoExcept, Offset, Pos);
      Emitter._ExpandVar(0, Offset, Pos);
      Emitter._InitStackLen(0, Offset, Pos);
      Emitter.FullEmit := True;
    end;
  end;

  FStackInfo := AStackInfo;
  Result := FStackInfo;
end;

function TLapeCompilerBase.IncStackInfo(var Offset: Integer; Emit: Boolean = True; Pos: PDocPos = nil): TLapeStackInfo;
begin
  Result := IncStackInfo(TLapeStackInfo.Create(lcoAlwaysInitialize in FOptions, lcoFullDisposal in FOptions, FStackInfo), Offset, Emit, Pos);
end;

function TLapeCompilerBase.IncStackInfo(Emit: Boolean = False): TLapeStackInfo;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := IncStackInfo(Offset, Emit);
end;

function TLapeCompilerBase.DecStackInfo(var Offset: Integer; InFunction: Boolean = False; Emit: Boolean = True; DoFree: Boolean = False; Pos: PDocPos = nil): TLapeStackInfo;
var
  i,
  CodePos, IncTryJump, InitStackPos: Integer;
  Item: TLapeVar;

  procedure RemoveIncTry;
  begin
    Emitter.Delete(FStackInfo.CodePos, ocSize + SizeOf(TOC_IncTry), Offset);
    Dec(InitStackPos, ocSize + SizeOf(TOC_IncTry));
  end;

  procedure RemoveExpandVar;
  begin
    Emitter.Delete(FStackInfo.CodePos + ocSize + SizeOf(TOC_IncTry), ocSize + SizeOf(TStackOffset), Offset);
    Dec(InitStackPos, ocSize + SizeOf(TStackOffset));
  end;

  procedure RemoveInitStack;
  begin
    Emitter.Delete(InitStackPos, ocSize + SizeOf(TStackOffset), Offset);
    if (IncTryJump > 0) then
      Emitter._IncTry(IncTryJump - ocSize - SizeOf(TStackOffset), Try_NoExcept, FStackInfo.CodePos, Pos);
  end;

  function NeedFinalization(v: TLapeVar): Boolean;
  begin
    Result := v <> nil;
    if Result then
      if (not FStackInfo.FullDisposal) then
        Result := v.NeedFinalization
      else if (v is TLapeParameterVar) then
        Result := (not (TLapeParameterVar(v).ParType in Lape_RefParams));
  end;

begin
  if (FStackInfo = nil) or (FStackInfo = EmptyStackInfo) then
    Result := nil
  else
  begin
    Result := FStackInfo.Owner;

    if Emit then
    begin
      CodePos := FStackInfo.CodePos;
      IncTryJump := 0;
      InitStackPos := CodePos + ocSize*2 + SizeOf(TOC_IncTry) + SizeOf(TStackOffset);

      if (FStackInfo.TotalNoParamSize <= 0) then
        RemoveExpandVar();
      if (FStackInfo.TotalSize <= 0) and (not InFunction) then
        RemoveIncTry()
      else
      begin
        Emitter._DecTry(Offset, Pos);
        IncTryJump := Offset - CodePos;
        Emitter._IncTry(IncTryJump, Try_NoExcept, CodePos, Pos);

        with FStackInfo.VarStack do
        begin
          i := 0;
          while (i < Count) do
          begin
            Item := Items[i];
            if NeedFinalization(Item) then
            begin
              FinalizeVar(_ResVar.New(Item), Offset, Pos);
              if (Item is TLapeStackTempVar) then
              begin
                //if TLapeStackTempVar(Item).Locked then
                //  WriteLn(Item.Name, ' ', Item.VarType.AsString, ' still locked! ', TLapeStackTempVar(Item).FLock);
                TLapeStackTempVar(Item).Locked := True;
              end;
            end;
            Inc(i);
          end;
          for i := 0 to Count - 1 do
            if (Item is TLapeStackTempVar) then
              TLapeStackTempVar(Item).Locked := False;
        end;

        if InFunction then
          Emitter._DecCall_EndTry(Offset, Pos)
        else
        begin
          Emitter._PopVar(Offset, Pos);
          Emitter._EndTry(Offset, Pos);
        end;

        if (not InFunction) then
          if FStackInfo.NeedInitialization then
            Emitter._ExpandVarAndInit(FStackInfo.TotalSize, CodePos, Pos)
          else
            Emitter._ExpandVar(FStackInfo.TotalSize, CodePos, Pos)
        else if (FStackInfo.TotalNoParamSize > 0) then
          if FStackInfo.NeedInitialization then
            Emitter._GrowVarAndInit(FStackInfo.TotalNoParamSize, CodePos, Pos)
          else
            Emitter._GrowVar(FStackInfo.TotalNoParamSize, CodePos, Pos);
      end;

      if (Emitter.MaxStack > 0) then
        Emitter._InitStackLen(Emitter.MaxStack, InitStackPos, Pos)
      else
        RemoveInitStack();
    end
    else
      FStackInfo.FullDisposal := lcoFullDisposal in FOptions;

    Emitter.NewStack(FStackInfo.FOldStackPos, FStackInfo.FOldMaxStack);
    if DoFree then
      FStackInfo.Free();
    FStackInfo := Result;
  end;
end;

function TLapeCompilerBase.DecStackInfo(InFunction: Boolean = False; Emit: Boolean = False; DoFree: Boolean = False): TLapeStackInfo;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := DecStackInfo(Offset, InFunction, Emit, DoFree);
end;

procedure TLapeCompilerBase.EmitCode(ACode: lpString; AVarNames: array of lpString; AVars: array of TLapeVar; AResVars: array of TResVar; var Offset: Integer; Pos: PDocPos = nil);
var
  FreeStack: Boolean;
  VarRefs: TLapeType_VarRefMap;
  VarRefsVar: TResVar;
  WithVar: TLapeWithDeclRec;
  i: Integer;
begin
  Assert(Length(AVarNames) = Length(AVars) + Length(AResVars));
  VarRefs := nil;
  VarRefsVar := NullResVar;
  WithVar := NullWithDecl;

  FreeStack := (FStackInfo = nil);
  if FreeStack then
    IncStackInfo(Offset, True, Pos);

  try
    VarRefs := TLapeType_VarRefMap.Create(Self);
    for i := 0 to High(AVarNames) do
      if (i < Length(AVars)) then
        VarRefs.addVar(AVars[i], AVarNames[i])
      else
        VarRefs.addVar(AResVars[i - Length(AVars)], AVarNames[i]);

    VarRefs.addVar(getGlobalVar('System'), 'System');
    VarRefsVar := _ResVar.New(VarRefs.NewGlobalVarP());

    WithVar.WithType := VarRefs;
    WithVar.WithVar := @VarRefsVar;
    FStackInfo.addWith(WithVar);

    try
      EmitCode(ACode, Offset, Pos);
    finally
      FStackInfo.delWith(1);
    end;
  finally
    if (VarRefsVar.VarPos.GlobalVar <> nil) then
      VarRefsVar.VarPos.GlobalVar.Free();
    if (VarRefs <> nil) then
      VarRefs.Free();
    if FreeStack then
      DecStackInfo(Offset, False, True, True, Pos);
  end;
end;

function TLapeCompilerBase.getBaseType(Name: lpString): TLapeType;
var
  BaseType: ELapeBaseType;
begin
  Name := LapeCase(Name);
  for BaseType := Low(FBaseTypes) to High(FBaseTypes) do
    if (FBaseTypes[BaseType] <> nil) and (LapeCase(FBaseTypes[BaseType].Name) = Name) then
      Exit(FBaseTypes[BaseType]);
  Result := nil;
end;

function TLapeCompilerBase.getBaseType(BaseType: ELapeBaseType): TLapeType;
begin
  Result := FBaseTypes[BaseType];
end;

function TLapeCompilerBase.addLocalDecl(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration;
begin
  Result := ADecl;
  if (ADecl <> nil) then
    if (AStackInfo = nil) then
      FGlobalDeclarations.addDeclaration(ADecl)
    else
      AStackInfo.addDeclaration(ADecl);
end;

function TLapeCompilerBase.addLocalDecl(ADecl: TLapeDeclaration): TLapeDeclaration;
begin
  Result := addLocalDecl(ADecl, FStackInfo);
end;

function TLapeCompilerBase.addGlobalDecl(ADecl: TLapeDeclaration): TLapeDeclaration;
begin
  Result := ADecl;
  if (ADecl <> nil) then
    FGlobalDeclarations.addDeclaration(ADecl);
end;

function TLapeCompilerBase.addManagedDecl(ADecl: TLapeDeclaration): TLapeDeclaration;
begin
  Result := ADecl;
  if (ADecl <> nil) and (ADecl.DeclarationList = nil) then
    FManagedDeclarations.addDeclaration(ADecl);
end;

function TLapeCompilerBase.addManagedVar(AVar: TLapeVar; PtrCheckOnly: Boolean = False): TLapeVar;
{$IFDEF Lape_SmallCode}
var
  i: Integer;
  GlobalVars: TLapeDeclArray;
{$ENDIF}
begin
  if (AVar = nil) or (AVar.DeclarationList <> nil) then
    Exit(AVar);
  {$IFDEF Lape_SmallCode}
  if (AVar is TLapeGlobalVar) and AVar.HasType() and AVar.isConstant and (AVar.Name = '') and
     (PtrCheckOnly or (AVar.VarType.EvalRes(op_cmp_Equal, AVar.VarType) <> nil))
  then
  begin
    GlobalVars := FManagedDeclarations.getByClass(TLapeDeclarationClass(AVar.ClassType), bTrue, True);
    for i := 0 to High(GlobalVars) do
      if (AVar = GlobalVars[i]) then
        Exit(AVar)
      else with TLapeGlobalVar(GlobalVars[i]) do
        if DoManage and isConstant and (Name = '') and HasType() and VarType.Equals(TLapeGlobalVar(AVar).VarType, False) then
          if (PtrCheckOnly and (Ptr = TLapeGlobalVar(AVar).Ptr)) or
             ((not PtrCheckOnly) and Equals(TLapeGlobalVar(AVar)))
          then
          begin
            AVar.Free();
            Exit(TLapeGlobalVar(GlobalVars[i]));
          end;
  end;
  {$ENDIF}

  Result := addManagedDecl(AVar) as TLapeVar;
  {$IFNDEF Lape_SmallCode}
  if (AVar is TLapeGlobalVar) then
    with AVar as TLapeGlobalVar do
      if HasType() and Readable and (Name = '') and ((AsString <> '') or (BaseType <> ltUnknown)) then
        FCachedDeclarations.Add(AsString + ':' + LapeTypeToString(BaseType), TLapeGlobalVar(AVar));
  {$ENDIF}
end;

function TLapeCompilerBase.addManagedType(AType: TLapeType): TLapeType;
{$IFDEF Lape_SmallCode}
var
  i: Integer;
  Types: TLapeDeclArray;
{$ENDIF}
begin
  if (AType = nil) or (AType.DeclarationList <> nil) then
    Exit(AType);

  {$IFDEF Lape_SmallCode}
  Types := FManagedDeclarations.getByClass(TLapeDeclarationClass(AType.ClassType), bTrue, True);
  for i := 0 to High(Types) do
    if (AType = Types[i]) then
      Exit(AType)
    else if TLapeType(Types[i]).Equals(AType, False) then
    begin
      AType.Free();
      Exit(TLapeType(Types[i]));
    end;
  {$ENDIF}
  Result := addManagedDecl(AType) as TLapeType;
end;

function TLapeCompilerBase.addStackVar(VarType: TLapeType; Name: lpString): TLapeStackVar;
begin
  Assert(FStackInfo <> nil);
  Result := FStackInfo.addVar(VarType, Name);
  Assert(not (Result is TLapeStackTempVar));
end;

function TLapeCompilerBase.getCachedConstant(Str: lpString; BaseType: ELapeBaseType = ltUnknown): TLapeGlobalVar;
begin
  Assert(getBaseType(BaseType) <> nil);

  {$IFNDEF Lape_SmallCode}
  if (BaseType in LapeStringTypes) then
    Result := FCachedDeclarations['"' + Str + '":' + LapeTypeToString(BaseType)]
  else
    Result := FCachedDeclarations[Str + ':' + LapeTypeToString(BaseType)];
  if (Result = nil) then
  {$ENDIF}

  Result := addManagedVar(getBaseType(BaseType).NewGlobalVarStr(Str)) as TLapeGlobalVar;
end;

function TLapeCompilerBase.getConstant(Str: lpString; BaseType: ELapeBaseType = ltString; DoGrow: Boolean = False; ForceType: Boolean = False): TLapeGlobalVar;
begin
  if (BaseType in LapeIntegerTypes) or ((BaseType = ltUnknown) and (not ForceType)) then
  begin
    if (BaseType = ltUnknown) then
      BaseType := DetermineIntType(Str)
    else if (not ForceType) then
      BaseType := DetermineIntType(Str, BaseType, DoGrow);
    Assert(BaseType in LapeIntegerTypes);
  end;

  Result := getCachedConstant(Str, BaseType);
end;

function TLapeCompilerBase.getConstant(i: Int64; IntType: ELapeBaseType = ltNativeInt; DoGrow: Boolean = False; ForceType: Boolean = False): TLapeGlobalVar;
begin
  Result := getConstant(lpString(IntToStr(i)), IntType, DoGrow, ForceType);
end;

function TLapeCompilerBase.getLabel(CodePos: Integer): TLapeGlobalVar;
begin
  Emitter.CheckOffset(CodePos);
  Result := addManagedVar(getGlobalType('!label').NewGlobalVarP(), True) as TLapeGlobalVar;

  PCodePos(Result.Ptr)^ := CodePos;
  Emitter.addCodePointer(Result.Ptr);
end;

procedure TLapeCompilerBase.getDestVar(var Dest, Res: TResVar; Op: EOperator);
begin
  if (op in AssignOperators) then
    Dest := NullResVar
  else if (op <> op_Deref) and (Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and
    Res.HasType() and Res.VarType.Equals(Dest.VarType, False)
  then
    Res := Dest
  else
  begin
    if (not Res.HasType()) or ((op <> op_Deref) and (
         ((Dest.VarPos.MemPos = mpStack) and (not Dest.HasType())) or
         ((Res.VarType.BaseType in LapeStackTypes) and
           ((Dest.VarPos.MemPos <> mpVar) or Dest.HasType())
       )))
    then
      Res.VarPos.MemPos := mpStack
    else
      with Res, VarPos do
      begin
        MemPos := mpVar;
        if (op = op_Deref) then
          StackVar := getTempVar(ltPointer)
        else
          StackVar := getTempVar(Res.VarType);
        StackVar.isConstant := True;
      end;
    Dest := NullResVar;
  end;
end;

function TLapeCompilerBase.getTempVar(VarType: ELapeBaseType; Lock: Integer = 1): TLapeStackTempVar;
begin
  Result := getTempVar(FBaseTypes[VarType], Lock);
end;

function TLapeCompilerBase.getTempVar(VarType: TLapeType; Lock: Integer = 1): TLapeStackTempVar;
begin
  Result := FStackInfo.getTempVar(VarType, Lock);
end;

function TLapeCompilerBase.getTempStackVar(VarType: ELapeBaseType): TResVar;
begin
  Result := getTempStackVar(FBaseTypes[VarType]);
end;

function TLapeCompilerBase.getTempStackVar(VarType: TLapeType): TResVar;
begin
  Result := StackResVar;
  Result.VarType := VarType;
end;

function TLapeCompilerBase.getPointerType(PType: ELapeBaseType; PConst: Boolean): TLapeType_Pointer;
begin
  Result := getPointerType(FBaseTypes[PType], PConst);
end;

function TLapeCompilerBase.getPointerType(PType: TLapeType; PConst: Boolean): TLapeType_Pointer;
begin
  Result := TLapeType_Pointer(addManagedType(TLapeType_Pointer.Create(Self, PType, PConst)));
end;

function TLapeCompilerBase.getTypeVar(AType: ELapeBaseType): TLapeGlobalVar;
begin
  Result := getTypeVar(FBaseTypes[AType]);
end;

function TLapeCompilerBase.getTypeVar(AType: TLapeType): TLapeGlobalVar;
var
  TType: TLapeTTypeClass;
begin
  if (AType is TLapeType_Enum) then
    TType := TLapeType_TypeEnum
  else
    TType := TLapeType_Type;
  Result := addManagedVar(addManagedType(TType.Create(AType, Self)).NewGlobalVarP()) as TLapeGlobalVar;
end;

function TLapeCompilerBase.getGlobalVar(AName: lpString): TLapeGlobalVar;
var
  Declarations: TLapeDeclArray;
begin
  Declarations := GlobalDeclarations.getByClassAndName(AName, TLapeGlobalVar, bTrue);
  if (Length(Declarations) > 1) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName])
  else if (Length(Declarations) > 0) and (Declarations[0] <> nil) then
    Result := Declarations[0] as TLapeGlobalVar
  else
    Result := nil;
end;

function TLapeCompilerBase.getGlobalType(AName: lpString): TLapeType;
var
  Declarations: TLapeDeclArray;
begin
  Declarations := GlobalDeclarations.getByClassAndName(AName, TLapeType, bTrue);
  if (Length(Declarations) > 1) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName])
  else if (Length(Declarations) > 0) and (Declarations[0] <> nil) then
    Result := Declarations[0] as TLapeType
  else
    Result := nil;
end;

function TLapeCompilerBase.getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration;
var
  Declarations: TLapeDeclArray;
  Stack: TLapeStackInfo;
begin
  Stack := AStackInfo;
  while (Stack <> nil) do
  begin
    if LocalOnly then
      Result := Stack.getDeclaration(AName, bUnknown, CheckWith)
    else
      Result := Stack.getDeclaration(AName, bTrue, CheckWith);

    if (Result is TLapeStackVar) and (TLapeStackVar(Result).Stack <> AStackInfo.VarStack) then
    begin
      Result := AStackInfo.getInheritedVar(TLapeStackVar(Result));
      if (Result = nil) then
        LapeExceptionFmt(lpeDeclarationOutOfScope, [AName]);
    end;

    if (Result <> nil) or LocalOnly then
      Exit;
    Stack := Stack.Owner;
  end;

  Declarations := GlobalDeclarations.getByName(AName, bTrue);
  if (Length(Declarations) > 1) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName])
  else if (Length(Declarations) > 0) and (Declarations[0] <> nil) then
    Result := Declarations[0]
  else
    Result := getBaseType(AName);
end;

function TLapeCompilerBase.getDeclaration(AName: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration;
begin
  Result := getDeclaration(AName, FStackInfo, LocalOnly, CheckWith);
end;

function TLapeCompilerBase.hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean;
begin
  if (AStackInfo <> nil) then
  begin
    if LocalOnly then
      Result := AStackInfo.hasDeclaration(AName, bUnknown, CheckWith)
    else
      Result := AStackInfo.hasDeclaration(AName, bTrue, CheckWith);

    if Result or LocalOnly then
      Exit;
  end;

  if (Length(GlobalDeclarations.getByName(AName, bTrue)) > 0) then
    Result := True
  else
    Result := getBaseType(AName) <> nil;
end;

function TLapeCompilerBase.hasDeclaration(AName: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean;
begin
  Result := hasDeclaration(AName, FStackInfo, LocalOnly, CheckWith);
end;

function TLapeCompilerBase.hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean;
begin
  if (AStackInfo <> nil) then
  begin
    if LocalOnly then
      Result := AStackInfo.hasDeclaration(ADecl, bUnknown, CheckWith)
    else
      Result := AStackInfo.hasDeclaration(ADecl, bTrue, CheckWith);
    if Result or LocalOnly then
      Exit;
  end;

  if GlobalDeclarations.HasSubDeclaration(ADecl, bFalse) then
    Result := True
  else if (ADecl <> nil) then
    Result := (getBaseType(ADecl.Name) <> nil)
  else
    Result := False;
end;

function TLapeCompilerBase.hasDeclaration(ADecl: TLapeDeclaration; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean;
begin
  Result := hasDeclaration(ADecl, FStackInfo, LocalOnly, CheckWith);
end;

procedure TLapeCompilerBase.Hint(Msg: lpString; Args: array of const; ADocPos: TDocPos);
begin
  if ({$IFNDEF FPC}@{$ENDIF}FOnHint <> nil) then
    FOnHint(Self, FormatLocation(Format(Msg, Args), ADocPos));
end;

initialization
  LapeReservedLocals := LapeCase(LapeReservedLocals);
  EmptyStackInfo := TLapeEmptyStack.Create();
finalization
  EmptyStackInfo.Free();
end.


