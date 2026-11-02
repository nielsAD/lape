{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All (script)type and (script)variable classes, including corresponding evaluation functions (runtime/compile time).
}
unit lpvartypes;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lptokenizer, lpcodeemitter;

type
  ECompilerOption = (
    lcoAssertions,                     // {$C} {$ASSERTIONS}
    lcoRangeCheck,                     // {$R} {$RANGECHECKS}
    lcoAlwaysInitialize,               // {$M} {$MEMORYINIT}
    lcoLooseSemicolon,                 // {$L} {$LOOSESEMICOLON}
    lcoLooseSyntax,                    // {$X} {$EXTENDEDSYNTAX}
    lcoScopedEnums,                    // {$S} {$SCOPEDENUMS}
    lcoConstAddress,                   // {$J} {$CONSTADDRESS}
    lcoHints,                          // {$H} {$HINTS}
    lcoCOperators,                     //      {$COPERATORS}
    lcoAutoObjectify,                  //      {$AUTOOBJECTIFY}
    lcoVerboseCompile,                 //      {$VERBOSECOMPILE}
    lcoDuplicateLocalNameHints,        //      {$DUPLICATELOCALNAMEHINTS}
    lcoArrayHelpers,                   //
    lcoInheritableRecords,             //
    lcoRelativeFileNames,              //
    lcoInitExternalResult,             // Ensure empty result for external calls (useful for ffi)
    lcoMethodDeclarationParentheses    // {$METHODDECLARATIONPARENTHESES}  Require method declarations to have () if no parameters.
  );
  ECompilerOptionsSet = set of ECompilerOption;
  PCompilerOptionsSet = ^ECompilerOptionsSet;

  EStackFlags = (
    lsfEmit,
    lsfFunction,
    lsfGlobal
  );
  EStackFlagsSet = set of EStackFlags;

const
  Lape_OptionsDef = [lcoArrayHelpers, lcoCOperators, lcoRangeCheck, lcoHints, lcoAlwaysInitialize, lcoConstAddress, lcoInheritableRecords];
  Lape_PackRecordsDef = 8;

type
  TLapeType = class;
  TLapeVar = class;
  TLapeStackVar = class;
  TLapeStackInfo = class;
  TLapeGlobalVar = class;
  TLapeType_Method = class;
  TLapeCompilerBase = class;

  TLapeBaseTypes = array[ELapeBaseType] of TLapeType;
  TLapeTypeArray = array of TLapeType;
  TLapeVarStack = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeStackVar>;
  TLapeVarMap = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TLapeGlobalVar>;

  TVarPos = record
    isPointer: Boolean;
    Offset: Integer;
    case MemPos: EMemoryPos of
      mpVar: (StackVar: TLapeStackVar);
      mpMem: (GlobalVar: TLapeGlobalVar);
      mpStack: (ForceVariable: Boolean);
  end;

  TResVar = record
  public
    VarType: TLapeType;
    VarPos: TVarPos;
  private
    FConstant: TInitBool;

    function getConstant: Boolean;
    procedure setConstant(AConst: Boolean);
  public
    class function New(AVar: TLapeVar): TResVar; static;

    function HasType: Boolean;
    function InScope(AStack: TLapeStackInfo): TResVar;
    procedure IncOffset(Offset: Integer);

    property isConstant: Boolean read getConstant write setConstant;
  end;
  TResVarArray = array of TResVar;

  ELapeParameterType = (lptNormal, lptConst, lptConstRef, lptVar, lptOut);
  TLapeParameter = record
    ParType: ELapeParameterType;
    VarType: TLapeType;
    Default: TLapeVar;
    Name: lpString;
    DocPos: TDocPos;
  end;
  TLapeParameterList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeParameter>;

  TLapeVar = class(TLapeDeclaration)
  protected
    FConstant: Boolean;

    function getBaseType: ELapeBaseType; virtual;
    function getSize: SizeInt; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;

    procedure setConstant(AConst: Boolean); virtual;
  public
    VarType: TLapeType;

    constructor Create(AVarType: TLapeType; AName: lpString = ''; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    function HasType: Boolean;

    property BaseType: ELapeBaseType read getBaseType;
    property Size: SizeInt read getSize;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;

    property isConstant: Boolean read FConstant write setConstant;
  end;

  TLapeStackVar = class(TLapeVar)
  protected
    FStack: TLapeVarStack;
    procedure setStack(Stack: TLapeVarStack); virtual;
    function getOffset: Integer; virtual;
  public
    constructor Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    destructor Destroy; override;

    property Stack: TLapeVarStack read FStack write setStack;
    property Offset: Integer read getOffset;
  end;

  // Temp var for a single statement.
  // (see TLapeStackInfo.ReleaseTemps), after which another statement can reuse.
  TLapeStackTempVar = class(TLapeStackVar)
  protected
    FInUse: Boolean;
  public
    constructor Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; AList: TLapeDeclarationList = nil); override;
    property InUse: Boolean read FInUse;
  end;

  TLapeParameterVar = class(TLapeStackVar)
  protected
    FParType: ELapeParameterType;
    function getSize: SizeInt; override;
    function getInitialization: Boolean; override;
    function getFinalization: Boolean; override;
  public
    constructor Create(AParType: ELapeParameterType; AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; AList: TLapeDeclarationList = nil); reintroduce; virtual;
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
    FPtr: Pointer;
    FOwnsPtr: Boolean;
    FKnown: Boolean;
    function getAsString: lpString; virtual;
    function getAsInt: Int64; virtual;
    procedure setConstant(AConst: Boolean); override;
    procedure setKnown(AKnown: Boolean); virtual;
  public
    // With memory of its own
    constructor Create(AVarType: TLapeType; AName: lpString = ''; AList: TLapeDeclarationList = nil); reintroduce; overload; virtual;
    // What is at Ptr, which stays the caller's
    constructor Create(AVarType: TLapeType; Ptr: Pointer; AName: lpString = ''; AList: TLapeDeclarationList = nil); reintroduce; overload; virtual;
    destructor Destroy; override;

    function CreateCopy(CopyContent: Boolean = True): TLapeGlobalVar; virtual;

    property Ptr: Pointer read FPtr;
    property OwnsPtr: Boolean read FOwnsPtr;
    property AsString: lpString read getAsString;
    property AsInteger: Int64 read getAsInt;
    // what it holds is known when compiling
    property Known: Boolean read FKnown write setKnown;
  end;

  // A method. Several of a name can be declared side by side, see findMethods
  TLapeMethod = class(TLapeGlobalVar)
  end;

  ELapeEvalFlag = (lefAssigning, lefConstAddress, lefConstRangeCheck, lefRangeCheck);
  ELapeEvalFlags = set of ELapeEvalFlag;

  TLapeType = class(TLapeDeclaration)
  protected
    FAliasType: TLapeType;
    FParentType: TLapeType;
    FDeclarations: TLapeDeclarationList;
    FBaseType: ELapeBaseType;
    FCompiler: TLapeCompilerBase;
    FSize: SizeInt;
    FInit: TInitBool;
    FLo: TLapeGlobalVar;
    FHi: TLapeGlobalVar;
    FAsString: lpString;
    FEmptyValue: TLapeGlobalVar;
    FArrayOf: TLapeType;

    function getEvalRes(Op: EOperator; Left, Right: ELapeBaseType): ELapeBaseType; virtual;
    function getEvalProc(Op: EOperator; Left, Right: ELapeBaseType): TLapeEvalProc; virtual;

    procedure setBaseType(ABaseType: ELapeBaseType); virtual;
    function getPadding: SizeInt; virtual;
    function getSize: SizeInt; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
    function getAsString: lpString; virtual;
    function getEmptyValue: TLapeGlobalVar; virtual;
    function getArrayOf: TLapeType; virtual;
    function getDeclarations: TLapeDeclarationList; virtual;
    // Whether it has anything that Left.Name could be
    function HasMembers: Boolean; virtual;
  public
    TypeID: Integer;

    constructor Create(ABaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual;
    destructor Destroy; override;
    // A new type that starts out as this one: it has what this one declares after its own
    function CreateCopy: TLapeType; virtual;
    // Another name for this type: what is declared on one is declared on both
    function CreateAlias: TLapeType; virtual;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; reintroduce; virtual;
    function CompatibleWith(Other: TLapeType): Boolean; virtual;
    procedure ClearCache; virtual;

    // The body of its ToString. Nothing: it has no way of its own
    function VarToStringBody: lpString; virtual;
    function VarToString(AVar: Pointer): lpString; virtual;
    function VarToInt(AVar: Pointer): Int64; virtual;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; virtual;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; virtual;

    function NewGlobalVarP(Ptr: Pointer = nil; AName: lpString = ''): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: AnsiString; AName: lpString = ''): TLapeGlobalVar; overload; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar; overload; virtual;

    procedure addDeclaration(ADecl: TLapeDeclaration); virtual;
    procedure ClearDeclarations; override;

    // Declared on it, or on its ParentType and so on
    function getDeclaration(AName: lpString): TLapeDeclaration; virtual;
    // Its methods of the name and those of the types it comes from (a nearer one hides one that takes the same)
    function findMethods(AName: lpString): TLapeDeclArray; virtual;
    // What it has under the name. Nil: methods, which a call picks from
    function getMember(AName: lpString): TLapeDeclaration; virtual;
    // Whether AName is something of the type itself, which comes before its methods of the name (a field, an inline helper)
    function HasField(AName: lpString): Boolean; virtual;
    // Whether .AName means anything: a declaration, or what the type does itself (a field, an element, a helper)
    function HasMember(AName: lpString): Boolean; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; overload; virtual;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; overload; virtual;
    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; virtual;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; virtual;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; virtual;

    function EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType; virtual;
    function EvalMemberConst(Left: TLapeGlobalVar; AName: lpString; Flags: ELapeEvalFlags = []): TLapeGlobalVar; virtual;
    function EvalMember(var Dest: TResVar; Left: TResVar; AName: lpString; Flags: ELapeEvalFlags): TResVar; virtual;

    procedure FinalizeConst(AVar: TLapeGlobalVar); virtual;

    // The type that it is another name for (`type TFoo = TBar;`)
    property AliasType: TLapeType read FAliasType write FAliasType;
    // The type whose declarations it has after its own: the one it is a copy of (of an array: its helpers)
    property ParentType: TLapeType read FParentType write FParentType;
    property Compiler: TLapeCompilerBase read FCompiler;
    property BaseType: ELapeBaseType read FBaseType write setBaseType;
    property Padding: SizeInt read getPadding;
    property Size: SizeInt read getSize;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
    property AsString: lpString read getAsString;
    // A constant of it, all zeros
    property EmptyValue: TLapeGlobalVar read getEmptyValue;
    // array of it (a TLapeType_DynArray)
    property ArrayOf: TLapeType read getArrayOf;
    // (what is declared on it; a method of it goes in here as it is, checked by who declares it, see TLapeMethodParser)
    property Declarations: TLapeDeclarationList read getDeclarations;
  end;

  TLapeType_Pointer = class(TLapeType)
  protected
    FPType: TLapeType;
    FPConst: Boolean;
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; PointerType: TLapeType = nil; ConstPointer: Boolean = True; AName: lpString = ''); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;

    function VarToStringBody: lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar; override;

    function HasType: Boolean;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;

    property PType: TLapeType read FPType;
    property PConst: Boolean read FPConst;
  end;

  TLapeType_StrictPointer = class(TLapeType_Pointer)
  protected
    function getAsString: lpString; override;
  public
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
  end;

  TLapeType_NilPointer = class(TLapeType_Pointer);

  TLapeType_Label = class(TLapeType_Pointer)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;
  end;

  TLapeType_Method = class(TLapeType)
  protected
    FParams: TLapeParameterList;
    FObjectType: TLapeType;
    procedure setBaseType(ABaseType: ELapeBaseType); override;
    function getSize: SizeInt; override;
    function getAsString: lpString; override;
    function getParamSize: SizeInt; virtual;
  public
    ImplicitParams: Integer;
    Res: TLapeType;
    MethodDef: EMethodDef;

    constructor Create(ACompiler: TLapeCompilerBase; ARes: TLapeType = nil; AName: lpString = ''); reintroduce; overload; virtual;
    constructor Create(ACompiler: TLapeCompilerBase; AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType = nil; AName: lpString = ''); reintroduce; overload; virtual;
    constructor Create(AMethod: TLapeType_Method); overload; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;
    function VarToStringBody: lpString; override;

    function EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True; IgnoreDefault: Boolean = False): Boolean; virtual;
    procedure addParam(Param: TLapeParameter); virtual;

    // (a method declaration, of the host or of the script: a TLapeMethod)
    function NewGlobalVar(Ptr: Pointer = nil): TLapeGlobalVar; overload; virtual;
    function NewGlobalVar(CodePos: TCodePos): TLapeGlobalVar; overload; virtual;
    // Method, of this type, with SelfVar: a TMethod of the two
    function BindSelf(Method: TLapeGlobalVar; SelfVar: TResVar): TResVar; virtual;
    function ParamTypes: TLapeTypeArray; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;

    property Params: TLapeParameterList read FParams;
    // The type it is declared on (a static method has this too)
    property ObjectType: TLapeType read FObjectType write FObjectType;
    property ParamSize: SizeInt read getParamSize;
  end;

  TLapeType_MethodOfObject = class(TLapeType_Method)
  protected
    FMethodRecord: TLapeType;

    function getSize: SizeInt; override;
    function getAsString: lpString; override;
    function getParamSize: SizeInt; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ARes: TLapeType = nil; AName: lpString = ''); override;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;
    function VarToStringBody: lpString; override;

    function NewGlobalVar(AMethod: TMethod): TLapeGlobalVar; overload; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;
  end;

  TLapeType_MethodOfType = class(TLapeType_MethodOfObject)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AObjType: TLapeType; ARes: TLapeType = nil; AName: lpString = ''); reintroduce; virtual;

    function CreateCopy: TLapeType; override;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;
  end;

  // Asked for a method that is not there (ToString of a type, arr.Sort), see addMethodGenerate. nil: not one of its own
  TLapeGenerateMethod = function(AName: lpString; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar of object;
  TLapeGenerateMethods = array of TLapeGenerateMethod;

  TLapeWithDeclaration = class(TLapeDeclaration)
  public
    WithType: TLapeType;
    WithVar: TResVar;

    constructor Create(AWithType: TLapeType); reintroduce; virtual;
    function HasMember(AName: lpString): Boolean; virtual;
  end;
  TLapeWithDeclarationList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeWithDeclaration>;

  // What is declared in a scope (variables, methods, types etc)
  TLapeStackInfo = class(TLapeDeclarationList)
  protected
    FVarStack: TLapeVarStack;
    FWithStack: TLapeWithDeclarationList;
    FResultVar: TLapeStackVar;
    FOldStackPos: Integer;
    FOldMaxStack: Integer;
    FTemps: TLapeVarStack; // the temporary variables that are handed out, in that order

    function getVar(Index: Integer): TLapeStackVar; virtual;
    function getVarCount: Integer; virtual;
    function getTotalSize: SizeInt; virtual;
    function getTotalParamSize: SizeInt; virtual;
    function getTotalNoParamSize: SizeInt; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
  protected
    FOwner: TLapeStackInfo;
    FAround: TLapeStackInfo;
    FDepth: Integer;
    FRoot: TLapeStackInfo;
    FCompiler: TLapeCompilerBase;
    FMethod: TLapeType;
    FCodePos: Integer;
    FForceInitialization: Boolean;

    // (only the current scope, or the one about to be, gets another owner: no scope in it keeps an old depth)
    procedure setOwner(AOwner: TLapeStackInfo); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AlwaysInitialize: Boolean = True; AOwner: TLapeStackInfo = nil); reintroduce; virtual;
    destructor Destroy; override;

    // The scope around it (nil is global); how many are around it (0 is global); the global one
    property Owner: TLapeStackInfo read FOwner write setOwner;
    // Where it was declared: Owner changes when a method is compiled, after the rest is read
    property Around: TLapeStackInfo read FAround;
    property Depth: Integer read FDepth;
    property Root: TLapeStackInfo read FRoot;
    property Compiler: TLapeCompilerBase read FCompiler;
    // The method that it is the stack of (nil: none)
    property Method: TLapeType read FMethod write FMethod;
    property CodePos: Integer read FCodePos write FCodePos;
    property ForceInitialization: Boolean read FForceInitialization write FForceInitialization;

    // Its variables go, and what else is declared in it goes to the compiler's managed declarations
    procedure Clear; override;
    // Its variables go: what else is declared in it stays
    procedure ClearVars; virtual;

    // Of a with (CheckWith; a new declaration, that the caller frees), then here
    function getDeclaration(AName: lpString; CheckWith: Boolean): TLapeDeclaration; virtual;
    function hasDeclaration(AName: lpString; CheckWith: Boolean): Boolean; virtual;
    // The methods of the name from here outwards, as getDeclaration looks (what is not a method stops it)
    function findMethods(AName: lpString): TLapeDeclArray; virtual;
    function findMethod(AName: lpString; AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    // To release every temporary variable that is handed out from here on
    // Before statement: note the count of TLapeStackTempVar
    // After statement: count down to mark, marking as unused now.
    function TempMark: Integer; virtual;
    procedure ReleaseTemps(Mark: Integer); virtual;
    function getTempVar(VarType: TLapeType): TLapeStackTempVar; virtual;

    procedure addDeclaration(ADecl: TLapeDeclaration); override;
    // Whether AName can not be declared here: Self, in a method of a type or what is in one
    function isReserved(AName: lpString): Boolean; virtual;
    function addVar(StackVar: TLapeStackVar): TLapeStackVar; overload; virtual;
    // (no name: a temporary; of the global scope: a global variable; else one on the stack)
    function addVar(VarType: TLapeType; AName: lpString = ''): TLapeVar; overload; virtual;
    function addVar(ParType: ELapeParameterType; VarType: TLapeType; AName: lpString = ''): TLapeStackVar; overload; virtual;
    function addSelfVar(ParType: ELapeParameterType; VarType: TLapeType): TLapeStackVar; virtual;
    function addResultVar(VarType: TLapeType): TLapeStackVar; virtual;

    function inheritVar(StackVar: TLapeStackVar): TLapeStackVar; virtual;
    function getInheritedVar(StackVar: TLapeStackVar): TLapeStackVar; virtual;

    // -1: its type has nothing that it could find, and it is not added
    function addWith(AWith: TLapeWithDeclaration): Integer; virtual;
    procedure delWith(ACount: Integer); virtual;

    property VarStack: TLapeVarStack read FVarStack;
    property WithStack: TLapeWithDeclarationList read FWithStack;
    property ResultVar: TLapeStackVar read FResultVar;
    property Vars[Index: Integer]: TLapeStackVar read getVar; default;
    property VarCount: Integer read getVarCount;
    property TotalSize: SizeInt read getTotalSize;
    property TotalParamSize: SizeInt read getTotalParamSize;
    property TotalNoParamSize: SizeInt read getTotalNoParamSize;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
    property OldStackPos: Integer read FOldStackPos write FOldStackPos;
    property OldMaxStack: Integer read FOldMaxStack write FOldMaxStack;
  end;

  TLapeCodeEmitter = class(TLapeCodeEmitterBase)
  public
    function _IncCall(ACodePos: TResVar; AParamSize: TParamSize): Integer;

    function _InvokeImportedProc(AMemPos: TResVar; AParamSize: TParamSize): Integer;
    function _InvokeImportedFunc(AMemPos, AResPos: TResVar; AParamSize: TParamSize): Integer;

    function _JmpRIf(Jmp: TCodeOffset; Cond: TResVar): Integer; virtual;
    function _JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar): Integer; virtual;

    function _Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar): Integer; virtual;
  end;

  TLapeHint = procedure(Sender: TLapeCompilerBase; Msg: lpString) of object;

  // no strings or floats
  TLapeConstantList = class(TLapeBaseClass)
  protected type
    TList = {$IFDEF FPC}specialize{$ENDIF} TLapeHashList<TLapeGlobalVar>;
  protected
    FList: TList;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Get(Compiler: TLapeCompilerBase; BaseType: ELapeBaseType; Value: Pointer): TLapeGlobalVar; virtual;
    function ToString: ansistring; override;
  end;

  TLapeCompilerBase = class(TLapeBaseDeclClass)
  protected
    FEmitter: TLapeCodeEmitter;
    FStackInfo: TLapeStackInfo;
    FBaseTypes: TLapeBaseTypes;
    FManagedDeclarations: TLapeDeclarationList;
    FConstants: TLapeConstantList;
    FMethodGenerates: TLapeGenerateMethods;

    FBaseOptions: ECompilerOptionsSet;
    FBaseOptions_PackRecords: UInt8;
    FOptions: ECompilerOptionsSet;
    FOptions_PackRecords: UInt8;

    FOnHint: TLapeHint;
    FHints: lpString;

    FTypeID: Integer;

    procedure Reset; virtual;
    procedure SetUniqueTypeID(Typ: TLapeType); virtual;
    procedure setEmitter(AEmitter: TLapeCodeEmitter); virtual;
    procedure setOptions(AOptions: ECompilerOptionsSet); virtual;
    procedure setPackRecords(APackRecords: UInt8); virtual;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure VarToDefault(AVar: TResVar); virtual;
    procedure FinalizeVar(AVar: TResVar); virtual;

    function IncStackInfo(AStackInfo: TLapeStackInfo; Flags: EStackFlagsSet): TLapeStackInfo; overload; virtual;
    function IncStackInfo(Flags: EStackFlagsSet): TLapeStackInfo; overload; virtual;
    function DecStackInfo(Flags: EStackFlagsSet; DoFree: Boolean = False): TLapeStackInfo; virtual;

    function getBaseType(BaseType: ELapeBaseType): TLapeType; virtual;
    function addManagedDecl(ADecl: TLapeDeclaration): TLapeDeclaration; virtual;
    function addManagedType(AType: TLapeType): TLapeType; virtual;
    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; overload; virtual;
    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; overload; virtual;

    function getLabel(CodePos: Integer): TLapeGlobalVar; virtual;

    function getConst(Value: Pointer; BaseType: ELapeBaseType): TLapeGlobalVar; virtual;
    function getIntConst(Value: Int64; BaseType: ELapeBaseType = ltUnknown): TLapeGlobalVar; virtual; // ltUnknown: the smallest whole number type that holds it
    function getBoolConst(Value: Boolean; BaseType: ELapeBaseType = ltBoolean): TLapeGlobalVar; virtual;
    function getCharConst(Value: Integer; BaseType: ELapeBaseType = ltAnsiChar): TLapeGlobalVar; virtual;
    function getFloatConst(Value: lpFloat; BaseType: ELapeBaseType = ltFloat): TLapeGlobalVar; virtual;
    function getStringConst(Value: lpString; BaseType: ELapeBaseType = ltString): TLapeGlobalVar; virtual;

    procedure getDestVar(var Dest, Res: TResVar; Op: EOperator); virtual;
    function getTempVar(VarType: ELapeBaseType): TLapeStackTempVar; overload; virtual;
    function getTempVar(VarType: TLapeType): TLapeStackTempVar; overload; virtual;
    function getTempStackVar(VarType: ELapeBaseType): TResVar; overload; virtual;
    function getTempStackVar(VarType: TLapeType): TResVar; overload; virtual;

    function getGlobalVar(AName: lpString): TLapeGlobalVar; virtual;
    function getGlobalType(AName: lpString): TLapeType; virtual;

    // Callback is asked for each method that is not there, by every name: it makes the ones that are its own
    procedure addMethodGenerate(Callback: TLapeGenerateMethod); virtual;
    function GenerateMethod(AName: lpString; AObjectType: TLapeType; const AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    // Of Methods, of the name AName: FitMethod, or what is generated. Of a global name what is generated goes before
    // a method that only fits, of a type after it
    function PickMethod(AName: lpString; const Methods: TLapeDeclArray; AObjectType: TLapeType; const AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    // (LocalOnly: what is declared in AStackInfo itself, not what a with of it has)
    function getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration; virtual;
    function hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False): Boolean; overload; virtual;
    function hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False): Boolean; overload; virtual;

    procedure Hint(Msg: lpString; Args: array of const; ADocPos: TDocPos);
    function getGlobalScope: TLapeStackInfo;

    property StackInfo: TLapeStackInfo read FStackInfo;
    property BaseTypes: TLapeBaseTypes read FBaseTypes;

    property GlobalScope: TLapeStackInfo read getGlobalScope;
    property ManagedDeclarations: TLapeDeclarationList read FManagedDeclarations;
    property Globals[AName: lpString]: TLapeGlobalVar read getGlobalVar; default;
    property Emitter: TLapeCodeEmitter read FEmitter write setEmitter;
    property Options: ECompilerOptionsSet read FOptions write setOptions default Lape_OptionsDef;
    property Options_PackRecords: UInt8 read FBaseOptions_PackRecords write setPackRecords default Lape_PackRecordsDef;
    property OnHint: TLapeHint read FOnHint write FOnHint;
    property Hints: lpString read FHints; // if OnHint=nil, this will have them
  end;

function HasOperatorOverload(Compiler: TLapeCompilerBase; op: EOperator; LeftType, RightType: TLapeType; out Res: TLapeType): Boolean;
procedure RequireOperators(Compiler: TLapeCompilerBase; ops: array of EOperator; Typ: TLapeType); overload;
procedure RequireOperators(Compiler: TLapeCompilerBase; ops: array of EOperator; LeftType, RightType: TLapeType); overload;

function ResolveCompoundOp(op:EOperator; typ:TLapeType): EOperator;
function getTypeArray(Arr: array of TLapeType): TLapeTypeArray;

// How far Given is from Asked when it is given for it (lape-lite's steps); -1: it cannot be given
function TypeDistance(Asked, Given: TLapeType): Integer;
// How far a call with AParams (and wanting AResult) is from Method; -1: it cannot be called so. Exact: types as asked
function MethodDistance(Method: TLapeType_Method; Exact: Boolean; const AParams: TLapeTypeArray; AResult: TLapeType): Integer;
// Adds the methods of Decls to Methods, but not one that takes the same as one of Methods (which are nearer).
// False: Decls are no methods, and hide the rest
function addMethods(var Methods: TLapeDeclArray; const Decls: TLapeDeclArray): Boolean;
// Of Methods: the one with just the header AType (nil: none)
function SameMethod(const Methods: TLapeDeclArray; AType: TLapeType_Method; IgnoreDefault: Boolean = False): TLapeGlobalVar;
// Of Methods: the nearest for a call with AParams, wanting AResult (nil: none, or two as near in one scope; with Fits the first)
function FitMethod(const Methods: TLapeDeclArray; NeedFullMatch: Boolean; const AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;

function GetMethodName(VarType: TLapeType): lpString;
function MethodToString(VarType: TLapeType): lpString;
function MethodOfObject(VarType: TLapeType): Boolean;

function IsMethod(typ: TLapeType): Boolean;
// In script code made here: what goes in front of something of this type for its value
function ValuePrefix(typ: TLapeType): lpString;
// The only one of Methods, when that is what its name means (it is not a property)
function OnlyMethod(const Methods: TLapeDeclArray): TLapeGlobalVar;
// Whether Methods are of a property (Indexable: one takes something)
function IsProperty(const Methods: TLapeDeclArray): Boolean; overload;
function IsProperty(const Methods: TLapeDeclArray; out Indexable: Boolean): Boolean; overload;

const
  TypeID_Unknown = Ord(Low(ELapeBaseType)) - 1;
  TypeID_User = Ord(High(ELapeBaseType)) + 1;

  NullResVar:  TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpNone;  GlobalVar: nil));
  VarResVar:   TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpVar;   StackVar : nil));
  StackResVar: TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpStack; ForceVariable: False));

  NullParameter: TLapeParameter = (ParType: lptNormal; VarType: nil; Default: nil; Name: ''; DocPos: (Line: 0; Col: 0; FileName: ''));

  Lape_RefParams   = [lptConstRef, lptOut, lptVar];
  Lape_ConstParams = [lptConst, lptConstRef];
  Lape_ValParams   = Lape_ConstParams + [lptNormal];
  Lape_SelfParam   = lptVar;

  // The name of the System unit, which `System.Name` is through. No local can be called that: it would hide it.
  LapeSystemName = 'System';

var
  _ResVar: TResVar;

implementation

uses
  lpvartypes_ord, lpvartypes_array, lpvartypes_object, lptree, lpinternalmethods,
  lpmessages, lpeval, lpinterpreter_types, lpeval_extra;

// I think doing a simple check rather than overload.getMethodIndex should be fine for op overloads
function HasOperatorOverload(Compiler: TLapeCompilerBase; op: EOperator; LeftType, RightType: TLapeType; out Res: TLapeType): Boolean;
var
  Methods: TLapeDeclArray;
  Method: TLapeType_Method;
  i: Integer;
begin
  Result := False;
  if not (op in OverloadableOperators) then
    Exit;

  Methods := Compiler.GlobalScope.findMethods('!op_' + op_name[op]);
  for i := 0 to High(Methods) do
  begin
    Method := TLapeType_Method(TLapeGlobalVar(Methods[i]).VarType);
    if (Method.Params.Count <> 2) or (Method.Res = nil) then
      Continue;

    if Method.Params[0].VarType.CompatibleWith(LeftType) and Method.Params[1].VarType.CompatibleWith(RightType) then
    begin
      Res := Method.Res;
      Exit(True);
    end;
  end;
end;

procedure RequireOperators(Compiler: TLapeCompilerBase; ops: array of EOperator; LeftType, RightType: TLapeType);
var
  i: Integer;
  unused: TLapeType;
begin
  for i := 0 to High(ops) do
  begin
    if (LeftType.EvalRes(ops[i], RightType) = nil) and (not HasOperatorOverload(Compiler, ops[i], LeftType, RightType, unused)) then
      if (LeftType.Name <> '') then
        LapeExceptionFmt(lpeOperatorRequiredForType, [op_str[ops[i]], LeftType.Name], Compiler.DocPos)
      else
        LapeExceptionFmt(lpeOperatorRequiredForType, [op_str[ops[i]], LeftType.AsString], Compiler.DocPos);
  end;
end;

procedure RequireOperators(Compiler: TLapeCompilerBase; ops: array of EOperator; Typ: TLapeType);
begin
  RequireOperators(Compiler, ops, Typ, Typ);
end;

function ResolveCompoundOp(op:EOperator; typ:TLapeType): EOperator;
begin
  case op of
    op_AssignDiv:
      if (typ.BaseType in LapeRealTypes) then
        Result := op_Divide
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

// In steps:
//     0  the type itself
//     1  a type that is the same but for its name
//    10  nothing can get lost: a whole number that it can hold all of, a wider fraction or string, a like pointer
//    50  a whole number of the other sign, that is as big at least
//   100  a whole number where a fraction is asked
//   200  the rest: something can get lost
// Within a step the nearest is the least far.
function TypeDistance(Asked, Given: TLapeType): Integer;

  // (whether Asked has every value that Given has)
  function HoldsAll: Boolean;
  begin
    Result := (Asked.VarLo().AsInteger <= Given.VarLo().AsInteger) and (UInt64(Asked.VarHi().AsInteger) >= UInt64(Given.VarHi().AsInteger));
  end;

begin
  // (pointers: a step for another type, for another constness, and for a pointee it was not given; as lite)
  if (Asked is TLapeType_Pointer) and (Given is TLapeType_Pointer) then
    if (not Asked.CompatibleWith(Given)) then
      Exit(-1)
    else
      Exit(Ord(not Asked.Equals(Given, False)) + Ord(TLapeType_Pointer(Asked).PConst <> TLapeType_Pointer(Given).PConst) +
        Ord(TLapeType_Pointer(Asked).HasType() and (not TLapeType_Pointer(Given).HasType())));

  if Asked.Equals(Given) then
    Exit(0);
  if (not Asked.CompatibleWith(Given)) then
    Exit(-1);

  if (Asked.BaseType in LapeOrdinalTypes) and (Given.BaseType in LapeOrdinalTypes) then
  begin
    if ((Asked.BaseType in LapeBoolTypes) and (Given.BaseType in LapeBoolTypes)) or HoldsAll() then
      Result := 10
    else if (Asked.BaseType in LapeIntegerTypes) and (Given.BaseType in LapeIntegerTypes) and (Asked.Size >= Given.Size) then
      Result := 50
    else
      Result := 200;

    Inc(Result, Abs(Asked.Size - Given.Size));
    if ((Asked.BaseType in LapeSignedOrdinalTypes) <> (Given.BaseType in LapeSignedOrdinalTypes)) then
      Inc(Result);
    Exit;
  end;

  // (methods that can be assigned hold the same)
  if (Asked is TLapeType_Method) and (Given is TLapeType_Method) then
    Exit(1)
  else if (Asked.BaseType = Given.BaseType) then
    Exit(10);

  if (Asked.BaseType in LapeRealTypes) and (Given.BaseType in LapeIntegerTypes) then
    Result := 100
  else if (((Asked.BaseType in LapeRealTypes) and (Given.BaseType in LapeRealTypes)) or
           ((Asked.BaseType in LapeStringTypes) and (Given.BaseType in LapeStringTypes + LapeCharTypes))) then
    if (Asked.BaseType >= Given.BaseType) then
      Result := 10
    else
      Result := 200
  else
    Exit(200 + Abs(Asked.Size - Given.Size));

  Inc(Result, Abs(Ord(Asked.BaseType) - Ord(Given.BaseType)));
end;

// (the variables a nested method takes of its outer ones come first and are not given: ImplicitParams; a parameter
//  that is written to takes only its own type)
function MethodDistance(Method: TLapeType_Method; Exact: Boolean; const AParams: TLapeTypeArray; AResult: TLapeType): Integer;
var
  i, Arg, Distance: Integer;
  Given: TLapeType;
begin
  if (Length(AParams) > Method.Params.Count - Method.ImplicitParams) or ((AResult <> nil) and (Method.Res = nil)) then
    Exit(-1);

  Result := 0;
  if (AResult <> nil) then
  begin
    Result := TypeDistance(AResult, Method.Res);
    if (Result < 0) then
      Exit;
  end;

  for i := Method.ImplicitParams to Method.Params.Count - 1 do
    with Method.Params[i] do
    begin
      Arg := i - Method.ImplicitParams;
      if (Arg >= Length(AParams)) then
        if (Default = nil) then
          Exit(-1)
        else
          Continue;

      // (a name of several methods is no type: of a call of several it takes a cast, TInts(@g))
      Given := AParams[Arg];
      if (Given = nil) then
        Exit(-1);

      if (VarType = nil) then
        Distance := 1
      else
        Distance := TypeDistance(VarType, Given);

      if (Distance < 0) or ((Distance > 0) and (VarType <> nil) and (Exact or (not (ParType in Lape_ValParams)))) then
        Exit(-1);
      Inc(Result, Distance);
    end;
end;

function addMethods(var Methods: TLapeDeclArray; const Decls: TLapeDeclArray): Boolean;
var
  Nearer: Integer;

  procedure Add(Decl: TLapeDeclaration);
  var
    i: Integer;
  begin
    for i := 0 to High(Methods) do
      if TLapeType_Method(TLapeGlobalVar(Methods[i]).VarType).EqualParams(TLapeType_Method(TLapeGlobalVar(Decl).VarType)) then
      begin
        // (of this scope: an override, which hides what it overrides)
        if (i >= Nearer) then
          Methods[i] := Decl;
        Exit;
      end;

    SetLength(Methods, Length(Methods) + 1);
    Methods[High(Methods)] := Decl;
  end;

var
  i: Integer;
begin
  Result := (Length(Decls) = 0) or (Decls[0] is TLapeMethod);
  if (not Result) then
    Exit;

  Nearer := Length(Methods);
  for i := 0 to High(Decls) do
    if (Decls[i] is TLapeMethod) then
      Add(Decls[i]);
end;

function SameMethod(const Methods: TLapeDeclArray; AType: TLapeType_Method; IgnoreDefault: Boolean = False): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(Methods) do
    if TLapeType_Method(TLapeGlobalVar(Methods[i]).VarType).EqualParams(AType, True, IgnoreDefault) then
      Exit(TLapeGlobalVar(Methods[i]));
end;

// (Methods come nearest first: a tie goes to the nearer scope or type, as FPC; a tie in one scope picks none)
function FitMethod(const Methods: TLapeDeclArray; NeedFullMatch: Boolean; const AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;
var
  i, Distance, Nearest: Integer;
  Best: TLapeDeclaration;
begin
  Result := nil;
  Best := nil;
  Nearest := High(Integer);

  for i := 0 to High(Methods) do
  begin
    Distance := MethodDistance(TLapeType_Method(TLapeGlobalVar(Methods[i]).VarType), NeedFullMatch, AParams, AResult);
    if (Distance < 0) then
      Continue;

    if (Distance < Nearest) then
    begin
      Best := Methods[i];
      Result := TLapeGlobalVar(Best);
      Nearest := Distance;
    end
    else if (Distance = Nearest) and (Methods[i].DeclarationList = Best.DeclarationList) then
      Result := nil;
  end;
end;

function OnlyMethod(const Methods: TLapeDeclArray): TLapeGlobalVar;
begin
  Result := nil;
  if (Length(Methods) = 1) and (not IsProperty(Methods)) then
    Result := TLapeGlobalVar(Methods[0]);
end;

function GetMethodName(VarType: TLapeType): lpString;
begin
  Result := '';

  if (VarType is TLapeType_Method) then
  begin
    if (VarType is TLapeType_MethodOfType) then
      Result := TLapeType_MethodOfType(VarType).ObjectType.Name + '.' + VarType.Name
    else
      Result := VarType.Name;
  end;
end;

function MethodToString(VarType: TLapeType): lpString;
var
  i: Integer;
  ParamType: TLapeType;
begin
  if (VarType is TLapeType_Method) then
    with TLapeType_Method(VarType) do
    begin
      if (Res <> nil) then
        Result := 'function('
      else
        Result := 'procedure(';

      for i := 0 to Params.Count - 1 do
      begin
        if (i > 0) then
          Result := Result + ', ';

        //case Params[i].ParType of
        //  lptConst:    Result := Result + 'const ';
        //  lptConstRef: Result := Result + 'constref ';
        //  lptVar:      Result := Result + 'var ';
        //  lptOut:      Result := Result + 'out ';
        //end;

        ParamType := Params[i].VarType;
        if (ParamType <> nil) then
          if (ParamType.Name <> '') then
            Result := Result + ParamType.Name
          else
            Result := Result + ParamType.AsString
        else
          Result := Result + '*unknown*';
      end;

      Result := Result + ')';
      if (Res <> nil) then
        if (Res.Name <> '') then
          Result := Result + ': ' + Res.Name
        else
          Result := Result + ': ' + Res.AsString;
    end
  else
    Result := '';
end;

function MethodOfObject(VarType: TLapeType): Boolean;
begin
  Result := VarType is TLapeType_MethodOfObject;
end;

function IsMethod(typ: TLapeType): Boolean;
begin
  Result := Typ is TLapeType_Method;
end;

function IsProperty(const Methods: TLapeDeclArray): Boolean;
begin
  Result := (Length(Methods) > 0) and (TLapeType_Method(TLapeGlobalVar(Methods[0]).VarType).MethodDef = mdProperty);
end;

function IsProperty(const Methods: TLapeDeclArray; out Indexable: Boolean): Boolean;
var
  i: Integer;
begin
  Indexable := False;
  Result := IsProperty(Methods);
  if Result then
    for i := 0 to High(Methods) do
      if (TLapeType_Method(TLapeGlobalVar(Methods[i]).VarType).Params.Count > 0) then
        Indexable := True;
end;

function ValuePrefix(typ: TLapeType): lpString;
begin
  if IsMethod(typ) then
    Result := '@'
  else
    Result := '';
end;

function TResVar.getConstant: Boolean;
begin
  if (FConstant <> bUnknown) then
    Result := (FConstant = bTrue)
  else if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    Result := VarPos.GlobalVar.isConstant
  else if (VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) then
    Result := VarPos.StackVar.isConstant
  else
    Result := not ((VarPos.MemPos = mpStack) and (VarPos.isPointer or VarPos.ForceVariable));
end;

procedure TResVar.setConstant(AConst: Boolean);
begin
  if ((VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil)) or ((VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil)) then
    if AConst then
      FConstant := bTrue
    else
      FConstant := bFalse
  else if (VarPos.MemPos = mpStack) then
    VarPos.ForceVariable := not AConst;
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

function TResVar.InScope(AStack: TLapeStackInfo): TResVar;
begin
  Result := Self;
  if (VarPos.MemPos = mpVar) and (AStack <> nil) and (VarPos.StackVar.Stack <> AStack.VarStack) then
  begin
    Result.VarPos.StackVar := AStack.getInheritedVar(VarPos.StackVar);
    Result.VarPos.isPointer := True;
    if (Result.VarPos.StackVar = nil) then
      LapeException(lpeParentOutOfScope, AStack.Compiler.DocPos);
  end;
end;

procedure TResVar.IncOffset(Offset: Integer);
var
  wasConst, wasKnown: Boolean;
begin
  if VarPos.isPointer or ((VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil)) then
    Inc(VarPos.Offset, Offset)
  else if (VarPos.MemPos = mpStack) then
    Dec(VarPos.Offset, Offset)
  else if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) and HasType() and (VarType.Compiler <> nil) then
  begin
    // (the declaration stays where it is)
    wasConst := isConstant;
    wasKnown := VarPos.GlobalVar.Known and wasConst;
    VarPos.GlobalVar := VarType.Compiler.addManagedDecl(VarType.NewGlobalVarP(Pointer(PtrUInt(VarPos.GlobalVar.Ptr) + Offset))) as TLapeGlobalVar;
    VarPos.GlobalVar.isConstant := wasConst;
    VarPos.GlobalVar.Known := wasKnown;
  end
  else
    LapeException(lpeImpossible);
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

procedure TLapeVar.setConstant(AConst: Boolean);
begin
  FConstant := AConst;
end;

constructor TLapeVar.Create(AVarType: TLapeType; AName: lpString = ''; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AName, AList);

  isConstant := (AName = '');
  VarType := AVarType;
end;

function TLapeVar.HasType: Boolean;
begin
  Result := VarType <> nil;
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

constructor TLapeStackVar.Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AName, AList);
  setStack(AStack);
end;

destructor TLapeStackVar.Destroy;
begin
  setStack(nil);
  inherited;
end;

constructor TLapeStackTempVar.Create(AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; AList: TLapeDeclarationList = nil);
begin
  inherited;
  isConstant := False;
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

constructor TLapeParameterVar.Create(AParType: ELapeParameterType; AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AStack, AName, AList);
  FParType := AParType;
  isConstant := (FParType in Lape_ConstParams);
end;

constructor TLapeStackInheritedVar.Create(AParent: TLapeStackVar; AStack: TLapeVarStack; AList: TLapeDeclarationList = nil);
var
  OwnName: lpString;
begin
  Assert(AParent <> nil);
  Assert(not (AParent is TLapeStackTempVar));

  OwnName := '!' + AParent.Name;
  if (AParent is TLapeStackInheritedVar) then
    AParent := TLapeStackInheritedVar(AParent).Parent;
  Assert(not (AParent is TLapeStackInheritedVar));

  if AParent.isConstant then
    inherited Create(lptConstRef, AParent.VarType, AStack, OwnName, AList)
  else
    inherited Create(lptVar, AParent.VarType, AStack, OwnName, AList);
  FDocPos := AParent.DocPos;

  FParent := AParent;
  FParent.IsUsed := True;
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

procedure TLapeGlobalVar.setConstant(AConst: Boolean);
begin
  inherited;
  FKnown := AConst;
end;

procedure TLapeGlobalVar.setKnown(AKnown: Boolean);
begin
  // (what is known may not be written)
  if AKnown then
    FConstant := True;
  FKnown := AKnown;
end;

constructor TLapeGlobalVar.Create(AVarType: TLapeType; AName: lpString = ''; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AName, AList);

  FOwnsPtr := True;
  if (Size > 0) then
    FPtr := AllocMem(Size);
end;

constructor TLapeGlobalVar.Create(AVarType: TLapeType; Ptr: Pointer; AName: lpString = ''; AList: TLapeDeclarationList = nil);
begin
  inherited Create(AVarType, AName, AList);

  FPtr := Ptr;
end;

destructor TLapeGlobalVar.Destroy;
begin
  if (FPtr <> nil) and FOwnsPtr then
  begin
    if HasType() then
      VarType.FinalizeConst(Self);
    FreeMem(FPtr);
  end;

  inherited;
end;

function TLapeGlobalVar.CreateCopy(CopyContent: Boolean = True): TLapeGlobalVar;
type
  TLapeClassType = class of TLapeGlobalVar;
begin
  if (not CopyContent) then
    Result := TLapeClassType(Self.ClassType).Create(VarType, FPtr)
  else
  begin
    Result := TLapeClassType(Self.ClassType).Create(VarType);
    if HasType() then
    begin
      Result.isConstant := False;
      VarType.EvalConst(op_Assign, Result, Self, []);
    end;
  end;
  Result.isConstant := Self.isConstant;
  Result.Known := Self.Known;
  Result.DocPos := FDocPos;
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

constructor TLapeType.Create(ABaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  inherited Create(AName);
  TypeID := TypeID_Unknown;
  FDeclarations := TLapeDeclarationList.Create();

  FBaseType := ABaseType;
  FCompiler := ACompiler;
  ClearCache();
end;

destructor TLapeType.Destroy;
begin
  FDeclarations.Free();
  inherited;
end;

function TLapeType.getEmptyValue: TLapeGlobalVar;
begin
  if (FEmptyValue = nil) then
    FEmptyValue := FCompiler.addManagedDecl(NewGlobalVarP()) as TLapeGlobalVar;
  Result := FEmptyValue;
end;

function TLapeType.getArrayOf: TLapeType;
begin
  if (FArrayOf = nil) then
    FArrayOf := FCompiler.addManagedType(TLapeType_DynArray.Create(Self, FCompiler));
  Result := FArrayOf;
end;

function TLapeType.getDeclarations: TLapeDeclarationList;
begin
  if (FAliasType <> nil) then
    Result := FAliasType.Declarations
  else
    Result := FDeclarations;
end;

procedure TLapeType.ClearDeclarations;
begin
  FDeclarations.Clear();
end;

procedure TLapeType.ClearCache;
begin
  FSize := 0;
  FLo := nil;
  FHi := nil;
  FEmptyValue := nil;
  FArrayOf := nil;
  FAsString := '';
end;

function TLapeType.VarToStringBody: lpString;
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
  else if (not (BaseType in LapeOrdinalTypes)) then
    Result := -1
  else
    case DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes) of
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
  if (FLo = nil) and (FCompiler <> nil) and (BaseType in LapeOrdinalTypes) then
    with FCompiler do
      Self.FLo := addManagedDecl(NewGlobalVarP(LapeTypeLow[DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes)])) as TLapeGlobalVar;
  Result := FLo;
end;

function TLapeType.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FHi = nil) and (FCompiler <> nil) and (BaseType in LapeOrdinalTypes) then
    with FCompiler do
      Self.FHi := addManagedDecl(NewGlobalVarP(LapeTypeHigh[DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes)])) as TLapeGlobalVar;
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
    LapeSameString(Other.AsString, AsString) // (names in it are as they were written)
  );
end;

function TLapeType.CompatibleWith(Other: TLapeType): Boolean;
begin
  Result := (EvalRes(op_Assign, Other) <> nil);
end;

function TLapeType.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType;
begin
  Result := TLapeClassType(Self.ClassType).Create(FBaseType, FCompiler, '');
  Result.DocPos := FDocPos;
  Result.FParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType.CreateAlias: TLapeType;
begin
  Result := CreateCopy();
  Result.FParentType := nil;
  Result.FAliasType := Self;
end;

function TLapeType.NewGlobalVarP(Ptr: Pointer = nil; AName: lpString = ''): TLapeGlobalVar;
begin
  if (Ptr = nil) then
    Result := TLapeGlobalVar.Create(Self, AName)
  else
    Result := TLapeGlobalVar.Create(Self, Ptr, AName);
end;

function TLapeType.NewGlobalVarStr(Str: AnsiString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(UnicodeString(Str), AName);
end;

function TLapeType.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeImpossible);
end;

procedure TLapeType.addDeclaration(ADecl: TLapeDeclaration);
begin
  if (ADecl.Name <> '') then
    if (not (ADecl is TLapeGlobalVar))  then
      LapeException(lpeImpossible)
    else if HasMember(ADecl.Name) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [ADecl.Name]);
  Declarations.addDeclaration(ADecl);
end;

function TLapeType.HasMembers: Boolean;
var
  Typ: TLapeType;
begin
  Result := (FBaseType in LapeStructTypes);
  Typ := Self;
  while (not Result) and (Typ <> nil) do
  begin
    while (Typ.FAliasType <> nil) do
      Typ := Typ.FAliasType;
    Result := (Typ.Declarations.Count > 0);
    Typ := Typ.ParentType;
  end;
end;

function TLapeType.getDeclaration(AName: lpString): TLapeDeclaration;
var
  Typ: TLapeType;
begin
  Typ := Self;
  while (Typ <> nil) do
  begin
    while (Typ.FAliasType <> nil) do
      Typ := Typ.FAliasType;
    if Typ.Declarations.Get(AName, Result) then
      Exit;
    Typ := Typ.ParentType;
  end;
  Result := nil;
end;

function TLapeType.findMethods(AName: lpString): TLapeDeclArray;
var
  Typ: TLapeType;
begin
  Result := nil;

  Typ := Self;
  while (Typ <> nil) do
  begin
    while (Typ.FAliasType <> nil) do
      Typ := Typ.FAliasType;
    if (not addMethods(Result, Typ.Declarations.GetAll(AName))) then
      Break;
    Typ := Typ.ParentType;
  end;
end;

function TLapeType.getMember(AName: lpString): TLapeDeclaration;
begin
  Result := getDeclaration(AName);
  if (Result is TLapeMethod) then
    Result := nil;
end;

function TLapeType.HasField(AName: lpString): Boolean;
begin
  Result := False;
end;

function TLapeType.HasMember(AName: lpString): Boolean;
begin
  Result := (EvalMemberRes(AName) <> nil) or (getDeclaration(AName) <> nil);
end;

function TLapeType.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Assert(FCompiler <> nil);

  if (op in CompoundOperators) and (lcoCOperators in Compiler.FOptions) then
    Exit(EvalRes(op_Assign, EvalRes(ResolveCompoundOp(op, Self), Right, flags), flags));

  if (Op = op_Addr) then
    Result := FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, Self, False))
  else if (Op in AssignOperators) and (Right <> nil) and (getEvalRes(Op, FBaseType, Right.BaseType) <> ltUnknown) then
    Result := Self
  else if (Right = nil) then
    Result := FCompiler.getBaseType(getEvalRes(Op, FBaseType, ltUnknown))
  else
    Result := FCompiler.getBaseType(getEvalRes(Op, FBaseType, Right.BaseType));

  if (not (op in UnaryOperators + [op_Assign])) and
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
begin
  if (Right = nil) then
    Result := EvalRes(Op, TLapeType(nil), Flags)
  else
    Result := EvalRes(Op, Right.VarType, Flags);

  if (not (op in UnaryOperators + [op_Assign])) and
     (Result = nil) and
     (Right <> nil) and
     Right.HasType()
  then
    Result := EvalRes(Op, Right.VarType, Flags);
end;

function TLapeType.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  Assert((Left = nil) or (Left.VarType = Self));

  Result := ((Left = nil) or Left.Known) and ((Right = nil) or Right.Known);
  if (not Result) and (Right <> nil) and Right.Known then
    if (op = op_Index) and (BaseType in [ltShortString, ltStaticArray]) then
      Result := Right.HasType() and (Right.VarType.BaseType in LapeOrdinalTypes);
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
      if (op = op_Addr) and Left.isConstant then
        if (lefConstAddress in Flags) and (Left.Name <> '') then
          ResType := FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, Self, True))
        else
          LapeException(lpeVariableExpected);

      if (ResType <> nil) or ((op = op_Deref) and (Left.BaseType = ltPointer)) then
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
      if (op = op_Assign) and (Right <> nil) and Right.HasType() then
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
      try
        if (Right = nil) then
          EvalProc(Result.Ptr, Left.Ptr, nil)
        else
          EvalProc(Result.Ptr, Left.Ptr, Right.Ptr);
      except
        FreeAndNil(Result);
        raise;
      end;
    end;
  finally
    if (not (op in AssignOperators)) and (Result <> nil) and (Left <> nil) then
    begin
      Result.isConstant := True;
      Result.Known := Left.Known and ((Right = nil) or Right.Known);
    end;
  end;
end;

function TLapeType.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;

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
      Res := Left.VarType.Eval(op, Dest, Left, Right, Flags);
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
      CastVar.VarPos.StackVar := FCompiler.getTempVar(CastVar.VarType);
      if DoRight then
        Res := Left.VarType.Eval(op, Dest, Left, CastVar.VarType.Eval(op_Assign, tmpVar, CastVar, Right, []), Flags)
      else
        Res := CastVar.VarType.Eval(op, Dest, CastVar.VarType.Eval(op_Assign, tmpVar, CastVar, Left, []), Right, Flags);

      Result := True;
    except
      Result := False;
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
    Exit(Eval(op_Assign, Dest, Left, Eval(ResolveCompoundOp(op, Left.VarType), Dest, Left, Right, Flags), Flags));
  end;

  Result.VarType := EvalRes(Op, Right.VarType, Flags);
  if (op = op_Addr) and Left.isConstant then
    if (lefConstAddress in Flags) and (
       ((Left.VarPos.MemPos = mpVar) and (not (Left.VarPos.StackVar is TLapeStackTempVar))) or
       ((Left.VarPos.MemPos = mpMem) and (Left.VarPos.GlobalVar.Name <> '')))
    then
      Result.VarType := FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, Self, True))
    else
      LapeException(lpeVariableExpected);

  if (not Right.HasType()) then
    EvalProc := getEvalProc(Op, FBaseType, ltUnknown)
  else if (op <> op_Assign) or CompatibleWith(Right.VarType) then
    EvalProc := getEvalProc(Op, FBaseType, Right.VarType.BaseType)
  else
    EvalProc := nil;

  if (not Result.HasType()) or (not ValidEvalFunction(EvalProc)) then
    if (op = op_Assign) and Right.HasType() then
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

    FCompiler.Emitter._Eval(EvalProc, Left, Right, NullResVar);
    Result := Left;
  end
  else
    FCompiler.Emitter._Eval(EvalProc, Result, Left, Right);

end;

function TLapeType.EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType;
var
  Decl: TLapeDeclaration;
begin
  Decl := getMember(AName);
  if (Decl is TLapeType) then
    Result := TLapeType(Decl)
  else if (Decl is TLapeVar) then
    Result := TLapeVar(Decl).VarType
  else
    Result := nil;
end;

function TLapeType.EvalMemberConst(Left: TLapeGlobalVar; AName: lpString; Flags: ELapeEvalFlags = []): TLapeGlobalVar;
begin
  Result := nil;
end;

function TLapeType.EvalMember(var Dest: TResVar; Left: TResVar; AName: lpString; Flags: ELapeEvalFlags): TResVar;
var
  Decl: TLapeDeclaration;
begin
  Assert(FCompiler <> nil);
  Result := NullResVar;
  Dest := NullResVar;

  Decl := getMember(AName);
  if (Decl = nil) then
    LapeExceptionFmt(lpeUnknownDeclaration, [AName]);

  if (Decl is TLapeType) then
    LapeException(lpeVariableExpected);

  Result := _ResVar.New(Decl as TLapeGlobalVar);
end;

procedure TLapeType.FinalizeConst(AVar: TLapeGlobalVar);

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
  Empty: TLapeGlobalVar;
  wasConstant, wasKnown: Boolean;
begin
  if (AVar = nil) or (not NeedFinalization) or FullNil(AVar.Ptr, Size) then
    Exit;

  // (it is given what an empty one has, also a constant)
  wasConstant := AVar.isConstant;
  wasKnown := AVar.Known;
  AVar.isConstant := False;

  Empty := NewGlobalVarP();
  try
    EvalConst(op_Assign, AVar, Empty, []);
  finally
    Empty.Free();
    AVar.isConstant := wasConstant;
    AVar.Known := wasKnown;
  end;
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
      FAsString := 'const' + inherited;
  Result := inherited;
end;

constructor TLapeType_Pointer.Create(ACompiler: TLapeCompilerBase; PointerType: TLapeType = nil; ConstPointer: Boolean = True; AName: lpString = '');
begin
  inherited Create(ltPointer, ACompiler, AName);
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

function TLapeType_Pointer.VarToStringBody: lpString;
begin
  Result := 'begin Result := System.ToString(Pointer(Param0));';
  if HasType() and (FCompiler.GlobalScope.findMethod('ToString', getTypeArray([PType])) <> nil) then
  begin
    Result := Result                   +
      'if (Param0 <> nil) then '       +
      'try'                            +
      '  Result := Result + '#39' ('#39' + System.ToString(' + ValuePrefix(PType) + 'Param0^) + '#39')'#39';' +
      'except end;';
  end;
  Result := Result + 'end;';
end;

function TLapeType_Pointer.VarToString(AVar: Pointer): lpString;
begin
  Result := PointerToString(AVar);
  if (AVar <> nil) and (PPointer(AVar)^ <> nil) and HasType() then
  try
    Result := Result + '(' + FPType.VarToString(PPointer(AVar)^) + ')';
  except end;
end;

function TLapeType_Pointer.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Pointer;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FPType, FPConst, '');
  Result.DocPos := FDocPos;
  Result.CopyHints(Self);
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_Pointer.NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
  PPointer(Result.FPtr)^ := Ptr;
end;

function TLapeType_Pointer.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar;
begin
  if LapeSameString(lpString(Str), 'nil') then
    Result := NewGlobalVar(nil, AName)
  else
    Result := NewGlobalVar(Pointer(StrToInt64(string(Str))), AName);
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
    // p + 1 of an untyped p is as p
    if (Result <> nil) and (Result <> Self) and (Result.BaseType = ltPointer) and (not TLapeType_Pointer(Result).HasType()) and (FCompiler <> nil) then
      if (not HasType()) and (ClassType = TLapeType_Pointer) then
        Result := Self
      else
        Result := FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, FPType, PConst));
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
    if (not Right.HasType()) or (not (Right.VarType.BaseType in LapeOrdinalTypes)) then
      if (Right <> nil) and Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeExpressionExpected)
    else
      Right.VarType := FCompiler.getBaseType(DetermineIntType(Right.VarType.Size, Right.VarType.BaseType in LapeSignedOrdinalTypes));

    if HasType() then
      TypeSize := FCompiler.getIntConst(FPType.Size)
    else
      TypeSize := FCompiler.getIntConst(1);
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
      Result.isConstant := Left.isConstant;
      Result.Known := Left.Known;
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
      if (op = op_Deref) and PConst then
      begin
        Result.isConstant := True;
        Result.Known := False;
      end
      else if (op = op_Deref) then
        Result.isConstant := False;
    end;
  end;
end;

function TLapeType_Pointer.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
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

    if (not Right.HasType()) or (not (Right.VarType.BaseType in LapeOrdinalTypes)) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeExpressionExpected)
    else
      Right.VarType := FCompiler.getBaseType(DetermineIntType(Right.VarType.Size, Right.VarType.BaseType in LapeSignedOrdinalTypes));

    IndexVar := Right;

    if HasType() and (PType.Size in LapePointerIndexEvalSizes) and (IndexVar.VarType.BaseType in LapeOrdinalTypes) then
    begin
      Result := NullResVar;
      if (Self is TLapeType_DynArray) then
        Result.VarType := FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, PType, PConst))
      else
        Result.VarType := Self;

      FCompiler.getDestVar(Dest, Result, op_Index);
      FCompiler.Emitter._Eval(getEvalProc_PointerIndex(PType.Size, DetermineIntType(IndexVar.VarType.Size, IndexVar.VarType.BaseType in LapeSignedOrdinalTypes)), Result, Left, IndexVar);
    end else
    begin
      if HasType() and (FPType.Size <> 1) then
        if (Right.VarPos.MemPos = mpMem) and (not Right.VarPos.isPointer) and (Right.VarPos.GlobalVar <> nil) and Right.VarPos.GlobalVar.Known then
          IndexVar := _ResVar.New(FCompiler.getIntConst(Right.VarPos.GlobalVar.AsInteger * FPType.Size))
        else
          IndexVar :=
            Right.VarType.Eval(op_Multiply, tmpVar, Right, _ResVar.New(FCompiler.getIntConst(FPType.Size)), []);

      Result := //Result := (Pointer + Index * PSize)
        Eval(op_Plus, Dest, Left, IndexVar, []);
    end;
    if wasConstant and (Result.VarPos.MemPos <> mpStack) then
      Result.isConstant := True;
  end
  else if (op = op_Deref) then
  begin
    // (where it is, is a pointer: of a Pointer what is there has no type)
    Result := NullResVar;
    Result.VarType := Self;
    FCompiler.getDestVar(Dest, Result, op_Deref);
    FCompiler.Emitter._Eval(getEvalProc(op_Deref, FBaseType, ltUnknown), Result, Left, NullResVar);
    Result.VarType := EvalRes(op_Deref);
    Result.VarPos.isPointer := True;
    // (what a pointer to const points to may not be written)
    Result.isConstant := PConst;
  end
  else
    Result := inherited;
end;

function TLapeType_StrictPointer.getAsString: lpString;
begin
  Result := 'strict Pointer(' + Name + ')';
end;

function TLapeType_StrictPointer.EvalRes(Op: EOperator; Right: TLapeType; Flags: ELapeEvalFlags): TLapeType;
begin
  if (not (Right is TLapeType_NilPointer)) and (op = op_Assign) and (Right <> Self) then
    Result := nil
  else
    Result := inherited EvalRes(Op, Right, Flags);
end;

constructor TLapeType_Label.Create(ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  inherited Create(ACompiler, nil, True, AName);
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

function TLapeType_Label.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
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
        if (FParams[i].VarType.Name <> '') then
          FAsString := FAsString + FParams[i].VarType.Name
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

constructor TLapeType_Method.Create(ACompiler: TLapeCompilerBase; ARes: TLapeType = nil; AName: lpString = '');
begin
  inherited Create(ltPointer, ACompiler, AName);

  FParams := TLapeParameterList.Create(NullParameter, dupAccept, False);
  Res := ARes;
end;

constructor TLapeType_Method.Create(ACompiler: TLapeCompilerBase; AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType = nil; AName: lpString = '');
var
  i: Integer;
  Param: TLapeParameter;
begin
  if (Length(AParams) <> Length(AParTypes)) or (Length(AParams) <> Length(AParDefaults)) then
    LapeExceptionFmt(lpeArrayLengthsDontMatch, [Format('%d, %d, %d', [Length(AParams), Length(AParTypes), Length(AParDefaults)])]);

  Create(ACompiler, ARes, AName);
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
  Create(AMethod.Compiler, AMethod.Res, AMethod.Name);
  FDocPos := AMethod.DocPos;
  Params.ImportFromArray(AMethod.Params.ExportToArray());

  ImplicitParams := AMethod.ImplicitParams;
  MethodDef := AMethod.MethodDef;

  FParentType := AMethod;
  TypeID := AMethod.TypeID;
  FBaseType := AMethod.FBaseType;

  CopyHints(AMethod);
end;

function TLapeType_Method.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Method;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, Res, '');
  Result.DocPos := FDocPos;
  TLapeType_Method(Result).Params.ImportFromArray(FParams.ExportToArray());

  TLapeType_Method(Result).ImplicitParams := ImplicitParams;
  TLapeType_Method(Result).MethodDef := MethodDef;

  Result.FBaseType := FBaseType;
  Result.CopyHints(Self);
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

destructor TLapeType_Method.Destroy;
begin
  FParams.Free();
  inherited;
end;

function TLapeType_Method.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result := (Other <> nil) and (Other.TypeID = TypeID) and (Size = Other.Size) and (Other is TLapeType_Method) and EqualParams(Other as TLapeType_Method, False, ContextOnly);
  if Result and (not ContextOnly) then
    Result := (Other.BaseType = BaseType);
end;

function TLapeType_Method.VarToStringBody: lpString;
begin
  Result := 'begin Result := '#39 + AsString + ' ('#39' + System.ToString(Pointer(@Param0)) + '#39')'#39'; end;';
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

function TLapeType_Method.NewGlobalVar(Ptr: Pointer = nil): TLapeGlobalVar;
begin
  FBaseType := ltImportedMethod;
  Result := TLapeMethod.Create(Self, Name);
  PPointer(Result.Ptr)^ := Ptr;
end;

function TLapeType_Method.NewGlobalVar(CodePos: TCodePos): TLapeGlobalVar;
begin
  FBaseType := ltScriptMethod;
  Result := TLapeMethod.Create(Self, Name);
  PCodePos(Result.Ptr)^ := CodePos;
end;

function TLapeType_Method.BindSelf(Method: TLapeGlobalVar; SelfVar: TResVar): TResVar;
begin
  Result := _ResVar.New(FCompiler.getTempVar(FCompiler.getGlobalType('TMethod')));
  Result.VarType := Self;
  FCompiler.Emitter._Eval({$IFDEF FPC}@{$ENDIF}lpeTMethod_ASGN, Result, _ResVar.New(Method), SelfVar);
end;

function TLapeType_Method.ParamTypes: TLapeTypeArray;
var
  i: Integer;
begin
  SetLength(Result, Params.Count);
  for i := 0 to High(Result) do
    Result[i] := Params[i].VarType;
end;

function TLapeType_Method.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
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

constructor TLapeType_MethodOfObject.Create(ACompiler: TLapeCompilerBase; ARes: TLapeType = nil; AName: lpString = '');
begin
  inherited;
  FMethodRecord := FCompiler.getGlobalType('TMethod');
  Assert(FMethodRecord <> nil);
end;

function TLapeType_MethodOfObject.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result := (Other is TLapeType_MethodOfObject) and inherited;
end;

function TLapeType_MethodOfObject.VarToStringBody: lpString;
begin
  Result := 'begin Result := '#39 + AsString + ' '#39' + System.ToString(TMethod(@Param0)); end;';
end;

function TLapeType_MethodOfObject.NewGlobalVar(AMethod: TMethod): TLapeGlobalVar;
begin
  Result := NewGlobalVar(Pointer(nil));
  TMethod(Result.Ptr^) := AMethod;
end;

function TLapeType_MethodOfObject.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (lcoAutoObjectify in FCompiler.Options) then
    if (op = op_Assign) and (Right <> nil) and (Right.ClassType = TLapeType_Method) and TLapeType_Method(Right).EqualParams(Self) then
    begin
      Result := Self;
      Exit;
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
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Left <> nil) then
    Left.VarType := FMethodRecord;

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

function TLapeType_MethodOfObject.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
var
  tmpRes: TResVar;
begin
  Assert(Left.VarType = Self);
  Left.VarType := FMethodRecord;

  if (lcoAutoObjectify in FCompiler.Options) then
    if (Op = op_Assign) and Right.HasType and (Right.VarType.ClassType = TLapeType_Method) then
    begin
      with TLapeTree_InternalMethod_Objectify.Create(FCompiler) do
      try
        addParam(TLapeTree_Var.Create(Right, FCompiler));

        Right := Compile();
      finally
        Free();
      end;
    end;

  if (Op in CompareOperators) then
    Result := inherited
  else
  try
    if CompatibleWith(Right.VarType) then
      Right.VarType := FMethodRecord
    else
    if Right.HasType() and (Right.VarType is TLapeType_Pointer) and (TLapeType_Pointer(Right.VarType).PType = FMethodRecord) then
    begin
      tmpRes := NullResVar;
      Right := Right.VarType.Eval(op_Deref, tmpRes, Right, NullResVar, []);
    end;

    Result := FMethodRecord.Eval(Op, Dest, Left, Right, Flags);
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

constructor TLapeType_MethodOfType.Create(ACompiler: TLapeCompilerBase; AObjType: TLapeType; ARes: TLapeType = nil; AName: lpString = '');
begin
  Assert(AObjType <> nil);
  inherited Create(ACompiler, ARes, AName);
  FObjectType := AObjType;
end;

function TLapeType_MethodOfType.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_MethodOfType;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FObjectType, Res, '');
  Result.DocPos := FDocPos;
  TLapeType_MethodOfType(Result).Params.ImportFromArray(FParams.ExportToArray());

  TLapeType_MethodOfType(Result).MethodDef := MethodDef;
  TLapeType_MethodOfType(Result).ImplicitParams := ImplicitParams;

  Result.FBaseType := FBaseType;
  Result.CopyHints(Self);
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_MethodOfType.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result :=
    (Other is TLapeType_MethodOfType) and
    ObjectType.Equals(TLapeType_MethodOfType(Other).ObjectType, ContextOnly) and
    inherited;
end;

constructor TLapeWithDeclaration.Create(AWithType: TLapeType);
begin
  inherited Create();
  WithType := AWithType;
  WithVar := NullResVar;
end;

function TLapeWithDeclaration.HasMember(AName: lpString): Boolean;
begin
  Result := (WithType <> nil) and WithType.HasMember(AName);
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

procedure TLapeStackInfo.setOwner(AOwner: TLapeStackInfo);
begin
  Assert(AOwner <> Self);
  FOwner := AOwner;
  if (AOwner = nil) then
  begin
    FDepth := 0;
    FRoot := Self;
  end
  else
  begin
    Assert(AOwner.Compiler = FCompiler);
    FDepth := AOwner.Depth + 1;
    FRoot := AOwner.Root;
  end;
end;

constructor TLapeStackInfo.Create(ACompiler: TLapeCompilerBase; AlwaysInitialize: Boolean = True; AOwner: TLapeStackInfo = nil);
begin
  inherited Create(False);

  FCompiler := ACompiler;
  Owner := AOwner;
  FAround := AOwner;
  FVarStack := TLapeVarStack.Create(nil, dupAccept, False);
  FTemps := TLapeVarStack.Create(nil, dupAccept, False);
  FWithStack := TLapeWithDeclarationList.Create(nil, dupAccept, False);
  CodePos := -1;

  ForceInitialization := AlwaysInitialize;
end;

destructor TLapeStackInfo.Destroy;
begin
  Clear();
  FreeAndNil(FVarStack);
  FreeAndNil(FTemps);
  FreeAndNil(FWithStack);

  inherited;
end;

procedure TLapeStackInfo.ClearVars;
var
  i: Integer;
begin
  if (FTemps <> nil) then
    FTemps.Clear();

  // (a variable that goes leaves FVarStack itself)
  if (FVarStack <> nil) then
    for i := FVarStack.Count - 1 downto 0 do
      FVarStack[i].Free();
end;

procedure TLapeStackInfo.Clear;
var
  Decls: TLapeDeclArray;
  i: Integer;
begin
  ClearVars();

  if (FCompiler <> nil) then
  begin
    Decls := ExportToArray();
    for i := 0 to High(Decls) do
      if (not (Decls[i] is TLapeStackVar)) then
        FCompiler.ManagedDeclarations.addDeclaration(Decls[i]);
  end;

  inherited;
end;

function TLapeStackInfo.getDeclaration(AName: lpString; CheckWith: Boolean): TLapeDeclaration;
var
  i: Integer;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if FWithStack[i].HasMember(AName) then
        Exit(FWithStack[i]);

  if (not Get(AName, Result)) then
    Result := nil;
end;

function TLapeStackInfo.findMethods(AName: lpString): TLapeDeclArray;
var
  Stack: TLapeStackInfo;
  i: Integer;
begin
  Result := nil;

  Stack := Self;
  while (Stack <> nil) do
  begin
    for i := Stack.WithStack.Count - 1 downto 0 do
      if Stack.WithStack[i].HasMember(AName) then
        Exit;

    // (what is not a method does not hide one)
    addMethods(Result, Stack.GetAll(AName));
    Stack := Stack.Around;
  end;
end;

function TLapeStackInfo.findMethod(AName: lpString; AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;
begin
  Result := FCompiler.PickMethod(AName, findMethods(AName), nil, AParams, AResult);
end;

function TLapeStackInfo.hasDeclaration(AName: lpString; CheckWith: Boolean): Boolean;
var
  i: Integer;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if FWithStack[i].HasMember(AName) then
        Exit(True);

  Result := (getDeclaration(AName, False) <> nil);
end;

function TLapeStackInfo.getTempVar(VarType: TLapeType): TLapeStackTempVar;

  function Fits(AVar: TLapeStackVar): Boolean;
  begin
    if VarType.NeedInitialization or VarType.NeedFinalization or AVar.NeedInitialization or AVar.NeedFinalization then
      Result := AVar.VarType.Equals(VarType)
    else
      Result := (AVar.Size = VarType.Size);
  end;

var
  i: Integer;
begin
  Result := nil;
  if (VarType = nil) then
    Exit;

  for i := 0 to FVarStack.Count - 1 do
    if (FVarStack[i] is TLapeStackTempVar) and (not TLapeStackTempVar(FVarStack[i]).InUse) and Fits(FVarStack[i]) then
    begin
      Result := FVarStack[i] as TLapeStackTempVar;
      Result.VarType := VarType;
      Result.isConstant := False;
      Break;
    end;

  if (Result = nil) then
    Result := addVar(VarType) as TLapeStackTempVar;

  Result.FInUse := True;
  FTemps.Add(Result);
end;

function TLapeStackInfo.TempMark: Integer;
begin
  Result := FTemps.Count;
end;

procedure TLapeStackInfo.ReleaseTemps(Mark: Integer);
var
  i: Integer;
begin
  Assert((Mark >= 0) and (Mark <= FTemps.Count));
  for i := FTemps.Count - 1 downto Mark do
    TLapeStackTempVar(FTemps.Delete(i)).FInUse := False;
end;

procedure TLapeStackInfo.addDeclaration(ADecl: TLapeDeclaration);

  // (a method overloads the others of its name: what the scope has of it are methods)
  function Overloads: Boolean;
  var
    Decls: TLapeDeclArray;
    i: Integer;
  begin
    if (not (ADecl is TLapeMethod)) then
      Exit(False);

    Decls := GetAll(ADecl.Name);
    for i := 0 to High(Decls) do
      if (not (Decls[i] is TLapeMethod)) then
        Exit(False);
    Result := True;
  end;

begin
  if (ADecl = nil) then
    Exit;

  // (a name is declared once in a scope, the global one too, whatever its withs have.
  // A global method is not checked: the program that runs Lape can put one in place of another, see addGlobalMethod)
  if (Depth > 0) or (not ((ADecl is TLapeGlobalVar) and (TLapeGlobalVar(ADecl).VarType is TLapeType_Method))) then
    if ((ADecl.Name = '') and Has(ADecl)) or
       ((ADecl.Name <> '') and (hasDeclaration(ADecl.Name, False) and (not Overloads())))
    then
      LapeExceptionFmt(lpeDuplicateDeclaration, [ADecl.Name]);

  inherited addDeclaration(ADecl);
end;

function TLapeStackInfo.isReserved(AName: lpString): Boolean;
var
  Stack: TLapeStackInfo;
begin
  // (System. is not to be hidden)
  Result := LapeSameString(AName, LapeSystemName);
  if Result or (Depth = 0) or (not LapeSameString(AName, 'Self')) then
    Exit;

  Stack := Self;
  while (Stack <> nil) do
  begin
    if (Stack.Method is TLapeType_Method) and (TLapeType_Method(Stack.Method).ObjectType <> nil) then
      Exit(True);
    Stack := Stack.Around;
  end;
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

function TLapeStackInfo.addVar(VarType: TLapeType; AName: lpString = ''): TLapeVar;
begin
  if (AName = '') then
    Result := addVar(TLapeStackTempVar.Create(VarType, nil, AName))
  else if (Depth = 0) then
  begin
    Result := VarType.NewGlobalVarP(nil, AName);
    Result.isConstant := False;
    addDeclaration(Result);
  end
  else
    Result := addVar(TLapeStackVar.Create(VarType, nil, AName));
end;

function TLapeStackInfo.addVar(ParType: ELapeParameterType; VarType: TLapeType; AName: lpString): TLapeStackVar;
begin
  Result := addVar(TLapeParameterVar.Create(ParType, VarType, nil, AName));
end;

function TLapeStackInfo.addSelfVar(ParType: ELapeParameterType; VarType: TLapeType): TLapeStackVar;
begin
  Assert(VarType <> nil);
  Result := addVar(TLapeParameterVar.Create(ParType, VarType, nil, 'Self'));
end;

function TLapeStackInfo.addResultVar(VarType: TLapeType): TLapeStackVar;
begin
  Assert(VarType <> nil);
  Result := addVar(TLapeParameterVar.Create(lptOut, VarType, nil, 'Result'));
  FResultVar := Result;
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

function TLapeStackInfo.addWith(AWith: TLapeWithDeclaration): Integer;
begin
  if (AWith = nil) or (AWith.WithType = nil) or (not AWith.WithType.HasMembers()) then
    Exit(-1);
  Result := FWithStack.Add(AWith);
end;

procedure TLapeStackInfo.delWith(ACount: Integer);
var
  i: Integer;
begin
  for i := FWithStack.Count - 1 downto FWithStack.Count - ACount do
    FWithStack.Delete(i);
end;

type
  EMyMemoryPos = (mmpNone, mmpPtr, mmpVar, mmpStk, mmpPVar, mmpPStk);
function getMemoryPos(AVar: TVarPos): EMyMemoryPos; {$IFDEF Lape_Inline}inline;{$ENDIF}
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

function TLapeCodeEmitter._IncCall(ACodePos: TResVar; AParamSize: TParamSize): Integer;
var
  e: Boolean;
begin
  Assert(ACodePos.HasType());
  e := False;

  case getMemoryPos(ACodePos.VarPos) of
    mmpVar: Result := _IncCall_Var(ACodePos.VarPos.StackVar.Offset + ACodePos.VarPos.Offset, AParamSize);
    mmpPVar: Result := _IncCall_PVar(ACodePos.VarPos.StackVar.Offset, ACodePos.VarPos.Offset, AParamSize);
    mmpPtr: Result := _IncCall_Ptr(ACodePos.VarPos.GlobalVar.Ptr, AParamSize);
    else e := True;
  end;

  if e then
    LapeException(lpeImpossible);
end;

function TLapeCodeEmitter._InvokeImportedProc(AMemPos: TResVar; AParamSize: TParamSize): Integer;
var
  e: Boolean;
begin
  Assert(AMemPos.HasType());
  e := False;

  case getMemoryPos(AMemPos.VarPos) of
    mmpVar: Result := _InvokeImported_Var(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AParamSize);
    mmpPVar: Result := _InvokeImported_PVar(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AParamSize);
    mmpPtr: Result := _InvokeImported_Ptr(AMemPos.VarPos.GlobalVar.Ptr, AParamSize);
    else e := True;
  end;

  if e then
    LapeException(lpeImpossible);
end;

function TLapeCodeEmitter._InvokeImportedFunc(AMemPos, AResPos: TResVar; AParamSize: TParamSize): Integer;
var
  e: Boolean;
begin
  Assert(AMemPos.HasType());
  Assert(AResPos.HasType());
  e := False;

  case getMemoryPos(AMemPos.VarPos) of
    mmpVar: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Var_Stk(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AParamSize, AResPos.VarType.Size);
      mmpVar: Result := _InvokeImported_Var_Var(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AParamSize);
      mmpPVar: Result := _InvokeImported_Var_PVar(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.StackVar.Offset, AResPos.VarPos.Offset, AParamSize);
      mmpPtr: Result := _InvokeImported_Var_Ptr(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.GlobalVar.Ptr, AParamSize);
      else e := True;
    end;
    mmpPVar: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_PVar_Stk(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AParamSize, AResPos.VarType.Size);
      mmpVar: Result := _InvokeImported_PVar_Var(AMemPos.VarPos.StackVar.Offset, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AMemPos.VarPos.Offset, AParamSize);
      mmpPVar: Result := _InvokeImported_PVar_PVar(AMemPos.VarPos.StackVar.Offset, AResPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AResPos.VarPos.Offset, AParamSize);
      mmpPtr: Result := _InvokeImported_PVar_Ptr(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AResPos.VarPos.GlobalVar.Ptr, AParamSize);
      else e := True;
    end;
    mmpPtr: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Ptr_Stk(AMemPos.VarPos.GlobalVar.Ptr, AParamSize, AResPos.VarType.Size);
      mmpVar: Result := _InvokeImported_Ptr_Var(AMemPos.VarPos.GlobalVar.Ptr, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AParamSize);
      mmpPVar: Result := _InvokeImported_Ptr_PVar(AMemPos.VarPos.GlobalVar.Ptr, AResPos.VarPos.StackVar.Offset, AResPos.VarPos.Offset, AParamSize);
      mmpPtr: Result := _InvokeImported_Ptr_Ptr(AMemPos.VarPos.GlobalVar.Ptr, AResPos.VarPos.GlobalVar.Ptr, AParamSize);
      else e := True;
    end;
    else e := True;
  end;

  if e then
    LapeException(lpeImpossible);
end;

function TLapeCodeEmitter._JmpRIf(Jmp: TCodeOffset; Cond: TResVar): Integer;
var
  e: Boolean;
begin
  Assert(Cond.HasType());
  e := False;

  case getMemoryPos(Cond.VarPos) of
    mmpStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_Stk(Jmp);
        2: Result := _JmpRIf16_Stk(Jmp);
        4: Result := _JmpRIf32_Stk(Jmp);
        8: Result := _JmpRIf64_Stk(Jmp);
        else e := True;
      end;
    mmpVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        2: Result := _JmpRIf16_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        4: Result := _JmpRIf32_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        8: Result := _JmpRIf64_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        else e := True;
      end;
    mmpPtr:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        2: Result := _JmpRIf16_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        4: Result := _JmpRIf32_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        8: Result := _JmpRIf64_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        else e := True;
      end;
    mmpPStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_PStk(Jmp);
        2: Result := _JmpRIf16_PStk(Jmp);
        4: Result := _JmpRIf32_PStk(Jmp);
        8: Result := _JmpRIf64_PStk(Jmp);
        else e := True;
      end;
    mmpPVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIf8_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        2: Result := _JmpRIf16_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        4: Result := _JmpRIf32_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        8: Result := _JmpRIf64_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        else e := True;
      end;
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidJump);
end;

function TLapeCodeEmitter._JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar): Integer;
var
  e: Boolean;
begin
  Assert(Cond.HasType());
  e := False;

  case getMemoryPos(Cond.VarPos) of
    mmpStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_Stk(Jmp);
        2: Result := _JmpRIfNot16_Stk(Jmp);
        4: Result := _JmpRIfNot32_Stk(Jmp);
        8: Result := _JmpRIfNot64_Stk(Jmp);
        else e := True;
      end;
    mmpVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        2: Result := _JmpRIfNot16_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        4: Result := _JmpRIfNot32_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        8: Result := _JmpRIfNot64_Var(Jmp, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset);
        else e := True;
      end;
    mmpPtr:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        2: Result := _JmpRIfNot16_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        4: Result := _JmpRIfNot32_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        8: Result := _JmpRIfNot64_Ptr(Jmp, Cond.VarPos.GlobalVar.Ptr);
        else e := True;
      end;
    mmpPStk:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_PStk(Jmp);
        2: Result := _JmpRIfNot16_PStk(Jmp);
        4: Result := _JmpRIfNot32_PStk(Jmp);
        8: Result := _JmpRIfNot64_PStk(Jmp);
        else e := True;
      end;
    mmpPVar:
      case Cond.VarType.Size of
        1: Result := _JmpRIfNot8_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        2: Result := _JmpRIfNot16_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        4: Result := _JmpRIfNot32_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        8: Result := _JmpRIfNot64_PVar(Jmp, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset);
        else e := True;
      end;
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidJump);
end;

function TLapeCodeEmitter._Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar): Integer;
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
    LapeException(lpeImpossible);
end;

constructor TLapeConstantList.Create;
begin
  inherited Create();
  FList := TList.Create();
end;

destructor TLapeConstantList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

// What they hold is the compiler's, freed with its other declarations
procedure TLapeConstantList.Clear;
begin
  FList.Clear();
end;

function TLapeConstantList.Get(Compiler: TLapeCompilerBase; BaseType: ELapeBaseType; Value: Pointer): TLapeGlobalVar;
var
  Hash: UInt32;
  Index: Integer;
begin
  Hash := LapeHashBytes(Value, LapeTypeSize[BaseType]);

  Index := FList.Find(Hash);
  while (Index < FList.Count) and (FList.Hashes[Index] = Hash) do
  begin
    if (FList[Index].BaseType = BaseType) and LapeSameValue(BaseType, FList[Index].Ptr, Value) then
      Exit(FList[Index]);
    Inc(Index);
  end;

  Result := Compiler.BaseTypes[BaseType].NewGlobalVarP();
  Move(Value^, Result.Ptr^, LapeTypeSize[BaseType]);
  Compiler.ManagedDeclarations.addDeclaration(Result);
  FList.Insert(Hash, Result);
end;

function TLapeConstantList.ToString: ansistring;
var
  BaseType: ELapeBaseType;
  i: Integer;
  Header: Boolean;
begin
  Result := '';
  for BaseType := Low(ELapeBaseType) to High(ELapeBaseType) do
  begin
    Header := False;
    for i := 0 to FList.Count - 1 do
      if (FList[i].BaseType = BaseType) then
      begin
        if (not Header) then
          Result := Result + LapeTypeToString(BaseType) + ':' + LineEnding;
        Header := True;
        Result := Result + '  ' + FList[i].AsString + LineEnding;
      end;
  end;
end;

procedure TLapeCompilerBase.setEmitter(AEmitter: TLapeCodeEmitter);
begin
  FEmitter.Free();
  FEmitter := AEmitter;
  if (FEmitter <> nil) then
    FEmitter.Reset();
end;

procedure TLapeCompilerBase.setOptions(AOptions: ECompilerOptionsSet);
begin
  FOptions     := AOptions;
  FBaseOptions := AOptions;
end;

procedure TLapeCompilerBase.setPackRecords(APackRecords: UInt8);
begin
  FOptions_PackRecords     := APackRecords;
  FBaseOptions_PackRecords := APackRecords;
end;

procedure TLapeCompilerBase.Reset;
begin
  FOptions := FBaseOptions;
  FOptions_PackRecords := FBaseOptions_PackRecords;
  FHints := '';

  if (FEmitter <> nil) then
    FEmitter.Reset();

  // back to what is global, without the variables of its code
  while (FStackInfo.Depth > 0) do
    DecStackInfo([]);
  GlobalScope.ClearVars();
  GlobalScope.WithStack.Clear();
end;

constructor TLapeCompilerBase.Create;
begin
  inherited Create();

  Options := Lape_OptionsDef;
  Options_PackRecords := Lape_PackRecordsDef;

  FOnHint := nil;

  FEmitter := TLapeCodeEmitter.Create();

  FManagedDeclarations := TLapeDeclarationList.Create();
  FStackInfo := TLapeStackInfo.Create(Self);
  FConstants := TLapeConstantList.Create();

  Clear();
end;

destructor TLapeCompilerBase.Destroy;
begin
  Clear();
  setEmitter(nil);

  // (Clear went back to what is global)
  Assert(FStackInfo.Depth = 0);
  FreeAndNil(FStackInfo);
  FreeAndNil(FManagedDeclarations);
  FreeAndNil(FConstants);

  inherited;
end;

procedure TLapeCompilerBase.Clear;
var
  i: Integer;
  BaseType: ELapeBaseType;
begin
  // (the stacks first: what is declared in them goes to the managed list, which frees all of it)
  Reset();

  GlobalScope.Delete(TLapeVar, True);
  FManagedDeclarations.Delete(TLapeVar, True);
  GlobalScope.Clear();
  for i := FManagedDeclarations.Count - 1 downto 0 do
    FManagedDeclarations.Items[i].ClearDeclarations();
  FManagedDeclarations.Clear();
  FConstants.Clear();
  FMethodGenerates := nil;
  FTypeID := TypeID_User;

  FBaseTypes[ltUInt8] := TLapeType_UInt8.Create(Self, LapeTypeToString(ltUInt8));
  FBaseTypes[ltInt8] := TLapeType_Int8.Create(Self, LapeTypeToString(ltInt8));
  FBaseTypes[ltUInt16] := TLapeType_UInt16.Create(Self, LapeTypeToString(ltUInt16));
  FBaseTypes[ltInt16] := TLapeType_Int16.Create(Self, LapeTypeToString(ltInt16));
  FBaseTypes[ltUInt32] := TLapeType_UInt32.Create(Self, LapeTypeToString(ltUInt32));
  FBaseTypes[ltInt32] := TLapeType_Int32.Create(Self, LapeTypeToString(ltInt32));
  FBaseTypes[ltUInt64] := TLapeType_UInt64.Create(Self, LapeTypeToString(ltUInt64));
  FBaseTypes[ltInt64] := TLapeType_Int64.Create(Self, LapeTypeToString(ltInt64));
  FBaseTypes[ltSingle] := TLapeType_Single.Create(Self, LapeTypeToString(ltSingle));
  FBaseTypes[ltDouble] := TLapeType_Double.Create(Self, LapeTypeToString(ltDouble));
  FBaseTypes[ltCurrency] := TLapeType_Currency.Create(Self, LapeTypeToString(ltCurrency));
  {$IFNDEF Lape_NoExtended}
  FBaseTypes[ltExtended] := TLapeType_Extended.Create(Self, LapeTypeToString(ltExtended));
  {$ENDIF}
  FBaseTypes[ltBoolean] := TLapeType_Boolean.Create(Self, LapeTypeToString(ltBoolean));
  FBaseTypes[ltByteBool] := TLapeType_ByteBool.Create(Self, LapeTypeToString(ltByteBool));
  FBaseTypes[ltWordBool] := TLapeType_WordBool.Create(Self, LapeTypeToString(ltWordBool));
  FBaseTypes[ltLongBool] := TLapeType_LongBool.Create(Self, LapeTypeToString(ltLongBool));
  FBaseTypes[ltAnsiChar] := TLapeType_AnsiChar.Create(Self, LapeTypeToString(ltAnsiChar));
  FBaseTypes[ltWideChar] := TLapeType_WideChar.Create(Self, LapeTypeToString(ltWideChar));
  FBaseTypes[ltShortString] := TLapeType_ShortString.Create(Self, High(UInt8), LapeTypeToString(ltShortString));
  FBaseTypes[ltAnsiString] := TLapeType_AnsiString.Create(Self, LapeTypeToString(ltAnsiString));
  FBaseTypes[ltWideString] := TLapeType_WideString.Create(Self, LapeTypeToString(ltWideString));
  FBaseTypes[ltUnicodeString] := TLapeType_UnicodeString.Create(Self, LapeTypeToString(ltUnicodeString));
  FBaseTypes[ltVariant] := TLapeType_Variant.Create(Self, LapeTypeToString(ltVariant));
  FBaseTypes[ltPointer] := TLapeType_Pointer.Create(Self, nil, False, LapeTypeToString(ltPointer));

  for BaseType in LapeBaseTypes do
    if (FBaseTypes[BaseType] <> nil) then
      GlobalScope.addDeclaration(FBaseTypes[BaseType]);

end;

procedure TLapeCompilerBase.VarToDefault(AVar: TResVar);
begin
  FinalizeVar(AVar);
end;

procedure TLapeCompilerBase.FinalizeVar(AVar: TResVar);
var
  tmpVar: TResVar;
begin
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) or (not AVar.HasType()) or (not AVar.VarType.NeedFinalization) then
    Exit;

  // (cleaned up: it is given what an empty one has, also a constant)
  AVar.isConstant := False;
  tmpVar := NullResVar;
  AVar.VarType.Eval(op_Assign, tmpVar, AVar, _ResVar.New(AVar.VarType.EmptyValue), []);
end;

function TLapeCompilerBase.IncStackInfo(AStackInfo: TLapeStackInfo; Flags: EStackFlagsSet): TLapeStackInfo;
begin
  Assert(FStackInfo <> nil);
  if (AStackInfo <> nil) then
  begin
    if (AStackInfo <> FStackInfo) then
      AStackInfo.Owner := FStackInfo;

    AStackInfo.OldMaxStack := Emitter.MaxStack;
    AStackInfo.OldStackPos := Emitter.NewStack();

    // the most it can need - DecStackInfo writes it
    if (lsfEmit in Flags) then
    begin
      AStackInfo.CodePos := Emitter._IncTry(0, Try_NoExcept);
      Emitter._ExpandVar(0);
      Emitter._InitStackLen(0);
    end;
  end;

  FStackInfo := AStackInfo;
  Result := FStackInfo;
end;

function TLapeCompilerBase.IncStackInfo(Flags: EStackFlagsSet): TLapeStackInfo;
begin
  Result := IncStackInfo(TLapeStackInfo.Create(Self, lcoAlwaysInitialize in FOptions, FStackInfo), Flags);
end;

function TLapeCompilerBase.DecStackInfo(Flags: EStackFlagsSet; DoFree: Boolean = False): TLapeStackInfo;

  procedure FinalizeStackVars;
  var
    i: Integer;
  begin
    // disallow reusing while they are cleaned up
    for i := 0 to FStackInfo.VarCount - 1 do
      if (FStackInfo.Vars[i] is TLapeStackTempVar) then
        TLapeStackTempVar(FStackInfo.Vars[i]).FInUse := True;

    i := 0;
    while (i < FStackInfo.VarCount) do
    begin
      if FStackInfo.Vars[i].NeedFinalization then
        FinalizeVar(_ResVar.New(FStackInfo.Vars[i]));
      Inc(i);
    end;
  end;

  // objects destroys must be called
  procedure FinalizeGlobalVars;
  var
    Decl: TLapeDeclaration;
    i: Integer;
  begin
    for i := 0 to GlobalScope.Count - 1 do
    begin
      Decl := GlobalScope.Items[i];
      if (Decl is TLapeGlobalVar) and (not Decl.DocPos.isEmpty) and (not TLapeGlobalVar(Decl).isConstant) and HasObject(TLapeGlobalVar(Decl).VarType) then
        FinalizeVar(_ResVar.New(TLapeGlobalVar(Decl)));
    end;
  end;

const
  ExpandVarSize = ocSize + SizeOf(TStackOffset);
  InitStackLenSize = ocSize + SizeOf(TStackOffset);
var
  NeedVar, NeedStack: Boolean;
  TryEnd, Start, At: Integer;
begin
  Result := FStackInfo.Owner;

  try
    if (lsfEmit in Flags) then
    begin
      Emitter._DecTry();
      TryEnd := Emitter.CodeLen;

      FinalizeStackVars();
      if (lsfGlobal in Flags) then
        FinalizeGlobalVars();

      if (lsfFunction in Flags) then
        Emitter._DecCall_EndTry()
      else
      begin
        Emitter._PopVar();
        Emitter._EndTry();
      end;

      NeedVar := (FStackInfo.TotalNoParamSize > 0);
      NeedStack := (Emitter.MaxStack > 0);

      Start := FStackInfo.CodePos;
      if (not NeedVar) then
        Inc(Start, ExpandVarSize);
      if (not NeedStack) then
        Inc(Start, InitStackLenSize);

      At := FStackInfo.CodePos;
      while (At < Start) do
        if (lsfFunction in Flags) then
          At := Emitter.PatchUnused(At)
        else
          At := Emitter.PatchJmpR(At, Start);
      Assert(At = Start);

      At := Emitter.PatchTry(At, TryEnd, Try_NoExcept);
      if NeedVar then
        if (lsfFunction in Flags) then
          if FStackInfo.NeedInitialization then
            At := Emitter.PatchGrowVarAndInit(At, FStackInfo.TotalNoParamSize)
          else
            At := Emitter.PatchGrowVar(At, FStackInfo.TotalNoParamSize)
        else
          if FStackInfo.NeedInitialization then
            At := Emitter.PatchExpandVarAndInit(At, FStackInfo.TotalSize)
          else
            At := Emitter.PatchExpandVar(At, FStackInfo.TotalSize);
      if NeedStack then
        At := Emitter.PatchInitStackLen(At, Emitter.MaxStack);
      Assert(At = FStackInfo.CodePos + ocSize + SizeOf(TOC_IncTry) + ExpandVarSize + InitStackLenSize);

      if (lsfFunction in Flags) then
        FStackInfo.CodePos := Start;
    end;
  finally
    Emitter.NewStack(FStackInfo.OldStackPos, FStackInfo.OldMaxStack);
    if (FStackInfo.Depth > 0) then
    begin
      Assert(Result <> nil);
      if DoFree then
        FStackInfo.Free();
      FStackInfo := Result;
    end;
  end;
end;

function TLapeCompilerBase.getBaseType(BaseType: ELapeBaseType): TLapeType;
begin
  Result := FBaseTypes[BaseType];
end;

function TLapeCompilerBase.addManagedDecl(ADecl: TLapeDeclaration): TLapeDeclaration;
begin
  Result := ADecl;
  if (ADecl <> nil) and (ADecl.DeclarationList = nil) then
    FManagedDeclarations.addDeclaration(ADecl);
end;

function TLapeCompilerBase.addManagedType(AType: TLapeType): TLapeType;
begin
  Result := addManagedDecl(AType) as TLapeType;
end;

function TLapeCompilerBase.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar;
begin
  if (AVar <> nil) then
  begin
    // (a method has the name of its header)
    Assert((AName = '') or (not (AVar is TLapeMethod)));

    if (AName <> '') then
      AVar.Name := AName;
    AVar.isConstant := False;
    GlobalScope.addDeclaration(AVar);
  end;
  Result := AVar;
end;

function TLapeCompilerBase.addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType;
begin
  if (Typ <> nil) then
  begin
    if (not ACopy) then
      SetUniqueTypeID(Typ);
    if (AName <> '') then
      Typ.Name := AName;
    GlobalScope.addDeclaration(Typ);
  end;
  Result := Typ;
end;

procedure TLapeCompilerBase.SetUniqueTypeID(Typ: TLapeType);
begin
  Typ.TypeID := FTypeID;
  Inc(FTypeID);
end;

function TLapeCompilerBase.getConst(Value: Pointer; BaseType: ELapeBaseType): TLapeGlobalVar;
begin
  Assert(BaseType in LapeIntegerTypes + LapeBoolTypes + LapeCharTypes);

  Result := FConstants.Get(Self, BaseType, Value);
end;

function TLapeCompilerBase.getIntConst(Value: Int64; BaseType: ELapeBaseType = ltUnknown): TLapeGlobalVar;
begin
  if (BaseType = ltUnknown) then
    BaseType := DetermineIntType(Value)
  else
    BaseType := DetermineIntType(Value, BaseType, False);
  Assert(BaseType in LapeIntegerTypes);

  Result := FConstants.Get(Self, BaseType, @Value);
end;

function TLapeCompilerBase.getBoolConst(Value: Boolean; BaseType: ELapeBaseType = ltBoolean): TLapeGlobalVar;
var
  Kept: Int64;
begin
  Assert(BaseType in LapeBoolTypes);

  Kept := 0;
  if Value then
    Kept := (FBaseTypes[BaseType] as TLapeType_SubRange).Range.Hi;

  Result := FConstants.Get(Self, BaseType, @Kept);
end;

function TLapeCompilerBase.getCharConst(Value: Integer; BaseType: ELapeBaseType = ltAnsiChar): TLapeGlobalVar;
begin
  Assert(BaseType in LapeCharTypes);

  Result := FConstants.Get(Self, BaseType, @Value);
end;

// Not shared: few repeat
function TLapeCompilerBase.getFloatConst(Value: lpFloat; BaseType: ELapeBaseType = ltFloat): TLapeGlobalVar;
begin
  Assert(BaseType in LapeRealTypes);

  Result := FBaseTypes[BaseType].NewGlobalVarP();
  case BaseType of
    ltSingle:   PSingle(Result.Ptr)^ := Value;
    ltDouble:   PDouble(Result.Ptr)^ := Value;
    ltCurrency: PCurrency(Result.Ptr)^ := Value;
    {$IFNDEF Lape_NoExtended}
    ltExtended: PExtended(Result.Ptr)^ := Value;
    {$ENDIF}
  end;
  FManagedDeclarations.addDeclaration(Result);
end;

// Not shared: few repeat, but '' is the type's empty value
function TLapeCompilerBase.getStringConst(Value: lpString; BaseType: ELapeBaseType = ltString): TLapeGlobalVar;
begin
  Assert(BaseType in LapeStringTypes);
  if (Value = '') then
    Exit(FBaseTypes[BaseType].EmptyValue);

  Result := FBaseTypes[BaseType].NewGlobalVarStr(Value);
  FManagedDeclarations.addDeclaration(Result);
end;

function TLapeCompilerBase.getLabel(CodePos: Integer): TLapeGlobalVar;
begin
  Result := addManagedDecl(getGlobalType('!label').NewGlobalVarP()) as TLapeGlobalVar;

  PCodePos(Result.Ptr)^ := CodePos;
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
           (((Dest.VarPos.MemPos <> mpVar) and (Dest.VarPos.MemPos <> mpNone)) or Dest.HasType())
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

function TLapeCompilerBase.getTempVar(VarType: ELapeBaseType): TLapeStackTempVar;
begin
  Result := getTempVar(FBaseTypes[VarType]);
end;

function TLapeCompilerBase.getTempVar(VarType: TLapeType): TLapeStackTempVar;
begin
  Result := FStackInfo.getTempVar(VarType);
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

function TLapeCompilerBase.getGlobalVar(AName: lpString): TLapeGlobalVar;
var
  Decls: TLapeDeclArray;
  i: Integer;
begin
  Result := nil;
  Decls := GlobalScope.GetAll(AName);
  if (Length(Decls) = 0) or (not (Decls[0] is TLapeGlobalVar)) then
    Exit;
  Result := TLapeGlobalVar(Decls[0]);

  // (an override of it: another method of the name is not what is asked for)
  if (Result is TLapeMethod) then
    for i := 1 to High(Decls) do
      if (Decls[i] is TLapeMethod) and TLapeType_Method(TLapeGlobalVar(Decls[0]).VarType).EqualParams(TLapeType_Method(TLapeGlobalVar(Decls[i]).VarType)) then
        Result := TLapeGlobalVar(Decls[i]);
end;

function TLapeCompilerBase.getGlobalType(AName: lpString): TLapeType;
var
  Decl: TLapeDeclaration;
begin
  Result := nil;
  if GlobalScope.Get(AName, Decl) and (Decl is TLapeType) then
    Result := TLapeType(Decl);
end;

procedure TLapeCompilerBase.addMethodGenerate(Callback: TLapeGenerateMethod);
begin
  SetLength(FMethodGenerates, Length(FMethodGenerates) + 1);
  FMethodGenerates[High(FMethodGenerates)] := Callback;
end;

function TLapeCompilerBase.GenerateMethod(AName: lpString; AObjectType: TLapeType; const AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(FMethodGenerates) do
  begin
    Result := FMethodGenerates[i](AName, AObjectType, AParams, AResult);
    if (Result <> nil) then
      Exit;
  end;
end;

function TLapeCompilerBase.PickMethod(AName: lpString; const Methods: TLapeDeclArray; AObjectType: TLapeType; const AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;
var
  isGlobal: Boolean;
begin
  isGlobal := (AObjectType = nil);

  Result := FitMethod(Methods, isGlobal, AParams, AResult);
  if (Result = nil) then
    Result := GenerateMethod(AName, AObjectType, AParams, AResult);
  if (Result = nil) and isGlobal then
    Result := FitMethod(Methods, False, AParams, AResult);
end;

function TLapeCompilerBase.getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration;
var
  Stack: TLapeStackInfo;
begin
  if (AStackInfo = nil) then
    AStackInfo := FStackInfo;

  // the scope itself, then each one around it: what is global is around all
  Stack := AStackInfo;
  while (Stack <> nil) do
  begin
    Result := Stack.getDeclaration(AName, CheckWith and (not LocalOnly));

    if (Result is TLapeStackVar) and (TLapeStackVar(Result).Stack <> AStackInfo.VarStack) then
    begin
      Result := AStackInfo.getInheritedVar(TLapeStackVar(Result));
      if (Result = nil) then
        LapeExceptionFmt(lpeDeclarationOutOfScope, [AName]);
    end;

    if (Result <> nil) then
      Exit;
    if LocalOnly then
      Break;
    Stack := Stack.Owner;
  end;
end;

function TLapeCompilerBase.hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False): Boolean;
var
  Stack: TLapeStackInfo;
begin
  if (AStackInfo = nil) then
    AStackInfo := FStackInfo;
  if LocalOnly and AStackInfo.isReserved(AName) then
    Exit(True);

  Stack := AStackInfo;
  while (Stack <> nil) do
  begin
    if Stack.hasDeclaration(AName, not LocalOnly) then
      Exit(True);
    if LocalOnly then
      Break;
    Stack := Stack.Owner;
  end;

  Result := False;
end;

function TLapeCompilerBase.hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False): Boolean;
var
  Stack: TLapeStackInfo;
begin
  if (AStackInfo = nil) then
    AStackInfo := FStackInfo;

  Stack := AStackInfo;
  while (Stack <> nil) do
  begin
    if Stack.Has(ADecl) then
      Exit(True);
    if LocalOnly then
      Break;
    Stack := Stack.Owner;
  end;

  Result := False;
end;

function TLapeCompilerBase.getGlobalScope: TLapeStackInfo;
begin
  Result := FStackInfo.Root;
end;

procedure TLapeCompilerBase.Hint(Msg: lpString; Args: array of const; ADocPos: TDocPos);
begin
  Msg := ADocPos.Format(Format(Msg, Args));
  if ({$IFNDEF FPC}@{$ENDIF}FOnHint <> nil) then
    FOnHint(Self, Msg)
  else
    FHints := FHints + Msg + LineEnding;
end;

end.
