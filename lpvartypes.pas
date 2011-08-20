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
    lcoLooseSyntax,                    // {$X} {$EXTENDEDSYNTAX}
    lcoAutoInvoke,                     // {$F} {$AUTOINVOKE}
    lcoScopedEnums,                    // {$S} {$SCOPEDENUMS}
    lcoVarStringChecks                 // {$V} {$VARSTRINGCHECKS}  TODO
  );
  ECompilerOptionsSet = set of ECompilerOption;
  PCompilerOptionsSet = ^ECompilerOptionsSet;

const
  Lape_OptionsDef = [lcoShortCircuit, lcoAlwaysInitialize, lcoAutoInvoke];
  Lape_PackRecordsDef = 2;

type
  TLapeType = class;
  TLapeVar = class;
  TLapeStackVar = class;
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

  TResVar = {$IFDEF FPC}object{$ELSE}record{$ENDIF}
  private
    function getVariable: Boolean;
    function getConstant: Boolean;
  public
    VarType: TLapeType;
    VarPos: TVarPos;

    class function New(AVar: TLapeVar): TResVar; {$IFNDEF FPC}static;{$ENDIF}
    function HasType: Boolean;

    procedure Spill(Unlock: Integer = 0);
    function IncLock(Count: Integer = 1): TResVar;
    function Declock(Count: Integer = 1): TResVar;

    procedure setConstant(IsConst: Boolean; ChangeStack: Boolean); overload;
    procedure setConstant(IsConst: Boolean); overload;

    property isConstant: Boolean read getConstant write setConstant;
    property isVariable: Boolean read getVariable;
  end;

  ELapeParameterType = (lptNormal, lptConst, lptVar, lptOut);
  TLapeParameter = record
    ParType: ELapeParameterType;
    VarType: TLapeType;
    Default: TLapeVar;
  end;
  TLapeParameterList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeParameter>;

  PLapeVar = ^TLapeVar;
  TLapeVar = class(TLapeDeclaration)
  protected
    function getBaseType: ELapeBaseType; virtual;
    function getSize: Integer; virtual;
    function getLo: TLapeGlobalVar; virtual;
    function getHi: TLapeGlobalVar; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
  public
    VarType: TLapeType;
    isConstant: Boolean;

    constructor Create(AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    function HasType: Boolean;

    property BaseType: ELapeBaseType read getBaseType;
    property Size: Integer read getSize;
    property Lo: TLapeGlobalVar read getLo;
    property Hi: TLapeGlobalVar read getHi;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
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
    function getSize: Integer; override;
    function getInitialization: Boolean; override;
    function getFinalization: Boolean; override;
  public
    constructor Create(AParType: ELapeParameterType; AVarType: TLapeType; AStack: TLapeVarStack; AName: lpString = ''; ADocPos: PDocPos = nil; AList: TLapeDeclarationList = nil); reintroduce; virtual;
    property ParType: ELapeParameterType read FParType;
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
    function Equals(Other: TLapeGlobalVar): Boolean; reintroduce; virtual;
    function CompatibleWith(Other: TLapeGlobalVar): Boolean; virtual;

    property Ptr: Pointer read FPtr;
    property AsString: lpString read getAsString;
    property AsInteger: Int64 read getAsInt;
  end;

  TLapeType = class(TLapeDeclaration)
  protected
    FBaseType: ELapeBaseType;
    FCompiler: TLapeCompilerBase;
    FSize: Integer;
    FInit: (__Unknown, __Yes, __No);
    FAsString: lpString;

    procedure setBaseType(ABaseType: ELapeBaseType); virtual;
    function getBaseIntType: ELapeBaseType; virtual;
    function getSize: Integer; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
    function getAsString: lpString; virtual;
  public
    constructor Create(ABaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; virtual;
    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; reintroduce; virtual;
    function CompatibleWith(Other: TLapeType): Boolean; virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; virtual;
    function VarToString(AVar: Pointer): lpString; virtual;
    function VarToInt(AVar: Pointer): Int64; virtual;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; virtual;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; virtual;

    function NewGlobalVarP(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;
	{$IFNDEF Lape_NoWideString}
    function NewGlobalVarStr(Str: WideString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;
	{$ENDIF}
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;

    function CanHaveChild: Boolean; virtual;
    function HasChild(AName: lpString): Boolean; overload; virtual;
    function HasChild(ADecl: TLapeDeclaration): Boolean; overload; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; overload; virtual;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType; overload; virtual;
    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; virtual;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; virtual;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; overload; virtual;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Pos: PDocPos = nil): TResVar; overload; virtual;

    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); overload; virtual;
    procedure Finalize(AVar: TResVar; UseCompiler: Boolean = True; Pos: PDocPos = nil); overload; virtual;
    procedure Finalize(AVar: TLapeVar; var Offset: Integer; UseCompiler: Boolean = False; Pos: PDocPos = nil); overload; virtual;
    procedure Finalize(AVar: TLapeVar; UseCompiler: Boolean = False; Pos: PDocPos = nil); overload; virtual;

    property Compiler: TLapeCompilerBase read FCompiler;
    property BaseType: ELapeBaseType read FBaseType write setBaseType;
    property BaseIntType: ELapeBaseType read getBaseIntType;
    property Size: Integer read getSize;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
    property AsString: lpString read getAsString;
  end;

  TLapeType_Type = class(TLapeType)
  protected
    FTType: TLapeType;
    function getAsString: lpString; override;
  public
    constructor Create(AType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function HasChild(AName: lpString): Boolean; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;

    property TType: TLapeType read FTType;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeType_Integer<_Type> = class(TLapeType)
  protected type
    PType = ^_Type;
  var public
    function NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeType_Float<_Type> = class(TLapeType)
  protected type
    PType = ^_Type;
  var public
    function NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeType_Char<_Type> = class(TLapeType)
  protected type
    PType = ^_Type;
  var public
    function NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  TLapeType_UInt8 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt8>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int8 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int8>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UInt16 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt16>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int16 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int16>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UInt32 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt32>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int32 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int32>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UInt64 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt64>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int64 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int64>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_Single = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Single>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Double = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Double>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Currency = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Currency>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Extended = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Extended>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_AnsiChar = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Char<AnsiChar>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_WideChar = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Char<WideChar>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_Variant = class(TLapeType)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function NewGlobalVar(Val: Variant; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  TLapeType_SubRange = class(TLapeType)
  protected
    FRange: TLapeRange;
    FVarType: TLapeType;
    function getAsString: lpString; override;
  public
    constructor Create(ARange: TLapeRange; ACompiler: TLapeCompilerBase; AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    property Range: TLapeRange read FRange;
    property VarType: TLapeType read FVarType;
  end;

  TEnumMap = TStringList;
  TLapeType_Enum = class(TLapeType_SubRange)
  protected
    FMemberMap: TEnumMap;
    FSmall: Boolean;
    function getAsString: lpString; override;
  public
    FreeMemberMap: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase; AMemberMap: TEnumMap; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    function hasMember(AName: lpString): Boolean; virtual;
    function addMember(Value: Int16; AName: lpString): Int16; overload; virtual;
    function addMember(AName: lpString): Int16; overload; virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property MemberMap: TEnumMap read FMemberMap;
    property Small: Boolean read FSmall;
  end;

  TLapeType_Boolean = class(TLapeType_Enum)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_Bool = class(TLapeType_SubRange)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_ByteBool = class(TLapeType_Bool)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_WordBool = class(TLapeType_Bool)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_LongBool = class(TLapeType_Bool)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  {$IFDEF Lape_SmallCode}
  TLapeType_EvalBool = TLapeType_Boolean;
  {$ELSE}
  TLapeType_EvalBool = TLapeType_LongBool;
  {$ENDIF}

  TLapeType_Set = class(TLapeType)
  protected
    FRange: TLapeType_SubRange;
    FSmall: Boolean;
    function getAsString: lpString; override;
  public
    constructor Create(ARange: TLapeType_SubRange; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Values: array of UInt8; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;

    property Range: TLapeType_SubRange read FRange;
    property Small: Boolean read FSmall;
  end;

  TLapeType_Pointer = class(TLapeType)
  protected
    FPType: TLapeType;
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; PointerType: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil; AsValue: Boolean = True): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function HasType: Boolean;
    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property PType: TLapeType read FPType;
  end;

  TLapeType_DynArray = class(TLapeType_Pointer)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    procedure VarSetLength(var AVar: Pointer; ALen: Integer); overload; virtual;
    procedure VarSetLength(AVar, ALen: TResVar; var Offset: Integer; Pos: PDocPos = nil); overload; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_StaticArray = class(TLapeType_DynArray)
  protected
    FRange: TLapeRange;

    function getSize: Integer; override;
    function getAsString: lpString; override;
  public
    constructor Create(ARange: TLapeRange; ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;
    function NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); override;

    property Range: TLapeRange read FRange;
  end;

  TLapeType_String = class(TLapeType_DynArray)
  public
    function VarToString(AVar: Pointer): lpString; override;
    function NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;
  {$IFNDEF Lape_NoWideString}
    function NewGlobalVarStr(Str: WideString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: WideString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;
  {$ENDIF}
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_AnsiString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_WideString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UnicodeString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_ShortString = class(TLapeType_StaticArray)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ASize: UInt8 = High(UInt8); AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: ShortString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TRecordField = record
    Offset: Word;
    FieldType: TLapeType;
  end;
  TRecordFieldMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TRecordField>;

  TLapeType_Record = class(TLapeType)
  protected
    FFieldMap: TRecordFieldMap;
    function getAsString: lpString; override;
  public
    FreeFieldMap: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;
    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;

    procedure addField(FieldType: TLapeType; AName: lpString; Alignment: Byte = 1); virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;
    function NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;

    function HasChild(AName: lpString): Boolean; override;
    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType; override;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); override;

    property FieldMap: TRecordFieldMap read FFieldMap;
  end;

  TLapeType_Union = class(TLapeType_Record)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil); override;
    procedure addField(FieldType: TLapeType; AName: lpString; Alignment: Byte = 1); override;
  end;

  TLapeType_Method = class(TLapeType)
  protected
    FParams: TLapeParameterList;
    procedure setBaseType(ABaseType: ELapeBaseType); override;
    function getSize: Integer; override;
    function getAsString: lpString; override;
    function getParamSize: Integer; virtual;
    function getParamInitialization: Boolean; virtual;
  public
    FreeParams: Boolean;
    Res: TLapeType;

    constructor Create(ACompiler: TLapeCompilerBase; AParams: TLapeParameterList; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(ACompiler: TLapeCompilerBase; AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;
    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; override;

    function EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True): Boolean; virtual;
    procedure addParam(Param: TLapeParameter); virtual;
    procedure setImported(AVar: TLapeGlobalVar; isImported: Boolean); virtual;

    function NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;
    function NewGlobalVar(CodePos: TCodePos; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; overload; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property Params: TLapeParameterList read FParams;
    property ParamSize: Integer read getParamSize;
    property ParamInitialization: Boolean read getParamInitialization;
  end;

  TLapeGetOverloadedMethod = function(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method;
    AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar of object;

  TLapeType_OverloadedMethod = class(TLapeType)
  protected
    FMethods: TLapeDeclarationList;
  public
    OnFunctionNotFound: TLapeGetOverloadedMethod;
    NeedFullMatch: Boolean;
    FreeMethods: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AMethods: TLapeDeclarationList; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    procedure addMethod(AMethod: TLapeGlobalVar; DoOverride: Boolean = False); virtual;
    function overrideMethod(AMethod: TLapeGlobalVar): TLapeGlobalVar; virtual;

    function getMethod(AType: TLapeType_Method): TLapeGlobalVar; overload; virtual;
    function getMethod(AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar; overload; virtual;
    function NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;

    function HasChild(ADecl: TLapeDeclaration): Boolean; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;

    property Methods: TLapeDeclarationList read FMethods;
  end;

  TLapeWithDeclRec = record
    WithVar: PLapeVar;
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

    function EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType; override;
    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

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
    function getCount: Integer; virtual;
    function getTotalSize: Integer; virtual;
    function getTotalParamSize: Integer; virtual;
    function getTotalNoParamSize: Integer; virtual;
    function getInitialization: Boolean; virtual;
    function getFinalization: Boolean; virtual;
  public
    Owner: TLapeStackInfo;
    FreeVars: Boolean;
    ForceInitialization: Boolean;
    CodePos: Integer;

    constructor Create(AlwaysInitialize: Boolean = True; AOwner: TLapeStackInfo = nil; ManageVars: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;

    function getDeclaration(Name: lpString; CheckWith: Boolean = True): TLapeDeclaration; virtual;
    function hasDeclaration(Name: lpString; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(Decl: TLapeDeclaration; CheckWith: Boolean = True): Boolean; overload; virtual;

    function getTempVar(VarType: TLapeType; Lock: Integer = 1): TLapeStackTempVar; virtual;
    function addDeclaration(Decl: TLapeDeclaration): Integer; override;
    function addVar(StackVar: TLapeStackVar): TLapeStackVar; overload; virtual;
    function addVar(VarType: TLapeType; Name: lpString = ''): TLapeStackVar; overload; virtual;
    function addVar(ParType: ELapeParameterType; VarType: TLapeType; Name: lpString = ''): TLapeStackVar; overload; virtual;
    function addWith(AWith: TLapeWithDeclRec): Integer; virtual;
    procedure delWith(Count: Integer); virtual;

    property VarStack: TLapeVarStack read FVarStack;
    property WithStack: TLapeWithDeclarationList read FWithStack;
    property Vars[Index: Integer]: TLapeStackVar read getVar; default;
    property Count: Integer read getCount;
    property TotalSize: Integer read getTotalSize;
    property TotalParamSize: Integer read getTotalParamSize;
    property TotalNoParamSize: Integer read getTotalNoParamSize;
    property NeedInitialization: Boolean read getInitialization;
    property NeedFinalization: Boolean read getFinalization;
    property OldStackPos: Integer read FOldStackPos;
    property OldMaxStack: Integer read FOldMaxStack;
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

    //function _JmpIf(Target: TCodePos; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    //function _JmpIf(Target: TCodePos; Cond: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIf(Jmp: TCodeOffset; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIf(Jmp: TCodeOffset; Cond: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    function _JmpRIfNot(Jmp: TCodeOffset; Cond: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;

    function _Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer; overload; virtual;
    function _Eval(AProc: TLapeEvalProc; Dest, Left, Right: TResVar; Pos: PDocPos = nil): Integer; overload; virtual;
  end;

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

    procedure Reset; virtual;
    procedure setEmitter(AEmitter: TLapeCodeEmitter); virtual;
  public
    FreeEmitter: Boolean;

    constructor Create(AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure FinalizeVar(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil); overload; virtual;
    procedure FinalizeVar(AVar: TResVar; Pos: PDocPos = nil); overload; virtual;

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

    procedure getDestVar(var Dest, Res: TResVar; Op: EOperator); virtual;
    function getTempVar(VarType: ELapeBaseType; Lock: Integer = 1): TLapeStackTempVar; overload; virtual;
    function getTempVar(VarType: TLapeType; Lock: Integer = 1): TLapeStackTempVar; overload; virtual;
    function getTempStackVar(VarType: ELapeBaseType): TResVar; overload; virtual;
    function getTempStackVar(VarType: TLapeType): TResVar; overload; virtual;
    function getPointerType(PType: ELapeBaseType): TLapeType_Pointer; overload; virtual;
    function getPointerType(PType: TLapeType): TLapeType_Pointer; overload; virtual;
    function getTypeVar(AType: ELapeBaseType): TLapeGlobalVar; overload; virtual;
    function getTypeVar(AType: TLapeType): TLapeGlobalVar; overload; virtual;

    function getGlobalVar(AName: lpString): TLapeGlobalVar; virtual;
    function getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration; overload; virtual;
    function getDeclaration(AName: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration; overload; virtual;
    function hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(AName: lpString; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;
    function hasDeclaration(ADecl: TLapeDeclaration; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; overload; virtual;

    property StackInfo: TLapeStackInfo read FStackInfo;
    property BaseTypes: TLapeBaseTypes read FBaseTypes;

    property GlobalDeclarations: TLapeDeclarationList read FGlobalDeclarations;
    property ManagedDeclarations: TLapeDeclarationList read FManagedDeclarations;
    property Globals[AName: lpString]: TLapeGlobalVar read getGlobalVar; default;
  published
    property Emitter: TLapeCodeEmitter read FEmitter write setEmitter;
    property Options: ECompilerOptionsSet read FOptions write FBaseOptions default Lape_OptionsDef;
    property Options_PackRecords: UInt8 read FOptions_PackRecords write FBaseOptions_PackRecords default Lape_PackRecordsDef;
  end;

function getTypeArray(Arr: array of TLapeType): TLapeTypeArray;
procedure ClearBaseTypes(var Arr: TLapeBaseTypes);
procedure LoadBaseTypes(var Arr: TLapeBaseTypes; Compiler: TLapeCompilerBase);

const
  BigLock = 256;

  NullResVar: TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpNone;  GlobalVar: nil));
  VarResVar:  TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpVar;   StackVar : nil));
  StackResVar:TResVar = (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpStack; ForceVariable: False));

  NullParameter: TLapeParameter = (ParType: lptNormal; VarType: nil; Default: nil);
  NullWithDecl: TLapeWithDeclRec = (WithVar: nil; WithType: nil);
  NullVarRef: TLapeVarRef = (Lock: -1; ResVar: (VarType: nil; VarPos: (isPointer: False; Offset: 0; MemPos: mpNone; GlobalVar: nil)); RefVar: nil);

  Lape_RefParams = [lptConst, lptOut, lptVar];
  Lape_ValParams = [lptConst, lptNormal];

var
  EmptyStackInfo: TLapeEmptyStack = nil;
  _ResVar: TResVar;

implementation

uses
  Variants,
  lpeval, lpexceptions, lpinterpreter;

function getTypeArray(Arr: array of TLapeType): TLapeTypeArray;
var
  i: Integer;
begin
  SetLength(Result, Length(Arr));
  for i := 0 to High(Arr) do
    Result[i] := Arr[i];
end;

procedure ClearBaseTypes(var Arr: TLapeBaseTypes);
var
  BaseType: ELapeBaseType;
begin
  for BaseType := Low(ELapeBaseType) to High(ELapeBaseType) do
    if (Arr[BaseType] <> nil) then
      FreeAndNil(Arr[BaseType]);
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
  Arr[ltPointer] := TLapeType_Pointer.Create(Compiler, nil, LapeTypeToString(ltPointer));
end;

function TResVar.getVariable: Boolean;
begin
  Result := ((VarPos.MemPos = mpStack) and (VarPos.isPointer or VarPos.ForceVariable)) or
    ((VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) and (not VarPos.GlobalVar.isConstant)) or
    ((VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) and (not VarPos.StackVar.isConstant));
end;

function TResVar.getConstant: Boolean;
begin
  if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    Result := VarPos.GlobalVar.isConstant
  else if (VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) then
    Result := VarPos.StackVar.isConstant
  else if (VarPos.MemPos = mpStack) then
    Result := not VarPos.ForceVariable
  else
    Result := True;
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

function TResVar.Declock(Count: Integer = 1): TResVar;
begin
  Result := Self;
  if (Count > 0) and (VarPos.MemPos = mpVar) and
     (VarPos.StackVar <> nil) and (VarPos.StackVar is TLapeStackTempVar)
  then
    TLapeStackTempVar(VarPos.StackVar).DecLock(Count);
end;

procedure TResVar.setConstant(IsConst: Boolean; ChangeStack: Boolean);
begin
  if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) then
    VarPos.GlobalVar.isConstant := isConst
  else if (VarPos.MemPos = mpVar) and (VarPos.StackVar <> nil) then
    VarPos.StackVar.isConstant := isConst
  else if ChangeStack and (VarPos.MemPos = mpStack) then
    VarPos.ForceVariable := not isConst;
end;

procedure TResVar.setConstant(IsConst: Boolean);
begin
  setConstant(IsConst, True);
end;

function TLapeVar.getBaseType: ELapeBaseType;
begin
  if HasType() then
    Result := VarType.BaseType
  else
    Result := ltUnknown;
end;

function TLapeVar.getSize: Integer;
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

procedure TLapeStackVar.setStack(Stack: TLapeVarStack);
begin
  if (Stack <> FStack) then
  begin
    if (FStack <> nil) then
      FStack.DeleteItem(Self);
    FStack := Stack;
    if (FStack <> nil) then
      FStack.add(Self);
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
      Result := Result + FStack[i].Size;
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
  isConstant := True;
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

function TLapeParameterVar.getSize: Integer;
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
  isConstant := (FParType = lptConst);
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
    FPtr := {$IFDEF FPC}Align(FBasePtr, 4){$ELSE}Pointer(PtrUInt(FBasePtr) + PtrUInt(FBasePtr) mod 4){$ENDIF};
    {$ELSE}
    //Assure aligned memory
    FBasePtr := AllocMem(Size + 16);
    FPtr := {$IFDEF FPC}Align(FBasePtr, 16){$ELSE}Pointer(PtrUInt(FBasePtr) + PtrUInt(FBasePtr) mod 16){$ENDIF};
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
      VarType.EvalConst(op_Assign, Result, Self);
  end;
end;

function TLapeGlobalVar.Equals(Other: TLapeGlobalVar): Boolean;
var
  Res: TLapeGlobalVar;
begin
  if (Other <> nil) and HasType() then
    Res := VarType.EvalConst(op_cmp_Equal, Self, Other)
  else
    Exit(False);

  try
    Result := (Res.BaseType in LapeBoolTypes) and (Res.AsInteger <> 0);
  finally
    Res.Free();
  end;
end;

function TLapeGlobalVar.CompatibleWith(Other: TLapeGlobalVar): Boolean;
begin
  Result := (Other <> nil) and HasType() and VarType.CompatibleWith(Other.VarType);
end;

function TLapeType.getSize: Integer;
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
  if (not (FBaseType in LapeOrdinalTypes + [ltPointer])) then
    Result := ltUnknown
  else if (FBaseType in LapeIntegerTypes) then
    Result := FBaseType
  else 
    Result := DetermineIntType(Size, False);
end;

function TLapeType.getInitialization: Boolean;
begin
  if (FInit = __Unknown) then
    if (FBaseType in LapeNoInitTypes) then
      FInit := __No
    else
      FInit := __Yes;
  Result := (FInit = __Yes) and (Size > 0);
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

  FBaseType := ABaseType;
  FCompiler := ACompiler;
  FSize := 0;
  FInit := __Unknown;
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
    Result := AsString;
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
  if (FCompiler = nil) or (BaseIntType = ltUnknown) then
    Result := nil
  else with FCompiler, getBaseType(BaseIntType) do
    Result := addManagedVar(NewGlobalVarP(LapeTypeLow[BaseIntType])) as TLapeGlobalVar;
end;

function TLapeType.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (BaseIntType = ltUnknown) then
    Result := nil
  else with FCompiler, getBaseType(BaseIntType) do
    Result := addManagedVar(NewGlobalVarP(LapeTypeHigh[BaseIntType])) as TLapeGlobalVar;
end;

function TLapeType.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  Result := (Other = Self) or (
    (Other <> nil) and
    ((not ContextOnly) or (ClassType = Other.ClassType)) and
    (Other.BaseType = BaseType) and
    (Other.Size = Size) and
    (Other.AsString = AsString)
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
  Result := TLapeClassType(Self.ClassType).Create(FBaseType, FCompiler, Name, @_DocPos);
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

{$IFNDEF Lape_NoWideString}
function TLapeType.NewGlobalVarStr(Str: WideString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(UnicodeString(Str), AName, ADocPos);
end;
{$ENDIF}

function TLapeType.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeImpossible);
end;

function TLapeType.CanHaveChild: Boolean;
begin
  Result := FBaseType in LapeStructTypes;
end;

function TLapeType.HasChild(AName: lpString): Boolean;
var
  DotName: TLapeGlobalVar;
begin
  if (not CanHaveChild()) or (FCompiler = nil) then
    Exit(False);
  DotName := FCompiler.getBaseType(ltString).NewGlobalVarStr(AName);
  try
    Result := EvalRes(op_Dot, DotName) <> nil;
  finally
    DotName.Free();
  end;
end;

function TLapeType.HasChild(ADecl: TLapeDeclaration): Boolean;
begin
  Result := False;
end;

function TLapeType.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  Assert(FCompiler <> nil);

  if (Op = op_Addr) then
    Result := FCompiler.getPointerType(Self)
  else if (Op = op_Assign) and (Right <> nil) and (getEvalRes(Op, FBaseType, Right.BaseType) <> ltUnknown) then
    Result := Self
  else if (Right = nil) then
    Result := FCompiler.getBaseType(getEvalRes(Op, FBaseType, ltUnknown))
  else
    Result := FCompiler.getBaseType(getEvalRes(Op, FBaseType, Right.BaseType));
end;

function TLapeType.EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType;
begin
  if (Right <> nil) then
    Result := EvalRes(Op, Right.VarType)
  else
    Result := EvalRes(Op, TLapeType(nil));
end;

function TLapeType.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  Assert((Left = nil) or (Left.VarType = Self));

  Result := ((Left = nil) or Left.isConstant) and ((Right = nil) or Right.isConstant);
  if (not Result) and (Right <> nil) and Right.isConstant then
    if (op = op_Dot) and CanHaveChild() and (Right.BaseType = ltString) then
      Result := HasChild(PlpString(Right.Ptr)^)
    else if (op = op_Index) and (BaseType in [ltUnknown{overloaded method}, ltShortString, ltStaticArray]) then
      Result := Right.HasType() and (Right.VarType.BaseIntType <> ltUnknown);
end;

function TLapeType.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  EvalProc: TLapeEvalProc;
  ResType: TLapeType;

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
          CastVar := Left.VarType.EvalConst(op_Assign, Left.VarType.NewGlobalVarP(), Right)
        else
          CastVar := Right.VarType.EvalConst(op_Assign, Right.VarType.NewGlobalVarP(), Left);

      try
        if DoRight then
          Res := CastVar.VarType.EvalConst(op, Left, CastVar)
        else
          Res := CastVar.VarType.EvalConst(op, CastVar, Right);
        Result := True;
      finally
        CastVar.Free();
      end;
    except
      Result := False;
    end;
  end;

begin
  Result := nil;
  Assert(FCompiler <> nil);
  Assert((Left <> nil) or (Right <> nil));

  if (Left = nil) then
  begin
    Left := Right;
    Right := nil;
  end;
  if (Op = op_UnaryPlus) then
    Exit(Left);

  try
    if (Right = nil) then
    begin
      ResType := EvalRes(Op);
      if (ResType <> nil) or ((op = op_Deref) and ((not Left.HasType()) or (Left.VarType.BaseType = ltPointer))) then
        if (op = op_Addr) then
          if Left.isConstant then
            LapeException(lpeVariableExpected)
          else
            Exit(TLapeGlobalVar.Create(ResType, @Left.Ptr))
        else if (op = op_Deref) then
          Exit(TLapeGlobalVar.Create(ResType, PPointer(Left.Ptr)^));

      EvalProc := getEvalProc(Op, FBaseType, ltUnknown);
    end
    else if (op <> op_Assign) or Left.VarType.CompatibleWith(Right.VarType) then
    begin
      EvalProc := getEvalProc(Op, FBaseType, Right.BaseType);
      ResType := EvalRes(Op, Right);
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
          LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op), AsString, Right.VarType.AsString])
        else
          Exit
      else if (op in UnaryOperators) then
        LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), AsString])
      else
        LapeExceptionFmt(lpeIncompatibleOperator, [LapeOperatorToString(op)]);

    if (Op = op_Assign) then
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
    if (op <> op_Assign) and (Result <> nil) and (Left <> nil) then
      Result.isConstant := (op <> op_Deref) and Left.isConstant and ((Right = nil) or Right.isConstant);
  end;
end;

function TLapeType.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  EvalProc: TLapeEvalProc;

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
      if DoRight then
        Right.VarType := Left.VarType
      else
        Left.VarType := Right.VarType;
      Res := Left.VarType.Eval(op, Dest, Left, Right, Offset, Pos);
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
          Res := CastVar.VarType.Eval(op, Dest, Left, CastVar.VarType.Eval(op_Assign, tmpVar, CastVar, Right, Offset, Pos), Offset, Pos)
        else
          Res := CastVar.VarType.Eval(op, Dest, CastVar.VarType.Eval(op_Assign, tmpVar, CastVar, Left, Offset, Pos), Right, Offset, Pos);
        Result := True;
      finally
        CastVar.Spill(1);
      end;
    except
      Result := False;
    end;
  end;

begin
  Result := NullResVar;
  Assert(FCompiler <> nil);
  Assert((Left.VarPos.MemPos <> NullResVar.VarPos.MemPos) or (Right.VarPos.MemPos <> NullResVar.VarPos.MemPos));

  if (Left.VarPos.MemPos = NullResVar.VarPos.MemPos) then
  begin
    Left := Right;
    Right := NullResVar;
  end;
  if (op = op_UnaryPlus) then
  begin
    Dest.Spill();
    Exit(Left);
  end;

  Result.VarType := EvalRes(Op, Right.VarType);
  if (not Result.HasType()) and (op = op_Deref) and ((not Left.HasType()) or (Left.VarType.BaseType = ltPointer)) then
    Result.VarType := TLapeType.Create(ltUnknown, FCompiler);

  try
    FCompiler.getDestVar(Dest, Result, op);

    if (not Right.HasType()) then
      EvalProc := getEvalProc(Op, FBaseType, ltUnknown)
    else if (op <> op_Assign) or Left.VarType.CompatibleWith(Right.VarType) then
      EvalProc := getEvalProc(Op, FBaseType, Right.VarType.BaseType)
    else
      EvalProc := nil;

    if (not Result.HasType()) or (not ValidEvalFunction(EvalProc)) then
      if (op = op_Assign) and Right.HasType() then
        LapeExceptionFmt(lpeIncompatibleAssignment, [Right.VarType.AsString, AsString])
      else if (not (op in UnaryOperators)) and ((not Left.HasType()) or (not Right.HasType()) or (not Left.VarType.Equals(Right.VarType, False))) then
        if (Left.HasType() and Right.HasType()  and Left.VarType.Equals(Right.VarType, False)) or
          (((not Left.HasType())  or  (not Right.HasType())   or  (Left.VarType.Size >= Right.VarType.Size)) and (not TryCast(True, Result)) and (not TryCast(False, Result))) or
          ((Left.HasType() and Right.HasType()) and (Left.VarType.Size <  Right.VarType.Size)  and (not TryCast(False, Result)) and (not TryCast(True, Result)))
        then
          LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op), AsString, Right.VarType.AsString])
        else
          Exit
      else if (op in UnaryOperators) then
        LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), AsString])
      else
        LapeExceptionFmt(lpeIncompatibleOperator, [LapeOperatorToString(op)]);

    if (op = op_Assign) then
    begin
      if (not Left.HasType()) or (not Right.HasType()) or (Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
        LapeException(lpeInvalidAssignment);

      FCompiler.Emitter._Eval(EvalProc, Left, Right, NullResVar, Offset, Pos);
      Result := Left;
    end
    else
    begin
      if (op = op_Addr) and (not Left.isVariable) then
        LapeException(lpeVariableExpected);
      FCompiler.Emitter._Eval(EvalProc, Result, Left, Right, Offset, Pos);
    end;

    if (op = op_Deref) then
      Result.VarPos.isPointer := (Result.VarPos.MemPos = mpVar);
    if (op <> op_Assign) then
      Result.setConstant((op <> op_Deref) and Result.isConstant, False);
  finally
    if Result.HasType() and (Result.VarType.ClassType = TLapeType) and (Result.VarType.BaseType = ltUnknown) then
      FreeAndNil(Result.VarType);
  end;
end;

function TLapeType.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Pos: PDocPos = nil): TResVar;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := Eval(op, Dest, Left, Right, Offset, Pos);
end;

procedure TLapeType.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);
var
  EmptyVar, tmpVar: TResVar;
  wasConstant: Boolean;

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

begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) or (not NeedFinalization) then
    Exit;
  if (AVar.VarPos.MemPos = mpMem) and (AVar.VarPos.GlobalVar <> nil) and FullNil(AVar.VarPos.GlobalVar.Ptr, Size) then
    Exit;

  wasConstant := AVar.isConstant;
  if wasConstant then
    AVar.isConstant := False;

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
      Eval(op_Assign, tmpVar, AVar, EmptyVar, Offset, Pos)
    else if (AVar.VarPos.MemPos = mpMem) and (AVar.VarPos.GlobalVar <> nil) then
      EvalConst(op_Assign, AVar.VarPos.GlobalVar, EmptyVar.VarPos.GlobalVar);
  finally
    if (not UseCompiler) or (FCompiler = nil) then
      FreeAndNil(EmptyVar.VarPos.GlobalVar);
    if wasConstant then
      AVar.isConstant := True;
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
end;

function TLapeType_Type.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Type;
begin
  Result := TLapeClassType(Self.ClassType).Create(FTType, FCompiler, Name, @_DocPos);
end;

function TLapeType_Type.HasChild(AName: lpString): Boolean;
begin
  Result := (FTType is TLapeType_Enum) and TLapeType_Enum(FTType).hasMember(AName);
end;

function TLapeType_Type.EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType;
begin
  if (Op = op_Dot) and (FTType <> nil) and (Right <> nil) and (Right.BaseType = ltString) and
     (FTType is TLapeType_Enum) and TLapeType_Enum(FTType).hasMember(PlpString(Right.Ptr)^)
  then
    Result := FTType
  else
    Result := inherited;
end;

function TLapeType_Type.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  FieldName: lpString;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Dot) and (Right <> nil) and Right.HasType() and (Right.VarType.BaseType = ltString) and (FTType is TLapeType_Enum) then
  begin
    Assert(Right.Ptr <> nil);
    FieldName := PlpString(Right.Ptr)^;

    if (FieldName <> '') and TLapeType_Enum(FTType).hasMember(FieldName) then
      Result := FTType.NewGlobalVarStr(FieldName)
    else
      LapeExceptionFmt(lpeUnknownDeclaration, [FieldName]);
  end
  else
    Result := inherited;
end;

function TLapeType_Integer{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PType(Result.Ptr)^ := Val;
end;

function TLapeType_Integer{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
{$IFNDEF FPC}var a: Int64; b: PType;{$ENDIF}
begin
  {$IFDEF FPC}
  Result := NewGlobalVar(StrToInt64(Str), AName, ADocPos);
  {$ELSE}
  a := StrToInt64(Str); b := @a;
  Result := NewGlobalVar(b^ , AName, ADocPos);
  {$ENDIF}
end;

function TLapeType_Float{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PType(Result.Ptr)^ := Val;
end;

function TLapeType_Float{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
{$IFNDEF FPC}var a: Extended; b: PType;{$ENDIF}
begin
  {$IFDEF FPC}
  Result := NewGlobalVar(StrToFloatDot(Str), AName, ADocPos);
  {$ELSE}
  a := StrToFloatDot(Str); b := @a;
  Result := NewGlobalVar(b^ , AName, ADocPos);
  {$ENDIF}
end;

function TLapeType_Char{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PType(Result.Ptr)^ := Val;
end;

function TLapeType_Char{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
{$IFNDEF FPC}var a: WideChar; b: PType;{$ENDIF}
var
  c: Integer;
begin
  if (Length(Str) <> 1) then
    try
      if (Str[1] = '#') then
        Delete(Str, 1, 1);
      c := StrToInt(Str);
      Result := NewGlobalVarP(nil, AName, ADocPos);
      case Size of
        SizeOf(UInt8):  PUInt8(Result.Ptr)^ := c;
        SizeOf(UInt16): PUInt16(Result.Ptr)^ := c;
        else LapeException(lpeImpossible);
      end;
      Exit;
    except
      LapeExceptionFmt(lpeInvalidValueForType, [AsString]);
    end;

  {$IFDEF FPC}
  Result := NewGlobalVar(_Type(Str[1]), AName, ADocPos);
  {$ELSE}
  a := Str[1]; b := @a;
  Result := NewGlobalVar(b^, AName, ADocPos);
  {$ENDIF}
end;

constructor TLapeType_UInt8.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt8, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int8.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt8, ACompiler, AName, ADocPos);
end;

constructor TLapeType_UInt16.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt16, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int16.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt16, ACompiler, AName, ADocPos);
end;

constructor TLapeType_UInt32.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt32, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int32.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt32, ACompiler, AName, ADocPos);
end;

constructor TLapeType_UInt64.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt64, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int64.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt64, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Single.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltSingle, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Double.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltDouble, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Currency.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltCurrency, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Extended.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltExtended, ACompiler, AName, ADocPos);
end;

constructor TLapeType_AnsiChar.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltAnsiChar, ACompiler, AName, ADocPos);
end;

constructor TLapeType_WideChar.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltWideChar, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Variant.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltVariant, ACompiler, AName, ADocPos);
end;

function TLapeType_Variant.NewGlobalVar(Val: Variant; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PVariant(Result.Ptr)^ := Val;
end;

function TLapeType_Variant.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVar(Str, AName, ADocPos);
end;

function TLapeType_SubRange.getAsString: lpString;
begin
  if (FAsString = '') then
    if (FVarType <> nil) then
      FAsString := FVarType.VarToString(@FRange.Lo) + '..' + FVarType.VarToString(@FRange.Hi)
    else
      FAsString := IntToStr(FRange.Lo) + '..' + IntToStr(FRange.Hi);
  Result := inherited;
end;

constructor TLapeType_SubRange.Create(ARange: TLapeRange; ACompiler: TLapeCompilerBase; AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUnknown, ACompiler, AName, ADocPos);
  if (AVarType = nil) and (ACompiler <> nil) then
    AVarType := ACompiler.getBaseType(DetermineIntType(ARange.Lo, ARange.Hi));
  if (AVarType <> nil) then
    FBaseType := AVarType.BaseType;
  FRange := ARange;
  FVarType := AVarType;
end;

function TLapeType_SubRange.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (ToStr.Methods = nil) or (FCompiler = nil) or (FVarType = nil) then
    Exit;

  Index := ToStr.Methods.Items.IndexOf(ToStr.getMethod(getTypeArray([FVarType]), FCompiler.getBaseType(ltString)));
  if (Index < 0) then
    Exit;

  Result := 'begin Result := ToString['+IntToStr(Index)+'](Param0); end;';
end;

function TLapeType_SubRange.VarToString(AVar: Pointer): lpString;
begin
  if (FVarType <> nil) then
    Result := FVarType.VarToString(AVar)
  else
    Result := IntToStr(VarToInt(AVar));
end;

function TLapeType_SubRange.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (BaseIntType = ltUnknown) then
    Result := nil
  else
    Result := FCompiler.addManagedVar(NewGlobalVarStr(IntToStr(Range.Lo))) as TLapeGlobalVar;
end;

function TLapeType_SubRange.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (BaseIntType = ltUnknown) then
    Result := nil
  else
    Result := FCompiler.addManagedVar(NewGlobalVarStr(IntToStr(Range.Hi))) as TLapeGlobalVar;
end;

function TLapeType_SubRange.CreateCopy: TLapeType;
type TLapeClassType = class of TLapeType_SubRange;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FCompiler, FVarType, Name, @_DocPos);
  Result.FBaseType := FBaseType;
end;

function TLapeType_SubRange.NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Value < FRange.Lo) or (Value > FRange.Hi) then
    LapeException(lpeOutOfTypeRange);

  Result := NewGlobalVarP(nil, AName);
  case BaseIntType of
    ltInt8:   PInt8(Result.Ptr)^   := Value;
    ltUInt8:  PUInt8(Result.Ptr)^  := Value;
    ltInt16:  PInt16(Result.Ptr)^  := Value;
    ltUInt16: PUInt16(Result.Ptr)^ := Value;
    ltInt32:  PInt32(Result.Ptr)^  := Value;
    ltUInt32: PUInt32(Result.Ptr)^ := Value;
    ltInt64:  PInt64(Result.Ptr)^  := Value;
    ltUInt64: PUInt64(Result.Ptr)^ := Value;
  end;
end;

function TLapeType_SubRange.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  with FVarType.NewGlobalVarStr(Str) do
  try
    Result := NewGlobalVar(AsInteger, AName, ADocPos);
  finally
    Free();
  end;
end;

function TLapeType_Enum.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := '(';
    for i := 0 to FMemberMap.Count - 1 do
    begin
      if (FMemberMap[i] = '') then
        Continue;
      if (FAsString <> '(') then
        FAsString := FAsString + ', ';
      FAsString := FAsString + FMemberMap[i] + '=' + IntToStr(i);
    end;
    FAsString := FAsString + ')';
  end;
  Result := inherited;
end;

constructor TLapeType_Enum.Create(ACompiler: TLapeCompilerBase; AMemberMap: TEnumMap; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(NullRange, ACompiler, nil, AName, ADocPos);
  FBaseType := ltLargeEnum;
  FVarType := nil;

  FreeMemberMap := (AMemberMap = nil);
  if (AMemberMap = nil) then
  begin
    AMemberMap := TEnumMap.Create();
    AMemberMap.CaseSensitive := {$IFDEF Lape_CaseSensitive}True{$ELSE}False{$ENDIF};
  end;
  FMemberMap := AMemberMap;

  while (FRange.Lo < FMemberMap.Count) and (FMemberMap[FRange.Lo] = '') do Inc(FRange.Lo);
  FRange.Hi := FMemberMap.Count - 1;
  FSmall := (FRange.Hi <= Ord(High(ELapeSmallEnum)));
  if FSmall then
    FBaseType := ltSmallEnum;
end;

destructor TLapeType_Enum.Destroy;
begin
  if FreeMemberMap then
    FMemberMap.Free();
  inherited;
end;

function TLapeType_Enum.hasMember(AName: lpString): Boolean;
begin
  Result := FMemberMap.IndexOf(AName) > -1;
end;

function TLapeType_Enum.addMember(Value: Int16; AName: lpString): Int16;
var
  i: Integer;
begin
  if (Value < FMemberMap.Count) then
    LapeException(lpeInvalidRange)
  else if (AName = '') or hasMember(AName) then
    LapeException(lpeDuplicateDeclaration);

  FAsString := '';
  Result:= Value;
  FRange.Hi := Value;
  if (FMemberMap.Count = 0) then
    FRange.Lo := Value;

  for i := FMemberMap.Count to Value - 1 do
    FMemberMap.add('');
  FMemberMap.add(AName);

  FSmall := (FRange.Hi <= Ord(High(ELapeSmallEnum)));
  if (not FSmall) then
    FBaseType := ltLargeEnum;
end;

function TLapeType_Enum.addMember(AName: lpString): Int16;
begin
  Result := addMember(FMemberMap.Count, AName);
end;

function TLapeType_Enum.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to FMemberMap.Count - 1 do
    if (FMemberMap[i] <> '') then
    begin
      if (Result <> '') then
        Result := Result + ', ';
      Result := Result + #39 + FMemberMap[i] + #39;
    end;
  Result := Format(
    'type TEnumToString = function(const Arr; Index, Lo, Hi: Int32): string;' + LineEnding +
    'begin Result := TEnumToString('+AIA+'_EnumToString)([%s], Ord(Param0), %d, %d); end;',
    [Result, FRange.Lo, FRange.Hi]
  );
end;

function TLapeType_Enum.VarToString(AVar: Pointer): lpString;
var
  i: Int64;
begin
  try
    Result := '';
    i := VarToInt(AVar);

    if (FBaseType in LapeBoolTypes) then
      if (i = 0) then
        Result := 'False'
      else if (Abs(i) > 1) then
        Result := 'True('+IntToStr(i)+')'
      else
        Result := 'True'
    else
    begin
      if (i > -1) and (i < FMemberMap.Count) then
        Result := FMemberMap[i];

      if (Result = '') then
        if (Name <> '') then
          Result := Name + '(' + IntToStr(i) + ')'
        else
          Result := 'InvalidEnum';
    end;
  except
    Result := 'EnumException';
  end;
end;

function TLapeType_Enum.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Enum;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FMemberMap, Name, @_DocPos);
  Result.FBaseType := FBaseType;
end;

function TLapeType_Enum.NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
  if FSmall then
    PLapeSmallEnum(Result.Ptr)^ := ELapeSmallEnum(Value)
  else
    PLapeLargeEnum(Result.Ptr)^ := ELapeLargeEnum(Value);
end;

function TLapeType_Enum.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Str <> '') and (Str[1] in ['-', '0'..'9']) then
    Result := NewGlobalVar(StrToInt(Str), AName, ADocPos)
  else
    Result := NewGlobalVar(FMemberMap.IndexOf(Str), AName, ADocPos);
end;

function TLapeType_Enum.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  Assert(FCompiler <> nil);

  if (Right <> nil) and (Right.BaseIntType <> ltUnknown) and
     (((BaseType in LapeBoolTypes) and (op in BinaryOperators + EnumOperators) and (Right.BaseType in LapeBoolTypes)) or
     ((op in EnumOperators) and ((not (Right.BaseType in LapeEnumTypes)) or Equals(Right))))
  then
  begin
    Result := FCompiler.getBaseType(BaseIntType).EvalRes(Op, FCompiler.getBaseType(Right.BaseIntType));
    if (not (op in CompareOperators)) then
      Result := Self;
  end
  else
    Result := inherited;
end;

function TLapeType_Enum.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  tmpType: TLapeType;
  tmpRes: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType = Self));

  if (Right <> nil) and Right.HasType() and (Right.VarType.BaseIntType <> ltUnknown) and
     (((BaseType in LapeBoolTypes) and (op in BinaryOperators + EnumOperators) and (Right.VarType.BaseType in LapeBoolTypes)) or
     ((op in EnumOperators) and ((not (Right.VarType.BaseType in LapeEnumTypes)) or Equals(Right.VarType))))
  then
  try
    tmpType := Right.VarType;
    if (BaseIntType = ltUnknown) or (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation);

    Left.VarType := FCompiler.getBaseType(BaseIntType);
    Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    Result := Left.VarType.EvalConst(Op, Left, Right);
    if (not (op in CompareOperators)) then
      if (Result.VarType.BaseIntType = BaseIntType) then
        Result.VarType := Self
      else
      try
        tmpRes := Result;
        Result := NewGlobalVarP();
        Result := EvalConst(op_Assign, Result, tmpRes);
      finally
        FreeAndNil(tmpRes);
      end;
  finally
    Left.VarType := Self;
    Right.VarType := tmpType;
  end
  else
    Result := inherited;
end;

function TLapeType_Enum.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpType: TLapeType;
  tmpVar, tmpDest: TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType = Self);

  if Right.HasType() and (Right.VarType.BaseIntType <> ltUnknown) and
     (((BaseType in LapeBoolTypes) and (op in BinaryOperators + EnumOperators) and (Right.VarType.BaseType in LapeBoolTypes)) or
     ((op in EnumOperators) and ((not (Right.VarType.BaseType in LapeEnumTypes)) or Equals(Right.VarType))))
  then
  try
    tmpVar := NullResVar;
    tmpDest := NullResVar;
    tmpType := Right.VarType;
    if (BaseIntType = ltUnknown) or (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
      LapeException(lpeInvalidEvaluation);

    Left.VarType := FCompiler.getBaseType(BaseIntType);
    Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    if Dest.HasType() and Equals(Dest.VarType) then
      tmpDest := Dest;
    Result := Left.VarType.Eval(Op, Dest, Left, Right, Offset, Pos);
    if (not (op in CompareOperators)) then
      if (Result.VarType.BaseIntType = BaseIntType) then
        Result.VarType := Self
      else
      try
        Dest := tmpDest;
        tmpDest := Result;
        Result := NullResVar;
        Result.VarType := Self;
        FCompiler.getDestVar(Dest, Result, op_Unknown);
        Result := Eval(op_Assign, tmpVar, Result, tmpDest, Offset, Pos);
      finally
        tmpDest.Spill(1);
      end;
  finally
    Left.VarType := Self;
    Right.VarType := tmpType;
  end
  else
    Result := inherited;
end;

constructor TLapeType_Boolean.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, nil, AName, ADocPos);
  addMember('False');
  addMember('True');
  FBaseType := ltBoolean;
end;

constructor TLapeType_Bool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(NullRange, ACompiler, ACompiler.getBaseType(ltBoolean).CreateCopy(), AName, ADocPos);
end;

destructor TLapeType_Bool.Destroy;
begin
  FVarType.Free();
  inherited;
end;

function TLapeType_Bool.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Bool;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, Name, @_DocPos);
  Result.FBaseType := FBaseType;
  TLapeType_Bool(Result).FRange := FRange;
  TLapeType_Bool(Result).FVarType.FSize := FVarType.FSize;
end;

function TLapeType_Bool.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  Result := FVarType.EvalRes(Op, Right);
end;

function TLapeType_Bool.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
begin
  Assert(Left.VarType = Self);
  Left.VarType := FVarType;
  try
    Result := FVarType.EvalConst(Op, Left, Right);
  finally
    Left.VarType := Self;
  end;
end;

function TLapeType_Bool.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  Assert(Left.VarType = Self);
  Left.VarType := FVarType;
  try
    Result := FVarType.Eval(Op, Dest, Left, Right, Offset, Pos);
  finally
    Left.VarType := Self;
  end;
end;

constructor TLapeType_ByteBool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  BoolRange: TLapeRange =(Lo: Ord(Low(UInt8{ByteBool})); Hi: Ord(High(UInt8{ByteBool})));
begin
  inherited Create(ACompiler, AName, ADocPos);
  FRange := BoolRange;
  FBaseType := ltByteBool;
  FVarType.FSize := LapeTypeSize[ltByteBool];
end;

constructor TLapeType_WordBool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  BoolRange: TLapeRange =(Lo: Ord(Low(UInt16{WordBool})); Hi: Ord(High(UInt16{WordBool})));
begin
  inherited Create(ACompiler, AName, ADocPos);
  FRange := BoolRange;
  FBaseType := ltWordBool;
  FVarType.FSize := LapeTypeSize[ltWordBool];
end;

constructor TLapeType_LongBool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  BoolRange: TLapeRange =(Lo: Ord(Low(UInt32{LongBool})); Hi: Ord(High(UInt32{LongBool})));
begin
  inherited Create(ACompiler, AName, ADocPos);
  FRange := BoolRange;
  FBaseType := ltLongBool;
  FVarType.FSize := LapeTypeSize[ltLongBool];
end;

function TLapeType_Set.getAsString: lpString;
begin
  if (FAsString = '') then
    FAsString := 'set of ' + FRange.AsString;
  Result := inherited;
end;

constructor TLapeType_Set.Create(ARange: TLapeType_SubRange; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ARange <> nil);
  inherited Create(ltLargeSet, ACompiler, AName, ADocPos);

  FRange := ARange;
  FSmall := (FRange.Range.Hi <= Ord(High(ELapeSmallEnum)));
  if FSmall then
    FBaseType := ltSmallSet;

  if (FRange.Range.Lo < Ord(Low(ELapeSmallEnum))) or (FRange.Range.Hi > Ord(High(ELapeLargeEnum))) then
    LapeException(lpeOutOfTypeRange);
end;

function TLapeType_Set.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (ToStr.Methods = nil) or (FCompiler = nil) or (FRange = nil) then
    Exit;

  Index := ToStr.Methods.Items.IndexOf(ToStr.getMethod(getTypeArray([FRange]), FCompiler.getBaseType(ltString)));
  if (Index < 0) then
    Exit;

  Result := 'type TSetToString = function(const ASet; AToString: Pointer; Lo, Hi: Int32): string;' + LineEnding + 'begin ';
  if FSmall then
    Result := Result + 'Result := TSetToString('+AIA+'_SmallSetToString)'
  else
    Result := Result + 'Result := TSetToString('+AIA+'_LargeSetToString)';
  Result := Format(Result + '(Param0, '+AIA+'ToString[%d], %d, %d); end;', [Index, FRange.Range.Lo, FRange.Range.Hi]);
end;

function TLapeType_Set.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
begin
  Result := '[';
  for i := FRange.Range.Lo to FRange.Range.Hi do
    if (FSmall and (ELapeSmallEnum(i) in PLapeSmallSet(AVar)^)) or ((not FSmall) and (ELapeLargeEnum(i) in PLapeLargeSet(AVar)^)) then
    begin
      if (Result <> '[') then
        Result := Result + ', ';
      Result := Result + FRange.VarToString(@i);
    end;
  Result := Result + ']';
end;

function TLapeType_Set.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Set;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FCompiler, Name, @_DocPos);
end;

function TLapeType_Set.NewGlobalVar(Values: array of UInt8; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := NewGlobalVarP(nil, AName);
  for i := 0 to High(Values) do
    if FSmall then
      PLapeSmallSet(Result.Ptr)^ := PLapeSmallSet(Result.Ptr)^ + [ELapeSmallEnum(Values[i])]
    else
      PLapeLargeSet(Result.Ptr)^ := PLapeLargeSet(Result.Ptr)^ + [ELapeLargeEnum(Values[i])]
end;

function TLapeType_Set.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  Result := nil;
  if (Right = nil) then
    Result := inherited
  else
    case getEvalRes(Op, FBaseType, Right.FBaseType) of
      ltSmallEnum, ltLargeEnum:
        if (not (Right.FBaseType in LapeEnumTypes)) or Right.Equals(FRange) then
          Result := FRange.VarType;
      ltSmallSet, ltLargeSet:
        if (not (Right.FBaseType in LapeEnumTypes + LapeSetTypes)) or Right.Equals(FRange) or Equals(Right) then
          Result := Self;
      else
        Result := inherited;
    end;
end;

function TLapeType_Pointer.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltPointer) then
    if HasType() then
      FAsString := '^'+FPType.AsString;
  Result := inherited;
end;

constructor TLapeType_Pointer.Create(ACompiler: TLapeCompilerBase; PointerType: TLapeType = nil; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltPointer, ACompiler, AName, ADocPos);
  FPType := PointerType;
end;

function TLapeType_Pointer.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
begin
  if (Other <> nil) and (Other.BaseType = BaseType) and ContextOnly then
    Result := (not HasType()) or (not TLapeType_Pointer(Other).HasType()) or inherited
  else
    Result := inherited;
end;

function TLapeType_Pointer.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
begin
  Result := 'begin Result := ToString(Pointer(Param0));';
  if HasType() and (ToStr <> nil) and (ToStr.getMethod(getTypeArray([PType])) <> nil) then
    Result := Result + 'if (Param0 <> nil) then ' +
      'try Result := Result + '#39' ('#39' + ToString(Param0^) + '#39')'#39'; except end;';
  Result := Result + 'end;';
end;

function TLapeType_Pointer.VarToString(AVar: Pointer): lpString;
begin
  if ((AVar = nil) or (PPointer(AVar)^ = nil)) then
    Result := 'nil'
  else
  begin
    Result := '0x'+IntToHex(PtrUInt(PPointer(AVar)^), 1);
    try
      if HasType() then
        Result := Result + '(' + FPType.VarToString(PPointer(AVar)^) + ')';
    except end;
  end;
end;

function TLapeType_Pointer.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Pointer;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FPType, Name, @_DocPos);
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
  Result := NewGlobalVar(Pointer(StrToInt64(Str)), AName, ADocPos);
end;

function TLapeType_Pointer.HasType: Boolean;
begin
  Result := (FPType <> nil);
end;

function TLapeType_Pointer.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  if (op = op_Deref) then
    Result := FPType
  else if (op = op_Index) then
    Result := Self
  else
  begin
    Result := inherited;
    if (Result <> nil) and (Result.BaseType = ltPointer) and (not TLapeType_Pointer(Result).HasType()) and (FCompiler <> nil) then
      Result := FCompiler.getPointerType(FPType);
  end;
end;

function TLapeType_Pointer.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  tmpType: TLapeType;
  TypeSize, IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Right <> nil) then
  begin
    tmpType := Right.VarType;
    if (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
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
        IndexVar := Right.VarType.EvalConst(op_Multiply, Right, TypeSize)
      else
        IndexVar := Right;

      Result := //Result := (Pointer + Index * PSize)
        EvalConst(
          op_Plus,
          Left,
          IndexVar
        );
      Result.isConstant := Left.isConstant;
    finally
      if (IndexVar <> nil) and (IndexVar <> Right) then
        IndexVar.Free();
      Right.VarType := tmpType;
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_Pointer.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpType: TLapeType;
  tmpVar, IndexVar: TResVar;
  wasConstant: Boolean;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  tmpVar := NullResVar;

  if (op = op_Index) then
  begin
    wasConstant := Left.isConstant;
    tmpType := Right.VarType;

    if (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else
      Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    IndexVar := Right;
    if HasType() and (FPType.Size <> 1) then
      IndexVar :=
        Right.VarType.Eval(
          op_Multiply,
          tmpVar,
          Right,
          _ResVar.New(FCompiler.getConstant(FPType.Size)),
          Offset,
          Pos
        );

    Result := //Result := (Pointer + Index * PSize)
      Eval(
        op_Plus,
        Dest,
        Left,
        IndexVar,
        Offset,
        Pos
      );

    Result.setConstant(wasConstant, False);
    Right.VarType := tmpType;
  end
  else
    Result := inherited;
end;

function TLapeType_DynArray.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltDynArray) then
    if HasType() then
      FAsString := 'array of ' + FPType.AsString
    else
      FAsString := 'array';
  Result := inherited;
end;

constructor TLapeType_DynArray.Create(ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ArrayType, AName, ADocPos);
  FBaseType := ltDynArray;
end;

function TLapeType_DynArray.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_DynArray;
begin
  Result := TLapeClassType(Self.ClassType).Create(FPType, FCompiler, Name, @_DocPos);
  Result.FBaseType := FBaseType;
end;

function TLapeType_DynArray.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (ToStr.Methods = nil) or (FCompiler = nil) or (not HasType()) then
    Exit;

  Index := ToStr.Methods.Items.IndexOf(ToStr.getMethod(getTypeArray([PType]), FCompiler.getBaseType(ltString)));
  if (Index < 0) then
    Exit;

  Result :=
    '  function _ElementToString(const p: Pointer): string;'                                + LineEnding +
    '  begin'                                                                               + LineEnding +
    '    Result := ToString['+IntToStr(Index)+'](p^);'                                      + LineEnding +
    '  end;'                                                                                + LineEnding +
    'type'                                                                                  + LineEnding +
    '  TArrayToString = function(const Arr; AToString: Pointer; Len, Size: Int32): string;' + LineEnding +
    'var'                                                                                   + LineEnding +
    '  Len: Int32;'                                                                         + LineEnding +
    'begin'                                                                                 + LineEnding +
    '  Len := Length(Param0);'                                                              + LineEnding +
    '  if (Len <= 0) then'                                                                  + LineEnding +
    '    Result := '#39'[]'#39''                                                            + LineEnding +
    '  else'                                                                                + LineEnding +
    '    Result := TArrayToString('+AIA+'_ArrayToString)('                                  + LineEnding +
    '      Param0['+IntToStr(VarLo().AsInteger)+'],'                                        + LineEnding +
    '      '+AIA+'_ElementToString, Len, SizeOf(Param0[0]));'                               + LineEnding +
    'end;';
end;

function TLapeType_DynArray.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
  p: Pointer;
begin
  Result := '[';
  if (AVar <> nil) and HasType() then
  begin
    p := PPointer(AVar)^;
    for i := 0 to Length(PCodeArray(AVar)^) - 1 do
    begin
      if (i > 0) then
        Result := Result + ', ';
      Result := Result + FPType.VarToString(Pointer(PtrUInt(p) + UInt32(FPType.Size * i)));
    end;
  end;
  Result := Result + ']';
end;

function TLapeType_DynArray.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(0);
end;

function TLapeType_DynArray.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (AVar = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(High(PCodeArray(AVar)^));
end;

procedure TLapeType_DynArray.VarSetLength(var AVar: Pointer; ALen: Integer);
var
  i, OldLen, NewSize: SizeInt;
  NewP: Pointer;
  DoFree: Boolean;
  tmpLeft, tmpRight: TLapeGlobalVar;
begin
  if (not (BaseType in LapeArrayTypes - [ltStaticArray, ltShortString])) then
    LapeException(lpeInvalidEvaluation);
  if (not HasType()) then
    Exit;

  if (BaseType in LapeStringTypes) then
  begin
    case BaseType of
      ltAnsiString:    SetLength(AnsiString(AVar), ALen);
      ltWideString:    SetLength(WideString(AVar), ALen);
      ltUnicodeString: SetLength(UnicodeString(AVar), ALen);
      else LapeException(lpeImpossible);
    end;
    Exit;
  end;

  NewSize := ALen * PType.Size;
  DoFree := NewSize <= 0;
  Inc(NewSize, SizeOf(PtrInt) + SizeOf(SizeInt));

  if (AVar = nil) then
  begin
    if DoFree then
      Exit;
    AVar := AllocMem(NewSize);

    PtrInt(AVar^) := 1;
    Inc(PtrUInt(AVar), SizeOf(PtrInt));
    SizeInt(AVar^) := ALen {$IFDEF FPC}-1{$ENDIF};
    Inc(PtrUInt(AVar), SizeOf(SizeInt));
    Exit;
  end;

  Dec(PtrUInt(AVar), SizeOf(SizeInt));
  OldLen := SizeInt(AVar^) {$IFDEF FPC}+1{$ENDIF};
  Dec(PtrUInt(AVar), SizeOf(PtrInt));

  if (PtrInt(AVar^) <= 1) then
  begin
    if (ALen = OldLen) then
    begin
      Inc(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
      Exit;
    end;

    if (ALen < OldLen) and PType.NeedFinalization then
    begin
      Inc(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
      for i := ALen to OldLen - 1 do
      begin
        tmpLeft := PType.NewGlobalVarP(Pointer(PtrInt(AVar) + (i * PType.Size)));
        try
          PType.Finalize(tmpLeft);
        finally
          tmpLeft.Free();
        end;
      end;
      Dec(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
    end;

    if DoFree then
    begin
      FreeMem(AVar);
      AVar := nil;
      Exit;
    end;
    ReallocMem(AVar, NewSize);
    PtrInt(AVar^) := 1;
    Inc(PtrUInt(AVar), SizeOf(PtrInt));
    SizeInt(AVar^) := ALen {$IFDEF FPC}-1{$ENDIF};
    Inc(PtrUInt(AVar), SizeOf(SizeInt));

    if (ALen > OldLen) then
      FillChar(Pointer(PtrInt(AVar) + (OldLen * PType.Size))^, (ALen - OldLen) * PType.Size, 0);
  end
  else
  begin
    Dec(PtrInt(AVar^));
    NewP := nil;
    VarSetLength(NewP, ALen);

    i := OldLen;
    if (ALen < OldLen) then
      i := ALen;
    Inc(PtrUInt(AVar), SizeOf(PtrInt) + SizeOf(SizeInt));
    for i := i - 1 downto 0 do
    begin
      tmpLeft := PType.NewGlobalVarP(Pointer(PtrInt(NewP) + (i * PType.Size)));
      tmpRight := PType.NewGlobalVarP(Pointer(PtrInt(AVar) + (i * PType.Size)));
      try
        PType.EvalConst(op_Assign, tmpLeft, tmpRight);
      finally
        tmpLeft.Free();
        tmpRight.Free();
      end;
    end;

    AVar := NewP;
  end;
end;

procedure TLapeType_DynArray.VarSetLength(AVar, ALen: TResVar; var Offset: Integer; Pos: PDocPos = nil);
begin
  Assert(FCompiler <> nil);
  FCompiler.EmitCode('SetLength(AVar, ALen);', ['AVar', 'ALen'], [TLapeVar(nil), TLapeVar(nil)], [AVar, ALen], Offset, Pos);
end;

function TLapeType_DynArray.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  if (op = op_Index) then
    Result := FPType
  else if (op = op_Assign) and (BaseType = ltDynArray) and HasType() and (Right <> nil) and (Right is ClassType) and FPType.Equals(TLapeType_DynArray(Right).FPType) then
    Result := Self
  else if (op = op_Plus) and (BaseType = ltDynArray) and HasType() and FPType.CompatibleWith(Right) then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_DynArray.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  tmpType: ELapeBaseType;
  IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) then
  begin
    tmpType := FBaseType;
    FBaseType := ltPointer;
    IndexVar := nil;
    try
      IndexVar := inherited EvalConst(Op, Left, Right);
      Result := //Result := Pointer[Index]^
        EvalConst(
          op_Deref,
          IndexVar,
          nil
        );
    finally
      if (IndexVar <> nil) then
        IndexVar.Free();
      FBaseType := tmpType;
    end;
  end
  else if (op = op_Assign) and (BaseType = ltDynArray) and (Left <> nil) and (Right <> nil) and CompatibleWith(Right.VarType) then
    if (Right.VarType.BaseType = ltDynArray) then
    begin
      if (PPointer(Left.Ptr)^ = PPointer(Right.Ptr)^) then
        Exit(Left);

      VarSetLength(PPointer(Left.Ptr)^, 0);
      Result := inherited;
      if (Result <> nil) and (Result.Ptr <> nil) and (PPointer(Result.Ptr)^ <> nil) then
        Inc(PtrInt(Pointer(PtrInt(Result.Ptr^) - SizeOf(SizeInt) - SizeOf(PtrInt))^));
    end
    else if (Right.VarType is TLapeType_StaticArray) then
    begin
      VarSetLength(PPointer(Left.Ptr)^, TLapeType_StaticArray(Right.VarType).Range.Hi - TLapeType_StaticArray(Right.VarType).Range.Lo + 1);
      Result := Left;

      IndexVar := EvalConst(op_Index, Left, FCompiler.getConstant(0));
      try
        IndexVar.VarType := Right.VarType;
        IndexVar.VarType.EvalConst(op_Assign, IndexVar, Right);
      finally
        IndexVar.Free();
      end;
    end
    else
      LapeException(lpeImpossible)
  else if (op = op_Plus) and (BaseType = ltDynArray) and (Left <> nil) and (Right <> nil) and HasType() and FPType.CompatibleWith(Right.VarType) then
  begin
    Result := EvalConst(op_Assign, NewGlobalVarP(), Left);
    IndexVar := FCompiler.getConstant(Length(PCodeArray(Result.Ptr)^));
    VarSetLength(PPointer(Result.Ptr)^, IndexVar.AsInteger + 1);

    IndexVar := EvalConst(op_Index, Result, IndexVar);
    try
      PType.EvalConst(op_Assign, IndexVar, Right);
    finally
      IndexVar.Free();
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_DynArray.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpType: ELapeBaseType;
  tmpVar: TLapeStackTempVar;
  IndexVar, tmpResVar: TResVar;
  wasConstant: Boolean;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);

  IndexVar := NullResVar;
  tmpResVar := NullResVar;
  tmpVar := nil;

  if (op = op_Index) then
  try
    tmpType := FBaseType;
    FBaseType := ltPointer;
    if (not Left.VarPos.isPointer) then
    begin
      Result := inherited Eval(Op, Dest, Left, Right, Offset, Pos);
      Result.VarPos.isPointer := True;
      Result.VarType := FPType;
    end
    else
    begin
      IndexVar := inherited Eval(Op, IndexVar, Left, Right, Offset, Pos);
      Result := //Result := Pointer[Index]^
        Eval(
          op_Deref,
          Dest,
          IndexVar,
          NullResVar,
          Offset,
          Pos
        );
      IndexVar.Spill(1);
    end;
  finally
    FBaseType := tmpType;
  end
  else if (op = op_Assign) and (BaseType = ltDynArray) and CompatibleWith(Right.VarType) then
  begin
    Result := Left;
    if (Left.VarPos.MemPos = mpStack) then
    begin
      tmpVar := FCompiler.getTempVar(Self, BigLock);
      tmpVar.isConstant := False;
      Left := _ResVar.New(tmpVar);
    end;

    if (Right.VarType.BaseType = ltDynArray) then
      FCompiler.EmitCode(
        'if (Pointer(Left) <> Pointer(Right)) then begin'                   +
        '  SetLength(Left, 0);'                                             +
        '  Pointer(Left) := Pointer(Right);'                                +
        '  if (Pointer(Left) <> nil) then'                                  +
        '    Inc(PtrInt(Pointer(Left)[-'                                    +
               IntToStr(SizeOf(SizeInt)+SizeOf(PtrInt))                     +
             ']^));'                                                        +
        'end;'
      , ['Left', 'Right'], [], [Left, Right], Offset, Pos)
    else if (Right.VarType is TLapeType_StaticArray) then
      FCompiler.EmitCode(
        'SetLength(Left, '+IntToStr(
          TLapeType_StaticArray(Right.VarType).Range.Hi -
          TLapeType_StaticArray(Right.VarType).Range.Lo + 1)                +
        ');'                                                                +
        'PType(@Left[0])^ := Right;',
        ['PType', 'Left', 'Right'], [FCompiler.getTypeVar(FCompiler.getPointerType(Right.VarType))],
        [Left, Right], Offset, Pos)
    else
      LapeException(lpeImpossible);

    if (tmpVar <> nil) then
    begin
      FCompiler.Emitter._PopVarToStack(Size, tmpVar.Offset, Offset, Pos);
      Left.Spill(BigLock);
    end;
  end
  else if (op = op_Plus) and (BaseType = ltDynArray) and HasType() and FPType.CompatibleWith(Right.VarType) then
  begin
    Result := NullResVar;
    Result.VarType := Self;
    FCompiler.getDestVar(Dest, Result, op);

    if (Result.VarPos.MemPos = mpStack) then
    begin
      tmpVar := FCompiler.getTempVar(Self);
      Result := _ResVar.New(tmpVar);
    end;

    wasConstant := Result.isConstant;
    if wasConstant then
      Result.isConstant := False;

    Result := Eval(op_Assign, tmpResVar, Result, Left, Offset, Pos);
    FCompiler.EmitCode(
      'SetLength(Result, Length(Result) + 1);' +
      'Result[High(Result)] := Right;'
    , ['Result', 'Right'], [], [Result, Right], Offset, Pos);

    if wasConstant then
      Result.isConstant := True;
  end
  else
    Result := inherited;
end;

function TLapeType_StaticArray.getSize: Integer;
begin
  if (not HasType()) then
    Exit(-1);
  FSize := (FRange.Hi - FRange.Lo + 1) * FPType.Size;
  Result := FSize;
end;

function TLapeType_StaticArray.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltStaticArray) then
  begin
    FAsString := 'array [' + IntToStr(FRange.Lo) + '..' + IntToStr(FRange.Hi) + ']';
    if HasType() then
      FAsString := FAsString + ' of ' + FPType.AsString;
  end;
  Result := inherited;
end;

constructor TLapeType_StaticArray.Create(ARange: TLapeRange; ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ArrayType, ACompiler, AName, ADocPos);
  FBaseType := ltStaticArray;
  if (ArrayType <> nil) and ArrayType.NeedInitialization then
    FInit := __Yes
  else
    FInit := __No;

  FRange := ARange;
end;

function TLapeType_StaticArray.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
begin
  Result := '[';
  if (AVar <> nil) and HasType() then
    for i := 0 to FRange.Hi - FRange.Lo do
    begin
      if (i > 0) then
        Result := Result + ', ';
      Result := Result + FPType.VarToString(Pointer(PtrInt(AVar) + (FPType.Size * i)));
    end;
  Result := Result + ']';
end;

function TLapeType_StaticArray.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(FRange.Lo);
end;

function TLapeType_StaticArray.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else 
    Result := FCompiler.getConstant(FRange.Hi);
end;

function TLapeType_StaticArray.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_StaticArray;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FPType, FCompiler, Name, @_DocPos);
  Result.FBaseTYpe := FBaseType;
end;

function TLapeType_StaticArray.NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
end;

function TLapeType_StaticArray.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  if (op = op_Assign) and (BaseType = ltStaticArray) and HasType() and (Right <> nil) and (Right is ClassType) and
     ((TLapeType_StaticArray(Right).Range.Hi - TLapeType_StaticArray(Right).Range.Lo) = (Range.Hi - Range.Lo)) and
     FPType.CompatibleWith(TLapeType_StaticArray(Right).FPType)
  then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_StaticArray.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  i: Integer;
  LeftVar, RightVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Left <> nil) and (Right <> nil) then
  begin
    Assert(HasType());

    i := Right.AsInteger;
    if (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else if (i < FRange.Lo) or (i > FRange.Hi) then
      LapeException(lpeOutOfTypeRange);

    Result := FPType.NewGlobalVarP(Pointer(PtrInt(Left.Ptr) + (FPType.Size * (i - FRange.Lo))));
    Result.isConstant := Left.isConstant;
  end
  else if (op = op_Assign) and (BaseType = ltStaticArray) and (Left <> nil) and (Right <> nil) and CompatibleWith(Right.VarType) then
  begin
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FRange.Hi - FRange.Lo do
    try
      LeftVar := FPType.NewGlobalVarP(Pointer(PtrInt(Left.Ptr) + (FPType.Size * i)));
      RightVar := FPType.NewGlobalVarP(Pointer(PtrInt(Right.Ptr) + (FPType.Size * i)));
      FPType.EvalConst(op_Assign, LeftVar, RightVar);
    finally
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;
    Result := Left;
  end
  else
    Result := inherited;
end;

function TLapeType_StaticArray.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpVar, LeftVar: TResVar;
  wasConstant: Boolean;
  tmpType: TLapeType;
  CounterVar, IndexLow, IndexHigh: TLapeVar;
  LoopOffset: Integer;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  tmpVar := NullResVar;
  LeftVar := NullResVar;

  if (op = op_Index) then
  try
    wasConstant := Left.isConstant;
    if wasConstant then
      Left.setConstant(False);

    if (not Left.VarPos.isPointer) or (Left.VarPos.Offset > 0) then
      LeftVar := Eval(op_Addr, tmpVar, Left, NullResVar, Offset, Pos)
    else
    begin
      LeftVar := Left.IncLock();
      LeftVar.VarPos.isPointer := False;
      LeftVar.VarType := FCompiler.getPointerType(PType);
    end;

    if (FRange.Lo = 0) then
      Result := inherited Eval(Op, Dest, LeftVar, Right, Offset, Pos)
    else
    try
      tmpType := Right.VarType;
      if (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
        if Right.HasType() then
          LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
        else
          LapeException(lpeInvalidEvaluation)
      else
        Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

      Result := //Result := @Pointer[Index - Lo]^
        inherited Eval(
          Op,
          Dest,
          LeftVar,
          Right.VarType.Eval(op_Minus, tmpVar, Right, _ResVar.New(
            FCompiler.getConstant(FRange.Lo, DetermineIntType(FRange.Lo, Right.VarType.BaseType, False), False, True)
          ), Offset, Pos),
          Offset,
          Pos
        );
    finally
      Right.VarType := tmpType;
    end;
  finally
    if (not Left.VarPos.isPointer) or (Left.VarPos.Offset > 0) then
      LeftVar.Spill(1);

    Left.setConstant(wasConstant);
    Result.setConstant(wasConstant);
  end
  else if (op = op_Assign) and (BaseType = ltStaticArray) and CompatibleWith(Right.VarType) then
    if (not NeedInitialization) and Equals(Right.VarType) and (Size > 0) then
    try
      tmpType := Right.VarType;
      Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
	    begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op_Assign, Dest, Left, Right, Offset, Pos);
	    end
      else
	    begin
        Left.VarType := Self;
        if (Left.VarPos.MemPos = mpStack) and (not Left.VarPos.ForceVariable) then
        begin
          FCompiler.Emitter._GrowStack(Size, Offset, Pos);
          Left.VarPos.ForceVariable := True;

          IndexLow := FCompiler.getTempVar(ltPointer, 1);
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), _ResVar.New(IndexLow), Left, NullResVar, Offset, Pos);

          wasConstant := True;
        end
        else
          wasConstant := False;

        IndexHigh := FCompiler.getConstant(Size);
        tmpVar := StackResVar;
        tmpVar.VarType := Compiler.getBaseType(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar, Offset, Pos);
        if wasConstant then
          FCompiler.Emitter._Eval(getEvalProc(op_Assign, ltPointer, ltPointer), tmpVar, _ResVar.New(IndexLow), NullResVar, Offset, Pos)
        else
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar, Offset, Pos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, _ResVar.New(IndexHigh), NullResVar, Offset, Pos);
        FCompiler.Emitter._InvokeImportedProc(_ResVar.New(FCompiler.getGlobalVar('!move')), SizeOf(Pointer)*3, Offset, Pos);
        Result := Left;

        if wasConstant then
        begin
          TLapeStackTempVar(IndexLow).Declock(1);
          Left.VarPos.ForceVariable := False;
        end;
      end;

    finally
      Left.VarType := Self;
      Right.VarType := tmpType;
    end
    else
    begin
      Assert(HasType());
      if (Left.VarPos.MemPos = mpStack) and (not Left.VarPos.ForceVariable) then
      begin
        FCompiler.Emitter._GrowStack(Size, Offset, Pos);
        Left.VarPos.ForceVariable := True;
        wasConstant := True;
      end
      else
        wasConstant := False;

      CounterVar := FCompiler.getTempVar(DetermineIntType(FRange.Lo, FRange.Hi, ltNativeInt), BigLock);
      IndexLow := FCompiler.getConstant(FRange.Lo, CounterVar.VarType.BaseType, False, True);
      IndexHigh := FCompiler.getConstant(FRange.Hi, CounterVar.VarType.BaseType, False, True);
      LeftVar := CounterVar.VarType.Eval(op_Assign, LeftVar, _ResVar.New(CounterVar), _ResVar.New(IndexLow), Offset, Pos);
      LoopOffset := Offset;
      FPType.Eval(op_Assign, tmpVar, Eval(op_Index, tmpVar, Left, LeftVar, Offset, Pos), Eval(op_Index, tmpVar, Right, LeftVar, Offset, Pos), Offset, Pos);
      CounterVar.VarType.Eval(op_Assign, tmpVar, LeftVar, CounterVar.VarType.Eval(op_Plus, tmpVar, LeftVar, _ResVar.New(FCompiler.getConstant(1, CounterVar.VarType.BaseType, False, True)), Offset, Pos), Offset, Pos);
      FCompiler.Emitter._JmpRIf(LoopOffset - Offset, CounterVar.VarType.Eval(op_cmp_LessThanOrEqual, tmpVar, LeftVar, _ResVar.New(IndexHigh), Offset, Pos), Offset, Pos);
      LeftVar.Spill(BigLock);

      Result := Left;
      if wasConstant then
        Left.VarPos.ForceVariable := False;
    end
  else
    Result := inherited;
end;

procedure TLapeType_StaticArray.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);
var
  i, LoopOffset: Integer;
  tmpVar, IndexVar: TResVar;
  Counter, LowIndex, HighIndex: TLapeVar;
begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) {or (not NeedFinalization)} then
    Exit;

  IndexVar := NullResVar;
  tmpVar := NullResVar;
  if UseCompiler and (FCompiler <> nil) then
  begin
    Counter := FCompiler.getTempVar(DetermineIntType(FRange.Lo, FRange.Hi, ltNativeInt), BigLock);
    LowIndex := FCompiler.addManagedVar(Counter.VarType.NewGlobalVarStr(IntToStr(FRange.Lo)));
    HighIndex := FCompiler.addManagedVar(Counter.VarType.NewGlobalVarStr(IntToStr(FRange.Hi)));
    IndexVar := Counter.VarType.Eval(op_Assign, IndexVar, _ResVar.New(Counter), _ResVar.New(LowIndex), Offset, Pos);
    LoopOffset := Offset;
    FCompiler.FinalizeVar(Eval(op_Index, tmpVar, AVar, IndexVar, Offset, Pos), Offset, Pos);
    Counter.VarType.Eval(op_Assign, tmpVar, IndexVar, Counter.VarType.Eval(op_Plus, tmpVar, IndexVar, _ResVar.New(FCompiler.addManagedVar(Counter.VarType.NewGlobalVarStr('1'))), Offset, Pos), Offset, Pos);
    FCompiler.Emitter._JmpRIf(LoopOffset - Offset, Counter.VarType.Eval(op_cmp_LessThanOrEqual, tmpVar, IndexVar, _ResVar.New(HighIndex), Offset, Pos), Offset, Pos);
    IndexVar.Spill(BigLock);
  end
  else if (AVar.VarPos.MemPos <> mpMem) then
    LapeException(lpeImpossible)
  else if NeedFinalization then
    for i := 0 to FRange.Hi - FRange.Lo do
    try
      IndexVar := AVar;
      IndexVar.VarType := FPType;
      IndexVar.VarPos.GlobalVar := IndexVar.VarType.NewGlobalVarP(Pointer(PtrInt(IndexVar.VarPos.GlobalVar.Ptr) + (FPType.Size * i)));
      FPType.Finalize(IndexVar, Offset, UseCompiler, Pos);
    finally
      FreeAndNil(IndexVar.VarPos.GlobalVar);
    end;
end;

function TLapeType_String.VarToString(AVar: Pointer): lpString;
begin
  if (FBaseType = ltAnsiString) then
 	  Result := '"'+PAnsiString(AVar)^+'"'
 	else if (FBaseType = ltWideString) then
 	  Result := '" '+PWideString(AVar)^+'"'
 	else if (FBaseType = ltUnicodeString) then
 	  Result := '"'+PUnicodeString(AVar)^+'"'
 	else
 	  Result := '"'+PlpString(AVar)^+'"';
end;

function TLapeType_String.NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PAnsiString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName, ADocPos);
end;

{$IFNDEF Lape_NoWideString}
function TLapeType_String.NewGlobalVarStr(Str: WideString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PWideString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: WideString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName, ADocPos);
end;
{$ENDIF}

function TLapeType_String.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PUnicodeString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName, ADocPos);
end;

function TLapeType_String.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  tmpType: TLapeType;
  IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Right <> nil) then
  begin
    tmpType := Right.VarType;
    if (not Right.HasType()) or (Right.VarType.BaseIntType = ltUnknown) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else if (Right.AsInteger < 1) then
      LapeException(lpeOutOfTypeRange)
    else
      Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    IndexVar := nil;
    try
      IndexVar := Right.VarType.EvalConst(op_Minus, Right, FCompiler.getConstant(1, ltUInt8, False, True));
      Result := //Result := Pointer[Index - 1]^
        inherited EvalConst(
          Op,
          Left,
          IndexVar
        );
    finally
      if (IndexVar <> nil) then
        IndexVar.Free();
      Right.VarType := tmpType;
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_String.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);

  Result := inherited;
  if (op = op_Index) and HasType() then
  begin
    if (not Result.VarPos.isPointer) then
      LapeException(lpeImpossible);
    Result.VarPos.Offset := -FPType.Size;
  end;
end;

constructor TLapeType_AnsiString.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltAnsiChar), ACompiler, AName, ADocPos);
  FBaseType := ltAnsiString;
end;

constructor TLapeType_WideString.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideChar), ACompiler, AName, ADocPos);
  FBaseType := ltWideString;
end;

constructor TLapeType_UnicodeString.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideChar), ACompiler, AName, ADocPos);
  FBaseType := ltUnicodeString;
end;

function TLapeType_ShortString.getAsString: lpString;
begin
  if (FAsString = '') then
  begin
    FAsString := inherited;
    FAsString := FAsString + '[' + IntToStr(FRange.Hi) + ']';
  end;
  Result := inherited;
end;

constructor TLapeType_ShortString.Create(ACompiler: TLapeCompilerBase; ASize: UInt8 = High(UInt8); AName: lpString = ''; ADocPos: PDocPos = nil);
var
  StrRange: TLapeRange;
begin
  StrRange.Lo := 0;
  StrRange.Hi := ASize;
  Assert(ACompiler <> nil);
  inherited Create(StrRange, ACompiler.getBaseType(ltAnsiChar), ACompiler, AName, ADocPos);
  FBaseType := ltShortString;
end;

function TLapeType_ShortString.VarToString(AVar: Pointer): lpString;
begin
  Result := '"'+PShortString(AVar)^+'"';
end;

function TLapeType_ShortString.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  if (Length(Str) >= FRange.Hi) then
    Delete(Str, FRange.Hi, Length(Str) - FRange.Hi + 1);
  PShortString(Result.Ptr)^ := Str;
end;

function TLapeType_ShortString.NewGlobalVar(Str: ShortString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PShortString(Result.Ptr)^ := Str;
end;

function TLapeType_ShortString.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  EvalProc: TLapeEvalProc;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Assign) and (Right <> nil) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (Right.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Right.VarType).Range.Hi) then
    begin
      EvalProc := getEvalProc(op_Assign, ltShortString, ltUInt8);
      Assert(ValidEvalFunction(EvalProc));
      EvalProc(Left.Ptr, Right.Ptr, @FRange.Hi);
    end
    else
    begin
      if (FRange.Hi < High(UInt8)) then
      begin
        Result := FCompiler.getBaseType(ltShortString).NewGlobalVarP();
        Result := Result.VarType.EvalConst(Op, Result, Right);
      end
      else
        Result := inherited;

      Result := inherited;
      if (Result <> nil) and Result.HasType() and (Result.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Result.VarType).Range.Hi) then
        with Result do
        begin
          Result := EvalConst(op_Assign, Left, Result);
          Free();
        end;
    end
  else
    Result := inherited;
end;

function TLapeType_ShortString.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  EvalProc: TLapeEvalProc;
  tmpString: TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  Result := NullResVar;
  tmpString := NullResVar;

  if (op = op_Assign) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (Right.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Right.VarType).Range.Hi) then
    begin
      EvalProc := getEvalProc(op_Assign, ltShortString, ltUInt8);
      Assert(ValidEvalFunction(EvalProc));
      FCompiler.Emitter._Eval(EvalProc, Left, Right, _ResVar.New(FCompiler.getConstant(FRange.Hi, ltUInt8, False, True)), Offset, Pos);
      Result := Left;
    end
    else
    begin
      if (FRange.Hi < High(UInt8)) then
      begin
        tmpString := StackResVar;
        tmpString.VarType := FCompiler.getBaseType(ltShortString);
        FCompiler.getDestVar(Dest, tmpString, op_Unknown);
        Result := tmpString.VarType.Eval(Op, Dest, tmpString, Right, Offset, Pos)
      end
      else
        Result := inherited;

      if Result.HasType() and (Result.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Result.VarType).Range.Hi) then
      begin
        Result := Eval(op_Assign, Dest, Left, Result, Offset, Pos);
        tmpString.Spill(1);
      end;
    end
  else
    Result := inherited;
end;

function TLapeType_Record.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := 'record ';
    for i := 0 to FFieldMap.Count - 1 do
      FAsString := FAsString + '[' + IntToStr(FFieldMap.ItemsI[i].Offset) + ']' + FFieldMap.ItemsI[i].FieldType.AsString + '; ';
    FAsString := FAsString + 'end';
  end;
  Result := inherited;
end;

constructor TLapeType_Record.Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  InvalidRec: TRecordField = (Offset: Word(-1); FieldType: nil);
begin
  inherited Create(ltRecord, ACompiler, AName, ADocPos);

  FreeFieldMap := (AFieldMap = nil);
  if (AFieldMap = nil) then
    AFieldMap := TRecordFieldMap.Create(InvalidRec);
  FFieldMap := AFieldMap;
end;

destructor TLapeType_Record.Destroy;
begin
  if FreeFieldMap then
    FFieldMap.Free();
  inherited;
end;

procedure TLapeType_Record.addField(FieldType: TLapeType; AName: lpString; Alignment: Byte = 1);
var
  Field: TRecordField;
begin
  if (FSize < 0) or (FFieldMap.Count < 1) then
    FSize := 0;
  if (FInit = __Unknown) or (FFieldMap.Count < 1) then
    FInit := __No;
  FAsString := '';
  Field.Offset := FSize;
  Field.FieldType := FieldType;
  if FFieldMap.ExistsKey(AName) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName]);
  FSize := FSize + FieldType.Size + (FieldType.Size mod Alignment);
  if (FInit <> __Yes) and FieldType.NeedInitialization then
    FInit := __Yes;
  FFieldMap[AName] := Field;
end;

function TLapeType_Record.Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result := inherited;
  if Result and (not ContextOnly) and (Other <> Self) and (Other is TLapeType_Record) then
  try
    for i := 0 to FFieldMap.Count - 1 do
      if (LapeCase(FFieldMap.Key[i]) <> LapeCase(TLapeType_Record(Other).FieldMap.Key[i])) then
        Exit(False);
  except
    Result := False;
  end;
end;

function TLapeType_Record.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  i: Integer;
begin
  Result := 'begin Result := '#39'{'#39;
  for i := 0 to FFieldMap.Count - 1 do
  begin
    if (i > 0) then
      Result := Result + ' + ' + #39', '#39;
    Result := Result + ' + '#39 + FFieldMap.Key[i] + ' = '#39' + ToString(Param0.' + FFieldMap.Key[i] + ')';
  end;
  Result := Result + ' + '#39'}'#39'; end;';
end;

function TLapeType_Record.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to FFieldMap.Count - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    Result := Result + FFieldMap.Key[i] + ' = ' + FFieldMap.ItemsI[i].FieldType.VarToString(Pointer(PtrUInt(AVar) + FFieldMap.ItemsI[i].Offset));
  end;
  Result := Result + '}';
end;

function TLapeType_Record.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Record;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FFieldMap, Name, @_DocPos);
  Result.FInit := FInit;
  Result.FSize := FSize;
end;

function TLapeType_Record.NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
end;

function TLapeType_Record.HasChild(AName: lpString): Boolean;
begin
  Result := FFieldMap.ExistsKey(AName);
end;

function TLapeType_Record.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
var
  i: Integer;
begin
  if (op = op_Assign) and (Right <> nil) and (Right is TLapeType_Record) and
     (TLapeType_Record(Right).FieldMap.Count = FFieldMap.Count) then
  begin
    for i := 0 to FFieldMap.Count - 1 do
      if (not FFieldMap.ItemsI[i].FieldType.CompatibleWith(TLapeType_Record(Right).FieldMap.ItemsI[i].FieldType)) then
      begin
        Result := inherited;
        Exit;
      end;
    Result := Self
  end
  else
    Result := inherited;
end;

function TLapeType_Record.EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType;
begin
  if (Op = op_Dot) and (Right <> nil) and (Right.BaseType = ltString) then
    Result := FFieldMap[PlpString(Right.Ptr)^].FieldType
  else
    Result := inherited;
end;

function TLapeType_Record.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  i: Integer;
  FieldName: lpString;
  LeftVar, RightVar, LeftFieldName, RightFieldName: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType = Self));

  if (Op = op_Dot) and (Left <> nil) and (Right <> nil) and Right.HasType() and (Right.VarType.BaseType = ltString) then
  begin
    Assert(Right.Ptr <> nil);
    FieldName := PlpString(Right.Ptr)^;

    if (FieldName <> '') and FFieldMap.ExistsKey(FieldName) then
      Result := FFieldMap[FieldName].FieldType.NewGlobalVarP(Pointer(PtrUInt(Left.Ptr) + FFieldMap[FieldName].Offset))
    else
      LapeExceptionFmt(lpeUnknownDeclaration, [FieldName]);
    Result.isConstant := Left.isConstant;
  end
  else if (op = op_Assign) and (Right <> nil) and Right.HasType() and CompatibleWith(Right.VarType) then
  begin
    LeftFieldName := nil;
    RightFieldName := nil;
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FFieldMap.Count - 1 do
    try
      LeftFieldName := FCompiler.getBaseType(ltString).NewGlobalVarStr(FFieldMap.Key[i]);
      RightFieldName := FCompiler.getBaseType(ltString).NewGlobalVarStr(TLapeType_Record(Right.VarType).FieldMap.Key[i]);

      LeftVar := EvalConst(op_Dot, Left, LeftFieldName);
      RightVar := Right.VarType.EvalConst(op_Dot, Right, RightFieldName);
      LeftVar.VarType.EvalConst(op_Assign, LeftVar, RightVar);
    finally
      if (LeftFieldName <> nil) then
        FreeAndNil(LeftFieldName);
      if (RightFieldName <> nil) then
        FreeAndNil(RightFieldName);
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;
    Result := Left;
  end
  else
    Result := inherited;
end;

function TLapeType_Record.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  i, FieldOffset: Integer;
  FieldName: lpString;
  tmpVar, LeftVar, RightVar, LeftFieldName, RightFieldName: TResVar;
  tmpType: TLapeType;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType = Self);
  tmpVar := NullResVar;

  if (Op = op_Dot) and (Right.VarPos.MemPos = mpMem) and Right.HasType() and (Right.VarType.BaseType = ltString) then
  begin
    Assert(Right.VarPos.GlobalVar.Ptr <> nil);
    FieldName := PlpString(Right.VarPos.GlobalVar.Ptr)^;

    Dest.Spill();
    Result := Left.IncLock();
    if (FieldName = '') or (not FFieldMap.ExistsKey(FieldName)) then
      LapeExceptionFmt(lpeUnknownDeclaration, [FieldName]);

    Result.VarType := FFieldMap[FieldName].FieldType;
    case Left.VarPos.MemPos of
      mpMem:
        begin
          Result.VarPos.GlobalVar := TLapeGlobalVar(FCompiler.addManagedVar(Result.VarType.NewGlobalVarP(Pointer(PtrUInt(Left.VarPos.GlobalVar.Ptr) + FFieldMap[FieldName].Offset)), True));
          Result.VarPos.GlobalVar.isConstant := Left.VarPos.GlobalVar.isConstant;
        end;
      mpVar:
        begin
          Result.VarPos.Offset := Result.VarPos.Offset + FFieldMap[FieldName].Offset;
          Result.VarPos.StackVar.isConstant := Left.VarPos.StackVar.isConstant;
        end;
      mpStack:
        begin
          Result.VarPos.Offset := Result.VarPos.Offset - FFieldMap[FieldName].Offset;
          Result.VarPos.ForceVariable := Left.VarPos.ForceVariable;
        end;
      else LapeException(lpeImpossible);
    end
  end
  else if (op = op_Assign) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (not NeedInitialization) and Equals(Right.VarType) and (Size > 0) and ((Left.VarPos.MemPos <> mpStack) or (DetermineIntType(Size, False) <> ltUnknown)) then
    try
      tmpType := Right.VarType;
	    Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
	    begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op_Assign, Dest, Left, Right, Offset, Pos);
	    end
	    else
	    begin
        RightVar := _ResVar.New(FCompiler.getConstant(Size));
        tmpVar := StackResVar;
        tmpVar.VarType := Compiler.getBaseType(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, RightVar, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._InvokeImportedProc(_ResVar.New(FCompiler.getGlobalVar('!move')), SizeOf(Pointer) * 3, Offset, @Self._DocPos);
        Result := Left;
	    end;

    finally
      Left.VarType := Self;
      Right.VarType := tmpType;
    end
    else
    begin
      for i := 0 to FFieldMap.Count - 1 do
      try
        LeftFieldName := _ResVar.New(FCompiler.getConstant(FFieldMap.Key[i]));
        RightFieldName := _ResVar.New(FCompiler.getConstant(TLapeType_Record(Right.VarType).FieldMap.Key[i]));

        LeftVar := Eval(op_Dot, tmpVar, Left, LeftFieldName, Offset, Pos);
        RightVar := Right.VarType.Eval(op_Dot, tmpVar, Right, RightFieldName, Offset, Pos);
        LeftVar.VarType.Eval(op_Assign, Dest, LeftVar, RightVar, Offset, Pos);

        if (LeftVar.VarPos.MemPos = mpStack) then
        begin
          if (i + 1 = FFieldMap.Count) then
            FieldOffset := Size
          else
            FieldOffset := FFieldMap.ItemsI[i + 1].Offset;
          FieldOffset := FieldOffset - (FFieldMap.ItemsI[i].Offset + FFieldMap.ItemsI[i].FieldType.Size);
          if (FieldOffset > 0) then
            FCompiler.Emitter._GrowStack(FieldOffset, Offset, Pos);
        end;
      finally
        LeftVar.Spill(1);
        RightVar.Spill(1);
      end;
      Result := Left;
    end
  else
    Result := inherited;
end;

procedure TLapeType_Record.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);
var
  i: Integer;
  FieldVar: TResVar;
begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) {or (not NeedFinalization)} then
    Exit;

  for i := 0 to FFieldMap.Count - 1 do
  try
    FieldVar := AVar;
    FieldVar.VarType := FFieldMap.ItemsI[i].FieldType;
    case FieldVar.VarPos.MemPos of
      mpMem:
        if UseCompiler and (FCompiler <> nil) then
          FieldVar.VarPos.GlobalVar := TLapeGlobalVar(FCompiler.addManagedVar(FieldVar.VarType.NewGlobalVarP(Pointer(PtrUInt(FieldVar.VarPos.GlobalVar.Ptr) + FFieldMap.ItemsI[i].Offset)), True))
        else
          FieldVar.VarPos.GlobalVar := FieldVar.VarType.NewGlobalVarP(Pointer(PtrUInt(FieldVar.VarPos.GlobalVar.Ptr) + FFieldMap.ItemsI[i].Offset));
      mpVar: FieldVar.VarPos.Offset := FieldVar.VarPos.Offset + FFieldMap.ItemsI[i].Offset;
      else LapeException(lpeImpossible);
    end;

    if UseCompiler and (FCompiler <> nil) then
      FCompiler.FinalizeVar(FieldVar, Offset, Pos)
    else if NeedFinalization then
      FieldVar.VarType.Finalize(FieldVar, Offset, UseCompiler, Pos);
  finally
    if ((not UseCompiler) or (FCompiler = nil)) and (FieldVar.VarPos.MemPos = mpMem) and (FieldVar.VarPos.GlobalVar <> nil) then
      FreeAndNil(FieldVar.VarPos.GlobalVar);
    FieldVar.Spill(1);
  end;
end;

function TLapeType_Union.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := 'union ';
    for i := 0 to FFieldMap.Count - 1 do
      FAsString := FAsString + FFieldMap.ItemsI[i].FieldType.AsString + '; ';
    FAsString := FAsString + 'end';
  end;
  Result := inherited;
end;

constructor TLapeType_Union.Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited;
  FBaseType := ltUnion;
end;

procedure TLapeType_Union.addField(FieldType: TLapeType; AName: lpString; Alignment: Byte = 1);
var
  Field: TRecordField;
  FieldSize: Integer;
begin
  if (FSize < 0) or (FFieldMap.Count < 1) then
    FSize := 0;
  if (FInit = __Unknown) or (FFieldMap.Count < 1) then
    FInit := __No;
  FAsString := '';
  Field.Offset := 0;
  Field.FieldType := FieldType;
  if FFieldMap.ExistsKey(AName) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName]);
  FieldSize := FieldType.Size + (FieldType.Size mod Alignment);
  if (FieldSize > FSize) then
    FSize := FieldSize;
  if (FInit <> __Yes) and FieldType.NeedInitialization then
    FInit := __Yes;
  FFieldMap[AName] := Field;
end;

procedure TLapeType_Method.setBaseType(ABaseType: ELapeBaseType);
begin
  Assert(ABaseType in [ltPointer, ltScriptMethod, ltImportedMethod]);
  FBaseType := ABaseType;
end;

function TLapeType_Method.getSize: Integer;
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

function TLapeType_Method.getParamSize: Integer;
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
    AParams := TLapeParameterList.Create(NullParameter, dupAccept);
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
    FParams.add(Param);
  end;
end;

function TLapeType_Method.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Method;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FParams, Res, Name, @_DocPos);
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
  Result := (Other <> nil) and (Other is TLapeType_Method) and EqualParams(Other as TLapeType_Method, ContextOnly);
  if Result and (not ContextOnly) then
    Result := (Other.BaseType = BaseType);
end;

function TLapeType_Method.EqualParams(Other: TLapeType_Method; ContextOnly: Boolean = True): Boolean;

  function _EqualTypes(const Left, Right: TLapeType): Boolean;
  begin
    Result := (Left = Right) or ((Left <> nil) and Left.Equals(Right, ContextOnly));
  end;

  function _EqualParams(const Left, Right: TLapeParameter): Boolean;
  begin
    Result := ((Left.ParType in Lape_RefParams) = (Right.ParType in Lape_RefParams)) and
      ((Left.ParType in Lape_ValParams) = (Right.ParType in Lape_ValParams)) and
      ((Left.Default <> nil) = (Right.Default <> nil)) and
      _EqualTypes(Left.VarType, Right.VarType);
  end;

var
  i: Integer;
begin
  Result := False;
  if (Other = nil) or (TLapeType_Method(Other).Params.Count <> Params.Count) or (not _EqualTypes(Res, TLapeType_Method(Other).Res)) then
    Exit
  else
    for i := 0 to Params.Count - 1 do
      if (not _EqualParams(Params[i], TLapeType_Method(Other).Params[i])) then
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

  if (AVar = nil) or (AVar.HasType() and (AVar.VarType.BaseType = MethodType)) then
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

function TLapeType_Method.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
var
  m: TLapeGlobalVar;
begin
  if (Right <> nil) and (Right is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right).getMethod(Self);
    if (m <> nil) then
      Right := m.VarType;
  end;

  if (FBaseType = ltPointer) and (Op = op_Assign) and (Right <> nil) and ((Right.BaseType = ltPointer) or Equals(Right)) then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_Method.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
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

function TLapeType_Method.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
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

constructor TLapeType_OverloadedMethod.Create(ACompiler: TLapeCompilerBase; AMethods: TLapeDeclarationList; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUnknown, ACompiler, AName, ADocPos);

  OnFunctionNotFound := nil;
  NeedFullMatch := False;

  FreeMethods := (AMethods = nil);
  if (AMethods = nil) then
    AMethods := TLapeDeclarationList.Create(nil);
  FMethods := AMethods;
end;

destructor TLapeType_OverloadedMethod.Destroy;
begin
  if FreeMethods then
    FMethods.Free();
  inherited;
end;

function TLapeType_OverloadedMethod.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_OverloadedMethod;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, FMethods, Name, @_DocPos);
end;

procedure TLapeType_OverloadedMethod.addMethod(AMethod: TLapeGlobalVar; DoOverride: Boolean = False);
var
  i: Integer;
begin
  if (AMethod = nil) or (not AMethod.HasType()) or
     (not ((AMethod.VarType is TLapeType_Method) or (AMethod.VarType is TLapeType_OverloadedMethod)))
  then
    LapeException(lpeImpossible);

  if (AMethod.VarType is TLapeType_OverloadedMethod) then
    with TLapeType_OverloadedMethod(AMethod.VarType) do
      for i := 0 to Methods.Items.Count - 1 do
        Self.addMethod(TLapeGlobalVar(Methods.Items[i]).CreateCopy(False))
  else if DoOverride then
  begin
    AMethod := overrideMethod(AMethod);
    if FList.FreeDecls and (AMethod <> nil) then
      AMethod.Free();
  end
  else
  begin
    for i := 0 to FMethods.Items.Count - 1 do
      if TLapeType_Method(TLapeGlobalVar(FMethods.Items[i]).VarType).EqualParams(AMethod.VarType as TLapeType_Method, False) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [AMethod.VarType.AsString]);

    AMethod.Name := Name;
    FMethods.addDeclaration(AMethod);
  end;
end;

function TLapeType_OverloadedMethod.overrideMethod(AMethod: TLapeGlobalVar): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FMethods.Items.Count - 1 do
    if TLapeType_Method(TLapeGlobalVar(FMethods.Items[i]).VarType).EqualParams(AMethod.VarType as TLapeType_Method, False) then
    begin
      Result := FMethods.Items[i] as TLapeGlobalVar;
      FMethods.Delete(i);
      Break;
    end;
  FMethods.addDeclaration(AMethod);
end;

function TLapeType_OverloadedMethod.getMethod(AType: TLapeType_Method): TLapeGlobalVar;
var
  i: Integer;
begin
  for i := 0 to FMethods.Items.Count - 1 do
    if TLapeType_Method(TLapeGlobalVar(FMethods.Items[i]).VarType).EqualParams(AType, False) then
      Exit(TLapeGlobalVar(FMethods.Items[i]));
  if ({$IFNDEF FPC}@{$ENDIF}OnFunctionNotFound <> nil) then
    Result := OnFunctionNotFound(Self, AType, nil, nil)
  else
    Result := nil;
end;

function TLapeType_OverloadedMethod.getMethod(AParams: TLapeTypeArray; AResult: TLapeType = nil): TLapeGlobalVar;
var
  MethodIndex, i, Weight, MinWeight: Integer;
  Match: Boolean;

  function SizeWeight(a, b: TLapeType): Integer; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := Abs(a.Size - b.Size) * 4;
    if (a.Size < b.Size) then
      Result := Result * 4
    else if (a.BaseType in LapeIntegerTypes) and (b.BaseType in LapeIntegerTypes) and
       ((a.VarLo().AsInteger <= b.VarLo().AsInteger) and (UInt64(a.VarHi().AsInteger) >= UInt64(b.VarHi().AsInteger)))
    then if ((a.VarLo().AsInteger < 0) xor (b.VarLo().AsInteger >= 0)) then
      Result := Result - 2
    else
      Result := Result - 1;
  end;

begin
  Result := nil;
  MinWeight := High(Integer);

  for MethodIndex := 0 to FMethods.Items.Count - 1 do
    with TLapeType_Method(TLapeGlobalVar(FMethods.Items[MethodIndex]).VarType) do
    begin
      if (Length(AParams) > Params.Count) or ((AResult <> nil) and (Res = nil)) then
        Continue;

      if (AResult = nil) or AResult.Equals(Res) then
        Weight := Params.Count * 4
      else if (not AResult.CompatibleWith(Res)) then
        Continue
      else
        Weight := SizeWeight(Res, AResult) + (Params.Count + 1) * 4;

      Match := True;
      for i := 0 to Params.Count - 1 do
      begin
        Match := False;
        if ((i >= Length(AParams)) or (AParams[i] = nil)) and (Params[i].Default = nil) then
          Break
        else if (i >= Length(AParams)) or (AParams[i] = nil) or (Params[i].VarType = nil) or Params[i].VarType.Equals(AParams[i]) then
          if (i >= Length(AParams)) or (AParams[i] = nil) or ((Params[i].VarType <> nil) and Params[i].VarType.Equals(AParams[i], False)) then
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
        else if (not Params[i].VarType.CompatibleWith(AParams[i])) then
          Break
        else
          Weight := Weight + SizeWeight(Params[i].VarType, AParams[i]);
        Match := True;
      end;

      if Match then
        if (Weight = MinWeight) then
          Result := nil
        else if (Weight < MinWeight) then
        begin
          Result := TLapeGlobalVar(FMethods.Items[MethodIndex]);
          MinWeight := Weight;
        end;
    end;

  if (Result = nil) and ({$IFNDEF FPC}@{$ENDIF}OnFunctionNotFound <> nil) then
    Result := OnFunctionNotFound(Self, nil, AParams, AResult);
end;

function TLapeType_OverloadedMethod.NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
end;

function TLapeType_OverloadedMethod.HasChild(ADecl: TLapeDeclaration): Boolean;
begin
  Result := FMethods.Items.ExistsItem(ADecl);
end;

function TLapeType_OverloadedMethod.EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType;
begin
  if (Op = op_Index) and (Right <> nil) and (Right.BaseType in LapeIntegerTypes) then
    if (FMethods.Items[Right.AsInteger] = nil) then
      LapeException(lpeOutOfTypeRange)
    else
      Result := TLapeGlobalVar(FMethods.Items[Right.AsInteger]).VarType
  else
    Result := inherited;
end;

function TLapeType_OverloadedMethod.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Index) and (Left <> nil) and (Right <> nil) and Right.HasType() and (Right.VarType.BaseType in LapeIntegerTypes) then
    if (FMethods.Items[Right.AsInteger] = nil) then
      LapeException(lpeOutOfTypeRange)
    else
      Result := TLapeGlobalVar(FMethods.Items[Right.AsInteger])
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
  FVarMap := TLapeVarRefMap.Create(NullVarRef);
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
  Result := FVarMap.ExistsKey(AName);
end;

function TLapeType_VarRefMap.EvalRes(Op: EOperator; Right: TLapeGlobalVar): TLapeType;
begin
  if (Op = op_Dot) and (Right <> nil) and (Right.BaseType = ltString) then
    if (FVarMap[PlpString(Right.Ptr)^].RefVar <> nil) then
      Result := FVarMap[PlpString(Right.Ptr)^].RefVar.VarType
    else
      Result := FVarMap[PlpString(Right.Ptr)^].ResVar.VarType
  else
    Result := inherited;
end;

function TLapeType_VarRefMap.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  Result := inherited;
  if Result and (op = op_Dot) then
    Result := (FVarMap[PlpString(Right.Ptr)^].RefVar is TLapeGlobalVar);
end;

function TLapeType_VarRefMap.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  FieldName: lpString;
begin
  Assert((Left = nil) or (Left.VarType = Self));
  if (Op = op_Dot) and (Right <> nil) and Right.HasType() and (Right.VarType.BaseType = ltString) then
  begin
    Assert(Right.Ptr <> nil);
    FieldName := PlpString(Right.Ptr)^;

    if (FieldName <> '') and (FVarMap[FieldName].RefVar <> nil) and (FVarMap[FieldName].RefVar is TLapeGlobalVar) then
      Result := FVarMap[FieldName].RefVar as TLapeGlobalVar
    else
      LapeExceptionFmt(lpeUnknownDeclaration, [FieldName]);
  end
  else
    Result := inherited;
end;

function TLapeType_VarRefMap.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  FieldName: lpString;
begin
  Assert(Left.VarType = Self);
  if (Op = op_Dot) and (Right.VarPos.MemPos = mpMem) and Right.HasType() and (Right.VarType.BaseType = ltString) then
  begin
    Assert(Right.VarPos.GlobalVar.Ptr <> nil);
    FieldName := PlpString(Right.VarPos.GlobalVar.Ptr)^;

    Dest.Spill();
    if (FieldName <> '') and (FVarMap[FieldName].RefVar <> nil) then
      Result := _ResVar.New(FVarMap[FieldName].RefVar)
    else if (FieldName <> '') and (FVarMap[FieldName].ResVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
      Result := FVarMap[FieldName].ResVar
    else
      LapeExceptionFmt(lpeUnknownDeclaration, [FieldName]);
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

  FVarMap.add(AName, Rec);
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

  FVarMap.add(AName, Rec);
end;

function TLapeStackInfo.getVar(Index: Integer): TLapeStackVar;
begin
  Result := FVarStack[Index];
end;

function TLapeStackInfo.getCount: Integer;
begin
  Result := FVarStack.Count;
end;

function TLapeStackInfo.getTotalSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FVarStack.Count - 1 do
    Result := Result + FVarStack[i].Size;
end;

function TLapeStackInfo.getTotalParamSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FVarStack.Count - 1 do
    if (FVarStack[i] is TLapeParameterVar) then
      Result := Result + FVarStack[i].Size;
end;

function TLapeStackInfo.getTotalNoParamSize: Integer;
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

constructor TLapeStackInfo.Create(AlwaysInitialize: Boolean = True; AOwner: TLapeStackInfo = nil; ManageVars: Boolean = True);
begin
  inherited Create(nil, False);

  Owner := AOwner;
  FVarStack := TLapeVarStack.Create(nil, dupIgnore);
  FWithStack := TLapeWithDeclarationList.Create(NullWithDecl, dupIgnore);
  FreeVars := ManageVars;
  ForceInitialization := AlwaysInitialize;
  CodePos := -1;
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

function TLapeStackInfo.getDeclaration(Name: lpString; CheckWith: Boolean = True): TLapeDeclaration;
var
  i: Integer;
  Declarations: TLapeDeclArray;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if (FWithStack[i].WithType <> nil) and FWithStack[i].WithType.hasChild(Name) then
        Exit(TLapeWithDeclaration.Create(FWithStack[i]));

  Declarations := getByName(Name);
  if (Length(Declarations) > 1) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Name])
  else if (Length(Declarations) > 0) and (Declarations[0] <> nil) then
    Result := Declarations[0]
  else
    Result := nil;
end;

function TLapeStackInfo.hasDeclaration(Name: lpString; CheckWith: Boolean = True): Boolean;
var
  i: Integer;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if (FWithStack[i].WithType <> nil) and FWithStack[i].WithType.hasChild(Name) then
        Exit(True);

  Result := (Length(getByName(Name)) > 0);
end;

function TLapeStackInfo.hasDeclaration(Decl: TLapeDeclaration; CheckWith: Boolean = True): Boolean;
var
  i: Integer;
begin
  if CheckWith then
    for i := FWithStack.Count - 1 downto 0 do
      if (FWithStack[i].WithType <> nil) and FWithStack[i].WithType.hasChild(Decl) then
        Exit(True);

  Result := FList.ExistsItem(Decl);
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
        Result.isConstant := True;
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
  else if FList.ExistsItem(Decl) or ((Decl.Name <> '') and hasDeclaration(Decl.Name)) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Decl.Name]);
  Result := FList.add(Decl);
end;

function TLapeStackInfo.addVar(StackVar: TLapeStackVar): TLapeStackVar;
begin
  if (StackVar = nil) then
    Exit(nil);
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

function TLapeStackInfo.addVar(ParType: ELapeParameterType; VarType: TLapeType; Name: lpString = ''): TLapeStackVar;
begin
  Result := addVar(TLapeParameterVar.Create(ParType, VarType, nil, Name));
end;

function TLapeStackInfo.addWith(AWith: TLapeWithDeclRec): Integer;
begin
  Result := FWithStack.add(AWith);
end;

procedure TLapeStackInfo.delWith(Count: Integer);
var
  i: Integer;
begin
  for i := FWithStack.Count - 1 downto FWithStack.Count - Count do
    FWithStack.Delete(i);
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
    //mmpStk: Result := _IncCall_Stk(AParamSize + ACodePos.VarType.Size, AParamSize, Offset, Pos);
    //mmpPStk: Result := _IncCall_PStk(AParamSize, Offset, Pos);
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
    //mmpStk: Result := _InvokeImported_Stk(AParamSize + AMemPos.VarType.Size, AParamSize, Offset, Pos);
    //mmpPStk: Result := _InvokeImported_PStk(AParamSize, Offset, Pos);
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
    {
    mmpStk: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Stk_Stk(AParamSize + AMemPos.VarType.Size, AParamSize, AResPos.VarType.Size, Offset, Pos);
      mmpPStk: Result := _InvokeImported_Stk_PStk(AParamSize + AMemPos.VarType.Size, AParamSize, Offset, Pos);
      mmpVar: Result := _InvokeImported_Stk_Var(AParamSize + AMemPos.VarType.Size, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPVar: Result := _InvokeImported_Stk_PVar(AParamSize + AMemPos.VarType.Size, AResPos.VarPos.StackVar.Offset, AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPtr: Result := _InvokeImported_Stk_Ptr(AParamSize + AMemPos.VarType.Size, AResPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      else e := True;
    end;
    }
    mmpVar: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Var_Stk(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AParamSize, AResPos.VarType.Size, Offset, Pos);
      {$IFDEF Lape_PStkD}
      mmpPStk: Result := _InvokeImported_Var_PStk(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AParamSize, Offset, Pos);
      {$ENDIF}
      mmpVar: Result := _InvokeImported_Var_Var(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPVar: Result := _InvokeImported_Var_PVar(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.StackVar.Offset, AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPtr: Result := _InvokeImported_Var_Ptr(AMemPos.VarPos.StackVar.Offset + AMemPos.VarPos.Offset, AResPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      else e := True;
    end;
    mmpPVar: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_PVar_Stk(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AParamSize, AResPos.VarType.Size, Offset, Pos);
      {$IFDEF Lape_PStkD}
      mmpPStk: Result := _InvokeImported_PVar_PStk(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AParamSize, Offset, Pos);
      {$ENDIF}
      mmpVar: Result := _InvokeImported_PVar_Var(AMemPos.VarPos.StackVar.Offset, AResPos.VarPos.StackVar.Offset + AResPos.VarPos.Offset, AMemPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPVar: Result := _InvokeImported_PVar_PVar(AMemPos.VarPos.StackVar.Offset, AResPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AResPos.VarPos.Offset, AParamSize, Offset, Pos);
      mmpPtr: Result := _InvokeImported_PVar_Ptr(AMemPos.VarPos.StackVar.Offset, AMemPos.VarPos.Offset, AResPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      else e := True;
    end;
    mmpPtr: case getMemoryPos(AResPos.VarPos) of
      mmpStk: Result := _InvokeImported_Ptr_Stk(AMemPos.VarPos.GlobalVar.Ptr, AParamSize, AResPos.VarType.Size, Offset, Pos);
      {$IFDEF Lape_PStkD}
      mmpPStk: Result := _InvokeImported_Ptr_PStk(AMemPos.VarPos.GlobalVar.Ptr, AParamSize, Offset, Pos);
      {$ENDIF}
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

{
function TLapeCodeEmitter._JmpIf(Target: TCodePos; Cond: TResVar; var Offset: Integer; Pos: PDocPos = nil): Integer;
var
  e: Boolean;
begin
  Assert(Cond.HasType());
  e := False;

  case getMemoryPos(Cond.VarPos) of
    mmpStk:
      case Cond.VarType.Size of
        1: Result := _JmpIf8_Stk(Target, Offset, Pos);
        2: Result := _JmpIf16_Stk(Target, Offset, Pos);
        4: Result := _JmpIf32_Stk(Target, Offset, Pos);
        8: Result := _JmpIf64_Stk(Target, Offset, Pos);
        else e := True;
      end;
    mmpVar:
      case Cond.VarType.Size of
        1: Result := _JmpIf8_Var(Target, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        2: Result := _JmpIf16_Var(Target, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        4: Result := _JmpIf32_Var(Target, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        8: Result := _JmpIf64_Var(Target, Cond.VarPos.StackVar.Offset + Cond.VarPos.Offset, Offset, Pos);
        else e := True;
      end;
    mmpPtr:
      case Cond.VarType.Size of
        1: Result := _JmpIf8_Ptr(Target, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        2: Result := _JmpIf16_Ptr(Target, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        4: Result := _JmpIf32_Ptr(Target, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        8: Result := _JmpIf64_Ptr(Target, Cond.VarPos.GlobalVar.Ptr, Offset, Pos);
        else e := True;
      end;
    mmpPStk:
      case Cond.VarType.Size of
        1: Result := _JmpIf8_PStk(Target, Offset, Pos);
        2: Result := _JmpIf16_PStk(Target, Offset, Pos);
        4: Result := _JmpIf32_PStk(Target, Offset, Pos);
        8: Result := _JmpIf64_PStk(Target, Offset, Pos);
        else e := True;
      end;
    mmpPVar:
      case Cond.VarType.Size of
        1: Result := _JmpIf8_PVar(Target, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        2: Result := _JmpIf16_PVar(Target, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        4: Result := _JmpIf32_PVar(Target, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        8: Result := _JmpIf64_PVar(Target, Cond.VarPos.StackVar.Offset, Cond.VarPos.Offset, Offset, Pos);
        else e := True;
      end;
    else e := True;
  end;

  if e then
    LapeException(lpeInvalidEvaluation);
end;

function TLapeCodeEmitter._JmpIf(Target: TCodePos; Cond: TResVar; Pos: PDocPos = nil): Integer;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := _JmpIf(Target, Cond, Offset, Pos);
end;
}

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

  FreeEmitter := ManageEmitter;
  if (AEmitter = nil) then
    AEmitter := TLapeCodeEmitter.Create();
  FEmitter := AEmitter;

  LoadBaseTypes(FBaseTypes, Self);
  FStackInfo := nil;

  FGlobalDeclarations := TLapeDeclarationList.Create(nil);
  FManagedDeclarations := TLapeDeclarationList.Create(nil);
  FCachedDeclarations := TLapeVarMap.Create(nil);
end;

destructor TLapeCompilerBase.Destroy;
begin
  Clear();
  setEmitter(nil);

  FreeAndNil(FGlobalDeclarations);
  FreeAndNil(FManagedDeclarations);
  FreeAndNil(FCachedDeclarations);
  ClearBaseTypes(FBaseTypes);

  inherited;
end;

procedure TLapeCompilerBase.Clear;
begin
  FGlobalDeclarations.Delete(TLapeVar, True);
  FManagedDeclarations.Delete(TLapeVar, True);
  FGlobalDeclarations.Delete(TLapeType_OverloadedMethod, True);
  FManagedDeclarations.Delete(TLapeType_OverloadedMethod, True);
  FGlobalDeclarations.Clear();
  FManagedDeclarations.Clear();
  FCachedDeclarations.Clear();
  Reset();
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
      AStackInfo.CodePos := Emitter._IncTry(0, Try_NoExcept, Offset, Pos);
      Emitter._ExpandVar(0, Offset, Pos);
      Emitter._InitStackLen(0, Offset, Pos);
    end;
  end;

  FStackInfo := AStackInfo;
  Result := FStackInfo;
end;

function TLapeCompilerBase.IncStackInfo(var Offset: Integer; Emit: Boolean = True; Pos: PDocPos = nil): TLapeStackInfo;
begin
  Result := IncStackInfo(TLapeStackInfo.Create(lcoAlwaysInitialize in FOptions, FStackInfo), Offset, Emit, Pos);
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
  i: Integer;
  InitStackPos: Integer;

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

  function NeedFinalization(v: TLapeVar): Boolean;
  begin
    if (v is TLapeParameterVar) then
      Result := (not (TLapeParameterVar(v).ParType in Lape_RefParams))
    else
      Result := (v <> nil) and ((lcoFullDisposal in FOptions) or v.NeedFinalization);
  end;

begin
  if (FStackInfo = nil) or (FStackInfo = EmptyStackInfo) then
    Result := nil
  else
  begin
    Result := FStackInfo.Owner;

    if Emit then
    begin
      InitStackPos := FStackInfo.CodePos + ocSize*2 + SizeOf(TOC_IncTry) + SizeOf(TStackOffset);
      if (FStackInfo.TotalNoParamSize <= 0) then
        RemoveExpandVar();
      if (FStackInfo.TotalSize <= 0) and (not InFunction) then
        RemoveIncTry()
      else
      begin
        Emitter._DecTry(Offset, Pos);
        Emitter._IncTry(Offset - FStackInfo.CodePos, Try_NoExcept, FStackInfo.CodePos, Pos);

        with FStackInfo.VarStack do
        begin
          i := 0;
          while (i < Count) do
          begin
            if NeedFinalization(Items[i]) then
            begin
              FinalizeVar(_ResVar.New(Items[i]), Offset, Pos);
              if (Items[i] is TLapeStackTempVar) then
              begin
                if TLapeStackTempVar(Items[i]).Locked then
                  WriteLn(Items[i].Name, ' ', Items[i].VarType.AsString, ' still locked! ', TLapeStackTempVar(Items[i]).FLock);
                TLapeStackTempVar(Items[i]).Locked := True;
              end;
            end;
            Inc(i);
          end;
          for i := 0 to Count - 1 do
            if (Items[i] is TLapeStackTempVar) then
              TLapeStackTempVar(Items[i]).Locked := False;
        end;

        if InFunction then
          Emitter._DecCall_EndTry(Offset, Pos)
        else
        begin
          Emitter._PopVar(Offset, Pos);
          Emitter._EndTry(Offset, Pos);
        end;

        //WriteLn('Vars on stack: ', FStackInfo.Count);

        if (not InFunction) then
          if FStackInfo.NeedInitialization then
            Emitter._ExpandVarAndInit(FStackInfo.TotalSize, FStackInfo.CodePos, Pos)
          else
            Emitter._ExpandVar(FStackInfo.TotalSize, FStackInfo.CodePos, Pos)
        else if (FStackInfo.TotalNoParamSize > 0) then
          if FStackInfo.NeedInitialization then
            Emitter._GrowVarAndInit(FStackInfo.TotalNoParamSize, FStackInfo.CodePos, Pos)
          else
            Emitter._GrowVar(FStackInfo.TotalNoParamSize, FStackInfo.CodePos, Pos);
      end;

      if (Emitter.MaxStack > 0) then
        Emitter._InitStackLen(Emitter.MaxStack, InitStackPos, Pos)
      else
        Emitter.Delete(InitStackPos, ocSize + SizeOf(TStackOffset), Offset);
    end;

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
  VarRefsVar: TLapeGlobalVar;
  WithVar: TLapeWithDeclRec;
  i: Integer;
begin
  Assert(Length(AVarNames) = Length(AVars) + Length(AResVars));
  VarRefs := nil;
  VarRefsVar := nil;
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
    VarRefsVar := VarRefs.NewGlobalVarP();

    WithVar.WithType := VarRefs;
    WithVar.WithVar := @VarRefsVar;
    FStackInfo.addWith(WithVar);

    try
      EmitCode(ACode, Offset, Pos);
    finally
      FStackInfo.delWith(1);
    end;
  finally
    if (VarRefsVar <> nil) then
      VarRefsVar.Free();
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
var
  i: Integer;
  GlobalVars: TLapeDeclArray;
begin
  if (AVar = nil) or (AVar.DeclarationList <> nil) then
    Exit(AVar);
  if (AVar is TLapeGlobalVar) and AVar.HasType() and AVar.isConstant and (AVar.Name = '') and
     (PtrCheckOnly or (AVar.VarType.EvalRes(op_cmp_Equal, AVar.VarType) <> nil))
  then
  begin
    GlobalVars := FManagedDeclarations.getByClass(TLapeDeclarationClass(AVar.ClassType), True);
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

  Result := addManagedDecl(AVar) as TLapeVar;
  {$IFNDEF Lape_SmallCode}
  if (AVar is TLapeGlobalVar) and AVar.HasType() and AVar.isConstant and (AVar.Name = '') then
    with AVar as TLapeGlobalVar do
      FCachedDeclarations.add(AsString + ':' + LapeTypeToString(BaseType), TLapeGlobalVar(AVar));
  {$ENDIF}
end;

function TLapeCompilerBase.addManagedType(AType: TLapeType): TLapeType;
var
  i: Integer;
  Types: TLapeDeclArray;
begin
  if (AType = nil) or (AType.DeclarationList <> nil) then
    Exit(AType);

  Types := FManagedDeclarations.getByClass(TLapeDeclarationClass(AType.ClassType), True);
  for i := 0 to High(Types) do
    if (AType = Types[i]) then
      Exit(AType)
    else if TLapeType(Types[i]).Equals(AType, False) then
    begin
      AType.Free();
      Exit(TLapeType(Types[i]));
    end;
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
  Result := nil;

  {$IFNDEF Lape_SmallCode}
  if (BaseType in LapeStringTypes) then
    Result := FCachedDeclarations['"' + Str + '":' + LapeTypeToString(BaseType)]
  else
    Result := FCachedDeclarations[Str + ':' + LapeTypeToString(BaseType)];
  {$ENDIF}

  if (Result = nil) then
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
  Result := getConstant(IntToStr(i), IntType, DoGrow, ForceType);
end;

procedure TLapeCompilerBase.getDestVar(var Dest, Res: TResVar; Op: EOperator);
begin
  if (op = op_Assign) then
    Dest.Spill()
  else if (op <> op_Deref) and (Dest.VarPos.MemPos <> mpNone) and
    Res.HasType() and Res.VarType.Equals(Dest.VarType)
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
      end;
    Dest.Spill();
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

function TLapeCompilerBase.getPointerType(PType: ELapeBaseType): TLapeType_Pointer;
begin
  Result := getPointerType(FBaseTypes[PType]);
end;

function TLapeCompilerBase.getPointerType(PType: TLapeType): TLapeType_Pointer;
begin
  Result := TLapeType_Pointer(addManagedType(TLapeType_Pointer.Create(Self, PType)));
end;

function TLapeCompilerBase.getTypeVar(AType: ELapeBaseType): TLapeGlobalVar;
begin
  Result := getTypeVar(FBaseTypes[AType]);
end;

function TLapeCompilerBase.getTypeVar(AType: TLapeType): TLapeGlobalVar;
begin
  Result := addManagedVar(addManagedType(TLapeType_Type.Create(AType, Self)).NewGlobalVarP()) as TLapeGlobalVar;
end;

function TLapeCompilerBase.getGlobalVar(AName: lpString): TLapeGlobalVar;
var
  Declarations: TLapeDeclArray;
begin
  Declarations := GlobalDeclarations.getByClassAndName(AName, TLapeGlobalVar);
  if (Length(Declarations) > 1) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName])
  else if (Length(Declarations) > 0) and (Declarations[0] <> nil) then
    Result := Declarations[0] as TLapeGlobalVar
  else
    Result := nil;
end;

function TLapeCompilerBase.getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration;
var
  Declarations: TLapeDeclArray;
begin
  if (AStackInfo <> nil) then
  begin
    Result := AStackInfo.getDeclaration(AName, CheckWith);
    if (Result <> nil) or LocalOnly then
      Exit;
  end;

  Declarations := GlobalDeclarations.getByName(AName);
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
    Result := AStackInfo.hasDeclaration(AName, CheckWith);
    if Result or LocalOnly then
      Exit;
  end;

  if (Length(GlobalDeclarations.getByName(AName)) > 0) then
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
    Result := AStackInfo.hasDeclaration(ADecl, CheckWith);
    if Result or LocalOnly then
      Exit;
  end;

  if GlobalDeclarations.Items.ExistsItem(ADecl) then
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

initialization
  EmptyStackInfo := TLapeEmptyStack.Create();
finalization
  EmptyStackInfo.Free();
end.

