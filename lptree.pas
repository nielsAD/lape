{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Syntax tree objects (and turning it into bytecode).
}
unit lptree;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lptokenizer;

type
  TLapeTree_Base = class;
  TLapeTree_ExprBase = class;
  TLapeTree_Visit = function(Node: TLapeTree_Base): Boolean of object; // True: stop the walk

  TLapeFlowStatement = record
    CodeOffset: Integer;
    DocPos: TDocPos;
    JumpSafe: Boolean;
  end;

  TLapeExpressionList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_ExprBase>;
  TLapeStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_Base>;
  TLapeFlowStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeFlowStatement>;

  ILapeTree_CanBreak = interface(IInterface)
    ['{F63EF9FB-2DD0-4C2C-B564-1333995EE9AF}']
    function canBreak: Boolean;
    procedure addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
  end;

  ILapeTree_CanContinue = interface(IInterface)
    ['{411AF7EA-9D38-48DE-90E9-362E2D0D09E9}']
    function canContinue: Boolean;
    procedure addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
  end;

  ILapeTree_CanExit = interface(IInterface)
    ['{D1963640-31F9-4BB3-B03E-0F1DB314D98E}']
    function canExit: Boolean;
    procedure addExitStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
  end;

  TLapeTreeType = class(TLapeType)
  protected
    FDecl: TLapeTree_ExprBase;
  public
    constructor Create(ADecl: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase = nil); reintroduce; virtual;
    destructor Destroy; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function getDecl: TLapeTree_ExprBase;
    property Decl: TLapeTree_ExprBase read FDecl;
  end;

  TLapeTree_Base = class(TLapeBaseDeclClass)
  protected
    FParent: TLapeTree_Base;
    FCompiler: TLapeCompilerBase;
    FCompilerOptions: ECompilerOptionsSet;

    function getDocPos: TDocPos; override;
    procedure setParent(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); virtual;
  public
    _DocPos: TDocPos;

    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(ASource: TLapeTree_Base); reintroduce; overload; virtual;
    destructor Destroy; override;

    class function isEmpty(Node: TLapeTree_Base): Boolean; virtual;

    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; virtual;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; virtual;

    function Compile(var Offset: Integer): TResVar; overload; virtual;
    function Compile: TResVar; overload; virtual;
    function CompileToTempVar(var Offset: Integer; out v: TResVar): Boolean; virtual;
    function isInvoking: Boolean; virtual;
    function Walk(Visit: TLapeTree_Visit): Boolean; virtual;

    property Parent: TLapeTree_Base read FParent write setParent;
    property Compiler: TLapeCompilerBase read FCompiler;
    property CompilerOptions: ECompilerOptionsSet read FCompilerOptions write FCompilerOptions;
  end;

  TLapeTree_ExprBase = class(TLapeTree_Base)
  protected
    FConstant: TInitBool;
    FResType: TLapeType;
    FRes: TLapeGlobalVar;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    procedure ClearCache; virtual;

    function isConstant: Boolean; virtual;
    function resType: TLapeType; virtual;
    function Evaluate: TLapeGlobalVar; virtual;

    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
  end;

  TLapeTree_DestExprBase = class(TLapeTree_ExprBase)
  protected
    FDest: TResVar;
    procedure setDest(ResVar: TResVar); virtual;
  public
    function CompileToTempVar(var Offset: Integer; out v: TResVar): Boolean; override;
    property Dest: TResVar read FDest write setDest;
  end;

  TLapeTree_OpenArray = class(TLapeTree_DestExprBase)
  protected
    FCanCast: TInitBool;
    FInvalidCastIndex: Integer;
    FType: TLapeType;
    FValues: TLapeStatementList;
    FRange: TLapeRange;

    procedure setType(AType: TLapeType); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    procedure ClearCache; override;
    function addValue(Val: TLapeTree_Base): Integer; virtual;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function canCastTo(AType: TLapeType; Strict: Boolean): Boolean;
    function canCast: Boolean; virtual;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
    function isInvoking: Boolean; override;


    property ToType: TLapeType read FType write setType;
    property Values: TLapeStatementList read FValues;
  end;

  TLapeTree_Cast = class(TLapeTree_DestExprBase)
  protected
    FCastTo: TLapeTree_ExprBase;
    FParam: TLapeTree_ExprBase;

    procedure DeleteChild(Node: TLapeTree_Base); override;

    procedure setCastTo(Node: TLapeTree_ExprBase); virtual;
    procedure setParam(Node: TLapeTree_ExprBase); virtual;
  public
    destructor Destroy; override;

    function Compile(var Offset: Integer): TResVar; overload; override;
    function Evaluate: TLapeGlobalVar; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function isInvoking: Boolean; override;

    property CastTo: TLapeTree_ExprBase read FCastTo write setCastTo;
    property Param: TLapeTree_ExprBase read FParam write setParam;
  end;

  TLapeTree_Operator = class(TLapeTree_DestExprBase)
  protected
    FRestructure: TInitBool;
    FOperatorType: EOperator;
    FLeft: TLapeTree_Base;
    FRight: TLapeTree_ExprBase;
    FAssigning: TInitBool;
    FInvoked: TInitBool;
    FOverloadOp: Boolean;

    function getLeft: TLapeTree_ExprBase; virtual;
    procedure setLeft(Node: TLapeTree_ExprBase); virtual;
    procedure setRight(Node: TLapeTree_ExprBase); virtual;
    procedure setParent(Node: TLapeTree_Base); override;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AOperatorType: EOperator; ASource: TLapeTree_Base); overload; virtual;
    destructor Destroy; override;

    procedure ClearCache; override;
    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function isAssigning: Boolean; virtual;

    function isInvoked: Boolean; virtual;
    function isInvoking: Boolean; override;
    // An assignment of what calls a method has that worked out first
    function RightFirst: Boolean; virtual;
    function isConstant: Boolean; override;

    function EvalFlags: ELapeEvalFlags; virtual;
    function resType(Restructure: Boolean): TLapeType; reintroduce; overload;
    function resType: TLapeType; overload; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property OperatorType: EOperator read FOperatorType write FOperatorType;
    // Whether it is what a call calls (bUnknown: worked out from where it is, see isInvoked)
    property Invoked: TInitBool read FInvoked write FInvoked;
    property Left: TLapeTree_ExprBase read getLeft write setLeft;
    property Right: TLapeTree_ExprBase read FRight write setRight;
  end;

  TLapeTree_NoFold = class(TLapeTree_ExprBase)
  public
    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
  end;

  TLapeTree_ResVar = class(TLapeTree_NoFold)
  protected
    FResVar: TResVar;
  public
    constructor Create(AResVar: TResVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AResVar: TResVar; ASource: TLapeTree_Base); overload; virtual;

    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property ResVar: TResVar read FResVar;
  end;

  TLapeTree_GlobalVar = class(TLapeTree_NoFold)
  protected
    FGlobalVar: TLapeGlobalVar;
    function getVarAsString: lpString; virtual;
    function getVarAsInt: Int64; virtual;
  public
    constructor Create(AGlobalVar: TLapeGlobalVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AGlobalVar: TLapeGlobalVar; ASource: TLapeTree_Base); overload; virtual;
    constructor Create(Ident: lpString; BaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload; virtual;
    constructor Create(Ident: lpString; BaseType: ELapeBaseType; ASource: TLapeTree_Base); overload; virtual;

    function Compile(var Offset: Integer): TResVar; override;

    property GlobalVar: TLapeGlobalVar read FGlobalVar;
    property VarAsString: lpString read getVarAsString;
    property VarAsInteger: Int64 read getVarAsInt;
  end;

  // `inherited`: the method that the one that it is in overrides, of the Self of that one (as Self.Method gives it)
  TLapeTree_Inherited = class(TLapeTree_NoFold)
  protected
    FMethod: TLapeInheritedMethod;
  public
    constructor Create(AMethod: TLapeInheritedMethod; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;

    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Method: TLapeInheritedMethod read FMethod;
  end;

  TLapeTree_WithVar = class(TLapeTree_ExprBase)
  protected
    FWithDeclRec: TLapeWithDeclRec;
  public
    constructor Create(AWithDeclRec: TLapeWithDeclRec; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AWithDeclRec: TLapeWithDeclRec; ASource: TLapeTree_Base); overload; virtual;

    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property WithDeclRec: TLapeWithDeclRec read FWithDeclRec;
  end;

  TLapeTree_VarType = class(TLapeTree_GlobalVar)
  protected
    FVarType: TLapeType;
  public
    constructor Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AVarType: TLapeType; ASource: TLapeTree_Base); overload; virtual;
    property VarType: TLapeType read FVarType;
  end;

  TLapeTree_Integer = class(TLapeTree_GlobalVar)
  public
    constructor Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AStr: lpString; ASource: TLapeTree_Base); overload;
    constructor Create(AValue: Int64; ASource: TLapeTree_Base); overload;
    constructor Create(AValue: UInt64; ASource: TLapeTree_Base); overload;
  end;

  TLapeTree_Float = class(TLapeTree_GlobalVar)
  public
    constructor Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AStr: lpString; ASource: TLapeTree_Base); overload;
    constructor Create(AValue: lpFloat; ASource: TLapeTree_Base); overload;
  end;

  TLapeTree_String = class(TLapeTree_GlobalVar)
  public
    constructor Create(AValue: AnsiString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AValue: UnicodeString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AValue: lpString; ASource: TLapeTree_Base); overload;
  end;
  TLapeTree_Field = class(TLapeTree_String);

  TLapeTree_Char = class(TLapeTree_GlobalVar)
  public
    constructor Create(ACode: UInt32; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(ACode: UInt32; ASource: TLapeTree_Base); overload;
  end;

  TLapeTree_Range = class(TLapeTree_Base)
  protected
    FLo: TLapeTree_ExprBase;
    FHi: TLapeTree_ExprBase;

    procedure setLo(Node: TLapeTree_ExprBase); virtual;
    procedure setHi(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    // Both ends are there and constant
    function isConstant: Boolean; virtual;
    function isInvoking: Boolean; override;

    property Lo: TLapeTree_ExprBase read FLo write setLo;
    property Hi: TLapeTree_ExprBase read FHi write setHi;
  end;

  TLapeTree_StatementList = class(TLapeTree_Base)
  protected
    FStatements: TLapeStatementList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addStatement(Statement: TLapeTree_Base; Prepend: Boolean = False): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    property Statements: TLapeStatementList read FStatements;
  end;

  TLapeDelayedFilter = (ldfAll, ldfMethods, ldfStatements);
  TLapeTree_DelayedStatementList = class(TLapeTree_StatementList)
  protected
    FDelay: array of Boolean;
    FGlobal: array of Boolean;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    procedure Clean; virtual;
    function GlobalCount(Global: Boolean = True): Integer; virtual;
    procedure OverrideDelayGlobal(AfterCompilation, IsGlobal: Boolean; StartIndex: Integer; Count: Integer = 1); virtual;

    function addStatement(Statement: TLapeTree_Base; Prepend: Boolean = False): Integer; overload; override;
    function addStatement(Statement: TLapeTree_Base; AfterCompilation, IsGlobal: Boolean): Integer; reintroduce; overload; virtual;
    function Compile(var Offset: Integer; AfterCompilation: Boolean; Filter: TLapeDelayedFilter = ldfAll): TResVar; overload; virtual;
    function Compile(AfterCompilation: Boolean; Filter: TLapeDelayedFilter = ldfAll): TResVar; overload; virtual;
  end;

  TLapeTree_Method = class(TLapeTree_Base, ILapeTree_CanExit)
  protected
    FStackInfo: TLapeStackInfo;
    FStatements: TLapeTree_StatementList;
    FExitStatements: TLapeFlowStatementList;

    procedure setStatements(Node: TLapeTree_StatementList); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    Method: TLapeGlobalVar;
    SelfVar: TResVar;
    FreeStackInfo: Boolean;

    constructor Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; virtual;
    destructor Destroy; override;

    function Compile(var Offset: Integer): TResVar; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    function canExit: Boolean; virtual;
    procedure addExitStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;

    property StackInfo: TLapeStackInfo read FStackInfo;
    property Statements: TLapeTree_StatementList read FStatements write setStatements;
  end;

  TLapeVarDecl = record
    VarDecl: TLapeVar;
    Default: TLapeTree_ExprBase;
  end;
  TLapeVarDeclList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeVarDecl>;

  TLapeTree_VarList = class(TLapeTree_Base)
  protected
    FVars: TLapeVarDeclList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addVar(AVar: TLapeVarDecl): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    property Vars: TLapeVarDeclList read FVars;
  end;

  TLapeTree_FinalizeVar = class(TLapeTree_Base)
  protected
    FVariable: TLapeVar;
  public
    constructor Create(AVar: TLapeVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AVar: TLapeVar; ASource: TLapeTree_Base); overload; virtual;

    function Compile(var Offset: Integer): TResVar; override;
    property Variable: TLapeVar read FVariable;
  end;

  TLapeTree_With = class(TLapeTree_Base)
  protected
    FWithList: TLapeExpressionList;
    FVarList: array[0..7] of TResVar;
    FBody: TLapeTree_Base;
    procedure setBody(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclRec; virtual;
    function Compile(var Offset: Integer): TResVar; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    property WithList: TLapeExpressionList read FWithList;
    property Body: TLapeTree_Base read FBody write setBody;
  end;

  TLapeTree_If = class(TLapeTree_Base)
  protected
    FStartBodyOffset: Integer;
    FCondition: TLapeTree_ExprBase;
    FBody: TLapeTree_Base;
    FElse: TLapeTree_Base;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure setElse(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
    function CompileCondition(var Offset: Integer): TResVar; virtual;
    function CompileBody(var Offset: Integer): TResVar; virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;
    function isInvoking: Boolean; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property Body: TLapeTree_Base read FBody write setBody;
    property ElseBody: TLapeTree_Base read FElse write setElse;
  end;

  TLapeTree_CaseBranch = class(TLapeTree_Base)
  protected
    FValues: TLapeStatementList;
    FCondition: TLapeTree_ExprBase;
    FStatement: TLapeTree_Base;
    FCaseEndJump: Integer;
    FFallThroughStatements: TLapeFlowStatementList;
    FBodyOffset: Integer;

    procedure DeleteChild(Node: TLapeTree_Base); override;

    procedure setCaseEndOffset(Offset: Integer); virtual;
    procedure setFallThroughOffset(Offset: Integer); virtual;
    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setStatement(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addValue(Value: TLapeTree_Base): Integer; virtual;
    procedure addFallThroughStatement(JumpSafe: Boolean; Offset: Integer; Pos: PDocPos = nil); virtual;

    function Compile(var Offset: Integer): TResVar; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property Statement: TLapeTree_Base read FStatement write setStatement;
    property BodyOffset: Integer read FBodyOffset;
    property CaseEndOffset: Integer write setCaseEndOffset;
    property FallThroughOffset: Integer write setFallThroughOffset;
  end;

  TLapeTree_Case = class(TLapeTree_Base)
  protected
  type
    TCaseBranches = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_CaseBranch>;
  protected
    FCondition: TLapeTree_ExprBase;
    FElse: TLapeTree_Base;
    FBranches: TCaseBranches;

    procedure DeleteChild(Node: TLapeTree_Base); override;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setElse(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    procedure addBranch(Branch: TLapeTree_CaseBranch); virtual;

    function Compile(var Offset: Integer): TResVar; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property ElseBody: TLapeTree_Base read FElse write setElse;
  end;

  TLapeTree_While = class(TLapeTree_If, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FContinueCount: Integer;
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;
    function CompileBody(var Offset: Integer): TResVar; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    function canBreak: Boolean; virtual;
    function canContinue: Boolean; virtual;
    procedure addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
    procedure addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
  end;

  TLapeTree_Repeat = class(TLapeTree_If, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;
    function CompileBody(var Offset: Integer): TResVar; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    function canBreak: Boolean; virtual;
    function canContinue: Boolean; virtual;
    procedure addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
    procedure addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil); virtual;
  end;

  TLapeTree_For = class(TLapeTree_While)
  protected
    FCounter: TLapeTree_ExprBase;
    FLimit: TLapeTree_ExprBase;
    FStep: TLapeTree_ExprBase;

    procedure setCounter(Node: TLapeTree_ExprBase); virtual;
    procedure setLimit(Node: TLapeTree_ExprBase); virtual;
    procedure setStep(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
    function CompileBody(var Offset: Integer): TResVar; override;
    procedure CompileBodyForIn(var Offset: Integer); virtual;
  public
    LoopType: ELoopType;
    LoopOverWhat: ELoopOverWhat;
    Container: TResVar;
    CheckInVar: TResVar; // loopOver optional: if not (item in CheckInVar) then Continue;

    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;
    function CompileForIn(var Offset: Integer): TResVar; virtual;
    
    property Counter: TLapeTree_ExprBase read FCounter write setCounter;
    property Limit: TLapeTree_ExprBase read FLimit write setLimit;
    property Step: TLapeTree_ExprBase read FStep write setStep;
  end;

  TLapeTree_Try = class(TLapeTree_Base)
  protected
    FBody: TLapeTree_Base;
    FExcept: TLapeTree_Base;
    FFinally: TLapeTree_Base;

    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure setExcept(Node: TLapeTree_Base); virtual;
    procedure setFinally(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    property Body: TLapeTree_Base read FBody write setBody;
    property ExceptBody: TLapeTree_Base read FExcept write setExcept;
    property FinallyBody: TLapeTree_Base read FFinally write setFinally;
  end;

function getFlowStatement(Offset: Integer; Pos: PDocPos = nil; JumpSafe: Boolean = False): TLapeFlowStatement; {$IFDEF Lape_Inline}inline;{$ENDIF}
function EnsureCompound(Compiler: TLapeCompilerBase; Statement: TLapeTree_Base; AllowNil: Boolean = True): TLapeTree_StatementList;
function GetMagicMethodOrNil(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; AResult: TLapeType = nil): TResVar;
function HasMagicMethod(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; ARes: TLapeType): Boolean;

const
  NullFlowStatement: TLapeFlowStatement = (CodeOffset: 0; DocPos: (Line: 0; Col: 0; FileName: ''); JumpSafe: False);

implementation

uses
  Math,
  lpvartypes_ord, lpvartypes_record, lpvartypes_array, lpvartypes_object,
  lpmessages, lpeval, lpinterpreter_types, lpinvoke, lpinternalmethods;

function getFlowStatement(Offset: Integer; Pos: PDocPos = nil; JumpSafe: Boolean = False): TLapeFlowStatement;
begin
  Result := NullFlowStatement;
  Result.CodeOffset := Offset;
  if (Pos <> nil) then
    Result.DocPos := Pos^;
  Result.JumpSafe := JumpSafe;
end;

function EnsureCompound(Compiler: TLapeCompilerBase; Statement: TLapeTree_Base; AllowNil: Boolean = True): TLapeTree_StatementList;
begin
  if (Statement = nil) then
    if AllowNil then
      Result := nil
    else
      Result := TLapeTree_StatementList.Create(Compiler)
  else if (Statement is TLapeTree_StatementList) then
    Result := TLapeTree_StatementList(Statement)
  else
  begin
    Result := TLapeTree_StatementList.Create(Statement);
    Result.Parent := Statement.Parent;
    Result.addStatement(Statement);
  end;
end;

function GetMagicMethodOrNil(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; AResult: TLapeType = nil): TResVar;
var
  Method: TLapeGlobalVar;
begin
  Result := NullResVar;
  if (Compiler = nil) then
    Exit;

  Method := Compiler[AName];
  if (Method <> nil) and (Method.VarType is TLapeType_OverloadedMethod) then
    Method := TLapeType_OverloadedMethod(Method.VarType).getMethod(getTypeArray(AParams), AResult)
  else
    Method := nil;

  if (Method = nil) then
    Method := Compiler.getConstant('nil', ltPointer);

  Result := _ResVar.New(Method);
  Result.VarType := Compiler.getBaseType(ltPointer);
end;

function HasMagicMethod(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; ARes: TLapeType): Boolean;

  function EqualTypes(const Left, Right: TLapeType): Boolean;
  begin
    Result := (Left = Right) or ((Left <> nil) and Left.Equals(Right, False));
  end;

  function EqualParams(const Method: TLapeType_Method): Boolean;
  var
    i: Integer;
  begin
    if (Method.Params.Count <> Length(AParams)) or (not EqualTypes(ARes, Method.Res)) then
      Exit(False);
    for i := 0 to High(AParams) do
      if not EqualTypes(AParams[i], Method.Params[i].VarType) then
        Exit(False);

    Result := True;
  end;

var
  Method: TLapeGlobalVar;
  i: Integer;
begin
  Result := False;

  Method := Compiler[AName];
  if (Method <> nil) and (Method.VarType is TLapeType_OverloadedMethod) then
    with TLapeType_OverloadedMethod(Method.VarType) do
    begin
      for i := 0 to MethodCount - 1 do
        if EqualParams(TLapeType_Method(Methods[i].VarType)) then
          Exit(True);
    end;
end;

constructor TLapeTreeType.Create(ADecl: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase);
begin
  if (ACompiler = nil) and (ADecl <> nil) then
    ACompiler := ADecl.Compiler;
  inherited Create(ltUnknown, ACompiler);
  FDecl := ADecl;
end;

destructor TLapeTreeType.Destroy;
begin
  if (FDecl <> nil) and (FDecl.Parent = nil) then
    FDecl.Free();
  inherited;
end;

function TLapeTreeType.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
end;

function TLapeTreeType.getDecl: TLapeTree_ExprBase;
begin
  Result := FDecl;
  FDecl := nil;
end;

function TLapeTree_Base.getDocPos: TDocPos;
begin
  Result := _DocPos;
end;

procedure TLapeTree_Base.setParent(Node: TLapeTree_Base);
begin
  Assert((Node = nil) or (Node.Compiler = FCompiler));
  if (FParent <> nil) and (FParent <> Node) then
    FParent.DeleteChild(Self);
  FParent := Node;
end;

procedure TLapeTree_Base.DeleteChild(Node: TLapeTree_Base);
begin
  {nothing}
end;

constructor TLapeTree_Base.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create();
  Assert(ACompiler <> nil);

  FParent := nil;
  FCompiler := ACompiler;
  FCompilerOptions := FCompiler.Options;

  if (ADocPos <> nil) then
    _DocPos := ADocPos^
  else
    _DocPos := NullDocPos;
end;

constructor TLapeTree_Base.Create(ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

destructor TLapeTree_Base.Destroy;
begin
  setParent(nil);
  inherited;
end;

class function TLapeTree_Base.isEmpty(Node: TLapeTree_Base): Boolean;
begin
  Result := (Node = nil) or (Node.ClassType = TLapeTree_Base) or (Node.ClassType = TLapeTree_ExprBase);
end;

function TLapeTree_Base.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
begin
  Result := Self;
end;

function TLapeTree_Base.setExpectedType(ExpectType: TLapeType): TLapeTree_Base;
begin
  Result := Self;
end;

function TLapeTree_Base.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeImpossible);
end;

function TLapeTree_Base.Compile: TResVar;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := Compile(Offset);
end;

function TLapeTree_Base.CompileToTempVar(var Offset: Integer; out v: TResVar): Boolean;
begin
  v := Compile(Offset);
  Result := (v.VarPos.MemPos <> mpStack) and v.HasType();
end;

function TLapeTree_Base.isInvoking: Boolean;
begin
  Result := False;
end;

function TLapeTree_Base.Walk(Visit: TLapeTree_Visit): Boolean;
begin
  Result := False;
end;

constructor TLapeTree_ExprBase.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  ClearCache();
end;

procedure TLapeTree_ExprBase.ClearCache;
begin
  FConstant := bUnknown;
  FRes := nil;
  FResType := nil;
end;

function TLapeTree_ExprBase.isConstant: Boolean;
begin
  Result := (FConstant = bTrue);
end;

function TLapeTree_ExprBase.resType: TLapeType;
begin
  Result := FResType;
end;

function TLapeTree_ExprBase.Evaluate: TLapeGlobalVar;
begin
  Result := FRes;
  if (Result = nil) then
    LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_ExprBase.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
var
  Replacement: TLapeTree_ExprBase;
  t: TLapeGlobalVar;
begin
  Result := inherited;

  if (not isEmpty(Self)) and isConstant() then
  begin
    try
      t := Evaluate();
    except
      t := nil;
      ClearCache();
    end;

    if (t <> nil) then
    begin
      if (t.VarType is TLapeTreeType) then
      begin
        Replacement := TLapeTreeType(t.VarType).getDecl();
        FreeAndNil(t.VarType);
        FreeAndNil(t);
      end
      else if (t.VarType is TLapeType_Type) then
        Replacement := TLapeTree_VarType.Create(TLapeType_Type(t.VarType).TType, Self)
      else
        Replacement := TLapeTree_GlobalVar.Create(t, Self);
      Replacement.Parent := FParent;

      FParent := nil;
      if DoFree then
        Free();
      Result := Replacement;
    end;
  end;
end;

procedure TLapeTree_DestExprBase.setDest(ResVar: TResVar);
begin
  FDest := ResVar;
end;

function TLapeTree_DestExprBase.CompileToTempVar(var Offset: Integer; out v: TResVar): Boolean;
var
  tmpDest: TResVar;
begin
  tmpDest := FDest;
  if (FDest.VarPos.MemPos <> VarResVar.VarPos.MemPos) then
    FDest := VarResVar;
  Result := inherited;
  FDest := tmpDest;
end;

procedure TLapeTree_OpenArray.setType(AType: TLapeType);
begin
  if (FType <> AType) then
  begin
    FType := AType;
    ClearCache();
  end;
end;

procedure TLapeTree_OpenArray.DeleteChild(Node: TLapeTree_Base);
begin
  inherited;
  if (FValues <> nil) then
    FValues.DeleteItem(Node);
end;

constructor TLapeTree_OpenArray.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  FType := nil;
  FValues := TLapeStatementList.Create(nil, dupAccept, False);
  FInvalidCastIndex := -1;
end;

destructor TLapeTree_OpenArray.Destroy;
var
  i: Integer;
begin
  for i := FValues.Count - 1 downto 0 do
    if (FValues[i] <> nil) and (FValues[i].Parent = Self) then
      FValues[i].Free();
  FreeAndNil(FValues);
  inherited;
end;

procedure TLapeTree_OpenArray.ClearCache;
begin
  inherited;
  FCanCast := bUnknown;
  FInvalidCastIndex := -1;
  FRange := NullRange;
end;

function TLapeTree_OpenArray.addValue(Val: TLapeTree_Base): Integer;
begin
  if (Val <> nil) then
  begin
    Result := FValues.Add(Val);
    Val.Parent := Self;
    ClearCache();
  end
  else
    Result := -1;
end;

function TLapeTree_OpenArray.setExpectedType(ExpectType: TLapeType): TLapeTree_Base;
var
  OldType: TLapeType;
begin
  Result := Self;
  if isEmpty(Self) then
    Exit;
    
  OldType := ToType;
  if (ExpectType <> nil) then
    ToType := ExpectType
  else
    ToType := resType();

  if (ToType <> OldType) and canCast() then
  begin
    //Reference type to static type
    if (FType.BaseType = ltDynArray) and isConstant() and (FRange.Lo = 0) and (FRange.Hi >= 0) then
      ToType := FCompiler.addManagedType(TLapeType_StaticArray.Create(FRange, TLapeType_DynArray(FType).PType, FCompiler, '', @_DocPos));
    Result := FoldConstants();
  end
  else
    ToType := OldType;
end;

function TLapeTree_OpenArray.canCastTo(AType: TLapeType; Strict: Boolean): Boolean;
var
  i: Integer;
  CastTo: TLapeType;
begin
  Result := False;

  if (AType is TLapeType_DynArray) then
    CastTo := TLapeType_DynArray(AType).PType
  else
  if (AType is TLapeType_Set) then
    CastTo := TLapeType_Set(AType).Element
  else
    CastTo := nil;

  if (CastTo <> nil) or (AType is TLapeType_Record) then
  begin
    for i := 0 to FValues.Count - 1 do
    begin
      if isEmpty(FValues[i]) then
        Continue;

      if (AType is TLapeType_Record) then
      begin
        if Strict and (TLapeType_Record(AType).FieldMap.Count <> FValues.Count) then
          Exit;
        CastTo := TLapeType_Record(AType).FieldMap.ItemsI[i].FieldType;
        if (CastTo = nil) then
          Exit;
      end;

      if (FValues[i] is TLapeTree_OpenArray) and TLapeTree_OpenArray(FValues[i]).canCastTo(CastTo, Strict) then
        Continue;
      if (FValues[i] is TLapeTree_ExprBase) and CastTo.CompatibleWith(TLapeTree_ExprBase(FValues[i]).resType()) then
        Continue;

      Exit;
    end;

    Result := True;
  end;
end;

function TLapeTree_OpenArray.canCast: Boolean;
var
  i: Integer;
  CastTo: TLapeType;
begin
  if (FCanCast = bUnknown) then
  begin
    FCanCast := bTrue;
    if (FType = nil) and FDest.HasType() then
      FType := FDest.VarType;
    if (FType = nil) then
      FType := resType();
    if (FType = nil) then
      FCanCast := bFalse;

    CastTo := nil;

    if (FCanCast <> bFalse) then
      if (FType is TLapeType_Set) then
        CastTo := TLapeType_Set(FType).Element
      else if (FType is TLapeType_DynArray) then
        CastTo := TLapeType_DynArray(FType).PType;
      if (CastTo = nil) xor (FType is TLapeType_Record) then
        FCanCast := bFalse;


    if (FCanCast <> bFalse) then
      for i := 0 to FValues.Count - 1 do
      begin
        if isEmpty(FValues[i]) then
          Continue
        else if (FType is TLapeType_Record) then
          CastTo := TLapeType_Record(FType).FieldMap.ItemsI[i].FieldType;

        if (CastTo <> nil) then
          FValues[i] := FValues[i].setExpectedType(CastTo)
        else
        begin
          FInvalidCastIndex := i;
          FCanCast := bFalse;
          Break;
        end;

        if (FValues[i] is TLapeTree_ExprBase) and CastTo.CompatibleWith(TLapeTree_ExprBase(FValues[i]).resType()) then
          {nothing}
        // (a range only in a set)
        else if (FValues[i] is TLapeTree_Range) and (FType is TLapeType_Set) and
          CastTo.CompatibleWith(TLapeTree_Range(FValues[i]).Lo.resType()) and
          CastTo.CompatibleWith(TLapeTree_Range(FValues[i]).Hi.resType())
        then
          {nothing}
        else
        begin
          FInvalidCastIndex := i;
          FCanCast := bFalse;
          Break;
        end;
      end;

    if (FCanCast <> bFalse) and (FType is TLapeType_StaticArray) and isConstant() and (FRange.Lo = 0) then
      if (FRange.Hi <> TLapeType_StaticArray(FType).Range.Hi - TLapeType_StaticArray(FType).Range.Lo) then
        FCanCast := bFalse;
  end;

  Result := (FCanCast = bTrue);
end;

function TLapeTree_OpenArray.isConstant: Boolean;
var
  i: Integer;
begin
  if (FConstant = bUnknown) then
    if (FType = nil) or (not canCast()) then
      FConstant := bFalse
    else
    begin
      FConstant := bTrue;
      FRange := NullRange;

      for i := 0 to FValues.Count - 1 do
        if isEmpty(FValues[i]) then
          {nothing}
        else if (FValues[i] is TLapeTree_ExprBase) and TLapeTree_ExprBase(FValues[i]).isConstant() then
          Inc(FRange.Hi)
        // (a range is only in a set, which has no length)
        else if (FValues[i] is TLapeTree_Range) and TLapeTree_Range(FValues[i]).isConstant() then
          {nothing}
        else
        begin
          FConstant := bFalse;
          FRange.Lo := -1;
          Break;
        end;
    end;
  Result := inherited;
end;

function TLapeTree_OpenArray.resType: TLapeType;

  function determineArrType(Typ: TLapeType): TLapeType;
  begin
    if (Typ = nil) then
      Result := nil
    else if (Typ.BaseType in LapeIntegerTypes - [ltUInt64, ltInt64]) then
      Result := FCompiler.getBaseType(ltInt32)
    else
      Result := Typ;
  end;

  function determineResType: TLapeType;
  var
    i: Integer;
    Res: TLapeType;
  begin
    Result := nil;
    FRange := NullRange;

    if (FValues.Count > 0) then
    begin
      for i := 0 to FValues.Count - 1 do
      begin
        if isEmpty(FValues[i]) then
          Continue
        else if (FValues[i] is TLapeTree_Range) and (TLapeTree_Range(FValues[i]).Hi <> nil) then
        begin
          // (a range makes it a set: of an enum, else it is no list)
          Res := TLapeTree_Range(FValues[i]).Hi.resType();
          if (Res <> nil) and (Res.BaseType in [ltSmallEnum, ltLargeEnum]) and ((Result = nil) or Res.CompatibleWith(Result)) then
            Exit(FCompiler.addManagedType(TLapeType_Set.Create(Res, FCompiler, '', @_DocPos)));
          FInvalidCastIndex := i;
          Exit(nil);
        end
        else if (not (FValues[i] is TLapeTree_ExprBase)) then
          Exit(nil)
        else
        begin
          Inc(FRange.Hi);
          Res := determineArrType(TLapeTree_ExprBase(FValues[i]).resType());
        end;

        if (Result = nil) then
          Result := Res
        else if (Res <> nil) and Res.Equals(Result) then
          {nothing}
        else if (Res <> nil) and (Res.BaseType > Result.BaseType) and Res.CompatibleWith(Result) then
          Result := Res
        else if (Res <> nil) and (Result.BaseType >= Res.BaseType) and Result.CompatibleWith(Res) then
          {nothing}
        else if FCompiler.getBaseType(ltVariant).CompatibleWith(Res) and FCompiler.getBaseType(ltVariant).CompatibleWith(Result) then
          Result := FCompiler.getBaseType(ltVariant)
        else
        begin
          Result := nil;
          Break;
        end;
      end;

      if (Result <> nil) then
        if (Result is TLapeType_Enum) and (Result.BaseType in [ltSmallEnum, ltLargeEnum]) and TLapeType_Enum(Result).canSet() then
          Result := FCompiler.addManagedType(TLapeType_Set.Create(Result, FCompiler, '', @_DocPos))
        else if (FRange.Lo < 0) then
          Result := FCompiler.addManagedType(TLapeType_DynArray.Create(Result, FCompiler, '', @_DocPos))
        else
          Result := FCompiler.addManagedType(TLapeType_StaticArray.Create(FRange, Result, FCompiler, '', @_DocPos));
    end
    else
      Result := FCompiler.addManagedType(TLapeType_DynArray.Create(FCompiler.getBaseType(ltVariant), FCompiler, '', @_DocPos));
  end;

begin
  if (FResType = nil) then
    if (FType <> nil) then
      FResType := FType
    else
      FResType := determineResType();
  Result := inherited;
end;

function TLapeTree_OpenArray.Evaluate: TLapeGlobalVar;

  procedure doSetOrDynArray;
  var
    i, ii: Integer;
    FieldVar, tmpVar: TLapeGlobalVar;
  begin
    FieldVar := nil;
    tmpVar := nil;

    for i := 0 to FValues.Count - 1 do
      if isEmpty(FValues[i]) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (FValues[i] is TLapeTree_ExprBase) then
      begin
        tmpVar := Result;
        Result := FType.EvalConst(op_Plus, tmpVar, TLapeTree_ExprBase(FValues[i]).Evaluate(), []);
        tmpVar.Free();
      end
      else if (FValues[i] is TLapeTree_Range) then
      try
        FieldVar := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@ii);
        for ii := TLapeTree_Range(FValues[i]).Lo.Evaluate().AsInteger to TLapeTree_Range(FValues[i]).Hi.Evaluate().AsInteger do
        begin
          tmpVar := Result;
          Result := FType.EvalConst(op_Plus, Result, FieldVar, []);
          tmpVar.Free();
        end;
      finally
        FieldVar.Free();
      end
      else
        LapeException(lpeInvalidCast, DocPos);
  end;

  procedure doStaticArray;
  var
    i, CounterInt: Integer;
    FieldVar, Counter: TLapeGlobalVar;
  begin
    FieldVar := nil;
    try
      CounterInt := TLapeType_StaticArray(FType).Range.Lo;
      Counter := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@CounterInt);

      for i := 0 to FValues.Count - 1 do
      begin
        if isEmpty(FValues[i]) then
          LapeException(lpeInvalidCast, FValues[i].DocPos)
        else if (FValues[i] is TLapeTree_ExprBase) then
        try
          try
            FieldVar := FType.EvalConst(op_Index, Result, Counter, [lefAssigning]);
            FieldVar.VarType.EvalConst(op_Assign, FieldVar, TLapeTree_ExprBase(FValues[i]).Evaluate(), []);
          except on E: lpException do
            LapeException(lpString(E.Message), FValues[i].DocPos);
          end;
        finally
          if (FieldVar <> nil) then
            FreeAndNil(FieldVar);
        end
        else
          LapeException(lpeInvalidCast, FValues[i].DocPos);
        Inc(CounterInt);
      end;

      if (CounterInt <> TLapeType_StaticArray(FType).Range.Hi + 1) then
        LapeException(lpeInvalidRange, DocPos);
    finally
      Counter.Free();
    end
  end;

  procedure doRecord;
  var
    i: Integer;
    FieldVar, tmpVar: TLapeGlobalVar;
  begin
    FieldVar := nil;
    tmpVar := nil;

    if (FValues.Count > TLapeType_Record(FType).FieldMap.Count) then
      LapeException(lpeInvalidRange, DocPos);
    for i := 0 to FValues.Count - 1 do
      if (FValues[i] is TLapeTree_ExprBase) then
      try
        tmpVar := FCompiler.getConstant(TLapeType_Record(FType).FieldMap.Key[i], ltString);
        try
          FieldVar := FType.EvalConst(op_Dot, Result, tmpVar, [lefAssigning]);
          FieldVar.VarType.EvalConst(op_Assign, FieldVar, TLapeTree_ExprBase(FValues[i]).Evaluate(), []);
        except on E: lpException do
          LapeException(lpString(E.Message), FValues[i].DocPos);
        end;
      finally
        if (FieldVar <> nil) then
          FreeAndNil(FieldVar);
      end
      else if (not isEmpty(FValues[i])) then
        LapeException(lpeInvalidCast, FValues[i].DocPos);
  end;

begin
  if (FRes = nil) then
  begin
    if (not canCast()) then
      LapeException(lpeInvalidEvaluation, DocPos);

    Result := FType.NewGlobalVarP();
    try
      if (FType is TLapeType_StaticArray) then
        doStaticArray()
      else if (FType is TLapeType_Set) or (FType is TLapeType_DynArray) then
        doSetOrDynArray()
      else if (FType is TLapeType_Record) then
        doRecord()
      else
        LapeException(lpeInvalidCast, DocPos);

      Result := FCompiler.addManagedDecl(Result) as TLapeGlobalVar;
      Result.isConstant := True;

      FRes := Result;
    except
      Result.Free();
      raise;
    end;
  end;
  Result := inherited;
end;

function TLapeTree_OpenArray.Compile(var Offset: Integer): TResVar;

  procedure doSetOrDynArray;
  var
    i: Integer;
    tmpVar, HiVar: TResVar;
    Counter: TLapeVar;
  begin
    tmpVar := NullResVar;
    Counter := nil;

    for i := 0 to FValues.Count - 1 do
      if isEmpty(FValues[i]) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (FValues[i] is TLapeTree_ExprBase) then
        with TLapeTree_Operator.Create(op_Plus, FValues[i]) do
        try
          Dest := Result;
          Left := TLapeTree_ResVar.Create(Result, FValues[i]);
          Right := TLapeTree_ResVar.Create(FValues[i].Compile(Offset), FValues[i]);
          Result := Compile(Offset);
          Assert(Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos);
        finally
          Free();
        end
      else if (FValues[i] is TLapeTree_Range) then
      begin
        Assert(FType is TLapeType_Set);
        Counter := FCompiler.getTempVar(TLapeType_Set(FType).Element);
        tmpVar := Counter.VarType.Eval(op_Assign, tmpVar, _ResVar.New(Counter), TLapeTree_Range(FValues[i]).Lo.Compile(Offset), [], Offset, @FValues[i]._DocPos);
        // (the last is kept in a variable: the loop reads it each round)
        if (not TLapeTree_Range(FValues[i]).Hi.CompileToTempVar(Offset, HiVar)) then
          LapeException(lpeInvalidEvaluation, FValues[i].DocPos);

        with TLapeTree_For.Create(FValues[i]) do
        try
          Counter := TLapeTree_ResVar.Create(tmpVar, FValues[i]);
          Limit := TLapeTree_ResVar.Create(HiVar, FValues[i]);
          Body := TLapeTree_Operator.Create(op_Plus, FValues[i]);
          with TLapeTree_Operator(Body) do
          begin
            Dest := Result;
            Left := TLapeTree_ResVar.Create(Result, FValues[i]);
            Right := TLapeTree_ResVar.Create(tmpVar, FValues[i]);
          end;
          Result := Compile(Offset);
          Assert(TLapeTree_Operator(Body).Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos);
        finally
          Free();
        end;
      end
      else
        LapeException(lpeInvalidCast, FValues[i].DocPos);
  end;

  procedure doStaticArray;
  var
    i: Integer;
  begin
    if (FValues.Count <> TLapeType_StaticArray(FType).Range.Hi - TLapeType_StaticArray(FType).Range.Lo + 1) then
      LapeException(lpeInvalidRange, DocPos);

    for i := 0 to FValues.Count - 1 do
      if (not (FValues[i] is TLapeTree_ExprBase)) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else
        with TLapeTree_Operator.Create(op_Assign, FValues[i]) do
        try
          Left := TLapeTree_Operator.Create(op_Index, FValues[i]);
          CompilerOptions := CompilerOptions - [lcoRangeCheck];

          with TLapeTree_Operator(Left) do
          begin
            Left := TLapeTree_ResVar.Create(Result, FValues[i]);
            Right := TLapeTree_Integer.Create(i + TLapeType_StaticArray(FType).Range.Lo, FValues[i]);
          end;
          Right := TLapeTree_ResVar.Create(FValues[i].Compile(Offset), FValues[i]);
          Compile(Offset);
        finally
          Free();
        end;
  end;

  procedure doRecord;
  var
    i: Integer;
    Val: TResVar;
  begin
    if (FValues.Count > TLapeType_Record(FType).FieldMap.Count) then
      LapeException(lpeInvalidRange, DocPos);

    for i := 0 to FValues.Count - 1 do
      if (FValues[i] is TLapeTree_ExprBase) then
        with TLapeTree_Operator.Create(op_Assign, FValues[i]) do
        try
          Left := TLapeTree_Operator.Create(op_Dot, FValues[i]);
          with TLapeTree_Operator(Left) do
          begin
            Left := TLapeTree_ResVar.Create(Result, FValues[i]);
            Right := TLapeTree_Field.Create(TLapeType_Record(FType).FieldMap.Key[i], FValues[i]);
          end;
          Right := TLapeTree_ResVar.Create(FValues[i].Compile(Offset), FValues[i]);
          Compile(Offset);
        finally
          Free();
        end
      else if (not isEmpty(FValues[i])) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (lcoAlwaysInitialize in FCompilerOptions) then
        with TLapeTree_Operator.Create(op_Dot, Self) do
        try
          Left := TLapeTree_ResVar.Create(Result, Self);
          Right := TLapeTree_Field.Create(TLapeType_Record(FType).FieldMap.Key[i], Self);
          Val := Compile(Offset);

          FCompiler.VarToDefault(Val, Offset, @_DocPos);
        finally
          Free();
        end;
  end;

begin
  Result := NullResVar;

  if (not canCast()) then
  begin
    if (FInvalidCastIndex > -1) and (FValues[FInvalidCastIndex] is TLapeTree_Range) and (not (FType is TLapeType_Set)) then
      LapeException(lpeRangeOnlyInSet, FValues[FInvalidCastIndex].DocPos)
    else if (FInvalidCastIndex > -1) and (FValues[FInvalidCastIndex] is TLapeTree_ExprBase) then
    begin
      with FValues[FInvalidCastIndex] as TLapeTree_ExprBase do
      begin
        if resType() <> nil then
          LapeExceptionFmt(lpeInvalidOpenArrayElement, [resType().AsString, FInvalidCastIndex], DocPos)
        else
          LapeExceptionFmt(lpeUnknownDeclarationOpenArray, [FInvalidCastIndex], DocPos);
      end;
    end else
      LapeException(lpeInvalidEvaluation, DocPos);
  end;

  Dest := NullResVar;
  Result := _ResVar.New(FCompiler.getTempVar(FType));

  if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    FCompiler.VarToDefault(Result, Offset, @_DocPos);

  Exclude(FCompilerOptions, lcoRangeCheck);
  if (FType is TLapeType_StaticArray) then
    doStaticArray()
  else if (FType is TLapeType_Set) or (FType is TLapeType_DynArray) then
    doSetOrDynArray()
  else if (FType is TLapeType_Record) then
    doRecord()
  else
    LapeException(lpeInvalidCast, DocPos);

  Result.isConstant := True;
end;

function TLapeTree_OpenArray.isInvoking: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FValues.Count - 1 do
    if (FValues[i] <> nil) and FValues[i].isInvoking() then
      Exit(True);
end;

procedure TLapeTree_Cast.DeleteChild(Node: TLapeTree_Base);
begin
  if (Node = FCastTo) then
    FCastTo := nil
  else
  if (Node = FParam) then
    FParam := nil;

  ClearCache();
end;

procedure TLapeTree_Cast.setCastTo(Node: TLapeTree_ExprBase);
begin
  if (FCastTo <> nil) and (FCastTo <> Node) then
  begin
    ClearCache();
    FCastTo.Free();
  end;

  FCastTo := Node;
  if (FCastTo <> nil) then
    FCastTo.Parent := Self;
end;

procedure TLapeTree_Cast.setParam(Node: TLapeTree_ExprBase);
begin
  if (FParam <> nil) and (FParam <> Node) then
  begin
    ClearCache();
    FParam.Free();
  end;

  FParam := Node;
  if (FParam <> nil) then
    FParam.Parent := Self;
end;

destructor TLapeTree_Cast.Destroy;
begin
  if (FParam <> nil) and (FParam.Parent = Self) then
    FreeAndNil(FParam);
  if (FCastTo <> nil) and (FCastTo.Parent = Self) then
    FreeAndNil(FCastTo);

  inherited Destroy();
end;

function TLapeTree_Cast.Compile(var Offset: Integer): TResVar;
var
  tmpVar, DestVar: TResVar;
begin
  if not (FCastTo is TLapeTree_VarType) then
    LapeException(lpeImpossible, DocPos);

  Result := NullResVar;
  tmpVar := NullResVar;
  DestVar := NullResVar;

  with TLapeTree_VarType(FCastTo) do
  try
    if (VarType = nil) or isEmpty(FParam) then
      LapeException(lpeInvalidCast);

    if (FParam is TLapeTree_DestExprBase) then
      TLapeTree_DestExprBase(FParam).Dest := Dest;
    FParam := FParam.setExpectedType(VarType) as TLapeTree_ExprBase;

    Result := FParam.Compile(Offset);
    if (not Result.HasType()) or
       ((VarType.Equals(Result.VarType) or (VarType.Size = Result.VarType.Size)) and
       ((not (VarType.BaseType in LapeRefTypes)) or (not VarType.CompatibleWith(Result.VarType))))
    then
    begin
      if (not (FParam is TLapeTree_DestExprBase)) or (TLapeTree_DestExprBase(FParam).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
        Dest := NullResVar;
      Result.VarType := VarType;
    end
    else if VarType.CompatibleWith(Result.VarType) or ((VarType.BaseType in LapeOrdinalTypes) and (Result.VarType.BaseType in LapeOrdinalTypes)) then
    begin
      if (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and FDest.HasType() and VarType.Equals(FDest.VarType) then
        DestVar := FDest
      else
      begin
        Dest := NullResVar;
        DestVar := _ResVar.New(Compiler.getTempVar(VarType));
      end;

      if (not VarType.CompatibleWith(Result.VarType)) then
      begin
        Result.VarType := FCompiler.getBaseType(DetermineIntType(Result.VarType.Size, Result.VarType.BaseType in LapeSignedOrdinalTypes));
        DestVar.VarType := FCompiler.getBaseType(DetermineIntType(DestVar.VarType.Size, DestVar.VarType.BaseType in LapeSignedOrdinalTypes));
      end;

      Result := DestVar.VarType.Eval(op_Assign, tmpVar, DestVar, Result, [], Offset, @Self._DocPos);

      Result.VarType := VarType;
      if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
        Result.isConstant := True;
    end
    else
      LapeException(lpeInvalidCast);
  except on E: lpException do
    LapeException(lpString(E.Message), Self.DocPos);
  end;
end;

function TLapeTree_Cast.Evaluate: TLapeGlobalVar;
var
  tmpRes: TLapeGlobalVar;
  tmpTyp: TLapeType;
begin
  Result := nil;
  if not (FCastTo is TLapeTree_VarType) then
    LapeException(lpeImpossible, DocPos);

  with TLapeTree_VarType(FCastTo) do
  try
    if (VarType = nil) or isEmpty(FParam) then
      LapeException(lpeInvalidCast);
    FParam := FParam.setExpectedType(VarType) as TLapeTree_ExprBase;

    Result := FParam.Evaluate();
    if (not Result.HasType()) or
       ((VarType.Equals(Result.VarType) or (VarType.Size = Result.VarType.Size)) and
       ((not (VarType.BaseType in LapeRefTypes)) or (not VarType.CompatibleWith(Result.VarType))))
    then
      Result := TLapeGlobalVar(FCompiler.addManagedDecl(VarType.NewGlobalVarP(Result.Ptr)))
    else if VarType.CompatibleWith(Result.VarType) then
      Result := TLapeGlobalVar(FCompiler.addManagedDecl(VarType.EvalConst(op_Assign, VarType.NewGlobalVarP(), Result, [])))
    else if (VarType.BaseType in LapeOrdinalTypes) and (Result.VarType.BaseType in LapeOrdinalTypes) then
    try
      tmpRes := Result;
      tmpTyp := tmpRes.VarType;
      Result := VarType.NewGlobalVarP();

      tmpRes.VarType := FCompiler.getBaseType(DetermineIntType(tmpTyp.Size, tmpTyp.BaseType in LapeSignedOrdinalTypes));
      Result.VarType := FCompiler.getBaseType(DetermineIntType(VarType.Size, VarType.BaseType in LapeSignedOrdinalTypes));

      Result := Result.VarType.EvalConst(op_Assign, Result, tmpRes, []);
      Result.VarType := VarType;

      Result := TLapeGlobalVar(FCompiler.addManagedDecl(Result));
    finally
      tmpRes.VarType := tmpTyp;
    end
    else
      LapeException(lpeInvalidCast);
  except
    on E: lpException do
      LapeException(lpString(E.Message), Self.DocPos);
  end;
end;

function TLapeTree_Cast.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (FCastTo is TLapeTree_VarType) then
    begin
      if (not isEmpty(FParam)) then
        FParam := FParam.setExpectedType(TLapeTree_VarType(FCastTo).VarType) as TLapeTree_ExprBase;
      if (not isEmpty(FParam)) and FParam.isConstant() then
        FConstant := bTrue
      else
        FConstant := bFalse
    end
    else
      FConstant := bFalse;

  Result := inherited;
end;

function TLapeTree_Cast.resType: TLapeType;
begin
  if (FCastTo is TLapeTree_VarType) then
    FResType := TLapeTree_VarType(FCastTo).VarType;

  Result := inherited;
end;

function TLapeTree_Cast.isInvoking: Boolean;
begin
  Result := (FParam <> nil) and FParam.isInvoking();
end;

function TLapeTree_Operator.getLeft: TLapeTree_ExprBase;
begin
  if (FLeft is TLapeTree_ExprBase) then
    Result := TLapeTree_ExprBase(FLeft)
  else
    Result := nil;
end;

procedure TLapeTree_Operator.setLeft(Node: TLapeTree_ExprBase);
begin
  if (FLeft <> nil) and (FLeft <> Node) then
  begin
    ClearCache();
    FLeft.Free();
  end;
  FLeft := Node;
  if (Node <> nil) then
  begin
    Node.Parent := Self;
    if (FRestructure = bUnknown) and (not (Node is TLapeTree_ExprBase)) then
      FRestructure := bFalse;
  end;
end;

procedure TLapeTree_Operator.setRight(Node: TLapeTree_ExprBase);
begin
  if (FRight <> nil) and (FRight <> Node) then
  begin
    ClearCache();
    FRight.Free();
  end;
  FRight := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Operator.setParent(Node: TLapeTree_Base);
begin
  if (Node <> FParent) then
    ClearCache();
  inherited;
end;

procedure TLapeTree_Operator.DeleteChild(Node: TLapeTree_Base);
begin
  if (Left = Node) then
    FLeft := nil
  else if (Right = Node) then
    FRight := nil;
  ClearCache();
end;

constructor TLapeTree_Operator.Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  FLeft := nil;
  FRight := nil;
  FOperatorType := AOperatorType;
  FOverloadOp := False;
end;

constructor TLapeTree_Operator.Create(AOperatorType: EOperator; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AOperatorType, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

destructor TLapeTree_Operator.Destroy;
begin
  setLeft(nil);
  setRight(nil);
  inherited;
end;

procedure TLapeTree_Operator.ClearCache;
begin
  FRestructure := bUnknown;
  FAssigning := bUnknown;
  FInvoked := bUnknown;
  inherited;
end;

function TLapeTree_Operator.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
begin
  if (not isEmpty(Self)) and (not isEmpty(Left)) then
  begin
    FLeft := FLeft.FoldConstants() as TLapeTree_ExprBase;
    if (not isEmpty(Right)) then
      FRight := FRight.setExpectedType(Left.resType()).FoldConstants() as TLapeTree_ExprBase;
  end;

  Result := inherited;
end;

function TLapeTree_Operator.setExpectedType(ExpectType: TLapeType): TLapeTree_Base;
var
  Typ: TLapeType;
  Cast: TLapeTree_Cast;
begin
  Result := Self;
  if (not isEmpty(Self)) and (ExpectType <> nil) then
  begin
    if (not isEmpty(Left)) then
      if (OperatorType = op_Deref) then
      begin
        Typ := Left.resType();
        if (Typ <> nil) and (Typ is TLapeType_Pointer) and (not TLapeType_Pointer(Typ).HasType()) then
        begin
          Cast := TLapeTree_Cast.Create(FCompiler);
          Cast.CastTo := TLapeTree_VarType.Create(FCompiler.getPointerType(ExpectType, False), Self);
          Cast.Param := Left;
          Left := Cast;
        end;
      end
      else
      begin
        FLeft := FLeft.setExpectedType(ExpectType) as TLapeTree_ExprBase;
        if (not isEmpty(Right)) then
          FRight := FRight.setExpectedType(Left.resType()) as TLapeTree_ExprBase;
      end;

    if (not isEmpty(Right)) then
      FRight := FRight.setExpectedType(ExpectType) as TLapeTree_ExprBase;
  end;
end;

function TLapeTree_Operator.isAssigning: Boolean;
begin
  if (FAssigning = bUnknown) then
  begin
    Result := (not isEmpty(FParent)) and (FParent is TLapeTree_Operator) and (
      (
        (TLapeTree_Operator(FParent).FLeft = Self) and
        (TLapeTree_Operator(FParent).OperatorType in AssignOperators)
      ) or (
        (TLapeTree_Operator(FParent).FLeft <> Self) and
        TLapeTree_Operator(FParent).isAssigning()
      )
    );

    if Result then
      FAssigning := bTrue
    else
      FAssigning := bFalse;
  end;

  Result := (FAssigning = bTrue);
end;

function TLapeTree_Operator.isInvoked: Boolean;
begin
  if (FInvoked = bUnknown) and (not isEmpty(FParent)) then
  begin
    Result := (
      ((FParent is TLapeTree_Invoke) and (TLapeTree_Invoke(FParent).Callee = Self)) or
      ((FParent is TLapeTree_Operator) and TLapeTree_Operator(FParent).isInvoked())
    );

    if Result then
      FInvoked := bTrue
    else
      FInvoked := bFalse;
  end;

  Result := (FInvoked = bTrue);
end;

// (an operator that a method of the script does is a call of that method)
function TLapeTree_Operator.isInvoking: Boolean;
begin
  Result := FOverloadOp or ((FLeft <> nil) and FLeft.isInvoking()) or ((FRight <> nil) and FRight.isInvoking());
end;

function TLapeTree_Operator.RightFirst: Boolean;
begin
  Result := (FOperatorType in AssignOperators) and (FLeft <> nil) and (FRight <> nil) and FRight.isInvoking();
end;

function TLapeTree_Operator.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
  begin
    if isEmpty(Left) then
      Result := False
    else if (FLeft is TLapeTree_GlobalVar) and
      TLapeTree_GlobalVar(FLeft).GlobalVar.HasType() and
      ((FRight = nil) or (FRight is TLapeTree_GlobalVar))
    then
      if (FRight <> nil) then
        Result := TLapeTree_GlobalVar(FLeft).GlobalVar.VarType.CanEvalConst(
          OperatorType,
          TLapeTree_GlobalVar(FLeft).GlobalVar,
          TLapeTree_GlobalVar(FRight).GlobalVar)
      else
        Result := TLapeTree_GlobalVar(FLeft).GlobalVar.VarType.CanEvalConst(
          OperatorType,
          TLapeTree_GlobalVar(FLeft).GlobalVar,
          nil)
    else
      Result := Left.isConstant() and ((FRight = nil) or FRight.isConstant());

    if Result then
      FConstant := bTrue
    else
      FConstant := bFalse;
  end;
  Result := inherited;
end;

function TLapeTree_Operator.EvalFlags: ELapeEvalFlags;
begin
  Result := [lefConstRangeCheck];
  if (lcoRangeCheck in FCompilerOptions) then
    Include(Result, lefRangeCheck);
  if (lcoConstAddress in FCompilerOptions) then
    Include(Result, lefConstAddress);
  if isAssigning() then
    Include(Result, lefAssigning);
  if isInvoked() then
    Include(Result, lefInvoking);
end;

// Restucture is for short-circuit where it's turned into if statements.
// from:
//   if (a=1) or (b=2) or (c=3) then
// becomes:
//   if (a=1) then
//     if (b=2) then
//       if (c=3) then
//
// Which could be improved to not have so many jumps
function TLapeTree_Operator.resType(Restructure: Boolean): TLapeType;
var
  LeftType, RightType: TLapeType;
  tmpLeft: TLapeTree_ExprBase;
  ResVar: TResVar;
begin
  if (FResType = nil) then
  try
    if (FLeft <> nil) then
      if (FLeft is TLapeTree_If) then
        Exit(FCompiler.getBaseType(ltEvalBool))
      else
        LeftType := TLapeTree_ExprBase(FLeft).resType()
    else
      LeftType := nil;

    if (FRight <> nil) then
    begin
      FRight := FRight.setExpectedType(LeftType) as TLapeTree_ExprBase;
      RightType := FRight.resType();
    end
    else
      RightType := nil;

    if (LeftType = nil) then
      LeftType := FCompiler.addManagedType(TLapeType.Create(ltUnknown, FCompiler));

    if (FRight is TLapeTree_OpenArray) and (FOperatorType = op_IN) then
    begin
      // see if we have an op overload first, before falling back to a set
      if HasOperatorOverload(FCompiler, op_In, LeftType, RightType, Result) then
        FOverloadOp := True
      else
        Result := FCompiler.getBaseType(ltEvalBool)
    end
    else if (FRight is TLapeTree_GlobalVar) then
      Result := LeftType.EvalRes(FOperatorType, TLapeTree_GlobalVar(FRight).GlobalVar, EvalFlags())
    else if (LeftType <> nil) then
      Result := LeftType.EvalRes(FOperatorType, RightType, EvalFlags());

    if (LeftType <> nil) and (FRight <> nil) and
       (lcoShortCircuit in FCompilerOptions) and (Result <> nil) and (Result.BaseType in LapeBoolTypes) and (FOperatorType in [op_AND, op_OR]) and
       (LeftType.BaseType in LapeBoolTypes) and (RightType <> nil) and (RightType.BaseType in LapeBoolTypes)
    then
    begin
      FRestructure := bTrue;
      if (Result = nil) then
        Result := FCompiler.getBaseType(ltEvalBool);
    end
    else
      FRestructure := bFalse;
  finally
    FResType := Result;
  end;

  Result := inherited resType();

  // check for op overload
  if (FResType = nil) and HasOperatorOverload(FCompiler, FOperatorType, LeftType, RightType, Result) then
  begin
    FResType := Result;
    FOverloadOp := True;
  end;

  if Restructure and (FRestructure = bTrue) then
  begin
    FRestructure := bFalse;

    tmpLeft := FLeft.FoldConstants() as TLapeTree_ExprBase;
    FLeft := TLapeTree_If.Create(Self);

    with TLapeTree_If(FLeft) do
    begin
      Condition := tmpLeft;

      ResVar := FCompiler.getTempStackVar(Result);
      ResVar.VarPos.ForceVariable := True;

      Body := TLapeTree_Operator.Create(op_Assign, Self);
      with TLapeTree_Operator(Body) do
      begin
        Left := TLapeTree_ResVar.Create(ResVar, Self);
        if (Self.FOperatorType = op_AND) then
          Right := Self.FRight.FoldConstants(False) as TLapeTree_ExprBase
        else
          Right := TLapeTree_GlobalVar.Create('True', ResVar.VarType.BaseType, Self);
      end;

      ElseBody := TLapeTree_Operator.Create(op_Assign, Self);
      with TLapeTree_Operator(ElseBody) do
      begin
        Left := TLapeTree_ResVar.Create(ResVar, Self);
        if (Self.FOperatorType = op_OR) then
          Right := Self.FRight.FoldConstants(False) as TLapeTree_ExprBase
        else
          Right := TLapeTree_GlobalVar.Create('False', ResVar.VarType.BaseType, Self);
      end;
    end;
    setRight(nil);
  end;
end;

function TLapeTree_Operator.resType: TLapeType;
begin
  Result := resType(False);
end;

function TLapeTree_Operator.Evaluate: TLapeGlobalVar;
var
  LeftVar, RightVar: TLapeGlobalVar;
  Short: Boolean;

  function canShort(Typ: TLapeType): Boolean;
  begin
    Result := (lcoShortCircuit in FCompilerOptions) and (Typ <> nil) and (Typ.BaseType in LapeBoolTypes);
  end;

begin
  if (FRes = nil) then
  begin
    if (FLeft <> nil) then
      if (FLeft is TLapeTree_ExprBase) then
        LeftVar := TLapeTree_ExprBase(FLeft).Evaluate()
      else
        LapeException(lpeInvalidEvaluation, DocPos)
    else
      LeftVar := nil;

    if (FOperatorType in AssignOperators) and ((LeftVar = nil) or (not LeftVar.Writeable)) then
      LapeException(lpeCannotAssign, [FLeft, Self]);

    if (LeftVar <> nil) and (not isEmpty(FRight)) then
    begin
      FRight := FRight.setExpectedType(LeftVar.VarType) as TLapeTree_ExprBase;
      if (FOperatorType = op_Index) then
        FRight := FRight.setExpectedType(FCompiler.getBaseType(ltSizeInt)) as TLapeTree_ExprBase;
    end;

    Short := (FOperatorType in [op_AND, op_OR]) and (LeftVar <> nil) and (FRight <> nil) and canShort(LeftVar.VarType) and canShort(FRight.resType());
    if Short and ((FOperatorType = op_AND) xor (LeftVar.AsInteger <> 0)) then
      Exit(LeftVar);

    if (FRight <> nil) then
      RightVar := FRight.Evaluate()
    else
      RightVar := nil;

    if Short and (RightVar <> nil) then
      Exit(RightVar);

    try
      if (LeftVar <> nil) and LeftVar.HasType() then
        FRes := LeftVar.VarType.EvalConst(FOperatorType, LeftVar, RightVar, EvalFlags())
      else with TLapeType.Create(ltUnknown, FCompiler) do
      try
        FRes := EvalConst(FOperatorType, LeftVar, RightVar, EvalFlags());
      finally
        Free();
      end;

      FRes := FCompiler.addManagedDecl(FRes) as TLapeGlobalVar;
    except on E: lpException do
      LapeException(lpString(E.Message), DocPos);
    end;
  end;
  Result := inherited;
end;

function TLapeTree_Operator.Compile(var Offset: Integer): TResVar;

  function doIf: TResVar;
  var
    wasConstant: Boolean;
  begin
    with TLapeTree_If(FLeft) do
    begin
      Result := NullResVar;
      Result.VarType := TLapeTree_Operator(Body).resType();
      FCompiler.getDestVar(FDest, Result, op_Unknown);

      wasConstant := not Result.Writeable;
      if wasConstant then
        Result.Writeable := True;

      if (Result.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
      begin
        TLapeTree_Operator(Body).Left := TLapeTree_ResVar.Create(Result, Self);
        TLapeTree_Operator(ElseBody).Left := TLapeTree_ResVar.Create(Result, Self);
      end;

      Result := Compile(Offset);
      if wasConstant then
        Result.Writeable := False;
    end;
  end;

  function doOperatorOverload(): TResVar;
  begin
    Dest := NullResVar;
    with TLapeTree_Invoke.Create('!op_' + op_name[FOperatorType], FCompiler, @_DocPos) do
    try
      addParam(Left);
      addParam(Right);
      Result := Compile(Offset);
    finally
      Right := Params[1];
      Left := Params[0];
      Free();
    end;
  end;

  // The list made a set of Element, when Element can be in one and the values fit it
  function inSet(List: TLapeTree_OpenArray; Element: TLapeType): Boolean;
  begin
    Result := (Element <> nil) and
      ((Element.BaseType in [ltAnsiChar, ltUInt8]) or
       ((Element.BaseType in [ltSmallEnum, ltLargeEnum]) and (Element is TLapeType_SubRange) and
        (TLapeType_SubRange(Element).Range.Lo >= Low(TLapeLargeSetItem)) and (TLapeType_SubRange(Element).Range.Hi <= High(TLapeLargeSetItem))));
    if Result then
    begin
      List.ToType := FCompiler.addManagedType(TLapeType_Set.Create(Element, FCompiler));
      Result := List.canCast();
    end;
  end;

  // x in [a, b..c] as a loop over the values:
  //   Found := False; if (x = a) then Found := True; for i := b to c do if (x = i) then Found := True;
  function doInList(LeftVar: TResVar): TResVar;
  var
    FoundVar: TResVar;

    // (the same variable, read as an integer of its size: a range is counted in numbers. No range of
    // Booleans: True of a LongBool is no 1)
    function AsInteger(AVar: TResVar): TResVar;
    begin
      if (not AVar.HasType()) or (not (AVar.VarType.BaseType in LapeOrdinalTypes)) or (AVar.VarType.BaseType in LapeBoolTypes) then
        LapeException(lpeInvalidRange, DocPos);
      Result := AVar;
      Result.VarType := FCompiler.getBaseType(DetermineIntType(AVar.VarType.Size, AVar.VarType.BaseType in LapeSignedOrdinalTypes));
    end;

    function CompileValue(Value: TLapeTree_Base): TResVar;
    begin
      if (not Value.CompileToTempVar(Offset, Result)) then
        LapeException(lpeInvalidEvaluation, Value.DocPos);
    end;

    // if (Left = Right) then Found := True
    function IfFound(Left, Right: TResVar): TLapeTree_If;
    var
      Condition, SetFound: TLapeTree_Operator;
    begin
      Result := TLapeTree_If.Create(Self);
      Condition := TLapeTree_Operator.Create(op_cmp_Equal, Self);
      Result.Condition := Condition;
      Condition.Left := TLapeTree_ResVar.Create(Left, Self);
      Condition.Right := TLapeTree_ResVar.Create(Right, Self);

      SetFound := TLapeTree_Operator.Create(op_Assign, Self);
      Result.Body := SetFound;
      SetFound.Left := TLapeTree_ResVar.Create(FoundVar, Self);
      SetFound.Right := TLapeTree_GlobalVar.Create(FCompiler.getConstant(1, ltEvalBool), Self);
    end;

  var
    i: Integer;
    Value: TLapeTree_Base;
    HiVar, CounterVar, tmpVar: TResVar;
    Statement: TLapeTree_Base;
  begin
    Dest := NullResVar;

    FoundVar := _ResVar.New(FCompiler.getTempVar(ltEvalBool));
    tmpVar := NullResVar;
    FoundVar.VarType.Eval(op_Assign, tmpVar, FoundVar, _ResVar.New(FCompiler.getConstant(0, ltEvalBool)), [], Offset, @_DocPos);

    for i := 0 to TLapeTree_OpenArray(FRight).Values.Count - 1 do
    begin
      Value := TLapeTree_OpenArray(FRight).Values[i];
      if (Value is TLapeTree_Range) then
      begin
        CounterVar := _ResVar.New(FCompiler.getTempVar(ltInt64));
        tmpVar := NullResVar;
        CounterVar.VarType.Eval(op_Assign, tmpVar, CounterVar, AsInteger(CompileValue(TLapeTree_Range(Value).Lo)), [], Offset, @Value._DocPos);
        HiVar := AsInteger(CompileValue(TLapeTree_Range(Value).Hi));

        Statement := TLapeTree_For.Create(Self);
        try
          TLapeTree_For(Statement).Counter := TLapeTree_ResVar.Create(CounterVar, Self);
          TLapeTree_For(Statement).Limit := TLapeTree_ResVar.Create(HiVar, Self);
          TLapeTree_For(Statement).Body := IfFound(AsInteger(LeftVar), CounterVar);
          Statement.Compile(Offset);
        finally
          Statement.Free();
        end;
      end
      else if (Value is TLapeTree_ExprBase) then
      begin
        Statement := IfFound(LeftVar, CompileValue(Value));
        try
          Statement.Compile(Offset);
        finally
          Statement.Free();
        end;
      end
      else
        LapeException(lpeInvalidSet, Value.DocPos);
    end;

    Result := FoundVar;
  end;

var
  LeftVar, RightVar: TResVar;

  procedure CompileLeft;
  begin
    if (FLeft <> nil) then
      LeftVar := TLapeTree_ExprBase(FLeft).Compile(Offset);

    if (FLeft <> nil) and (not LeftVar.HasType()) then
      LeftVar.VarType := FCompiler.addManagedType(TLapeType.Create(ltUnknown, FCompiler));

    if (FOperatorType in AssignOperators) and (not LeftVar.Writeable) then
      LapeException(lpeCannotAssign, [FLeft, Self]);
  end;

  procedure ExpectRight(LeftType: TLapeType);
  begin
    case FOperatorType of
      // a list here is a set already (see inSet below)
      op_IN:
        if (Right is TLapeTree_OpenArray) then
          FRight := FRight.FoldConstants(True) as TLapeTree_ExprBase;

      // if array appending right must be dyn array element type
      op_Plus, op_AssignPlus:
        begin
          if (LeftType is TLapeType_DynArray) then
            FRight := FRight.setExpectedType(TLapeType_DynArray(LeftType).PType) as TLapeTree_ExprBase;
        end;

      // if indexing sizeint is wanted
      op_Index:
        begin
          FRight := FRight.setExpectedType(FCompiler.getBaseType(ltSizeInt)) as TLapeTree_ExprBase;
        end;

      // otherwise use leftVar
      else
        FRight := FRight.setExpectedType(LeftType) as TLapeTree_ExprBase;
    end;
  end;

var
  Typ: TLapeType;
begin
  Result := NullResVar;
  RightVar := NullResVar;
  LeftVar := NullResVar;

  if (FLeft <> nil) then
    if (resType(True) <> nil) and (FLeft is TLapeTree_If) then
      Exit(doIf())
    else if FOverloadOp then
      Exit(doOperatorOverload());

  // x in [...]: a set of x's type when that can be one (enum, AnsiChar, Byte), else a loop over the values
  if (FOperatorType = op_IN) and (FRight is TLapeTree_OpenArray) and
     (not inSet(TLapeTree_OpenArray(FRight), TLapeTree_ExprBase(FLeft).resType())) then
  begin
    CompileLeft();
    Exit(doInList(LeftVar));
  end;

  // (not for a variable by itself: where it is cannot move, and the right side is worked out in it, see below)
  if RightFirst() and (not (FLeft is TLapeTree_ResVar)) and (not (FLeft is TLapeTree_GlobalVar)) then
  begin
    ExpectRight(TLapeTree_ExprBase(FLeft).resType());
    RightVar := FRight.Compile(Offset);
    // (one on the stack is put in a temporary: the left side is worked out after it)
    if (RightVar.VarPos.MemPos = mpStack) and RightVar.HasType() then
    begin
      Typ := RightVar.VarType;
      RightVar := _ResVar.New(FCompiler.getTempVar(Typ));
      FCompiler.Emitter._PopStackToVar(Typ.Size, RightVar.VarPos.StackVar.Offset, Offset, @FRight._DocPos);
    end;
    CompileLeft();
  end
  else
  begin
    CompileLeft();

    if (FRight <> nil) then
    begin
      ExpectRight(LeftVar.VarType);

      // If assigning see if we can set Right.Dest to Left. If so exit early
      if (FOperatorType = op_Assign) and (FLeft <> nil) and LeftVar.Writeable and
         (FRight is TLapeTree_DestExprBase) and (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      begin
        TLapeTree_DestExprBase(FRight).Dest := LeftVar;
        RightVar := FRight.Compile(Offset);
        if (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
        begin
          Dest := NullResVar;
          Result := LeftVar;
          TLapeTree_DestExprBase(FRight).Dest := NullResVar;

          Exit;
        end;
      end else
        RightVar := FRight.Compile(Offset);
    end;
  end;

  try
    Result := LeftVar.VarType.Eval(FOperatorType, FDest, LeftVar, RightVar, EvalFlags(), Offset, @_DocPos);
  except
    on E: lpException do
      LapeExceptionReraise(E, DocPos);
  end;
end;

function TLapeTree_NoFold.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
begin
  Result := Self;
end;

constructor TLapeTree_ResVar.Create(AResVar: TResVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  FResVar := AResVar;

  FResType := FResVar.VarType;
  FRes := FResVar.VarPos.GlobalVar;
  if (FResVar.VarPos.MemPos = mpMem) and (FResVar.VarPos.GlobalVar <> nil) and FResVar.VarPos.GlobalVar.Readable then
    FConstant := bTrue
  else
    FConstant := bFalse;
end;

constructor TLapeTree_ResVar.Create(AResVar: TResVar; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AResVar, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

function TLapeTree_ResVar.Evaluate: TLapeGlobalVar;
begin
  if (FResVar.VarPos.MemPos <> mpMem) then
    LapeException(lpeInvalidEvaluation, DocPos)
  else
    Result := inherited;
end;

function TLapeTree_ResVar.Compile(var Offset: Integer): TResVar;
begin
  Result := FResVar;
end;

function TLapeTree_GlobalVar.getVarAsString: lpString;
begin
  if (FGlobalVar <> nil) then
    Result := FGlobalVar.AsString
  else
    Result := '';
end;

function TLapeTree_GlobalVar.getVarAsInt: Int64;
begin
  if (FGlobalVar <> nil) then
    Result := FGlobalVar.AsInteger
  else
    Result := 0;
end;

constructor TLapeTree_GlobalVar.Create(AGlobalVar: TLapeGlobalVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  FGlobalVar := AGlobalVar;
  if (ACompiler <> nil) and (FGlobalVar <> nil) and (FGlobalVar.DeclarationList = nil) then
    FGlobalVar := TLapeGlobalVar(FCompiler.addManagedDecl(FGlobalVar));

  if (FGlobalVar = nil) then
    FConstant := bFalse
  else
  begin
    FResType := FGlobalVar.VarType;
    FRes := FGlobalVar;
    if FGlobalVar.Readable then
      FConstant := bTrue
    else
      FConstant := bFalse;
  end;
end;

constructor TLapeTree_GlobalVar.Create(AGlobalVar: TLapeGlobalVar; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AGlobalVar, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_GlobalVar.Create(Ident: lpString; BaseType: ELapeBaseType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  Create(ACompiler.getConstant(Ident, BaseType), ACompiler, ADocPos);
end;

constructor TLapeTree_GlobalVar.Create(Ident: lpString; BaseType: ELapeBaseType; ASource: TLapeTree_Base);
begin
  Assert((ASource <> nil) and (ASource.Compiler <> nil));
  Create(ASource.Compiler.getConstant(Ident, BaseType), ASource);
end;

function TLapeTree_GlobalVar.Compile(var Offset: Integer): TResVar;
begin
  Result := _ResVar.New(FGlobalVar);
end;


constructor TLapeTree_Inherited.Create(AMethod: TLapeInheritedMethod; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(AMethod <> nil);
  inherited Create(ACompiler, ADocPos);
  FMethod := AMethod;
end;

function TLapeTree_Inherited.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FMethod.VarType;
  Result := inherited;
end;

function TLapeTree_Inherited.Compile(var Offset: Integer): TResVar;
var
  SelfVar, Called: TResVar;
begin
  Result := _ResVar.New(FMethod);
  if (FMethod.SelfVar = nil) or (not MethodOfObject(Result.VarType)) then
    Exit;

  // (the method with the Self that it is called on, as TLapeType.Eval does for Self.Method)
  SelfVar := _ResVar.New(FMethod.SelfVar).InScope(FCompiler.StackInfo, @_DocPos);
  Called := Result;
  Result := _ResVar.New(FCompiler.getTempVar(FCompiler.getGlobalType('TMethod')));
  Result.VarType := Called.VarType;
  FCompiler.Emitter._Eval(getEvalProc(op_Dot, ltUnknown, ltPointer), Result, Called, SelfVar, Offset, @_DocPos);
  Result.Readable := SelfVar.Writeable;
end;

constructor TLapeTree_WithVar.Create(AWithDeclRec: TLapeWithDeclRec; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  FWithDeclRec := AWithDeclRec;
  FResType := FWithDeclRec.WithType;
end;

constructor TLapeTree_WithVar.Create(AWithDeclRec: TLapeWithDeclRec; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AWithDeclRec, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

function TLapeTree_WithVar.isConstant: Boolean;
begin
  Result := (FWithDeclRec.WithVar <> nil) and
    (FWithDeclRec.WithVar^.VarPos.MemPos = mpMem) and
    (FWithDeclRec.WithVar^.VarPos.GlobalVar <> nil) and
    FWithDeclRec.WithVar^.VarPos.GlobalVar.Readable;
end;

function TLapeTree_WithVar.Evaluate: TLapeGlobalVar;
begin
  if isConstant() then
    Result := FWithDeclRec.WithVar^.VarPos.GlobalVar
  else
    Result := nil;
end;

function TLapeTree_WithVar.Compile(var Offset: Integer): TResVar;
begin
  if (FWithDeclRec.WithVar = nil) or (FWithDeclRec.WithVar^.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    LapeException(lpeInvalidWithReference, DocPos);
  Result := FWithDeclRec.WithVar^.InScope(FCompiler.StackInfo, @_DocPos);
end;

constructor TLapeTree_VarType.Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getTypeVar(AVarType), ACompiler, ADocPos);
  FVarType := AVarType;
end;

constructor TLapeTree_VarType.Create(AVarType: TLapeType; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AVarType, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Integer.Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  if (Pos('_', AStr) > 0) then
    AStr := lpString(StringReplace(AStr, lpString('_'), lpString(''), [rfReplaceAll]));
  inherited Create(ACompiler.getConstant(AStr, ltNativeInt), ACompiler, ADocPos);
end;

constructor TLapeTree_Integer.Create(AStr: lpString; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AStr, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Integer.Create(AValue: Int64; ASource: TLapeTree_Base);
begin
  Create(lpString(IntToStr(AValue)), ASource);
end;

constructor TLapeTree_Integer.Create(AValue: UInt64; ASource: TLapeTree_Base);
begin
  Create(lpString(UIntToStr(AValue)), ASource);
end;

constructor TLapeTree_Float.Create(AStr: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getConstant(StringReplace(AStr, lpString('_'), lpString(''), [rfReplaceAll]), ltFloat), ACompiler, ADocPos);
end;

constructor TLapeTree_Float.Create(AStr: lpString; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AStr, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Float.Create(AValue: lpFloat; ASource: TLapeTree_Base);
begin
  Create(lpString(FloatToStr(AValue)), ASource);
end;

constructor TLapeTree_String.Create(AValue: AnsiString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(lpString(AValue), ltAnsiString, ACompiler, ADocPos);
end;

constructor TLapeTree_String.Create(AValue: UnicodeString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(lpString(AValue), ltUnicodeString, ACompiler, ADocPos);
end;

constructor TLapeTree_String.Create(AValue: lpString; ASource: TLapeTree_Base);
begin
  inherited Create(AValue, ltString, ASource);
end;

constructor TLapeTree_Char.Create(ACode: UInt32; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  Assert(ACode <= $FFFF);
  if (ACode > 255) then
    inherited Create('#' + lpString(IntToStr(ACode)), ltWideChar, ACompiler, ADocPos)
  else
    inherited Create('#' + lpString(IntToStr(ACode)), ltChar, ACompiler, ADocPos);
end;

constructor TLapeTree_Char.Create(ACode: UInt32; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(ACode, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

procedure TLapeTree_Range.setLo(Node: TLapeTree_ExprBase);
begin
  if (FLo <> nil) and (FLo <> Node) then
    FLo.Free();
  FLo := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Range.setHi(Node: TLapeTree_ExprBase);
begin
  if (FHi <> nil) and (FHi <> Node) then
    FHi.Free();
  FHi := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Range.DeleteChild(Node: TLapeTree_Base);
begin
  if (FLo = Node) then
    FLo := nil
  else if (FHi = Node) then
    FHi := nil;
end;

constructor TLapeTree_Range.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  setLo(nil);
  setHi(nil);
end;

destructor TLapeTree_Range.Destroy;
begin
  setLo(nil);
  setHi(nil);
  inherited;
end;

function TLapeTree_Range.isConstant: Boolean;
begin
  Result := (not isEmpty(FLo)) and (not isEmpty(FHi)) and FLo.isConstant() and FHi.isConstant();
end;

function TLapeTree_Range.isInvoking: Boolean;
begin
  Result := ((FLo <> nil) and FLo.isInvoking()) or ((FHi <> nil) and FHi.isInvoking());
end;

procedure TLapeTree_StatementList.DeleteChild(Node: TLapeTree_Base);
begin
  FStatements.DeleteItem(Node);
end;

constructor TLapeTree_StatementList.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FStatements := TLapeStatementList.Create(nil, dupAccept, False);
end;

destructor TLapeTree_StatementList.Destroy;
var
  i: Integer;
begin
  for i := FStatements.Count - 1 downto 0 do
    if (FStatements[i] <> nil) and (FStatements[i].Parent = Self) then
      FStatements[i].Free();
  FreeAndNil(FStatements);

  inherited;
end;

function TLapeTree_StatementList.addStatement(Statement: TLapeTree_Base; Prepend: Boolean = False): Integer;
begin
  Result := FStatements.Add(Statement);
  if Prepend then
    FStatements.MoveItem(Result, 0);
  if (Statement <> nil) then
    Statement.Parent := Self;
end;

function CompileStatement(Statement: TLapeTree_Base; var Offset: Integer): TResVar;
var
  Stack: TLapeStackInfo;
  Mark: Integer;
begin
  Stack := Statement.Compiler.StackInfo;
  Mark := 0;
  if (Stack <> nil) then
    Mark := Stack.TempMark();
  Statement.CompileToTempVar(Offset, Result);
  Assert(Statement.Compiler.StackInfo = Stack);
  if (Stack <> nil) then
    Stack.ReleaseTemps(Mark);
end;

function TLapeTree_StatementList.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
begin
  Result := NullResVar;
  for i := 0 to FStatements.Count - 1 do
    if (FStatements[i] <> nil) then
      Result := CompileStatement(FStatements[i], Offset);
end;

function TLapeTree_StatementList.Walk(Visit: TLapeTree_Visit): Boolean;
var
  i: Integer;
begin
  for i := 0 to FStatements.Count - 1 do
    if (FStatements[i] <> nil) and Visit(FStatements[i]) then
      Exit(True);
  Result := False;
end;

procedure TLapeTree_DelayedStatementList.DeleteChild(Node: TLapeTree_Base);
var
  i, Len: Integer;
begin
  i := FStatements.IndexOf(Node);
  if (i > -1) then
  begin
    FStatements.Delete(i);
    Len := FStatements.Count;
    for i := i to Len - 1 do
    begin
      Swap(FDelay[i], FDelay[i + 1]);
      Swap(FGlobal[i], FGlobal[i + 1]);
    end;
    SetLength(FDelay, Len);
    SetLength(FGlobal, Len);
  end;
end;

procedure TLapeTree_DelayedStatementList.Clean;
var
  i: Integer;
begin
  for i := FStatements.Count - 1 downto 0 do
    if (not FGlobal[i]) and (FStatements[i] <> nil) and (FStatements[i].Parent = Self) then
      FStatements[i].Free();
end;

function TLapeTree_DelayedStatementList.GlobalCount(Global: Boolean = True): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := FStatements.Count - 1 downto 0 do
    if (FGlobal[i] = Global) and (FStatements[i] <> nil) then
      Inc(Result);
end;

procedure TLapeTree_DelayedStatementList.OverrideDelayGlobal(AfterCompilation, IsGlobal: Boolean; StartIndex: Integer; Count: Integer = 1);
var
  i: Integer;
begin
  if (StartIndex + Count >= FStatements.Count) or (Count <= 0) then
    Count := FStatements.Count - StartIndex - 1;
  for i := StartIndex to StartIndex + Count do
  begin
    FDelay[i] := AfterCompilation;
    FGlobal[i] := IsGlobal;
  end;
end;

function TLapeTree_DelayedStatementList.addStatement(Statement: TLapeTree_Base; Prepend: Boolean = False): Integer;
begin
  Result := inherited;
  SetLength(FDelay, FStatements.Count);
  SetLength(FGlobal, FStatements.Count);
end;

function TLapeTree_DelayedStatementList.addStatement(Statement: TLapeTree_Base; AfterCompilation, IsGlobal: Boolean): Integer;
begin
  Result := addStatement(Statement);
  if (Result > -1) then
  begin
    FDelay[Result] := AfterCompilation;
    FGlobal[Result] := IsGlobal;
  end;
end;

function TLapeTree_DelayedStatementList.Compile(var Offset: Integer; AfterCompilation: Boolean; Filter: TLapeDelayedFilter = ldfAll): TResVar;
var
  i: Integer;
begin
  Result := NullResVar;
  i := 0;

  while (i < FStatements.Count) do
  begin
    if (AfterCompilation = FDelay[i]) and (FStatements[i] <> nil) then
      if (Filter = ldfAll) or ((Filter <> ldfMethods) xor (FStatements[i] is TLapeTree_Method)) then
        Result := CompileStatement(FStatements[i], Offset);

    Inc(i);
  end;
end;

function TLapeTree_DelayedStatementList.Compile(AfterCompilation: Boolean; Filter: TLapeDelayedFilter = ldfAll): TResVar;
var
  Offset: Integer;
begin
  Offset := -1;
  Result := Compile(Offset, AfterCompilation, Filter);
end;

procedure TLapeTree_Method.setStatements(Node: TLapeTree_StatementList);
begin
  if (FStatements <> nil) and (FStatements <> Node) then
    FStatements.Free();
  FStatements := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Method.DeleteChild(Node: TLapeTree_Base);
begin
  if (Node = FStatements) then
    FStatements := nil;
end;

constructor TLapeTree_Method.Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(AMethod <> nil);
  Assert(AMethod.VarType is TLapeType_Method);
  Assert(AStackInfo <> nil);
  inherited Create(ACompiler, ADocPos);

  FreeStackInfo := True;
  Method := AMethod;
  SelfVar := NullResVar;
  FStackInfo := AStackInfo;
  FExitStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, True);
end;

destructor TLapeTree_Method.Destroy;
begin
  setStatements(nil);
  if FreeStackInfo then
    FStackInfo.Free();
  FExitStatements.Free();
  inherited;
end;

function TLapeTree_Method.Compile(var Offset: Integer): TResVar;
var
  i, fo, mo, co: Integer;
begin
  Assert(Method <> nil);

  if (lcoVerboseCompile in FCompilerOptions) then
    FCompiler.Hint('Compiling: "%s"', [GetMethodName(Method.VarType)], DocPos);

  FExitStatements.Clear();

  Result := NullResVar;
  fo := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
  mo := FCompiler.Emitter.getCodeOffset(Offset);

  PCodePos(Method.Ptr)^ := mo;
  FCompiler.Emitter.addCodePointer(Method.Ptr, GetMethodName(Method.VarType));

  FCompiler.IncStackInfo(FStackInfo, Offset, [lsfEmit], @_DocPos);
  try
    FStatements.Compile(Offset);
    for i := 0 to FExitStatements.Count - 1 do
      with FExitStatements[i] do
      begin
        co := CodeOffset;
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos);
      end;
  finally
    FCompiler.DecStackInfo(Offset, [lsfFunction, lsfEmit], False, @_DocPos);
    FCompiler.Emitter._JmpR(Offset - fo, fo, @_DocPos);
  end;
end;

function TLapeTree_Method.Walk(Visit: TLapeTree_Visit): Boolean;
begin
  Result := (FStatements <> nil) and Visit(FStatements);
end;

function TLapeTree_method.canExit: Boolean;
begin
  Result := True;
end;

procedure TLapeTree_Method.addExitStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  FExitStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe));
end;

procedure TLapeTree_VarList.DeleteChild(Node: TLapeTree_Base);
var
  i: Integer;
begin
  for i := FVars.Count - 1 downto 0 do
    if (FVars[i].Default = Node) then
      FVars.Delete(i);
end;

constructor TLapeTree_VarList.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
const
  NullVar: TLapeVarDecl = (VarDecl: nil; Default: nil);
begin
  inherited;
  FVars := TLapeVarDeclList.Create(NullVar, dupError, False);
end;

destructor TLapeTree_VarList.Destroy;
var
  i: Integer;
begin
  for i := FVars.Count - 1 downto 0 do
    if (FVars[i].Default <> nil) and (FVars[i].Default.Parent = Self) then
      FVars[i].Default.Free();
  FreeAndNil(FVars);

  inherited;
end;

function TLapeTree_VarList.addVar(AVar: TLapeVarDecl): Integer;
begin
  Result := FVars.Add(AVar);
  if (AVar.Default <> nil) then
    AVar.Default.Parent := Self;
end;

function TLapeTree_VarList.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  wasConstant: Boolean;
begin
  Result := NullResVar;
  i := 0;
  while (i < FVars.Count) do
    if (FVars[i].VarDecl <> nil) and (FVars[i].Default <> nil) then
      with FVars[i], TLapeTree_Operator.Create(op_Assign, Self) do
      try
        wasConstant := (VarDecl is TLapeVar) and (not TLapeVar(VarDecl).Writeable);
        if wasConstant then
          TLapeVar(VarDecl).Writeable := True;

        Left := TLapeTree_ResVar.Create(_ResVar.New(VarDecl), Self);
        Right := Default;
        Result := Compile(Offset);

        if wasConstant then
          TLapeVar(VarDecl).Writeable := False;
      finally
        Free();
      end
    else
      Inc(i);
end;

constructor TLapeTree_FinalizeVar.Create(AVar: TLapeVar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  FVariable := AVar;
end;

constructor TLapeTree_FinalizeVar.Create(AVar: TLapeVar; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AVar, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

function TLapeTree_FinalizeVar.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  if (FVariable <> nil) and FVariable.HasType() then
    FVariable.VarType.Finalize(FVariable, Offset, True, @_DocPos);
end;

procedure TLapeTree_With.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

constructor TLapeTree_With.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBody := nil;
  FWithList := TLapeExpressionList.Create(nil, dupAccept, False);
end;

destructor TLapeTree_With.Destroy;
var
  i: Integer;
begin
  for i := FWithList.Count - 1 downto 0 do
    if (FWithList[i] <> nil) and (FWithList[i].Parent = Self) then
      FWithList[i].Free();
  FreeAndNil(FWithList);

  setBody(nil);
  inherited;
end;

function TLapeTree_With.addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclRec;
var
  i: Integer;
begin
  Result := NullWithDecl;
  if (FWithList.Count = Length(FVarList)) then
    LapeException(lpeInvalidWithReference, [AWith, Self]);

  i := FWithList.Add(AWith);
  if (AWith <> nil) and (i > -1) then
  begin
    AWith.Parent := Self;
    if (AWith is TLapeTree_GlobalVar) and TLapeTree_GlobalVar(AWith).isConstant then
      FVarList[i] := _ResVar.New(TLapeTree_GlobalVar(AWith).GlobalVar)
    else
      FVarList[i] := NullResVar;

    Result.WithVar := @FVarList[i];
    Result.WithType := AWith.resType();
  end;

  if (Result.WithType = nil) then
    LapeException(lpeInvalidWithReference, [AWith, Self])
end;

function TLapeTree_With.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  Compiled: array[0..7] of Boolean;
begin
  Result := NullResVar;

  if (FCompiler.StackInfo = nil) then
    LapeException(lpeStatementNotAllowed, DocPos);

  for i := 0 to FWithList.Count - 1 do
  begin
    Compiled[i] := (FVarList[i].VarPos.MemPos = NullResVar.VarPos.MemPos);
    if Compiled[i] then
    begin
      FVarList[i] := FWithList[i].Compile(Offset);
      if (not FVarList[i].HasType()) or (FVarList[i].VarPos.MemPos in [mpNone, mpStack]) then
        LapeException(lpeInvalidCondition, FWithList[i].DocPos);
    end;
  end;

  if (FBody <> nil) then
    FBody.CompileToTempVar(Offset, Result);

  // (what it compiled is compiled again when it is compiled again)
  for i := 0 to FWithList.Count - 1 do
    if Compiled[i] then
      FVarList[i] := NullResVar;
end;

function TLapeTree_With.Walk(Visit: TLapeTree_Visit): Boolean;
begin
  Result := (FBody <> nil) and Visit(FBody);
end;

procedure TLapeTree_If.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_If.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_If.setElse(Node: TLapeTree_Base);
begin
  if (FElse <> nil) and (FElse <> Node) then
    FElse.Free();
  FElse := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_If.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FBody = Node) then
    FBody := nil
  else if (FElse = Node) then
    FElse := nil;
end;

function TLapeTree_If.CompileCondition(var Offset: Integer): TResVar;
begin
  Assert(FCondition <> nil);

  Result := FCondition.Compile(Offset);
  if (not Result.HasType()) or (not (Result.VarType.BaseType in LapeIfTypes)) then
    Exit(NullResVar);

  // Need bool expression:
  // Example `if ptr then` to `if ptr <> 0 then`
  if (not (Result.VarType.BaseType in LapeBoolTypes)) then
  begin
    with TLapeTree_Operator.Create(op_cmp_NotEqual, Self) do
    try
      Left := TLapeTree_ResVar.Create(Result, Self);
      if (Result.VarType.BaseType in LapeIntegerTypes + [ltPointer]) then
        Right := TLapeTree_GlobalVar.Create(FCompiler.getConstant(0), Self)
      else if (Result.VarType.BaseType = ltVariant) then
        Right := TLapeTree_GlobalVar.Create(FCompiler.getConstant(0, ltEvalBool), Self)
      else
        Right := TLapeTree_GlobalVar.Create(Result.VarType.NewGlobalVarP(), Self);

      Result := Compile(Offset);
    finally
      Free();
    end;
  end;
end;

function TLapeTree_If.CompileBody(var Offset: Integer): TResVar;
begin
  if (FStartBodyOffset <= 0) then
    FStartBodyOffset := FCompiler.Emitter.CheckOffset(Offset);

  if (FBody <> nil) then
    FBody.CompileToTempVar(Offset, Result)
  else
    Result := NullResVar;
end;

constructor TLapeTree_If.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FBody := nil;
  FElse := nil;
end;

destructor TLapeTree_If.Destroy;
begin
  setCondition(nil);
  setBody(nil);
  setElse(nil);
  inherited;
end;

function TLapeTree_If.Compile(var Offset: Integer): TResVar;
var
  ConditionVar: TResVar;
  if_o, if_e: Integer;
begin
  Assert(FCondition <> nil);
  Result := NullResVar;
  FStartBodyOffset := 0;

  ConditionVar := CompileCondition(Offset);
  if (not ConditionVar.HasType) then
    LapeException(lpeInvalidCondition, [FCondition, Self]);

  FCompiler.Emitter.FullEmit := False;
  if_o := FCompiler.Emitter._JmpRIfNot(0, ConditionVar, Offset, @_DocPos);
  FCompiler.Emitter.FullEmit := True;

  if (FBody <> nil) or (FElse = nil) then
  begin
    Result := CompileBody(Offset);

    if (FElse <> nil) then
      if_e := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
    FCompiler.Emitter._JmpRIfNot(Offset - if_o, ConditionVar, if_o, @_DocPos);
  end;

  if (FElse <> nil) then
  begin
    FElse.CompileToTempVar(Offset, Result);

    if (FBody <> nil) then
      FCompiler.Emitter._JmpR(Offset - if_e, if_e, @_DocPos)
    else
      FCompiler.Emitter._JmpRIf(Offset - if_o, ConditionVar, if_o, @_DocPos);
  end;

end;

function TLapeTree_If.isInvoking: Boolean;
begin
  Result := ((FCondition <> nil) and FCondition.isInvoking()) or ((FBody <> nil) and FBody.isInvoking()) or
            ((FElse <> nil) and FElse.isInvoking());
end;

function TLapeTree_If.Walk(Visit: TLapeTree_Visit): Boolean;
begin
  Result := ((FBody <> nil) and Visit(FBody)) or ((FElse <> nil) and Visit(FElse));
end;

procedure TLapeTree_CaseBranch.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else
  if (FStatement = Node) then
    FStatement := nil
  else
  if (FValues <> nil) then
    FValues.DeleteItem(Node);
end;

procedure TLapeTree_CaseBranch.setCaseEndOffset(Offset: Integer);
begin
  FCompiler.Emitter._JmpR(Offset - FCaseEndJump, FCaseEndJump, @_DocPos);
end;

procedure TLapeTree_CaseBranch.setFallThroughOffset(Offset: Integer);
var
  i: Integer;
  FlowStatement: TLapeFlowStatement;
begin
  for i := 0 to FFallThroughStatements.Count - 1 do
  begin
    FlowStatement := FFallThroughStatements[i];
    with FlowStatement do
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - CodeOffset, CodeOffset, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - CodeOffset, CodeOffset, @DocPos);

    FFallThroughStatements[i] := FlowStatement;
  end;
end;

procedure TLapeTree_CaseBranch.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_CaseBranch.setStatement(Node: TLapeTree_Base);
begin
  if (FStatement <> nil) and (FStatement <> Node) then
    FStatement.Free();
  FStatement := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

constructor TLapeTree_CaseBranch.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FValues := TLapeStatementList.Create(nil, dupAccept, False);
  FFallThroughStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, False);
end;

destructor TLapeTree_CaseBranch.Destroy;
var
  i: Integer;
begin
  setCondition(nil);
  setStatement(nil);

  for i := FValues.Count - 1 downto 0 do
    if (FValues[i] <> nil) and (FValues[i].Parent = Self) then
      FValues[i].Free();
  FreeAndNil(FValues);
  FreeAndNil(FFallThroughStatements);

  inherited Destroy;
end;

function TLapeTree_CaseBranch.addValue(Value: TLapeTree_Base): Integer;
begin
  Result := FValues.Add(Value);
  if (Value <> nil) then
    Value.Parent := Self;
end;

procedure TLapeTree_CaseBranch.addFallThroughStatement(JumpSafe: Boolean; Offset: Integer; Pos: PDocPos);
begin
  FFallThroughStatements.Add(getFlowStatement(Offset, Pos, JumpSafe));
end;

function TLapeTree_CaseBranch.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  ConditionVar, CompareVar: TResVar;
  Values: TLapeStatementList.TTArray;
  JumpOffsets: TIntegerArray;
begin
  Result := NullResVar;

  Assert(FValues.Count > 0);
  Assert(FCondition <> nil);

  if (not FCondition.CompileToTempVar(Offset, ConditionVar)) then
    LapeException(lpeInvalidCondition, DocPos);

  SetLength(JumpOffsets, FValues.Count);

  Values := FValues.ExportToArray();

  for i := 0 to High(Values) do
  begin
    if (Values[i] is TLapeTree_ExprBase) then
    begin
      with TLapeTree_Operator.Create(op_cmp_Equal, Self) do
      try
        Right := TLapeTree_ExprBase(Values[i]).FoldConstants(False) as TLapeTree_ExprBase;
        Left := TLapeTree_ResVar.Create(ConditionVar, Self);

        CompareVar := Compile(Offset);
      finally
        Free();
      end;
    end
    else
    if (Values[i] is TLapeTree_Range) then
    begin
      if (TLapeTree_Range(Values[i]).Lo = nil) or (TLapeTree_Range(Values[i]).Hi = nil) then
        LapeException(lpeInvalidRange, DocPos);

      with TLapeTree_Operator.Create(op_AND, Self) do
      try
        Left := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, Self);
        with TLapeTree_Operator(Left) do
        begin
          Left := TLapeTree_ResVar.Create(ConditionVar, Self);
          Right := TLapeTree_Range(Values[i]).Lo.FoldConstants(False) as TLapeTree_ExprBase;
        end;

        Right := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
        with TLapeTree_Operator(Right) do
        begin
          Left := TLapeTree_ResVar.Create(ConditionVar, Self);
          Right := TLapeTree_Range(Values[i]).Hi.FoldConstants(False) as TLapeTree_ExprBase;
        end;

        CompareVar := Compile(Offset);
      finally
        Free();
      end;
    end else
      LapeException(lpeInvalidEvaluation, DocPos);

    FCompiler.Emitter.FullEmit := False;

    if (i = High(Values)) then
      JumpOffsets[i] := FCompiler.Emitter._JmpRIfNot(0, CompareVar, Offset, @_DocPos) // IF NOT: Jump past statement
    else
      JumpOffsets[i] := FCompiler.Emitter._JmpRIf(0, CompareVar, Offset, @_DocPos);   // IF: Jump to statement

    FCompiler.Emitter.FullEmit := True;
  end;

  FBodyOffset := Offset;
  if (FStatement <> nil) then
    Result := FStatement.Compile(Offset);

  FCompiler.Emitter.FullEmit := False;
  FCaseEndJump := FCompiler.Emitter._JmpR(0, Offset, @_DocPos); // Jump to case statement end. Set after branches are compiled.
  FCompiler.Emitter.FullEmit := True;

  for i := 0 to High(JumpOffsets) do
    if (i = High(JumpOffsets)) then
      FCompiler.Emitter._JmpRIfNot(Offset - JumpOffsets[i], CompareVar, JumpOffsets[i], @_DocPos)
    else
      FCompiler.Emitter._JmpRIf(FBodyOffset - JumpOffsets[i], CompareVar, JumpOffsets[i], @_DocPos);
end;

procedure TLapeTree_Case.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FElse = Node) then
    FElse := nil
  else
  if (FBranches <> nil) then
    FBranches.DeleteItem(TLapeTree_CaseBranch(Node));
end;

procedure TLapeTree_Case.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Case.setElse(Node: TLapeTree_Base);
begin
  if (FElse <> nil) and (FElse <> Node) then
    FElse.Free();
  FElse := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

constructor TLapeTree_Case.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FElse := nil;
  FBranches := TCaseBranches.Create(nil, dupAccept, False);
end;

destructor TLapeTree_Case.Destroy;
var
  i: Integer;
begin
  for i := FBranches.Count - 1 downto 0 do
    if (FBranches[i] <> nil) and (FBranches[i].Parent = Self) then
      FBranches[i].Free();
  FreeAndNil(FBranches);

  setCondition(nil);
  setElse(nil);

  inherited;
end;

procedure TLapeTree_Case.addBranch(Branch: TLapeTree_CaseBranch);
begin
  FBranches.Add(Branch);
  if (Branch <> nil) then
    Branch.Parent := Self;
end;

function TLapeTree_Case.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  ConditionVar: TResVar;
begin
  Result := NullResVar;

  Assert(FCondition <> nil);
  if (not FCondition.CompileToTempVar(Offset, ConditionVar)) then
    LapeException(lpeInvalidCondition, DocPos);

  if (FBranches.Count > 0) then
  begin
    for i := 0 to FBranches.Count - 1 do
    begin
      FBranches[i].Condition := TLapeTree_ResVar.Create(ConditionVar, FCondition);

      Result := FBranches[i].Compile(Offset);
    end;

    for i := 1 to FBranches.Count - 1 do
      FBranches[i - 1].FallThroughOffset := FBranches[i].BodyOffset;
    FBranches[FBranches.Count - 1].FallThroughOffset := Offset;
  end;

  if (FElse <> nil) then
    Result := FElse.Compile(Offset);

  for i := 0 to FBranches.Count - 1 do
    FBranches[i].CaseEndOffset := Offset;

end;

function TLapeTree_Case.Walk(Visit: TLapeTree_Visit): Boolean;
var
  i: Integer;
begin
  for i := 0 to FBranches.Count - 1 do
    if (FBranches[i].FStatement <> nil) and Visit(FBranches[i].FStatement) then
      Exit(True);
  Result := (FElse <> nil) and Visit(FElse);
end;

function TLapeTree_While.CompileBody(var Offset: Integer): TResVar;
var
  i, co: Integer;
  ConditionVar: TResVar;
begin
  Result := inherited;

  if (FContinueStatements <> nil) then
  begin
    FContinueCount := FContinueStatements.Count;
    for i := 0 to FContinueCount - 1 do
      with FContinueStatements[i] do
      begin
        co := CodeOffset;
        if JumpSafe then
          FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
        else
          FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
      end;
  end;

  ConditionVar := CompileCondition(Offset);
  if (not ConditionVar.HasType) then
    LapeException(lpeInvalidCondition, [FCondition, Self]);

  FCompiler.Emitter._JmpRIf(FStartBodyOffset - Offset, ConditionVar, Offset, @_DocPos);
end;

constructor TLapeTree_While.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBreakStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, True);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, True);
end;

destructor TLapeTree_While.Destroy;
begin
  FBreakStatements.Free();
  FContinueStatements.Free();
  inherited;
end;

function TLapeTree_While.Compile(var Offset: Integer): TResVar;
var
  i, co: Integer;
begin
  FContinueCount := 0;
  FBreakStatements.Clear();
  FContinueStatements.Clear();
  Result := inherited;

  if (FContinueCount < FContinueStatements.Count) then
    LapeException(lpeStatementNotAllowed, FContinueStatements[FContinueCount].DocPos);

  for i := 0 to FBreakStatements.Count - 1 do
    with FBreakStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;
end;

function TLapeTree_While.canBreak: Boolean;
begin
  Result := True;
end;

function TLapeTree_While.canContinue: Boolean;
begin
  Result := True;
end;

procedure TLapeTree_While.addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
end;

procedure TLapeTree_While.addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
end;

function TLapeTree_Repeat.CompileBody(var Offset: Integer): TResVar;
var
  i, co: Integer;
  ConditionVar: TResVar;
begin
  Result := inherited;

  for i := 0 to FContinueStatements.Count - 1 do
    with FContinueStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  ConditionVar := CompileCondition(Offset);
  if (not ConditionVar.HasType) then
    LapeException(lpeInvalidCondition, [FCondition, Self]);
  FCompiler.Emitter._JmpRIfNot(FStartBodyOffset - Offset, ConditionVar, Offset, @_DocPos);
end;

constructor TLapeTree_Repeat.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;

  FBreakStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, True);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, True);
end;

destructor TLapeTree_Repeat.Destroy;
begin
  FBreakStatements.Free();
  FContinueStatements.Free();
  inherited;
end;

function TLapeTree_Repeat.Compile(var Offset: Integer): TResVar;
var
  i, co, StartOffset: Integer;
begin
  FStartBodyOffset := 0;

  FBreakStatements.Clear();
  FContinueStatements.Clear();

  Result := CompileBody(Offset);

  for i := 0 to FBreakStatements.Count - 1 do
    with FBreakStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;
end;

function TLapeTree_Repeat.canBreak: Boolean;
begin
  Result := True;
end;

function TLapeTree_Repeat.canContinue: Boolean;
begin
  Result := True;
end;

procedure TLapeTree_Repeat.addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
end;

procedure TLapeTree_Repeat.addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil);
begin
  if JumpSafe then
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
end;

procedure TLapeTree_For.setCounter(Node: TLapeTree_ExprBase);
begin
  if (FCounter <> nil) and (FCounter <> Node) then
    FCounter.Free();
  FCounter := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_For.setLimit(Node: TLapeTree_ExprBase);
begin
  if (FLimit <> nil) and (FLimit <> Node) then
    FLimit.Free();
  FLimit := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_For.setStep(Node: TLapeTree_ExprBase);
begin
  if (FStep <> nil) and (FStep <> Node) then
    FStep.Free();
  FStep := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_For.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCounter = Node) then
    FCounter := nil
  else if (FLimit = Node) then
    FLimit := nil
  else if (FStep = Node) then
    FStep := nil
  else
    inherited;
end;

function TLapeTree_For.CompileBody(var Offset: Integer): TResVar;
var
  i, co: Integer;
  IncDec: TLapeTree_InternalMethod;
  tmpBody: TLapeTree_Base;
  tmpContinueStatements: TLapeFlowStatementList;
begin
  Assert((FCondition <> nil) and (FCondition is TLapeTree_Operator));
  Assert((TLapeTree_Operator(FCondition).Right <> nil) and (TLapeTree_Operator(FCondition).Right is TLapeTree_ResVar));

  FStartBodyOffset := FCompiler.Emitter.CheckOffset(Offset);

  if (LoopType = loopOver) then
    CompileBodyForIn(Offset);

  if (FBody <> nil) then
    FBody.CompileToTempVar(Offset, Result)
  else
    Result := NullResVar;

  FContinueCount := FContinueStatements.Count;
  for i := 0 to FContinueCount - 1 do
    with FContinueStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  if LoopType = loopDown then
    IncDec := TLapeTree_InternalMethod_Dec.Create(Self)
  else
    IncDec := TLapeTree_InternalMethod_Inc.Create(Self);
  
  try
    IncDec.addParam(TLapeTree_ResVar.Create(TLapeTree_Operator(FCondition).Left.Compile(Offset), FCondition));
    if (FStep <> nil) then
    begin
      IncDec.addParam(FStep);
      IncDec.Compile(Offset);
      Step := IncDec.Params.Delete(1);
    end
    else
      IncDec.Compile(Offset);
  finally
    IncDec.Free();
  end;
  
  tmpBody := FBody;
  tmpContinueStatements := FContinueStatements;
  try
    FBody := nil;
    FContinueStatements := nil;
    inherited;
  finally
    FBody := tmpBody;
    FContinueStatements := tmpContinueStatements;
  end;
end;

procedure TLapeTree_For.CompileBodyForIn(var Offset: Integer);
var
  counterVar, valueVar, tmpVar: TResVar;
begin
  Assert(LoopType = loopOver);

  FCounter.CompileToTempVar(Offset, counterVar);

  if (LoopOverWhat = loopOverArray) then
    with TLapeTree_Operator.Create(op_Assign, FCounter) do
    try
      Left := TLapeTree_ResVar.Create(counterVar, FCounter);
      Right := TLapeTree_Operator.Create(op_Index, FCounter);
      with TLapeTree_Operator(Right) do
      begin
        CompilerOptions := CompilerOptions - [lcoRangeCheck];
        Left := TLapeTree_ResVar.Create(Container, FLimit);
        Right := TLapeTree_ResVar.Create(TLapeTree_Operator(FCondition).Left.Compile(Offset), FCondition);
      end;
      Compile(Offset);
    finally
      Free();
    end
  else
  begin
    // (the number goes in the counter as it is: as an integer of the counter's size)
    valueVar := counterVar;
    valueVar.VarType := FCompiler.getBaseType(DetermineIntType(counterVar.VarType.Size, counterVar.VarType.BaseType in LapeSignedOrdinalTypes));
    tmpVar := NullResVar;
    valueVar.VarType.Eval(op_Assign, tmpVar, valueVar, TLapeTree_Operator(FCondition).Left.Compile(Offset), [], Offset, @FCounter._DocPos);
  end;

  if (CheckInVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
    with TLapeTree_If.Create(FLimit) do
    try
      Condition := TLapeTree_Operator.Create(op_IN, Self);
      TLapeTree_Operator(Condition).Left := TLapeTree_ResVar.Create(counterVar, Self);
      TLapeTree_Operator(Condition).Right := TLapeTree_ResVar.Create(CheckInVar, Self);

      ElseBody := TLapeTree_InternalMethod_Continue.Create(Self);
      ElseBody.FParent := Self;

      Compile(Offset);
    finally
      Free();
    end;
end;

constructor TLapeTree_For.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCounter := nil;
  FLimit := nil;
  FStep := nil;
  LoopType := loopUp;
end;

destructor TLapeTree_For.Destroy;
begin
  setCounter(nil);
  setLimit(nil);
  setStep(nil);
  inherited;
end;

function TLapeTree_For.Compile(var Offset: Integer): TResVar;
var
  Count, Lim: TResVar;
  CounterVar: TLapeVar;
begin
  Result := NullResVar;
  Assert(FCondition = nil);
  Assert(FCounter <> nil);
  Assert(FLimit <> nil);

  if (LoopType = loopOver) then
    Exit(CompileForIn(Offset));

  CounterVar := nil;
  Count := FCounter.Compile(Offset);
  try
    if (not FLimit.CompileToTempVar(Offset, Lim)) or (not Lim.HasType()) or (not (Lim.VarType.BaseType in LapeOrdinalTypes + [ltPointer])) then
      LapeException(lpeInvalidEvaluation, FLimit.DocPos);

    if Count.HasType() and (not Count.Writeable) then
    begin
      CounterVar := FCompiler.getTempVar(Count.VarType);
      Count := CounterVar.VarType.Eval(op_Assign, Result, _ResVar.New(CounterVar), Count, [], Offset, @FCounter._DocPos);
    end;
    if (not Count.HasType()) or (not Count.Writeable) or (not (Count.VarType.BaseType in LapeOrdinalTypes + [ltPointer])) then
      LapeException(lpeInvalidIterator, FCounter.DocPos);

    if LoopType = loopDown then
      FCondition := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, Self)
    else
      FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
    TLapeTree_Operator(FCondition).Left := TLapeTree_ResVar.Create(Count, FCounter);
    TLapeTree_Operator(FCondition).Right := TLapeTree_ResVar.Create(Lim, FLimit);

    Result := inherited;
  finally
    setCondition(nil);
  end;
end;

function TLapeTree_For.CompileForIn(var Offset: Integer): TResVar;
var
  upper, lower: TResVar;
  method: TLapeTree_ExprBase;
  Element: TLapeType;
  Enum: TLapeType_Enum;
  Range: TLapeRange;

  procedure AssignTo(Target: TResVar; Expr: TLapeTree_ExprBase);
  var
    Assignment: TLapeTree_Operator;
  begin
    Assignment := TLapeTree_Operator.Create(op_Assign, Self);
    try
      Assignment.Left := TLapeTree_ResVar.Create(Target, FCounter);
      Assignment.Right := Expr;
      Assignment.Compile(Offset);
    finally
      Assignment.Free();
    end;
  end;

begin
  Result := NullResVar;
  Container := NullResVar;

  Assert(FCondition = nil);
  Assert(FCounter <> nil);
  Assert(FLimit <> nil);

  if (not FLimit.CompileToTempVar(Offset, Container)) or (not Container.HasType()) then
    LapeException(lpeInvalidEvaluation, FLimit.DocPos);

  lower := _ResVar.New(FCompiler.getTempVar(ltSizeInt));

  // (of a set or an enum: a number from the first to the last, given to the counter as one of the element)
  if (LoopOverWhat in [loopOverSet, loopOverEnum]) then
  begin
    if (LoopOverWhat = loopOverSet) then
    begin
      if (not (Container.VarType is TLapeType_Set)) then
        LapeException(lpeInvalidEvaluation, FLimit.DocPos);

      // (only those that are in it)
      CheckInVar := Container;
      Element := TLapeType_Set(Container.VarType).Element;
      Range := TLapeType_Set(Container.VarType).Range;
    end
    else
    begin
      Element := Container.VarType;
      if (Element is TLapeType_Type) then
        Element := TLapeType_Type(Element).TType;

      // (an enum, or a range of one: its numbers have to be its elements, as FPC)
      if (Element is TLapeType_Enum) then
        Enum := TLapeType_Enum(Element)
      else if (Element is TLapeType_SubRange) and TLapeType_SubRange(Element).isEnumRange() then
        Enum := TLapeType_SubRange(Element).VarType as TLapeType_Enum
      else
        LapeException(lpeInvalidEvaluation, FLimit.DocPos);

      Range := TLapeType_SubRange(Element).Range;
      if Enum.HasGap(Range) then
        LapeExceptionFmt(lpeCannotLoopEnumWithGaps, [Element.AsString], FLimit.DocPos);
    end;

    if (FCounter.resType() <> nil) and (FCounter.resType().EvalRes(op_Assign, Element) = nil) then
      LapeExceptionFmt(lpeIncompatibleAssignment, [Element.AsString, FCounter.resType().AsString], FCounter.DocPos);

    AssignTo(lower, TLapeTree_GlobalVar.Create(FCompiler.getConstant(Range.Lo, ltSizeInt), Self));
    upper := _ResVar.New(FCompiler.getConstant(Range.Hi, ltSizeInt));
  end
  else
  begin
    upper := _ResVar.New(FCompiler.getTempVar(ltSizeInt));

    method := TLapeTree_InternalMethod_Low.Create(FCounter);
    TLapeTree_InternalMethod_Low(method).addParam(TLapeTree_ResVar.Create(Container, FCounter));
    AssignTo(lower, method.FoldConstants() as TLapeTree_ExprBase);

    method := TLapeTree_InternalMethod_High.Create(FLimit);
    TLapeTree_InternalMethod_High(method).addParam(TLapeTree_ResVar.Create(Container, FLimit));
    AssignTo(upper, method.FoldConstants() as TLapeTree_ExprBase);
  end;

  try
    FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
    TLapeTree_Operator(FCondition).Left := TLapeTree_ResVar.Create(lower, FCounter);
    TLapeTree_Operator(FCondition).Right := TLapeTree_ResVar.Create(upper, FLimit);
    Result := inherited Compile(Offset);
  finally
    setCondition(nil);
  end;
end;

procedure TLapeTree_Try.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Try.setExcept(Node: TLapeTree_Base);
begin
  if (FExcept <> nil) and (FExcept <> Node) then
    FExcept.Free();
  FExcept := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Try.setFinally(Node: TLapeTree_Base);
begin
  if (FFinally <> nil) and (FFinally <> Node) then
    FFinally.Free();
  FFinally := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Try.DeleteChild(Node: TLapeTree_Base);
begin
  if (FBody = Node) then
    FBody := nil
  else if (FExcept = Node) then
    FExcept := nil
  else if (FFinally = Node) then
    FFinally := nil;
end;

constructor TLapeTree_Try.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBody := nil;
  FExcept := nil;
  FFinally := nil;
end;

destructor TLapeTree_Try.Destroy;
begin
  setBody(nil);
  setExcept(nil);
  setFinally(nil);
  inherited;
end;

function TLapeTree_Try.Compile(var Offset: Integer): TResVar;
var
  o_try, o_except, o_jmp: Integer;
  tmp: TResVar;
begin
  Result := NullResVar;
  Assert((FExcept <> nil) or (FFinally <> nil));

  FCompiler.Emitter.FullEmit := False;
  o_try := FCompiler.Emitter._IncTry(0, 0, Offset, @_DocPos);
  FCompiler.Emitter.FullEmit := True;

  o_except := 0;
  if (FBody <> nil) then
    FBody.CompileToTempVar(Offset, Result);

  FCompiler.Emitter._DecTry(Offset, @_DocPos);
  if (FExcept <> nil) then
  begin
    o_jmp := FCompiler.Emitter._JmpR(0, Offset, @FExcept._DocPos);
    if (FFinally = nil) then
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoFinally, o_try, @_DocPos)
    else
      o_except := Offset;
    FCompiler.Emitter._CatchException(Offset, @FExcept._DocPos);

    FExcept.CompileToTempVar(Offset, tmp);

    FCompiler.Emitter._JmpR(Offset - o_jmp, o_jmp, @FExcept._DocPos);
  end;
  if (FFinally <> nil) then
  begin
    if (o_except <> 0) then
      FCompiler.Emitter._IncTry(o_except - o_try, Offset - o_except, o_try, @_DocPos)
    else
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoExcept, o_try, @_DocPos);

    FFinally.CompileToTempVar(Offset, tmp);
  end;
  FCompiler.Emitter._EndTry(Offset, @_DocPos);
end;

function TLapeTree_Try.Walk(Visit: TLapeTree_Visit): Boolean;
begin
  Result := ((FBody <> nil) and Visit(FBody)) or ((FExcept <> nil) and Visit(FExcept)) or
            ((FFinally <> nil) and Visit(FFinally));
end;

end.

