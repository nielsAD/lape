{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Syntax tree objects (and turning it into bytecode).
}
unit lptree;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpparser;

type
  TLapeTree_Base = class;
  TLapeTree_ExprBase = class;

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
    function addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean;
  end;

  ILapeTree_CanContinue = interface(IInterface)
    ['{411AF7EA-9D38-48DE-90E9-362E2D0D09E9}']
    function addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean;
  end;

  ILapeTree_CanExit = interface(IInterface)
    ['{D1963640-31F9-4BB3-B03E-0F1DB314D98E}']
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
    procedure setParent(Parent: TLapeTree_Base); virtual;
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
    function CompileToTempVar(var Offset: Integer; out v: TResVar; Lock: Integer = 0): Boolean; virtual;

    property Parent: TLapeTree_Base read FParent write setParent;
    property Compiler: TLapeCompilerBase read FCompiler;
    property CompilerOptions: ECompilerOptionsSet read FCompilerOptions write FCompilerOptions;
  end;

  TLapeTree_ExprBase = class(TLapeTree_Base)
  private
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
    function CompileToBoolVar(var Offset: Integer; out ConditionVar: TResVar): Boolean; virtual;
  end;

  TLapeTree_DestExprBase = class(TLapeTree_ExprBase)
  protected
    FDest: TResVar;
    procedure setDest(ResVar: TResVar); virtual;
  public
    function CompileToTempVar(var Offset: Integer; out v: TResVar; Lock: Integer = 0): Boolean; override;
    property Dest: TResVar read FDest write setDest;
  end;

  TLapeTree_OpenArray = class(TLapeTree_DestExprBase)
  protected
    FCanCast: TInitBool;
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

    function canCast: Boolean; virtual;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property ToType: TLapeType read FType write setType;
    property Values: TLapeStatementList read FValues;
  end;

  TLapeTree_Invoke = class(TLapeTree_DestExprBase)
  protected
    FExpr: TLapeTree_ExprBase;
    FRealIdent: TLapeTree_ExprBase;
    FParams: TLapeExpressionList;

    procedure setExpr(Node: TLapeTree_ExprBase); virtual;
    procedure setRealIdent(Node: TLapeTree_ExprBase); virtual;
    function getRealIdent(ExpectType: TLapeType): TLapeTree_ExprBase; overload; virtual;
    function getRealIdent: TLapeTree_ExprBase; overload; virtual;

    procedure DeleteChild(Node: TLapeTree_Base); override;
    function getParamTypes: TLapeTypeArray; virtual;
    function getParamTypesStr: lpString; virtual;
  public
    constructor Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(Ident: TLapeTree_ExprBase; ASource: TLapeTree_Base); overload; virtual;
    constructor Create(Ident: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); overload; virtual;
    constructor Create(Ident: lpString; ASource: TLapeTree_Base); overload; virtual;
    constructor Create(Ident: TLapeGlobalVar; ASource: TLapeTree_Base); overload; virtual;
    constructor Create(Ident: TLapeType; ASource: TLapeTree_Base); overload; virtual;
    destructor Destroy; override;

    procedure ClearCache; override;
    function addParam(p: TLapeTree_ExprBase): Integer; virtual;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Expr: TLapeTree_ExprBase read FExpr write setExpr;
    property RealIdent: TLapeTree_ExprBase read getRealIdent write setRealIdent;
    property Params: TLapeExpressionList read FParams;
  end;

  TLapeTree_InternalMethodClass = class of TLapeTree_InternalMethod;
  TLapeTree_InternalMethod = class(TLapeTree_Invoke)
  private
    FForceParam: Boolean;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    procedure ClearCache; override;
    property ForceParam: Boolean read FForceParam;
  end;

  TLapeTree_InternalMethod_Write = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_WriteLn = class(TLapeTree_InternalMethod_Write)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ToStr = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Assert = class(TLapeTree_InternalMethod)
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_IsScriptMethod = class(TLapeTree_InternalMethod)
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Break = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Continue = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;
  
  TLapeTree_InternalMethod_Operator = class(TLapeTree_InternalMethod)
  public
    constructor Create(AOperator:EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce;
  end;

  TLapeTree_InternalMethod_Exit = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Halt = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_New = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;
  
  TLapeTree_InternalMethod_Dispose = class(TLapeTree_InternalMethod)
  public
    FunctionOnly: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Default = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Swap = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_SizeOf = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Ord = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Low = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_High = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Length = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_SetLength = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Copy = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Delete = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Insert = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Succ = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Pred = class(TLapeTree_InternalMethod_Succ)
  public
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Inc = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Dec = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Label = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Goto = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_Callback = class;
  TLapeTreeCallback = procedure(Self: TLapeTree_Callback) of object;

  TLapeTree_Callback = class(TLapeTree_Base)
  protected
    FCallback: TLapeTreeCallback;
    FData: Pointer;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ACallback: TLapeTreeCallback; AData: Pointer = nil; ADocPos: PDocPos = nil); reintroduce;
    function Compile(var Offset: Integer): TResVar; override;

    property Callback: TLapeTreeCallback read FCallback;
    property Data: Pointer read FData write FData;
  end;

  TLapeTree_Operator = class(TLapeTree_DestExprBase)
  protected
    FRestructure: TInitBool;
    FOperatorType: EOperator;
    FLeft: TLapeTree_Base;
    FRight: TLapeTree_ExprBase;
    FAssigning: TInitBool;

    function getLeft: TLapeTree_ExprBase; virtual;
    procedure setLeft(Node: TLapeTree_ExprBase); virtual;
    procedure setRight(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(AOperatorType: EOperator; ASource: TLapeTree_Base); overload; virtual;
    destructor Destroy; override;

    procedure ClearCache; override;
    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function isAssigning: Boolean; virtual;
    function isConstant: Boolean; override;

    function EvalFlags: ELapeEvalFlags; virtual;
    function resType(Restructure: Boolean): TLapeType; reintroduce; overload;
    function resType: TLapeType; overload; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;

    property OperatorType: EOperator read FOperatorType write FOperatorType;
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
    constructor Create(AValue: Extended; ASource: TLapeTree_Base); overload;
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
    constructor Create(AValue: WideChar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload;
    constructor Create(AValue: WideChar; ASource: TLapeTree_Base); overload;
  end;

  TLapeTree_Range = class(TLapeTree_Base)
  protected
    FLo: TLapeTree_ExprBase;
    FHi: TLapeTree_ExprBase;

    procedure setLo(Node: TLapeTree_ExprBase); virtual;
    procedure setHi(Node: TLapeTree_ExprBase); virtual;
    function getDiff: Int64; virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    property Lo: TLapeTree_ExprBase read FLo write setLo;
    property Hi: TLapeTree_ExprBase read FHi write setHi;
    property Diff: Int64 read getDiff;
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
    FVarList: array of TResVar;
    FBody: TLapeTree_Base;
    procedure setBody(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclRec; virtual;
    function Compile(var Offset: Integer): TResVar; override;

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
    function CompileBody(var Offset: Integer): TResVar; virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property Body: TLapeTree_Base read FBody write setBody;
    property ElseBody: TLapeTree_Base read FElse write setElse;
  end;

  TLapeTree_MultiIf = class(TLapeTree_If)
  protected
    FValues: TLapeStatementList;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(Ident: TLapeTree_ExprBase; OpenArray: TLapeTree_OpenArray; FreeArray: Boolean = True); overload; virtual;
    destructor Destroy; override;

    function addValue(p: TLapeTree_Base): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;
    property Values: TLapeStatementList read FValues;
  end;

  TLapeCaseFieldList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_MultiIf>;
  TLapeTree_Case = class(TLapeTree_Base, ILapeTree_CanContinue)
  protected
    FCondition: TLapeTree_ExprBase;
    FElse: TLapeTree_Base;
    FFields: TLapeCaseFieldList;

    FContinueStatements: TLapeFlowStatementList;
    procedure CompileContinue(Sender: TLapeTree_Callback); virtual;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setElse(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;

    function addField(p: TLapeTree_MultiIf): Integer; virtual;
    function Compile(var Offset: Integer): TResVar; override;

    function addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean; virtual;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property ElseBody: TLapeTree_Base read FElse write setElse;
    property Fields: TLapeCaseFieldList read FFields;
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

    function addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean; virtual;
    function addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean; virtual;
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
  public
    WalkDown: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    property Counter: TLapeTree_ExprBase read FCounter write setCounter;
    property Limit: TLapeTree_ExprBase read FLimit write setLimit;
    property Step: TLapeTree_ExprBase read FStep write setStep;
  end;

  TLapeTree_Repeat = class(TLapeTree_Base, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FCondition: TLapeTree_ExprBase;
    FBody: TLapeTree_Base;
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    destructor Destroy; override;
    function Compile(var Offset: Integer): TResVar; override;

    function addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean; virtual;
    function addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean; virtual;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property Body: TLapeTree_Base read FBody write setBody;
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

    property Body: TLapeTree_Base read FBody write setBody;
    property ExceptBody: TLapeTree_Base read FExcept write setExcept;
    property FinallyBody: TLapeTree_Base read FFinally write setFinally;
  end;

function getFlowStatement(Offset: Integer; Pos: PDocPos = nil; JumpSafe: Boolean = False): TLapeFlowStatement; {$IFDEF Lape_Inline}inline;{$ENDIF}
function EnsureCompound(Compiler: TLapeCompilerBase; Statement: TLapeTree_Base; AllowNil: Boolean = True): TLapeTree_StatementList;
function GetMagicMethodOrNil(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; AResult: TLapeType = nil): TResVar;

const
  NullFlowStatement: TLapeFlowStatement = (CodeOffset: 0; DocPos: (Line: 0; Col: 0; FileName: ''); JumpSafe: False);

implementation

uses
  Math,
  {$IFDEF Lape_NeedAnsiStringsUnit}AnsiStrings,{$ENDIF}
  lpvartypes_ord, lpvartypes_record, lpvartypes_array,
  lpexceptions, lpeval, lpinterpreter;

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
  if (Compiler = nil) then
    Result := NullResVar;

  Method := Compiler[AName];
  if (Method <> nil) and (Method.VarType is TLapeType_OverloadedMethod) then
    Method := TLapeType_OverloadedMethod(Method.VarType).getMethod(getTypeArray(AParams), AResult);

  if (Method = nil) then
    Method := Compiler.addManagedVar(Compiler.getBaseType(ltPointer).NewGlobalVarP()) as TLapeGlobalVar;

  Result := _ResVar.New(Method);
  Result.VarType := Compiler.getBaseType(ltPointer);
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

procedure TLapeTree_Base.setParent(Parent: TLapeTree_Base);
begin
  Assert((Parent = nil) or (Parent.Compiler = FCompiler));
  if (FParent <> nil) and (FParent <> Parent) then
    FParent.DeleteChild(Self);
  FParent := Parent;
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

function TLapeTree_Base.CompileToTempVar(var Offset: Integer; out v: TResVar; Lock: Integer = 0): Boolean;
begin
  v := Compile(Offset).IncLock(Lock);
  Result := (v.VarPos.MemPos <> mpStack) and v.HasType();
end;

class function TLapeTree_Base.isEmpty(Node: TLapeTree_Base): Boolean;
begin
  Result := (Node = nil) or (Node.ClassType = TLapeTree_Base) or (Node.ClassType = TLapeTree_ExprBase);
end;

function TLapeTree_Base.setExpectedType(ExpectType: TLapeType): TLapeTree_Base;
begin
  Result := Self;
end;

function TLapeTree_Base.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
begin
  Result := Self;
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
    LapeException(lpeImpossible);
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
    end;

    if (t <> nil) then
    begin
      if (t.VarType is TLapeTreeType) then
      begin
        Replacement := TLapeTreeType(t.VarType).getDecl();
        FreeAndNil(t.VarType);
        FreeAndNil(t);
      end
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

function TLapeTree_ExprBase.CompileToBoolVar(var Offset: Integer; out ConditionVar: TResVar): Boolean;
var
  tmpVar, tmpCondition: TResVar;
begin
  Result := False;
  tmpVar := NullResVar;
  
  ConditionVar := Compile(Offset);
  if (not ConditionVar.HasType()) or (not (ConditionVar.VarType.BaseType in LapeIfTypes)) then
    Exit;

  if (not (ConditionVar.VarType.Size in [1, 2, 4, 8])) then
    with FCompiler do
    begin
      tmpCondition := ConditionVar;
      if getBaseType(ltEvalBool).CompatibleWith(ConditionVar.VarType) then
        ConditionVar := getBaseType(ltEvalBool).Eval(op_Assign, tmpVar, getTempStackVar(ltEvalBool), ConditionVar, [], Offset, @_DocPos)
      else if (ConditionVar.VarType.BaseType in LapeStringTypes) then
        ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, _ResVar.New(getConstant('')), [], Offset, @_DocPos)
      else
        ConditionVar := ConditionVar.VarType.Eval(op_cmp_NotEqual, tmpVar, ConditionVar, _ResVar.New(getConstant(0)), [], Offset, @_DocPos);
      tmpCondition.Spill(1);
    end;

  Result := True;
end;

procedure TLapeTree_DestExprBase.setDest(ResVar: TResVar);
begin
  FDest.Spill();
  FDest := ResVar;
end;

function TLapeTree_DestExprBase.CompileToTempVar(var Offset: Integer; out v: TResVar; Lock: Integer = 0): Boolean;
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

function TLapeTree_OpenArray.canCast: Boolean;
var
  i: Integer;
  HasRange: Boolean;
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

    HasRange := False;
    CastTo := nil;

    if (FCanCast <> bFalse) then
      if (FType is TLapeType_Set) then
        CastTo := TLapeType_Set(FType).Range
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
          FCanCast := bFalse;
          Break;
        end;

        if (FValues[i] is TLapeTree_ExprBase) and CastTo.CompatibleWith(TLapeTree_ExprBase(FValues[i]).resType()) then
          {nothing}
        else if (FValues[i] is TLapeTree_Range) and
          CastTo.CompatibleWith(TLapeTree_Range(FValues[i]).Lo.resType()) and
          CastTo.CompatibleWith(TLapeTree_Range(FValues[i]).Hi.resType())
        then
          HasRange := True
        else
        begin
          FCanCast := bFalse;
          Break;
        end;
      end;

    if (FCanCast <> bFalse) and (FType is TLapeType_StaticArray) then
      if isConstant() and (FRange.Lo = 0) then
        if (FRange.Hi = TLapeType_StaticArray(FType).Range.Hi - TLapeType_StaticArray(FType).Range.Lo) then
          {nothing}
        else
          FCanCast := bFalse
      else if HasRange then
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
        else if (FValues[i] is TLapeTree_Range) and (TLapeTree_Range(FValues[i]).Diff >= 0) then
          Inc(FRange.Hi, TLapeTree_Range(FValues[i]).Diff)
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
          Res := TLapeTree_Range(FValues[i]).Hi.resType();
          if (Res <> nil) and (Res is TLapeType_SubRange) and ((Result = nil) or Res.CompatibleWith(Result)) then
            Exit(FCompiler.addManagedType(TLapeType_Set.Create(Res as TLapeType_SubRange, FCompiler, '', @_DocPos)))
          else if (TLapeTree_Range(FValues[i]).Diff >= 0) then
            Inc(FRange.Hi, TLapeTree_Range(FValues[i]).Diff)
          else
            FRange.Lo := -1;
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
        if (Result is TLapeType_Enum) then
          Result := FCompiler.addManagedType(TLapeType_Set.Create(TLapeType_Enum(Result), FCompiler, '', @_DocPos))
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
    i, ii: Integer;
    FieldVar, tmpVar, Counter: TLapeGlobalVar;
    CounterInt: Integer;
  begin
    FieldVar := nil;
    tmpVar := nil;
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
        else if (FValues[i] is TLapeTree_Range) then
        try
          tmpVar := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@ii);
          for ii := TLapeTree_Range(FValues[i]).Lo.Evaluate().AsInteger to TLapeTree_Range(FValues[i]).Hi.Evaluate().AsInteger do
          try
            try
              FieldVar := FType.EvalConst(op_Index, Result, Counter, [lefAssigning]);
              FieldVar.VarType.EvalConst(op_Assign, FieldVar, tmpVar, []);
            except on E: lpException do
              LapeException(lpString(E.Message), FValues[i].DocPos);
            end;
          finally
            Inc(CounterInt);
            if (FieldVar <> nil) then
              FreeAndNil(FieldVar);
          end;
        finally
          Dec(CounterInt);
          tmpVar.Free();
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

      Result := FCompiler.addManagedVar(Result) as TLapeGlobalVar;
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
    tmpVar: TResVar;
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
          Left := TLapeTree_ResVar.Create(Result.IncLock(), FValues[i]);
          Right := TLapeTree_ResVar.Create(FValues[i].Compile(Offset), FValues[i]);
          Result := Compile(Offset);
          Assert(Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos);
        finally
          Free();
        end
      else if (FValues[i] is TLapeTree_Range) then
      begin
        if (FType is TLapeType_Set) then
          Counter := FCompiler.getTempVar(TLapeType_Set(FType).Range)
        else
          Counter := FCompiler.getTempVar(ltInt32);
        tmpVar := Counter.VarType.Eval(op_Assign, tmpVar, _ResVar.New(Counter), TLapeTree_Range(FValues[i]).Lo.Compile(Offset), [], Offset, @FValues[i]._DocPos);

        with TLapeTree_For.Create(FValues[i]) do
        try
          Counter := TLapeTree_ResVar.Create(tmpVar.IncLock(), FValues[i]);
          Limit := TLapeTree_ResVar.Create(TLapeTree_Range(FValues[i]).Hi.Compile(Offset), FValues[i]);
          Body := TLapeTree_Operator.Create(op_Plus, FValues[i]);
          with TLapeTree_Operator(Body) do
          begin
            Dest := Result;
            Left := TLapeTree_ResVar.Create(Result.IncLock(), FValues[i]);
            Right := TLapeTree_ResVar.Create(tmpVar.IncLock(), FValues[i]);
          end;
          Result := Compile(Offset);
          Assert(TLapeTree_Operator(Body).Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos);
        finally
          Free();
          tmpVar.Spill(1);
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
          with TLapeTree_Operator(Left) do
          begin
            Left := TLapeTree_ResVar.Create(Result.IncLock(), FValues[i]);
            Right := TLapeTree_Integer.Create(i + TLapeType_StaticArray(FType).Range.Lo, FValues[i]);
          end;
          Right := TLapeTree_ResVar.Create(FValues[i].Compile(Offset), FValues[i]);
          Compile(Offset).Spill(1);
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
            Left := TLapeTree_ResVar.Create(Result.IncLock(), FValues[i]);
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
          Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
          Right := TLapeTree_Field.Create(TLapeType_Record(FType).FieldMap.Key[i], Self);
          Val := Compile(Offset);

          FCompiler.VarToDefault(Val, Offset, @_DocPos);
          Val.Spill(1);
        finally
          Free();
        end;
  end;

begin
  Result := NullResVar;
  if (not canCast()) then
    LapeException(lpeInvalidEvaluation, DocPos);

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

procedure TLapeTree_Invoke.setExpr(Node: TLapeTree_ExprBase);
begin
  if (FExpr <> nil) and (FExpr <> Node) then
  begin
    setRealIdent(nil);
    ClearCache();
    FExpr.Free();
  end;

  FExpr := Node;
  if (Node <> nil) then
    Node.Parent := Self;

  if (not (FExpr is TLapeTree_VarType)) and
     (FExpr is TLapeTree_GlobalVar) and
     (TLapeTree_GlobalVar(FExpr).GlobalVar <> nil) and
     (TLapeTree_GlobalVar(FExpr).GlobalVar.VarType is TLapeType_Type)
  then
    setExpr(TLapeTree_VarType.Create(TLapeType_Type(TLapeTree_GlobalVar(FExpr).GlobalVar.VarType).TType, Expr));
end;

procedure TLapeTree_Invoke.setRealIdent(Node: TLapeTree_ExprBase);
begin
  if (Node <> nil) and (FExpr = nil) then
    setExpr(Node);

  Assert((FExpr <> nil) or (FRealIdent = nil));
  if (FRealIdent <> nil) and (FRealIdent <> Node) and (FRealIdent <> FExpr) then
  begin
    FExpr.Parent := Self;
    FRealIdent.Free();
  end;

  FRealIdent := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

function TLapeTree_Invoke.getRealIdent(ExpectType: TLapeType): TLapeTree_ExprBase;
var
  Typ: TLapeType;
  Index: Integer;
begin
  if (FRealIdent = nil) and (not isEmpty(FExpr)) then
  begin
    Typ := FExpr.resType();

    if (not (FExpr is TLapeTree_VarType)) and (Typ is TLapeType_OverloadedMethod) then
    begin
      Index := TLapeType_OverloadedMethod(Typ).getMethodIndex(getParamTypes(), ExpectType);
      if (Index >= 0) then
      begin
        Result := TLapeTree_Operator.Create(op_Index, FExpr);
        TLapeTree_Operator(Result).Left := FExpr;
        TLapeTree_Operator(Result).Right := TLapeTree_Integer.Create(Index, Result);

        FRealIdent := TLapeTree_ExprBase(Result.FoldConstants(False));
        FExpr := TLapeTree_Operator(Result).Left;

        if (FRealIdent <> Result) then
        begin
          FExpr.Parent := Self;
          Result.Free();
        end;
      end;
    end;

    if (FRealIdent = nil) and (ExpectType = nil) then
      FRealIdent := FExpr;
  end;

  Result := FRealIdent;
end;

function TLapeTree_Invoke.getRealIdent: TLapeTree_ExprBase;
begin
  Result := getRealIdent(nil);
end;

procedure TLapeTree_Invoke.DeleteChild(Node: TLapeTree_Base);
begin
  if (FExpr = Node) then
    FExpr := nil
  else if (FRealIdent = Node) then
    FRealIdent := nil
  else
    FParams.DeleteItem(TLapeTree_ExprBase(Node));
  ClearCache();
end;

function TLapeTree_Invoke.getParamTypes: TLapeTypeArray;
var
  i: Integer;
begin
  SetLength(Result, FParams.Count);
  for i := 0 to FParams.Count - 1 do
    if isEmpty(FParams[i]) then
      Result[i] := nil
    else
      Result[i] := FParams[i].resType();
end;

function TLapeTree_Invoke.getParamTypesStr: lpString;
var
  i: Integer;
  ParamType: TLapeType;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    if isEmpty(FParams[i]) then
      ParamType := nil
    else
      ParamType := FParams[i].resType();
    if (ParamType <> nil) then
      Result := Result + ParamType.AsString
    else
      Result := Result + '*unknown*';
  end;
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  setExpr(Ident);
  FParams := TLapeExpressionList.Create(nil, dupAccept, False);
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeTree_ExprBase; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(Ident, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Invoke.Create(Ident: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  Create(TLapeTree_GlobalVar.Create(ACompiler[Ident], ACompiler, ADocPos), ACompiler, ADocPos);
end;

constructor TLapeTree_Invoke.Create(Ident: lpString; ASource: TLapeTree_Base);
begin
  Assert((ASource <> nil) and (ASource.Compiler <> nil));
  Create(TLapeTree_GlobalVar.Create(ASource.Compiler[Ident], ASource), ASource);
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeGlobalVar; ASource: TLapeTree_Base);
begin
  Create(TLapeTree_GlobalVar.Create(Ident, ASource), ASource);
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeType; ASource: TLapeTree_Base);
begin
  Create(TLapeTree_VarType.Create(Ident, ASource), ASource);
end;

destructor TLapeTree_Invoke.Destroy;
var
  i: Integer;
begin
  setExpr(nil);
  setRealIdent(nil);

  for i := FParams.Count - 1 downto 0 do
    if (FParams[i] <> nil) and (FParams[i].Parent = Self) then
      FParams[i].Free();
  FreeAndNil(FParams);

  inherited;
end;

procedure TLapeTree_Invoke.ClearCache;
begin
  setRealIdent(nil);
  inherited;
end;

function TLapeTree_Invoke.addParam(p: TLapeTree_ExprBase): Integer;
begin
  Result := FParams.Add(p);
  ClearCache();
  if (p <> nil) then
    p.Parent := Self;
end;

function TLapeTree_Invoke.setExpectedType(ExpectType: TLapeType): TLapeTree_Base;
begin
  if (ExpectType <> nil) then
    setRealIdent(nil);
  getRealIdent(ExpectType);
  Result := Self;
end;

function TLapeTree_Invoke.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (RealIdent is TLapeTree_VarType) then
      if (FParams.Count = 1) and (not isEmpty(FParams[0])) and FParams[0].isConstant() then
        FConstant := bTrue
      else
        FConstant := bFalse
    else
      FConstant := bFalse;

  Result := inherited;
end;

function TLapeTree_Invoke.resType: TLapeType;
var
  Typ: TLapeType;
  IdentExpr: TLapeTree_ExprBase;
begin
  if (FResType = nil) then
  begin
    IdentExpr := RealIdent;

    if (not isEmpty(IdentExpr)) then
      if (IdentExpr is TLapeTree_VarType) then
        FResType := TLapeTree_VarType(IdentExpr).VarType
      else
      begin
        Typ := IdentExpr.resType();
        if (Typ is TLapeType_Method) then
          FResType := TLapeType_Method(Typ).Res;
      end;
  end;

  Result := inherited;
end;

function TLapeTree_Invoke.Evaluate: TLapeGlobalVar;
var
  IdentExpr: TLapeTree_ExprBase;
  IdentVar: TLapeGlobalVar;

  function DoCast: TLapeGlobalVar;
  begin
    Result := nil;
    Assert(IdentExpr is TLapeTree_VarType);

    with TLapeTree_VarType(IdentExpr) do
    try
      if (FParams.Count <> 1) or (VarType = nil) or isEmpty(FParams[0]) then
        LapeException(lpeInvalidCast);
      FParams[0] := FParams[0].setExpectedType(VarType) as TLapeTree_ExprBase;

      Result := FParams[0].Evaluate();
      if VarType.Equals(Result.VarType) or (not Result.HasType()) or (VarType.Size = Result.VarType.Size) then
        Result := TLapeGlobalVar(FCompiler.addManagedVar(VarType.NewGlobalVarP(Result.Ptr), Result.Name <> ''))
      else if VarType.CompatibleWith(Result.VarType) then
        Result := TLapeGlobalVar(FCompiler.addManagedVar(VarType.EvalConst(op_Assign, VarType.NewGlobalVarP(), Result, [])))
      else
        LapeException(lpeInvalidCast);
    except on E: lpException do
      LapeException(lpString(E.Message), Self.DocPos);
    end;
  end;

  function DoImportedMethod(IdentVar: TLapeGlobalVar): TLapeGlobalVar;
  var
    i, Index: Integer;
    Par: TLapeGlobalVar;
    ParamVars: array of Pointer;
  begin
    Assert(IdentVar <> nil);
    Assert(IdentVar.VarType.BaseType = ltImportedMethod);
    Result := nil;

    with TLapeType_Method(IdentVar.VarType) do
    begin
      if (Res = nil) then
        LapeException(lpeCannotEvalConstProc, IdentExpr.DocPos);

      for i := 1 to ImplicitParams do
        FParams.Insert(nil, 0);

      if (FParams.Count > Params.Count) then
        if (FParams.Count > 0) then
          LapeException(lpeTooMuchParameters, FParams[Params.Count].DocPos)
        else
          LapeException(lpeTooMuchParameters, Self.DocPos);

      SetLength(ParamVars, FParams.Count + 1);
      Index := 0;

      if (IdentVar.VarType is TLapeType_MethodOfObject) then
      begin
        ParamVars[0] := TMethod(IdentVar.Ptr^).Data;
        Inc(Index);
      end;

      for i := 0 to FParams.Count - 1 do
      begin
        if (i > FParams.Count) or isEmpty(FParams[i]) then
          if (Params[i].Default <> nil) and (Params[i].Default is TLapeGlobalVar) then
            Par := Params[i].Default as TLapeGlobalVar
          else
            LapeExceptionFmt(lpeNoDefaultForParam, [i + 1 - ImplicitParams], [FParams[i], Self])
        else
        begin
          FParams[i] := FParams[i].setExpectedType(Params[i].VarType) as TLapeTree_ExprBase;
          Par := FParams[i].Evaluate();
        end;

        if (Par = nil) or (not (Params[i].ParType in Lape_ValParams)) then
          LapeException(lpeCannotInvoke, FParams[i].DocPos);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(Par.VarType)) then
          if Params[i].VarType.CompatibleWith(Par.VarType) then
          try
            Par := TLapeGlobalVar(FCompiler.addManagedVar(Params[i].VarType.EvalConst(op_Assign, Params[i].VarType.NewGlobalVarP(), Par, [])));
          except on E: lpException do
            LapeException(lpString(E.Message), FParams[i].DocPos);
          end
          else
            LapeExceptionFmt(lpeVariableOfTypeExpected, [Params[i].VarType.AsString, Par.VarType.AsString], FParams[i].DocPos);

        ParamVars[Index] := Par.Ptr;
        Inc(Index);
      end;

      Result := Res.NewGlobalVarP();
      TLapeImportedFunc(IdentVar.Ptr^)(@ParamVars[0], Result.Ptr);
      Result := TLapeGlobalVar(FCompiler.addManagedVar(Result));
    end;
  end;

begin
  IdentExpr := RealIdent;
  if (FRes = nil) and (IdentExpr <> nil) then
    if (IdentExpr is TLapeTree_VarType) then
      FRes := DoCast()
    else
    begin
      IdentVar := IdentExpr.Evaluate();

      if (IdentVar = nil) or (IdentVar.Ptr = nil) or
         (not IdentVar.HasType()) or
         (IdentVar.VarType.BaseType <> ltImportedMethod)
      then
        if (IdentVar <> nil) and IdentVar.HasType() and (IdentVar.VarType is TLapeType_OverloadedMethod) then
          LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], IdentExpr.DocPos)
        else
          LapeException(lpeCannotInvoke, DocPos);

      FRes := DoImportedMethod(IdentVar);
    end;
  Result := inherited;
end;

function TLapeTree_Invoke.Compile(var Offset: Integer): TResVar;
type
  TResVarArray = array of TResVar;
var
  i: Integer;
  IdentExpr: TLapeTree_ExprBase;
  IdentVar: TResVar;
  ParamVars: TResVarArray;

  function DoCast: TResVar;
  var
    tmpVar, DestVar, tmpRes: TResVar;
  begin
    Assert(IdentExpr is TLapeTree_VarType);
    Result := NullResVar;
    tmpVar := NullResVar;
    DestVar := NullResVar;

    with TLapeTree_VarType(IdentExpr) do
    try
      if (FParams.Count <> 1) or (VarType = nil) or isEmpty(FParams[0]) then
        LapeException(lpeInvalidCast);

      if (FParams[0] is TLapeTree_DestExprBase) then
        TLapeTree_DestExprBase(FParams[0]).Dest := Dest;
      FParams[0] := FParams[0].setExpectedType(VarType) as TLapeTree_ExprBase;

      Result := FParams[0].Compile(Offset);
      if VarType.Equals(Result.VarType) or (not Result.HasType()) or (VarType.Size = Result.VarType.Size) then
      begin
        if (not (FParams[0] is TLapeTree_DestExprBase)) or (TLapeTree_DestExprBase(FParams[0]).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          Dest := NullResVar;
        Result.VarType := VarType;
      end
      else if VarType.CompatibleWith(Result.VarType) then
      begin
        if (FDest.VarPos.MemPos <> NullResVar.VarPos.MemPos) and FDest.HasType() and VarType.Equals(FDest.VarType) then
          DestVar := FDest
        else
        begin
          Dest := NullResVar;
          DestVar := _ResVar.New(Compiler.getTempVar(VarType));
        end;

        tmpRes := Result;
        Result := DestVar.VarType.Eval(op_Assign, tmpVar, DestVar, Result, [], Offset, @Self._DocPos);
        tmpRes.Spill(1);

        if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          Result.isConstant := True;
      end
      else
        LapeException(lpeInvalidCast);
    except on E: lpException do
      LapeException(lpString(E.Message), Self.DocPos);
    end;
  end;

  function AssignToStack(AVar: TResVar; DocPos: TDocPos; Addr: Boolean = True): TResVar;
  var
    tmpVar: TResVar;
  begin
    if (AVar.VarPos.MemPos = mpStack) then
      Exit(AVar);
    if (AVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) and ((AVar.VarType <> nil) or Addr) then
    begin
      Result := NullResVar;
      tmpVar := NullResVar;

      Result.VarPos.MemPos := mpStack;
      if Addr then
      begin
        Result.VarType := FCompiler.getBaseType(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, AVar, NullResVar, Offset, @DocPos);
      end
      else
      begin
        if AVar.HasType() and AVar.VarType.NeedInitialization then
          FCompiler.Emitter._InitStack(AVar.VarType.Size, Offset, @DocPos);
        Result.VarType := AVar.VarType;
        Result := AVar.VarType.Eval(op_Assign, tmpVar, Result, AVar, [], Offset, @DocPos);
      end;
    end
    else
      LapeException(lpeImpossible, DocPos);
  end;

  procedure AssignToTempVar(var ParamVar: TResVar; Param: TLapeParameter; MemPos: EMemoryPos; DocPos: TDocPos);
  var
    Par, tmpVar, tmpRes: TResVar;
  begin
    if (Param.ParType in Lape_ValParams) and (Param.VarType <> nil) and Param.VarType.CompatibleWith(ParamVar.VarType) then
    try
      tmpVar := NullResVar;
      Par := NullResVar;

      Par.VarPos.MemPos := MemPos;
      Par.VarType := Param.VarType;
      if (MemPos = mpVar) then
        Par.VarPos.StackVar := Compiler.getTempVar(Par.VarType)
      else if (MemPos = mpStack) and Par.HasType() and Par.VarType.NeedInitialization then
        FCompiler.Emitter._InitStack(Par.VarType.Size, Offset, @DocPos);
      Par.setConstant(False, False);

      tmpRes := ParamVar;
      ParamVar := Param.VarType.Eval(op_Assign, tmpVar, Par, ParamVar, [], Offset, @DocPos);
      tmpRes.Spill(1);
    except on E: lpException do
      LapeException(lpString(E.Message), DocPos);
    end
    else
      LapeExceptionFmt(lpeVariableOfTypeExpected, [Param.VarType.AsString, ParamVar.VarType.AsString], DocPos);
  end;

  function DoScriptMethod(IdentVar: TResVar; var ParamVars: TResVarArray): TResVar;
  var
    i: Integer;

    function getStackVar(Node: TLapeTree_Base; var Offs: Integer): TResVar;

      function CanStack(t: TLapeType): Boolean;
      begin
        Result := (t <> nil) and (t.BaseType in LapeStackTypes);
      end;

    begin
      if (Node is TLapeTree_DestExprBase) and CanStack(TLapeTree_DestExprBase(Node).resType()) then
        TLapeTree_DestExprBase(Node).Dest := StackResVar;
      Result := Node.Compile(Offs);
      if (Result.VarPos.MemPos = mpVar) and (Result.VarPos.StackVar is TLapeStackTempVar) then
        with TLapeStackTempVar(Result.VarPos.StackVar) do
          if (not Result.VarPos.isPointer) and Locked and (not NeedFinalization) then
          begin
            DecLock();
            if (not Locked) then
              Result := FCompiler.PopVarToStack(Result, Offs, @Node._DocPos)
            else
              IncLock();
          end;
    end;

  begin
    Assert(IdentVar.VarType is TLapeType_Method);
    Assert(IdentVar.VarType.BaseType in [ltPointer, ltScriptMethod]);
    Assert(Length(ParamVars) = TLapeType_Method(IdentVar.VarType).Params.Count);
    Result := NullResVar;

    with TLapeType_Method(IdentVar.VarType) do
    begin
      //if ParamInitialization then
      //  FCompiler.Emitter._InitStack(ParamSize, Offset, @Self._DocPos);

      for i := 0 to Params.Count - 1 do
      try
        if (ParamVars[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          if  (Params[i].ParType in Lape_RefParams) or
             ((Params[i].VarType <> nil) and (Params[i].VarType.NeedFinalization))
          then
            FParams[i].CompileToTempVar(Offset, ParamVars[i])
          else
            ParamVars[i] := getStackVar(FParams[i], Offset);

        if (ParamVars[i].VarPos.MemPos = NullResVar.VarPos.MemPos) or
           ((Params[i].VarType <> ParamVars[i].VarType) and (not ParamVars[i].HasType()))
        then
          LapeException(lpeCannotInvoke, [FParams[i], Self]);

        if (Params[i].ParType in Lape_RefParams) then
        begin
          if (not (Params[i].ParType in Lape_ValParams)) and (not ParamVars[i].Writeable) then
            LapeException(lpeVariableExpected)
          else if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(ParamVars[i].VarType)) then
            AssignToTempVar(ParamVars[i], Params[i], mpVar, Self._DocPos);
          AssignToStack(ParamVars[i], Self._DocPos);
        end
        else if (Params[i].VarType <> nil) and ((ParamVars[i].VarPos.MemPos <> mpStack) or (not Params[i].VarType.Equals(ParamVars[i].VarType))) then
        begin
          AssignToTempVar(ParamVars[i], Params[i], mpStack, Self._DocPos);
          if (ParamVars[i].VarPos.MemPos <> mpStack) or (not ParamVars[i].HasType()) then
            LapeException(lpeCannotInvoke);
        end;
      except on E: lpException do
        LapeException(lpString(E.Message), [FParams[i], Self]);
      end;

      Dest := NullResVar;
      if (Res <> nil) then
      begin
        Result := _ResVar.New(FCompiler.getTempVar(Res));

        if (lcoAlwaysInitialize in FCompilerOptions) then
          FCompiler.VarToDefault(Result, Offset, @Self._DocPos);

        Result.isConstant := True;
        AssignToStack(Result, Self._DocPos);
      end;

      FCompiler.Emitter._IncCall(IdentVar, ParamSize, Offset, @Self._DocPos);
    end;
  end;

  function DoImportedMethod(IdentVar: TResVar; var ParamVars: TResVarArray): TResVar;
  var
    i: Integer;
    {$IFDEF Lape_InitExternalFuncResult}
    wasConstant: Boolean;
    {$ENDIF}
  begin
    Assert(IdentVar.VarType is TLapeType_Method);
    Assert(IdentVar.VarType.BaseType in [ltPointer, ltImportedMethod]);
    Assert(Length(ParamVars) = TLapeType_Method(IdentVar.VarType).Params.Count);
    Result := NullResVar;

    with TLapeType_Method(IdentVar.VarType) do
    begin
      for i := 0 to Params.Count - 1 do
      begin
        if (ParamVars[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
          FParams[i].CompileToTempVar(Offset, ParamVars[i]);

        if (ParamVars[i].VarPos.MemPos = mpStack) or
           ((Params[i].VarType <> ParamVars[i].VarType) and (not ParamVars[i].HasType()))
        then
          LapeException(lpeCannotInvoke, [FParams[i], Self])
        else if (not (Params[i].ParType in Lape_ValParams)) and (not ParamVars[i].Writeable) then
          LapeException(lpeVariableExpected, [FParams[i], Self]);

        if (Params[i].VarType <> nil) and (not Params[i].VarType.Equals(ParamVars[i].VarType)) then
          AssignToTempVar(ParamVars[i], Params[i], mpVar, Self._DocPos);

        AssignToStack(ParamVars[i], Self._DocPos);
      end;

      i := Params.Count;
      if (IdentVar.VarType is TLapeType_MethodOfObject) then
        Inc(i);

      if (Res = nil) then
      begin
        Dest := NullResVar;
        FCompiler.Emitter._InvokeImportedProc(IdentVar, i * SizeOf(Pointer), Offset, @Self._DocPos)
      end
      else
      begin
        Result.VarType := Res;
        if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          FDest := VarResVar;
        FCompiler.getDestVar(FDest, Result, op_Unknown);

        {$IFDEF Lape_InitExternalFuncResult}
        wasConstant := not Result.Writeable;
        if wasConstant then
          Result.Writeable := True;
        FCompiler.VarToDefault(Result, Offset, @Self._DocPos);
        if wasConstant then
          Result.Writeable := False;
        {$ENDIF}

        FCompiler.Emitter._InvokeImportedFunc(IdentVar, Result, i * SizeOf(Pointer), Offset, @Self._DocPos)
      end;
    end;
  end;

  function DoCombiMethod(IdentVar: TResVar; var ParamVars: TResVarArray): TResVar;
  var
    i, o_if, o_else: Integer;
    IsScriptMethod, tmpDest, tmpRes: TResVar;
    ParamsCopy: TResVarArray;
  begin
    with TLapeTree_InternalMethod_IsScriptMethod.Create(Self) do
    try
      addParam(TLapeTree_ResVar.Create(IdentVar.IncLock(), Self));
      IsScriptMethod := Compile(Offset);
    finally
      Free();
    end;

    FCompiler.Emitter.FullEmit := False;
    o_if := FCompiler.Emitter._JmpRIfNot(0, IsScriptMethod, Offset, @_DocPos);
    FCompiler.Emitter.FullEmit := True;

    ParamsCopy := Copy(ParamVars);
    tmpRes := DoScriptMethod(IdentVar, ParamsCopy);
    tmpDest := FDest;
    FDest := tmpRes;
    for i := 0 to High(ParamVars) do
      ParamsCopy[i].Spill(1);

    o_else := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
    FCompiler.Emitter._JmpRIfNot(Offset - o_if, IsScriptMethod, o_if, @_DocPos);
    Result := DoImportedMethod(IdentVar, ParamVars);
    FCompiler.Emitter._JmpR(Offset - o_else, o_else, @_DocPos);
    FDest := tmpDest;

    IsScriptMethod.Spill(1);
    Assert(Result.VarPos.MemPos = tmpRes.VarPos.MemPos);
  end;

begin
  Result := NullResVar;
  IdentExpr := RealIdent;
  Assert(IdentExpr <> nil);

  if (IdentExpr is TLapeTree_VarType) then
    Result := DoCast()
  else
  begin
    IdentVar := IdentExpr.Compile(Offset);

    if (not IdentVar.HasType()) or
       (not (IdentVar.VarType is TLapeType_Method))
    then
      if IdentVar.HasType() and (IdentVar.VarType is TLapeType_OverloadedMethod) then
        LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], IdentExpr.DocPos)
      else
        LapeException(lpeCannotInvoke, IdentExpr.DocPos);

    {$IFDEF Lape_InitExternalFuncResult}
    Dest := NullResVar;
    {$ENDIF}

    with TLapeType_Method(IdentVar.VarType) do
    begin
      for i := 1 to ImplicitParams do
        FParams.Insert(nil, 0);

      if (FParams.Count > Params.Count) then
        if (FParams.Count > 0) then
          LapeException(lpeTooMuchParameters, FParams[Params.Count].DocPos)
        else
          LapeException(lpeTooMuchParameters, Self.DocPos);

      if (IdentVar.VarType is TLapeType_MethodOfObject) and (IdentVar.VarPos.MemPos <> mpStack) then
      begin
        Result := IdentVar;
        Result.VarType := FCompiler.getBaseType(ltPointer);

        Result.IncOffset(SizeOf(Pointer));
        AssignToStack(Result, IdentExpr.DocPos, False);
        Result.DecOffset(SizeOf(Pointer));
      end;

      SetLength(ParamVars, Params.Count);
      for i := 0 to Params.Count - 1 do
        if (i >= FParams.Count) or isEmpty(FParams[i]) then
          if (Params[i].Default <> nil) then
          begin
            ParamVars[i] := _ResVar.New(Params[i].Default);
            if (not (Params[i].ParType in Lape_ValParams)) and (not ParamVars[i].Writeable) then
              LapeException(lpeVariableExpected, [FParams[i], Self]);
          end
          else
            LapeExceptionFmt(lpeNoDefaultForParam, [i + 1 - ImplicitParams], [FParams[i], Self])
        else
        begin
          FParams[i] := FParams[i].setExpectedType(Params[i].VarType) as TLapeTree_ExprBase;
          ParamVars[i] := NullResVar;
        end;

      if (IdentVar.VarType.BaseType = ltScriptMethod) then
        Result := DoScriptMethod(IdentVar, ParamVars)
      else if (IdentVar.VarType.BaseType = ltImportedMethod) then
        Result := DoImportedMethod(IdentVar, ParamVars)
      else
        Result := DoCombiMethod(IdentVar, ParamVars);
    end;

    IdentVar.Spill(1);
    for i := 0 to High(ParamVars) do
      ParamVars[i].Spill(1);
  end;
end;

constructor TLapeTree_InternalMethod.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(TLapeTree_ExprBase(nil), ACompiler, ADocPos);
  FConstant := bFalse;
  FForceParam := False;
end;

procedure TLapeTree_InternalMethod.ClearCache;
begin
  {nothing}
end;

constructor TLapeTree_InternalMethod_Write.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
var
  _Write: TLapeGlobalVar;
begin
  inherited;
  FForceParam := True;

  _Write := ACompiler['_Write'];
  if (_Write <> nil) and (_Write.VarType is TLapeType_OverloadedMethod) then
    _Write := _Write.VarType.ManagedDeclarations[0] as TLapeGlobalVar;

  Assert(_Write <> nil);
  setExpr(TLapeTree_GlobalVar.Create(_Write, Self));
end;

function TLapeTree_InternalMethod_Write.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  TempParams: TLapeExpressionList;
begin
  Assert(RealIdent <> nil);
  if (FParams.Count < 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  TempParams := FParams;
  FParams := TLapeExpressionList.Create(nil, dupAccept, False);
  FParams.Add(TempParams[0]);
  try
    for i := 0 to TempParams.Count - 1 do
    begin
      FParams[0] := TempParams[i];

      if (not isEmpty(TempParams[i])) and (TempParams[i].resType() <> nil) and (not (TempParams[i].resType().BaseType in LapeStringTypes)) then
      begin
        TempParams[i] := TLapeTree_Invoke.Create('ToString', FParams[0]);
        TempParams[i].Parent := Self;
        TLapeTree_Invoke(TempParams[i]).addParam(FParams[0]);
        FParams.Add(TempParams[i]);
      end;

      Result := inherited;
    end;
  finally
    FParams.Free();
    FParams := TempParams;
  end;
end;

function TLapeTree_InternalMethod_WriteLn.Compile(var Offset: Integer): TResVar;
var
  _WriteLn: TLapeGlobalVar;
begin
  if (FParams.Count > 0) then
    Result := inherited
  else
    Result := NullResvar;

  _WriteLn := FCompiler['_WriteLn'];
  if (_WriteLn <> nil) and (_WriteLn.VarType is TLapeType_OverloadedMethod) then
    _WriteLn := _WriteLn.VarType.ManagedDeclarations[0] as TLapeGlobalVar;
  Assert(_WriteLn <> nil);

  with TLapeTree_Invoke.Create(_WriteLn, Self) do
  try
    Compile(Offset).Spill(1);
  finally
    Free();
  end;
end;

constructor TLapeTree_InternalMethod_ToStr.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltString);
end;

function TLapeTree_InternalMethod_ToStr.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  Res, Par: TLapeTree_ExprBase;
begin
  Result := NullResVar;
  Res := nil;
  if (FParams.Count < 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  for i := 0 to FParams.Count - 1 do
    if (not isEmpty(FParams[i])) and (FParams[i].resType() <> nil) and (FParams[i].resType().BaseType <> ltString) then
    begin
      Par := FParams[i];
      FParams[i] := TLapeTree_Invoke.Create('ToString', FParams[i]);
      TLapeTree_Invoke(FParams[i]).addParam(Par);
      FParams[i].Parent := Self;
    end;

  try
  if (FParams.Count < 2) then
    Res := FParams[0]
  else
    for i := 0 to FParams.Count - 2 do
    begin
      Par := TLapeTree_Operator.Create(op_Plus, Self);
      if (Res <> nil) then
        TLapeTree_Operator(Par).Left := Res
      else
        TLapeTree_Operator(Par).FLeft := FParams[i];
      TLapeTree_Operator(Par).FRight := FParams[i + 1];
      Res := Par;
    end;

    if (Res is TLapeTree_DestExprBase) then
      TLapeTree_DestExprBase(Res).Dest := FDest
    else
      Dest := NullResVar;

    Result := Res.Compile(Offset);
    if (Res is TLapeTree_DestExprBase) then
      FDest := TLapeTree_DestExprBase(Res).Dest;
  finally
    if (FParams.Count >= 2) and (Res <> nil) then
      Res.Free();
  end;
end;

constructor TLapeTree_InternalMethod_Assert.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create('_Assert', ACompiler, ADocPos);
  FForceParam := True;
end;

function TLapeTree_InternalMethod_Assert.Compile(var Offset: Integer): TResVar;
begin
  if (not (lcoAssertions in FCompilerOptions)) then
    Exit;
  Result := inherited;
end;

constructor TLapeTree_InternalMethod_IsScriptMethod.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltEvalBool);
end;

function TLapeTree_InternalMethod_IsScriptMethod.Compile(var Offset: Integer): TResVar;
var
  tmpVar, DestVar, Param: TResVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  tmpVar := NullResVar;
  Param := FParams[0].Compile(Offset);
  if (not Param.HasType()) or (not (Param.VarType is TLapeType_Method)) then
    LapeException(lpeInvalidCondition, DocPos);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;

  Param.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType := Param.VarType;

  Param.VarType.Eval(op_Assign, tmpVar, DestVar, Param, [], Offset, @_DocPos);
  Param.Spill(1);

  FCompiler.Emitter._IsInternal(Offset, @_DocPos);
  Result.VarPos.MemPos := mpStack;
  Result.VarType := resType();

  if (FDest.VarPos.MemPos = mpVar) and ((not FDest.HasType()) or FDest.VarType.Equals(Result.VarType)) then
  begin
    if (not FDest.HasType()) then
    begin
      Dest := _ResVar.New(Compiler.getTempVar(Result.VarType));
      Dest.isConstant := True;
    end;

    FCompiler.Emitter._PopStackToVar(Result.VarType.Size, FDest.VarPos.StackVar.Offset, Offset, @_DocPos);
    Result := FDest;
  end
  else
    Dest := NullResVar;
end;

function TLapeTree_InternalMethod_Break.Compile(var Offset: Integer): TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanBreak;
  BreakDepth, BreakCount: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;
  Node := FParent;

  if (not (FParams.Count in [0, 1])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) or isEmpty(FParams[0]) then
    BreakDepth := 1
  else
    with FParams[0].Evaluate() do
    begin
      BreakDepth := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0].DocPos)
      else if (BreakDepth < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0].DocPos);
    end;

  BreakCount := 1;
  JumpSafe := False;
  while (Node <> nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanBreak, FoundNode) = 0) then
      if (BreakCount < BreakDepth) or (not FoundNode.addBreakStatement(JumpSafe, Offset, @_DocPos)) then
        Inc(BreakCount)
      else
      begin
        FoundNode := nil;
        Break;
      end
    else if (Node is TLapeTree_Try) then
      JumpSafe := True;
    Node := Node.Parent;
  end;

  if (Node = nil) then
    if (BreakCount < BreakDepth) then
      LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
    else
      LapeException(lpeCannotBreak, DocPos);
end;

function TLapeTree_InternalMethod_Continue.Compile(var Offset: Integer): TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanContinue;
  ContinueDepth, ContinueCount: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;
  Node := FParent;

  if (not (FParams.Count in [0, 1])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) or isEmpty(FParams[0]) then
    ContinueDepth := 1
  else
    with FParams[0].Evaluate() do
    begin
      ContinueDepth := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0]._DocPos)
      else if (ContinueDepth < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0]._DocPos);
    end;

  ContinueCount := 1;
  JumpSafe := False;
  while (Node <> nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanContinue, FoundNode) = 0) then
      if (ContinueCount < ContinueDepth) or (not FoundNode.addContinueStatement(JumpSafe, Offset, @_DocPos)) then
        Inc(ContinueCount)
      else
      begin
        FoundNode := nil;
        Break;
      end
    else if (Node is TLapeTree_Try) then
      JumpSafe := True;
    Node := Node.Parent;
  end;

  if (Node = nil) then
    if (ContinueCount < ContinueDepth) then
      LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
    else
      LapeException(lpeCannotContinue, DocPos);
end;

constructor TLapeTree_InternalMethod_Operator.Create(AOperator:EOperator; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create('!op_'+op_name[AOperator], ACompiler, ADocPos);
end;

function TLapeTree_InternalMethod_Exit.Compile(var Offset: Integer): TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanExit;
  ResultDecl: TLapeDeclaration;
begin
  Result := NullResVar;
  Dest := NullResVar;
  Node := FParent;

  if (FParams.Count <> 0) then
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos);

  if (FParams.Count = 1) then
    with TLapeTree_Operator.Create(op_Assign, FParams[0]) do
    try
      ResultDecl := FCompiler.getDeclaration('Result');
      if (ResultDecl = nil) or (not (ResultDecl is TLapeParameterVar)) then
        LapeExceptionFmt(lpeWrongNumberParams, [0], DocPos);
      Left := TLapeTree_ResVar.Create(_ResVar.New(ResultDecl as TLapeVar), FParams[0]);
      Right := TLapeTree_ResVar.Create(FParams[0].Compile(Offset), FParams[0]);
      Compile(Offset);
    finally
      Free();
    end;

  while (Node <> nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanExit, FoundNode) = 0) then
    begin
      FoundNode.addExitStatement(True, Offset, @_DocPos);
      FoundNode := nil;
      Break;
    end;
    Node := Node.Parent;
  end;

  if (Node = nil) then
    FCompiler.Emitter._JmpSafe(EndJump, Offset, @_DocPos);
end;

function TLapeTree_InternalMethod_Halt.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 0) then
    LapeException(lpeTooMuchParameters, DocPos);

  FCompiler.Emitter._JmpSafe(EndJump, Offset, @_DocPos);
end;

constructor TLapeTree_InternalMethod_New.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FForceParam := True;
end;

function TLapeTree_InternalMethod_New.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
  VarType: TLapeType;
  IsPointer: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile(Offset);
  VarType := Param.VarType;
  IsPointer := (VarType <> nil) and (VarType.BaseType = ltPointer);

  if IsPointer then
    if (VarType is TLapeType_Pointer) then
      VarType := TLapeType_Pointer(VarType).PType
    else
      LapeException(lpeImpossible, _DocPos);

  if (VarType = nil) or (not Param.Writeable) then
    LapeException(lpeVariableExpected, [FParams[0], Self]);

  with TLapeTree_Operator.Create(op_Assign, Self) do
  try
    Left := TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]);

    if isPointer then
    begin
      Right := TLapeTree_Invoke.Create('AllocMem', Self);
      TLapeTree_Invoke(Right).addParam(TLapeTree_Integer.Create(VarType.Size, Self.FParams[0]));
    end
    else
      Right := TLapeTree_GlobalVar.Create(VarType.NewGlobalVarP(), Self.FParams[0]);

    Compile(Offset);
  finally
    Free();
  end;

  Param.Spill(1);
end;

constructor TLapeTree_InternalMethod_Dispose.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FForceParam := True;
  FunctionOnly := False;
end;

function TLapeTree_InternalMethod_Dispose.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
  VarType: TLapeType;
  IsPointer: Boolean;
  _Dispose: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile(Offset);
  VarType := Param.VarType;
  IsPointer := (not FunctionOnly) and (VarType <> nil) and (VarType.BaseType = ltPointer);

  if IsPointer then
    if (VarType is TLapeType_Pointer) then
      VarType := TLapeType_Pointer(VarType).PType
    else
      LapeException(lpeImpossible, _DocPos);

  if ((VarType = nil) and (not IsPointer)) or (not Param.Writeable) then
    LapeException(lpeVariableExpected, [FParams[0], Self]);

  _Dispose := FCompiler['_Dispose'];
  Assert((_Dispose <> nil) and (_Dispose.VarType is TLapeType_OverloadedMethod));
  _Dispose := TLapeType_OverloadedMethod(_Dispose.VarType).getMethod(getTypeArray([VarType]));

  if (_Dispose <> nil) then
    with TLapeTree_Invoke.Create(_Dispose, Self) do
    try
      if IsPointer then
      begin
        addParam(TLapeTree_Operator.Create(op_Deref, Self.FParams[0]));
        TLapeTree_Operator(Params[0]).Left := TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]);
      end
      else
        addParam(TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]));
      Compile(Offset).Spill(1);
    finally
      Free();
    end;

  if (not FunctionOnly) then
    if isPointer then
      with TLapeTree_Invoke.Create('FreeMem', Self) do
      try
        addParam(TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]));
        Compile(Offset).Spill(1);
      finally
        Free();
      end
    else if (_Dispose = nil) then
      FCompiler.VarToDefault(Param, Offset, @_DocPos);

  Param.Spill(1);
end;

procedure TLapeTree_InternalMethod_Default.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_Default.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) and FParams[0].isConstant() then
      FConstant := bTrue
    else
      FConstant := bFalse;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Default.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      FResType := FParams[0].resType();
      if (FResType <> nil) and (FResType is TLapeType_Type) then
        FResType := TLapeType_Type(FResType).TType;
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Default.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) and isConstant() then
  begin
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (ParamType = nil) or (not (ParamType is TLapeType_Type)) then
      LapeException(lpeTypeExpected, DocPos)
    else
      ParamType := TLapeType_Type(ParamType).TType;

    FRes := TLapeGlobalVar(FCompiler.addManagedDecl(ParamType.NewGlobalVarP()));
  end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Default.Compile(var Offset: Integer): TResVar;
begin
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Result := FParams[0].Compile(Offset);
  if (not Result.HasType()) or (Result.VarType is TLapeType_Type) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (not Result.Writeable) then
    LapeException(lpeVariableExpected, DocPos);

  FCompiler.VarToDefault(Result, Offset, @_DocPos);
end;

constructor TLapeTree_InternalMethod_Swap.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create('_swap', ACompiler, ADocPos);
end;

function TLapeTree_InternalMethod_Swap.Compile(var Offset: Integer): TResVar;
var
  Type1, Type2: TLapeType;
begin
  Result := NullResVar;
  if (FParams.Count <> 2) or isEmpty(FParams[0]) or isEmpty(FParams[1]) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);

  Type1 := FParams[0].resType();
  Type2 := FParams[1].resType();

  if ((Type1 <> nil) <> (Type2 <> nil)) or
      (Type1 <> nil) and (
       (Type1.Size = 0) or
       (Type1.Size <> Type2.Size) or
       (not Type1.Equals(Type2)))
  then
    LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], [Self]);

  addParam(TLapeTree_Integer.Create(Type1.Size, Self));
  try
    inherited;
  finally
    FParams.Delete(2).Free();
  end;
end;

constructor TLapeTree_InternalMethod_SizeOf.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FConstant := bTrue;
  FResType := ACompiler.getBaseType(ltInt32);
end;

function TLapeTree_InternalMethod_SizeOf.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
  begin
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType = nil) then
      LapeException(lpeInvalidEvaluation, DocPos);

    FRes := FCompiler.getConstant(ParamType.Size, ltInt32, False, True);
  end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_SizeOf.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeCannotEvalRunTime, DocPos);
end;

procedure TLapeTree_InternalMethod_Ord.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_Ord.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) and FParams[0].isConstant() then
      FConstant := bTrue
    else
      FConstant := bFalse;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType <> nil) then
        FResType := FCompiler.getBaseType(ParamType.BaseIntType);
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.Evaluate: TLapeGlobalVar;
begin
  if (FRes = nil) then
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else
      setExpr(TLapeTree_VarType.Create(resType(), Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.Compile(var Offset: Integer): TResVar;
begin
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  setExpr(TLapeTree_VarType.Create(resType(), Self));
  Result := inherited;
end;

constructor TLapeTree_InternalMethod_Low.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FConstant := bTrue;
end;

function TLapeTree_InternalMethod_Low.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
        FResType := ParamType
      else if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes - LapeStringTypes + [ltShortString]) then
        FResType := FCompiler.getBaseType(ltInt32);
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Low.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isConstant() then
    begin
      ParamType := FParams[0].resType();

      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes - LapeStringTypes + [ltShortString])) then
        LapeException(lpeInvalidEvaluation, DocPos);

      FRes := ParamType.VarLo();
    end;

  Result := inherited;
  if (Result = nil) then
    LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_InternalMethod_Low.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  LapeException(lpeCannotEvalRunTime, DocPos);
end;

procedure TLapeTree_InternalMethod_High.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_High.isConstant: Boolean;
var
  ParamType: TLapeType;
begin
  if (FConstant = bUnknown) then
  begin
    FConstant := bTrue;
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (not (ParamType is TLapeType_Type)) and (ParamType.BaseType = ltDynArray) then
        FConstant := bFalse;
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_High.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
        FResType := ParamType
      else if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes - LapeStringTypes + [ltShortString]) then
        FResType := FCompiler.getBaseType(ltInt32);
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_High.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isConstant() then
    begin
      ParamType := FParams[0].resType();

      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes - LapeStringTypes + [ltShortString])) then
        LapeException(lpeInvalidEvaluation, DocPos);

      FRes := ParamType.VarHi();
    end;

  Result := inherited;
  if (Result = nil) then
    LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_InternalMethod_High.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
begin
  if isConstant() or (resType() = nil) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Result := NullResVar;
  Result.VarPos.MemPos := mpStack;
  Result.VarType := Compiler.getBaseType(ltPointer);

  Param := FParams[0].Compile(Offset);
  FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, Param, NullResVar, Offset, @Self._DocPos);

  Result := NullResVar;
  Result.VarType := FCompiler.getBaseType(ltInt32);
  if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    FDest := VarResVar;
  FCompiler.getDestVar(FDest, Result, op_Unknown);
  FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!high']), Result, SizeOf(Pointer), Offset, @Self._DocPos);

  Param.Spill(1);
end;

procedure TLapeTree_InternalMethod_Length.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_Length.isConstant: Boolean;
var
  ParamType: TLapeType;
begin
  if (FConstant = bUnknown) then
  begin
    FConstant := bFalse;
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType <> nil) and (ParamType.BaseType  = ltStaticArray) then
        FConstant := bTrue;
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Length.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) then
  begin
    FResType := FCompiler.getBaseType(ltInt32);
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType <> nil) and (ParamType.BaseType = ltShortString) then
        FResType := FCompiler.getBaseType(ltUInt8);
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Length.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
  begin
    if (not isConstant()) then
      LapeException(lpeCannotEvalRunTime, DocPos)
    else if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (ParamType <> nil) and (ParamType is TLapeType_Type) then
      ParamType := TLapeType_Type(ParamType).TType;
    if (ParamType = nil) or (ParamType.BaseType <> ltStaticArray) then
      LapeException(lpeInvalidEvaluation, DocPos);

    with TLapeType_StaticArray(ParamType) do
      FRes := FCompiler.getConstant(Range.Hi - Range.Lo + 1, ltInt32, False, True);
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Length.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
begin
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile(Offset);
  if (not Param.HasType()) or (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray])) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (Param.VarType.BaseType = ltShortString) then
  begin
    Dest := NullResVar;
    Result := Param;
    Result.VarType := FCompiler.getBaseType(ltUInt8);
  end
  else
  begin
    Result := NullResVar;
    Result.VarPos.MemPos := mpStack;
    Result.VarType := Compiler.getBaseType(ltPointer);
    FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, Param, NullResVar, Offset, @Self._DocPos);

    Result := NullResVar;
    Result.VarType := FCompiler.getBaseType(ltInt32);
    if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      FDest := VarResVar;
    FCompiler.getDestVar(FDest, Result, op_Unknown);

    case Param.VarType.BaseType of
      ltAnsiString: FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!astr_getlen']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      ltWideString: FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!wstr_getlen']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      ltUnicodeString: FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!ustr_getlen']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      else FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!length']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
    end;
  end;

  Param.Spill(1);
end;

function TLapeTree_InternalMethod_SetLength.Compile(var Offset: Integer): TResVar;
type
  TSetLength = class of TLapeTree_InternalMethod_SetLength;
var
  Param, Len, tmpVar: TResVar;
  ArrayType, tmpType: TLapeType;
  _ArraySetLength: TLapeGlobalVar;
  Counter: TLapeVar;
  i: Integer;
begin
  Result := NullResVar;
  Dest := NullResVar;
  tmpVar := NullResVar;
  Exclude(FCompilerOptions, lcoRangeCheck);

  if (FParams.Count < 2) or isEmpty(FParams[0]) or isEmpty(FParams[1]) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);

  if (not FParams[0].CompileToTempVar(Offset, Param)) or (not Param.HasType()) or
     (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray, ltShortString]))
  then
    LapeException(lpeInvalidEvaluation, DocPos);

  tmpType := Param.VarType;
  ArrayType := TLapeType_DynArray(Param.VarType).PType;

  if (not FParams[1].CompileToTempVar(Offset, Len)) or (ArrayType = nil) or
     (not Len.HasType()) or (not (Len.VarType.BaseType in LapeIntegerTypes))
	then
    LapeException(lpeInvalidEvaluation, DocPos);

  case Param.VarType.BaseType of
    ltAnsiString:    _ArraySetLength := FCompiler['_Astr_Setlen'];
    ltWideString:    _ArraySetLength := FCompiler['_Wstr_Setlen'];
    ltUnicodeString: _ArraySetLength := FCompiler['_Ustr_Setlen'];
    else
    begin
      _ArraySetLength := FCompiler['_ArraySetLength'];
      Param.VarType := FCompiler.getBaseType(ltPointer);
    end;
  end;

  try
    with TLapeTree_Invoke.Create(_ArraySetLength, Self) do
    try
      addParam(TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]));
      addParam(TLapeTree_ResVar.Create(Len.IncLock(), Self.FParams[1]));
      if (not (Param.VarType.BaseType in LapeStringTypes)) then
      begin
        addParam(TLapeTree_Integer.Create(ArrayType.Size, Self.FParams[0]));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Dispose', [ArrayType]), Self));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
      end;
      Result := Compile(Offset);
    finally
      Free();
    end;

    if (FParams.Count > 2) then
    begin
      Param.VarType := tmpType;
      Counter := FCompiler.getTempVar(ltInt32);
      tmpVar := Counter.VarType.Eval(op_Assign, tmpVar, _ResVar.New(Counter), _ResVar.New(FCompiler.getConstant(0)), [], Offset, @_DocPos);

      with TLapeTree_For.Create(Self) do
      try
        Counter := TLapeTree_ResVar.Create(tmpVar.IncLock(), Self);
        Limit := TLapeTree_Operator.Create(op_Minus, Self);
        with TLapeTree_Operator(Limit) do
        begin
          Left := TLapeTree_ResVar.Create(Len.IncLock(), FParams[1]);
          Right := TLapeTree_Integer.Create(1, Self);
        end;
        Body := TSetLength(Self.ClassType).Create(Self);
        with TLapeTree_InternalMethod_SetLength(Body) do
          for i := 0 to Self.FParams.Count - 1 do
            if (i = 0) then
            begin
              addParam(TLapeTree_Operator.Create(op_Index, Self.FParams[0]));
              with TLapeTree_Operator(Params[0]) do
              begin
                Left := TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]);
                Right := TLapeTree_ResVar.Create(tmpVar.IncLock(), Self.FParams[0]);
              end;
            end
            else if (i <> 1) then
              Params.Add(Self.FParams[i]);
        Compile(Offset).Spill(1);
      finally
        Free();
        tmpVar.Spill(1);
      end;
    end;
  finally
    Param.Spill(1);
    Len.Spill(1);
  end;
end;

function TLapeTree_InternalMethod_Copy.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count > 0) and (not isEmpty(FParams[0])) then
  begin
    ParamType := FParams[0].resType();

    if (ParamType <> nil) then
    begin
      if (ParamType.BaseType in LapeArrayTypes) then
        FResType := ParamType;

      if (ParamType is TLapeType_StaticArray) then
        if (ParamType.BaseType in LapeStringTypes) then
          FResType := FCompiler.getBaseType(ltAnsiString)
        else with TLapeType_StaticArray(ParamType) do
          FResType := FCompiler.addManagedType(TLapeType_DynArray.Create(PType, FCompiler));
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Copy.Compile(var Offset: Integer): TResVar;
var
  ParRes, StartRes: TResVar;
  Param, Start, TmpExpr: TLapeTree_ExprBase;
  wasConstant: Boolean;
  ArrayType: TLapeType;
  _ArrayCopy: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count < 1) or (FParams.Count > 3) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (not FParams[0].CompileToTempVar(Offset, ParRes)) or (not ParRes.HasType()) or
     (not (ParRes.VarType.BaseType in LapeArrayTypes))
  then
    LapeException(lpeInvalidEvaluation, DocPos);

  ArrayType := TLapeType_DynArray(ParRes.VarType).PType;
  Param := TLapeTree_ResVar.Create(ParRes.IncLock(), FParams[0]);
  Start := FParams[1];

  if isEmpty(Start) then
    StartRes := NullResVar
  else if (not Start.CompileToTempVar(Offset, StartRes)) or (not StartRes.HasType()) or (StartRes.VarType.BaseIntType = ltUnknown) then
    LapeException(lpeInvalidEvaluation, DocPos)
  else
  begin
    StartRes.VarType := FCompiler.getBaseType(StartRes.VarType.BaseIntType);
    Start := TLapeTree_ResVar.Create(StartRes.IncLock(), Self.FParams[1]);
  end;

  wasConstant :=  not ParRes.Writeable;
  if wasConstant then
    ParRes.Writeable := True;

  try
    case ParRes.VarType.BaseType of
      ltShortString:   _ArrayCopy := FCompiler['_SStr_Copy'];
      ltAnsiString:    _ArrayCopy := FCompiler['_AStr_Copy'];
      ltWideString:    _ArrayCopy := FCompiler['_WStr_Copy'];
      ltUnicodeString: _ArrayCopy := FCompiler['_UStr_Copy'];
      else
      begin
        _ArrayCopy := FCompiler['_ArrayCopy'];
        if (ParRes.VarType is TLapeType_StaticArray) then
        begin
          TmpExpr := TLapeTree_Operator.Create(op_Addr, Param);
          TLapeTree_Operator(TmpExpr).Left := Param;
          Param := TmpExpr;

          if (TLapeType_StaticArray(ParRes.VarType).Range.Lo <> 0) then
          begin
            TmpExpr := TLapeTree_InternalMethod_Low.Create(FCompiler);
            TLapeTree_InternalMethod_Low(TmpExpr).addParam(TLapeTree_ResVar.Create(ParRes.IncLock(), Self.FParams[0]));
            TmpExpr := TLapeTree_ExprBase(TmpExpr.FoldConstants());

            if isEmpty(Start) then
              Start := TmpExpr
            else with TLapeTree_Operator.Create(op_Minus, FCompiler) do
            try
              Left := Start;
              Right := TmpExpr;
              Start := TLapeTree_ExprBase(GetSelf());
            except
              Free();
            end;
          end;
        end
        else
          TLapeTree_ResVar(Param).FResVar.VarType := FCompiler.getBaseType(ltPointer);
      end;
    end;

    Result := _ResVar.New(FCompiler.getTempVar(resType()));
    FCompiler.VarToDefault(Result, Offset, @Self._DocPos);

    with TLapeTree_Invoke.Create(_ArrayCopy, Self) do
    try
      addParam(Param);
      addParam(Start);
      addParam(Self.FParams[2]);

      if (not (ParRes.VarType.BaseType in LapeStringTypes)) then
      begin
        TmpExpr := TLapeTree_InternalMethod_Length.Create(Self);
        TLapeTree_InternalMethod_Length(TmpExpr).addParam(TLapeTree_ResVar.Create(ParRes.IncLock(), Self.FParams[0]));
        addParam(TLapeTree_ExprBase(TmpExpr.FoldConstants()));

        addParam(TLapeTree_Integer.Create(ArrayType.Size, Self.FParams[0]));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));

        Result.VarType := FCompiler.getBaseType(ltPointer);
      end;

      addParam(TLapeTree_ResVar.Create(Result.IncLock(), Self));
      Compile(Offset).Spill(1);

      Result.VarType := Self.resType();
      Result.isConstant := True;
    finally
      Self.addParam(Params[2]);
      Free();
    end;
  finally
    if wasConstant then
      ParRes.Writeable := False;

    ParRes.Spill(1);
    StartRes.Spill(1);
  end;
end;

function TLapeTree_InternalMethod_Delete.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
  ArrayType: TLapeType;
  _ArrayDelete: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count < 2) or (FParams.Count > 3) or isEmpty(FParams[0]) or isEmpty(FParams[1]) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);

  if (not FParams[0].CompileToTempVar(Offset, Param)) or (not Param.HasType()) or
     (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray, ltShortString]))
  then
    LapeException(lpeInvalidEvaluation, DocPos);

  ArrayType := TLapeType_DynArray(Param.VarType).PType;
  case Param.VarType.BaseType of
    ltAnsiString:    _ArrayDelete := FCompiler['_Astr_Delete'];
    ltWideString:    _ArrayDelete := FCompiler['_WStr_Delete'];
    ltUnicodeString: _ArrayDelete := FCompiler['_UStr_Delete'];
    else
    begin
      _ArrayDelete := FCompiler['_ArrayDelete'];
      Param.VarType := FCompiler.getBaseType(ltPointer);
    end;
  end;

  try
    with TLapeTree_Invoke.Create(_ArrayDelete, Self) do
    try
      addParam(TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]));
      addParam(Self.FParams[1]);
      addParam(Self.FParams[1]);

      if (not (Param.VarType.BaseType in LapeStringTypes)) then
      begin
        addParam(TLapeTree_Integer.Create(ArrayType.Size, Self.FParams[0]));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Dispose', [ArrayType]), Self));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
      end;

      Result := Compile(Offset);
    finally
      Self.addParam(Params[1]);
      Self.addParam(Params[1]);
      Free();
    end;
  finally
    Param.Spill(1);
  end;
end;

function TLapeTree_InternalMethod_Insert.Compile(var Offset: Integer): TResVar;
var
  SrcRes, DstRes: TResVar;
  Src, TmpExpr: TLapeTree_ExprBase;
  wasConstant: Boolean;
  ArrayType: TLapeType;
  _ArrayDelete: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TmpExpr := nil;

  if (FParams.Count < 2) or (FParams.Count > 4) or isEmpty(FParams[0]) or isEmpty(FParams[1]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (not FParams[1].CompileToTempVar(Offset, DstRes)) or (not DstRes.HasType()) or
     (not (DstRes.VarType.BaseType in LapeArrayTypes - [ltStaticArray, ltShortString]))
  then
    LapeException(lpeInvalidEvaluation, DocPos);

  ArrayType := TLapeType_DynArray(DstRes.VarType).PType;
  FParams[0] := TLapeTree_ExprBase(FParams[0].setExpectedType(DstRes.VarType).setExpectedType(ArrayType));
  if (not FParams[0].CompileToTempVar(Offset, SrcRes)) or (not SrcRes.HasType()) or (ArrayType = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  wasConstant :=  not SrcRes.Writeable;
  if wasConstant then
    SrcRes.Writeable := True;
  Src := TLapeTree_ResVar.Create(SrcRes.IncLock(), FParams[0]);

  try
    case DstRes.VarType.BaseType of
      ltAnsiString:    _ArrayDelete := FCompiler['_AStr_Insert'];
      ltWideString:    _ArrayDelete := FCompiler['_WStr_Insert'];
      ltUnicodeString: _ArrayDelete := FCompiler['_UStr_Insert'];
      else
      begin
        _ArrayDelete := FCompiler['_ArrayInsert'];

        if DstRes.VarType.Equals(SrcRes.VarType) then
          TLapeTree_ResVar(Src).FResVar.VarType := FCompiler.getBaseType(ltPointer)
        else if ArrayType.Equals(SrcRes.VarType) or (
          (SrcRes.VarType is TLapeType_StaticArray) and
          ArrayType.Equals(TLapeType_StaticArray(SrcRes.VarType).PType))
        then
        begin
          TmpExpr := TLapeTree_Operator.Create(op_Addr, Src);
          TLapeTree_Operator(TmpExpr).Left := Src;
          Src := TmpExpr;

          if ArrayType.Equals(SrcRes.VarType) then
            TmpExpr := TLapeTree_Integer.Create(1, FParams[0])
          else
            TmpExpr := nil;
        end
        else
            LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], DocPos);

        DstRes.VarType := FCompiler.getBaseType(ltPointer);
      end;
    end;

    with TLapeTree_Invoke.Create(_ArrayDelete, Self) do
    try
      addParam(Src);
      addParam(TLapeTree_ResVar.Create(DstRes.IncLock(), Self.FParams[1]));
      addParam(Self.FParams[2]);
      addParam(Self.FParams[2]);

      if (not (DstRes.VarType.BaseType in LapeStringTypes)) then
      begin
        if (TmpExpr = nil) then
        begin
          TmpExpr := TLapeTree_InternalMethod_Length.Create(Self);
          TLapeTree_InternalMethod_Length(TmpExpr).addParam(TLapeTree_ResVar.Create(SrcRes.IncLock(), Self.FParams[0]));
        end;
        addParam(TLapeTree_ExprBase(TmpExpr.FoldConstants()));

        addParam(TLapeTree_Integer.Create(ArrayType.Size, Self.FParams[0]));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Dispose', [ArrayType]), Self));
        addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
      end;

      Result := Compile(Offset);
    finally
      Self.addParam(Params[2]);
      Self.addParam(Params[2]);
      Free();
    end;
  finally
    if wasConstant then
      SrcRes.Writeable := False;

    SrcRes.Spill(1);
    DstRes.Spill(1);
  end;
end;

procedure TLapeTree_InternalMethod_Succ.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_Succ.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if  (FParams.Count in [1, 2]) and (not isEmpty(FParams[0])) and FParams[0].isConstant() and
       ((FParams.Count = 1) or       ((not isEmpty(FParams[1])) and FParams[1].isConstant())) then
      FConstant := bTrue
    else
      FConstant := bFalse;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Succ.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count in [1, 2]) and (not isEmpty(FParams[0])) then
    begin
      FResType := FParams[0].resType();
      if (FResType <> nil) and (FResType is TLapeType_Type) then
        FResType := TLapeType_Type(FResType).TType;
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Succ.Evaluate: TLapeGlobalVar;
var
  VarParam, CountParam: TLapeGlobalVar;
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  ResultType: TLapeType;
begin
  if (FRes = nil) then
  begin
    CountParam := nil;
    OldCountParam := nil;

    if (FParams.Count = 2) then
      if isEmpty(FParams[0]) then
        LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos)
      else if isEmpty(FParams[1]) then
        LapeExceptionFmt(lpeNoDefaultForParam, [2], DocPos)
      else
        OldCountParam := FParams.Delete(1)
    else if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    try
      if (OldCountParam <> nil) then
        CountParam := OldCountParam.Evaluate()
      else
        CountParam := FCompiler.getConstant(1);

      VarParam := FParams[0].Evaluate();
      if (VarParam = nil) or (not VarParam.HasType()) or (not VarParam.VarType.IsOrdinal(True)) then
        LapeException(lpeInvalidEvaluation, DocPos);

      if (VarParam.VarType.BaseType = ltPointer) then
        with TLapeTree_Operator.Create(op_Index, Self) do
        try
          Left := TLapeTree_GlobalVar.Create(VarParam, FParams[0]);
          Right := TLapeTree_GlobalVar.Create(CountParam, Self);
          FRes := Evaluate();
        finally
          Free();
        end
      else
      begin
        ResultType := VarParam.VarType;
        VarParam := FCompiler.getBaseType(VarParam.VarType.BaseIntType).NewGlobalVarP(VarParam.Ptr);
        with TLapeTree_Operator.Create(op_Plus, Self) do
        try
          Left := TLapeTree_GlobalVar.Create(VarParam, FParams[0]);
          Right := TLapeTree_GlobalVar.Create(CountParam, Self);
          Result := Evaluate();

          OldVarParam := FParams.Delete(0);
          addParam(TLapeTree_GlobalVar.Create(Result, Self));
          try
            setExpr(TLapeTree_VarType.Create(ResultType, FParams[0]));
            FRes := inherited;
          finally
            FParams.Delete(0).Free();
            addParam(OldVarParam);
          end;
        finally
          VarParam.Free();
          Free();
        end;
      end;
    finally
      if (OldCountParam <> nil) then
      begin
        CountParam.Free();
        addParam(OldCountParam);
      end;
    end;
  end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Succ.Compile(var Offset: Integer): TResVar;
var
  VarParam, CountParam, tmpDest: TResVar;
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  ResultType: TLapeType;
begin
  CountParam := NullResVar;
  OldCountParam := nil;
  if (FParams.Count = 2) then
    if isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos)
    else if isEmpty(FParams[1]) then
      LapeExceptionFmt(lpeNoDefaultForParam, [2], DocPos)
    else
      OldCountParam := FParams.Delete(1)
  else if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  try
    if (OldCountParam <> nil) then
      CountParam := OldCountParam.Compile(Offset)
    else
      CountParam := _ResVar.New(FCompiler.getConstant(1));

    VarParam := FParams[0].Compile(Offset);
    if (not VarParam.HasType()) or (not VarParam.VarType.IsOrdinal(True)) then
      LapeException(lpeInvalidEvaluation, DocPos);

    if (VarParam.VarType.BaseType = ltPointer) then
      with TLapeTree_Operator.Create(op_Index, Self) do
      try
        Dest := Self.FDest;
        Left := TLapeTree_ResVar.Create(VarParam, FParams[0]);
        Right := TLapeTree_ResVar.Create(CountParam.IncLock(), Self);
        Result := Compile(Offset);
        Self.FDest := Dest;
      finally
        Free();
      end
    else
    begin
      ResultType := VarParam.VarType;
      VarParam.VarType := FCompiler.getBaseType(VarParam.VarType.BaseIntType);
      with TLapeTree_Operator.Create(op_Plus, Self) do
      try
        Dest := Self.Dest;
        Left := TLapeTree_ResVar.Create(VarParam, FParams[0]);
        Right := TLapeTree_ResVar.Create(CountParam.IncLock(), Self);
        Result := Compile(Offset);

        if (Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
          tmpDest := Dest
        else
          tmpDest := NullResVar;
        OldVarParam := FParams.Delete(0);
        addParam(TLapeTree_ResVar.Create(Result, Self));
        try
          setExpr(TLapeTree_VarType.Create(VarParam.VarType, FParams[0]));
          Result := inherited;
          Result.VarType := ResultType;
          if (Self.Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
            Self.Dest := tmpDest;
        finally
          FParams.Delete(0).Free();
          addParam(OldVarParam);
        end;
      finally
        Free();
      end;
    end;
  finally
    if (OldCountParam <> nil) then
      addParam(OldCountParam)
    else
      CountParam.Spill(1);
  end;
end;

function TLapeTree_InternalMethod_Pred.Evaluate: TLapeGlobalVar;
var
  Negation: TLapeTree_Operator;
begin
  if (FRes = nil) then
  begin
    Negation := nil;

    if (FParams.Count < 2) then
      addParam(TLapeTree_GlobalVar.Create(FCompiler.getConstant(-1), Self))
    else if (not isEmpty(FParams[1])) and (FParams.Count = 2) then
    begin
      Negation := TLapeTree_Operator.Create(op_UnaryMinus, Self);
      Negation.Left := FParams.Delete(1);
      addParam(Negation.FoldConstants(False) as TLapeTree_ExprBase);
    end;

    FRes := inherited;

    if (FParams.Count = 2) then
    begin
      if (Negation <> nil) then
      begin
        addParam(Negation.Left);
        if (Negation.Parent = nil) then
          Negation.Free();
      end;
      FParams.Delete(1).Free();
    end;
  end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Pred.Compile(var Offset: Integer): TResVar;
var
  Negation: TLapeTree_Operator;
begin
  Negation := nil;

  if (FParams.Count < 2) then
    addParam(TLapeTree_GlobalVar.Create(FCompiler.getConstant(-1), Self))
  else if (not isEmpty(FParams[1])) and (FParams.Count = 2) then
  begin
    Negation := TLapeTree_Operator.Create(op_UnaryMinus, Self);
    Negation.Left := FParams.Delete(1);
    addParam(Negation.FoldConstants(False) as TLapeTree_ExprBase);
  end;

  Result := inherited;

  if (FParams.Count = 2) then
  begin
    if (Negation <> nil) then
    begin
      addParam(Negation.Left);
      if (Negation.Parent = nil) then
        Negation.Free();
    end;
    FParams.Delete(1).Free();
  end;
end;

function TLapeTree_InternalMethod_Inc.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count in [1, 2]) and (not isEmpty(FParams[0])) then
    begin
      FResType := FParams[0].resType();
      if (FResType <> nil) and (FResType is TLapeType_Type) then
        FResType := TLapeType_Type(FResType).TType;
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Inc.Compile(var Offset: Integer): TResVar;
var
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  Succ: TLapeTree_Operator;
begin
  Result := NullResVar;
  if (not (FParams.Count in [1, 2])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Dest := NullResVar;
  OldVarParam := FParams.Delete(0);
  if (FParams.Count > 0) then
    OldCountParam := FParams.Delete(0)
  else
    OldCountParam := nil;
  Succ := nil;

  try
    Result := OldVarParam.Compile(Offset);
    Succ := TLapeTree_Operator.Create(op_Assign, Self);
    Succ.Left := TLapeTree_ResVar.Create(Result.IncLock(), OldVarParam);
    Succ.Right := TLapeTree_InternalMethod_Succ.Create(Self);
    TLapeTree_InternalMethod_Succ(Succ.Right).addParam(TLapeTree_ResVar.Create(Result.IncLock(), OldVarParam));
    if (OldCountParam <> nil) then
      TLapeTree_InternalMethod_Succ(Succ.Right).addParam(OldCountParam);
    Result := Succ.Compile(Offset);
  finally
    addParam(OldVarParam);
    if (OldCountParam <> nil) then
      addParam(OldCountParam);
    if (Succ <> nil) then
      Succ.Free();
  end;
end;

function TLapeTree_InternalMethod_Dec.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count in [1, 2]) and (not isEmpty(FParams[0])) then
    begin
      FResType := FParams[0].resType();
      if (FResType <> nil) and (FResType is TLapeType_Type) then
        FResType := TLapeType_Type(FResType).TType;
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Dec.Compile(var Offset: Integer): TResVar;
var
  OldVarParam, OldCountParam: TLapeTree_ExprBase;
  Pred: TLapeTree_Operator;
begin
  Result := NullResVar;
  Dest := NullResVar;
  if (not (FParams.Count in [1, 2])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  OldVarParam := FParams.Delete(0);
  if (FParams.Count > 0) then
    OldCountParam := FParams.Delete(0)
  else
    OldCountParam := nil;
  Pred := nil;

  try
    Result := OldVarParam.Compile(Offset);
    Pred := TLapeTree_Operator.Create(op_Assign, Self);
    Pred.Left := TLapeTree_ResVar.Create(Result.IncLock(), OldVarParam);
    Pred.Right := TLapeTree_InternalMethod_Pred.Create(Self);
    TLapeTree_InternalMethod_Pred(Pred.Right).addParam(TLapeTree_ResVar.Create(Result.IncLock(), OldVarParam));
    if (OldCountParam <> nil) then
      TLapeTree_InternalMethod_Pred(Pred.Right).addParam(OldCountParam);
    Result := Pred.Compile(Offset);
  finally
    addParam(OldVarParam);
    if (OldCountParam <> nil) then
      addParam(OldCountParam);
    if (Pred <> nil) then
      Pred.Free();
  end;
end;

constructor TLapeTree_InternalMethod_Label.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FForceParam := True;;
end;

function TLapeTree_InternalMethod_Label.Compile(var Offset: Integer): TResVar;
var
  ResVar: TResVar;
  Lbl: TLapeGlobalVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ResVar := FParams[0].Compile(Offset);
  FCompiler.Emitter.CheckOffset(Offset);

  if (not ResVar.HasType()) or (ResVar.VarType.BaseType <> ltPointer) or (not (ResVar.VarType is TLapeType_Pointer)) then
    LapeException(lpeInvalidLabel, [FParams[0]]);

  if (ResVar.VarType is TLapeType_Label) then
  begin
    if ResVar.Readable or (ResVar.VarPos.MemPos <> mpMem) or (not ResVar.VarPos.GlobalVar.isNull()) then
      LapeException(lpeInvalidLabel, [FParams[0]]);

    PCodeOffset(ResVar.VarPos.GlobalVar.Ptr)^ := Offset;
    FCompiler.Emitter.addCodePointer(ResVar.VarPos.GlobalVar.Ptr);
    ResVar.isConstant := True;
  end
  else with TLapeTree_Operator.Create(op_Assign, Self) do
  try
    Lbl := FCompiler.getLabel(Offset);
    Left := TLapeTree_ResVar.Create(ResVar, Self);
    Right := TLapeTree_GlobalVar.Create(Lbl, Self);

    Compile(Offset);
    PCodePos(Lbl.Ptr)^ := Offset;
  finally
    Free();
  end;
end;

constructor TLapeTree_InternalMethod_Goto.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FForceParam := True;
end;

function TLapeTree_InternalMethod_Goto.Compile(var Offset: Integer): TResVar;
var
  tmpVar, ResVar, Param: TResVar;
begin
  Result := NullResVar;
  tmpVar := NullResVar;
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile(Offset);
  if (not Param.HasType()) or (not (Param.VarType.BaseType in [ltPointer] + LapeProcTypes)) then
    LapeException(lpeInvalidLabel, [FParams[0]]);

  ResVar := FCompiler.getTempStackVar(ltPointer);
  ResVar := ResVar.VarType.Eval(op_Assign, tmpVar, ResVar, Param, [], Offset, @_DocPos);
  FCompiler.Emitter._JmpVar(Offset, @_DocPos);

  Param.Spill(1);
  ResVar.Spill(1);
end;

constructor TLapeTree_Callback.Create(ACompiler: TLapeCompilerBase; ACallback: TLapeTreeCallback; AData: Pointer = nil; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  Assert({$IFNDEF FPC}@{$ENDIF}ACallback <> nil);

  FCallback := ACallback;
  FData := AData;
end;

function TLapeTree_Callback.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  FCallback(Self);
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
  Cast: TLapeTree_Invoke;
begin
  Result := Self;
  if (not isEmpty(Self)) and (ExpectType <> nil) then
  begin
    if (not isEmpty(Left)) then
      if(OperatorType = op_Deref) then
      begin
        Typ := Left.resType();
        if (Typ <> nil) and (Typ is TLapeType_Pointer) and (not TLapeType_Pointer(Typ).HasType()) then
        begin
          Cast := TLapeTree_Invoke.Create(FCompiler.getPointerType(ExpectType), Self);
          Cast.addParam(Left);
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
        (TLapeTree_Operator(FParent).OperatorType = op_Assign)
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
  Result := [];
  if (lcoRangeCheck in FCompilerOptions) then
    Include(Result, lefRangeCheck);
  if isAssigning() then
    Include(Result, lefAssigning);
end;

function TLapeTree_Operator.resType(Restructure: Boolean): TLapeType;
var
  LeftType, RightType, tmpRes: TLapeType;
  tmpLeft: TLapeTree_ExprBase;
  tmpVar, ResVar: TResVar;
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
      if (FRight <> nil) and (FRight is TLapeTree_GlobalVar) then
        Result := LeftType.EvalRes(FOperatorType, TLapeTree_GlobalVar(FRight).GlobalVar, EvalFlags())
      else if (LeftType <> nil) then
        Result := LeftType.EvalRes(FOperatorType, RightType, EvalFlags());

      if (LeftType <> nil) and (FRight <> nil) and
         ((Result = nil)  and ((FOperatorType = op_IN) and (FRight is TLapeTree_OpenArray)) or
         ((lcoShortCircuit in FCompilerOptions) and (Result <> nil) and (Result.BaseType in LapeBoolTypes) and (FOperatorType in [op_AND, op_OR]) and
          (LeftType.BaseType in LapeBoolTypes) and (RightType <> nil) and (RightType.BaseType in LapeBoolTypes)))
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

  if (FResType = nil) and (FLeft <> nil) and (FRight <> nil) and (LeftType.BaseType <> ltUnknown) and
     (FOperatorType in OverloadableOperators) then
  begin
    with TLapeTree_InternalMethod_Operator.Create(FOperatorType, FCompiler, nil) do
    try
      tmpVar := NullResVar;
      tmpVar.VarType := LeftType;
      addParam(TLapeTree_ResVar.Create(tmpVar, FCompiler, nil));
      tmpVar.VarType := RightType;
      addParam(TLapeTree_ResVar.Create(tmpVar, FCompiler, nil));
      tmpRes := resType();
      if tmpRes <> nil then Result := tmpRes;
    finally
      Free();
      FResType := Result;
    end;
  end;
  
  if Restructure and (FRestructure = bTrue) then
  begin
    FRestructure := bFalse;

    tmpLeft := FLeft.FoldConstants() as TLapeTree_ExprBase;
    if (FOperatorType = op_IN) then
      FLeft := TLapeTree_MultiIf.Create(tmpLeft, FRight as TLapeTree_OpenArray)
    else
      FLeft := TLapeTree_If.Create(Self);

    with TLapeTree_If(FLeft) do
    begin
      if (FOperatorType <> op_IN) then
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
    try
      if (FLeft <> nil) then
        if (FLeft is TLapeTree_ExprBase) then
          LeftVar := TLapeTree_ExprBase(FLeft).Evaluate()
        else
          LapeException(lpeInvalidEvaluation, DocPos)
      else
        LeftVar := nil;

      if (FOperatorType = op_Assign) and ((LeftVar = nil) or (not LeftVar.Writeable)) then
        LapeException(lpeCannotAssign, [FLeft, Self]);

      if (LeftVar <> nil) and (not isEmpty(FRight)) then
      begin
        FRight := FRight.setExpectedType(LeftVar.VarType) as TLapeTree_ExprBase;
        if (FOperatorType = op_Index) then
          FRight := FRight.setExpectedType(FCompiler.getBaseType(ltInt32)) as TLapeTree_ExprBase;
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
          Result := LeftVar.VarType.EvalConst(FOperatorType, LeftVar, RightVar, EvalFlags())
        else with TLapeType.Create(ltUnknown, FCompiler) do
        try
          Result := EvalConst(FOperatorType, LeftVar, RightVar, EvalFlags());
        finally
          Free();
        end;

        Result := FCompiler.addManagedVar(Result) as TLapeGlobalVar;
      except on E: lpException do
        LapeException(lpString(E.Message), DocPos);
      end;
    finally
      FRes := Result;
    end;
  Result := inherited;
end;

function TLapeTree_Operator.Compile(var Offset: Integer): TResVar;
var
  LeftVar, RightVar: TResVar;
  DoneAssignment: Boolean;

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
        TLapeTree_Operator(Body).Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        TLapeTree_Operator(ElseBody).Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      end;

      Result := Compile(Offset);
      if wasConstant then
        Result.Writeable := False;
    end;
  end;

  function TryOperatorOverload(): TResVar;
  begin
    Result := NullResVar;
    if (FOperatorType in OverloadableOperators) and RightVar.HasType() and
       RightVar.HasType() and (not (FOperatorType in UnaryOperators)) then
      with TLapeTree_InternalMethod_Operator.Create(FOperatorType, FCompiler, @_DocPos) do
      try
        addParam(TLapeTree_ResVar.Create(LeftVar, FCompiler, @_DocPos));
        addParam(TLapeTree_ResVar.Create(RightVar, FCompiler, @_DocPos));
        Result := Compile(Offset);
      finally 
        Free();
      end;
  end;
  
begin
  Result := NullResVar;
  DoneAssignment := False;

  if (FLeft <> nil) then
    if (resType(True) <> nil) and (FLeft is TLapeTree_If) then
      Exit(doIf())
    else
      LeftVar := TLapeTree_ExprBase(FLeft).Compile(Offset)
  else
    LeftVar := NullResVar;

  if (FOperatorType = op_Assign) and (not LeftVar.Writeable) then
    LapeException(lpeCannotAssign, [FLeft, Self]);

  if (FRight <> nil) then
  begin
    FRight := FRight.setExpectedType(LeftVar.VarType) as TLapeTree_ExprBase;
    if (FOperatorType = op_Index) then
      FRight := FRight.setExpectedType(FCompiler.getBaseType(ltInt32)) as TLapeTree_ExprBase;

    if (FOperatorType = op_Assign) and
      (FLeft <> nil) and LeftVar.Writeable and
      (FRight is TLapeTree_DestExprBase) and
      (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos)
    then
    begin
      TLapeTree_DestExprBase(FRight).Dest := LeftVar;
      DoneAssignment := True;
    end;

    RightVar := FRight.Compile(Offset);
    DoneAssignment := DoneAssignment and (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos);
  end
  else
    RightVar := NullResVar;

  try
    if DoneAssignment then
    begin
      Result := LeftVar;
      Dest := NullResVar;
      TLapeTree_DestExprBase(FRight).Dest := NullResVar;
    end else
    try
      if LeftVar.HasType() then
        Result := LeftVar.VarType.Eval(FOperatorType, FDest, LeftVar, RightVar, EvalFlags(), Offset, @_DocPos)
      else with TLapeType.Create(ltUnknown, FCompiler) do
      try
        Result := Eval(OperatorType, FDest, LeftVar, RightVar, EvalFlags(), Offset, @_DocPos);
      finally
        Free();
      end;
    except
      on E: lpException do
      begin
        Dest := NullResVar;
        Result := TryOperatorOverload();
        if not Result.HasType() then
          LapeException(lpString(E.Message), DocPos)
        else
          Exit;
      end;
    end;
  finally
    LeftVar.Spill(1);
    RightVar.Spill(1);
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
    FGlobalVar := TLapeGlobalVar(FCompiler.addManagedVar(FGlobalVar));

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
  Create(ACompiler.getConstant(Ident, BaseType, False, True), ACompiler, ADocPos);
end;

constructor TLapeTree_GlobalVar.Create(Ident: lpString; BaseType: ELapeBaseType; ASource: TLapeTree_Base);
begin
  Assert((ASource <> nil) and (ASource.Compiler <> nil));
  Create(ASource.Compiler.getConstant(Ident, BaseType, False, True), ASource);
end;

function TLapeTree_GlobalVar.Compile(var Offset: Integer): TResVar;
begin
  Result := _ResVar.New(FGlobalVar);
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
  Result := FWithDeclRec.WithVar^.IncLock();
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
  inherited Create(ACompiler.getConstant(StringReplace(AStr, lpString('_'), lpString(''), [rfReplaceAll]), ltNativeInt), ACompiler, ADocPos);
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
  inherited Create(ACompiler.getConstant(StringReplace(AStr, lpString('_'), lpString(''), [rfReplaceAll]), ltExtended), ACompiler, ADocPos);
end;

constructor TLapeTree_Float.Create(AStr: lpString; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AStr, ASource.Compiler, @ASource._DocPos);
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Float.Create(AValue: Extended; ASource: TLapeTree_Base);
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

constructor TLapeTree_Char.Create(AValue: WideChar; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  if (AValue > #255) then
    inherited Create(lpString(AValue), ltWideChar, ACompiler, ADocPos)
  else
    inherited Create(lpString(AValue), ltChar, ACompiler, ADocPos);
end;

constructor TLapeTree_Char.Create(AValue: WideChar; ASource: TLapeTree_Base);
begin
  inherited Create(lpString(AValue), ltWideChar, ASource);
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

function TLapeTree_Range.getDiff: Int64;
begin
  if isEmpty(FLo) or isEmpty(FHi) or
     (not (FLo is TLapeTree_GlobalVar)) or
     (not (FHi is TLapeTree_GlobalVar)) or
     (not FLo.isConstant()) or (not FHi.isConstant())
  then
    Result := -1
  else
    Result := Max(TLapeTree_GlobalVar(FHi).getVarAsInt() - TLapeTree_GlobalVar(FLo).getVarAsInt() + 1, 0);
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

function TLapeTree_StatementList.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
begin
  Result := NullResVar;
  for i := 0 to FStatements.Count - 1 do
    if (FStatements[i] <> nil) then
    begin
      Result.Spill(1);
      FStatements[i].CompileToTempVar(Offset, Result);
    end;
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
      begin
        Result.Spill(1);
        FStatements[i].CompileToTempVar(Offset, Result);
      end;

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
  FExitStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore, True);
end;

destructor TLapeTree_Method.Destroy;
begin
  setStatements(nil);
  SelfVar.Spill();
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
  FExitStatements.Clear();

  Result := NullResVar;
  fo := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
  mo := FCompiler.Emitter.getCodeOffset(Offset);

  PCodePos(Method.Ptr)^ := mo;
  FCompiler.Emitter.addCodePointer(Method.Ptr);

  FCompiler.IncStackInfo(FStackInfo, Offset, True, @_DocPos);
  try
    if MethodOfObject(Method.VarType) then
      with TLapeTree_InternalMethod_Assert.Create(Self) do
      try
        addParam(TLapeTree_Operator.Create(op_cmp_NotEqual, Self));
        addParam(TLapeTree_String.Create(lpeVariableExpected, Self));

        with TLapeTree_Operator(Params[0]) do
        begin
          Left := TLapeTree_Operator.Create(op_Addr, Self);
          TLapeTree_Operator(Left).Left := TLapeTree_ResVar.Create(_ResVar.New(FStackInfo.Vars[0]), Self);
          Right := TLapeTree_GlobalVar.Create('nil', ltPointer, Self);
        end;

        Compile(Offset).Spill(1);
      finally
        Free();
      end;

    FStatements.Compile(Offset).Spill(1);

    for i := 0 to FExitStatements.Count - 1 do
      with FExitStatements[i] do
      begin
        co := CodeOffset;
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos);
      end;
  finally
    FCompiler.DecStackInfo(Offset, True, True, False, @_DocPos);
    FCompiler.Emitter._JmpR(Offset - fo, fo, @_DocPos);
  end;
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
  FWithList := TLapeExpressionList.Create(nil, dupAccept, True);
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
  i := FWithList.Add(AWith);

  if (AWith <> nil) and (i > -1) then
  begin
    AWith.Parent := Self;
    SetLength(FVarList, FWithList.Count);

    if (AWith is TLapeTree_GlobalVar) and TLapeTree_GlobalVar(AWith).isConstant then
      FVarList[i] := _ResVar.New(TLapeTree_GlobalVar(AWith).GlobalVar)
    else
      FVarList[i] := NullResVar;

    Result.WithVar := @FVarList[i];
    Result.WithType := AWith.resType();
  end;

  if (Result.WithType = nil) or (not Result.WithType.CanHaveChild()) then
    LapeException(lpeInvalidWithReference, [AWith, Self])
end;

function TLapeTree_With.Compile(var Offset: Integer): TResVar;
var
  i: Integer;
  NewStack: Boolean;
  ResVarList: array of TResVar;
begin
  Result := NullResVar;

  SetLength(ResVarList, Length(FVarList));
  for i := 0 to High(FVarList) do
    ResVarList[i] := FVarList[i].IncLock(BigLock);

  NewStack := (FCompiler.StackInfo = nil);
  if NewStack then
    FCompiler.IncStackInfo(True);

  for i := 0 to FWithList.Count - 1 do
    if (FVarList[i].VarPos.MemPos = NullResVar.VarPos.MemPos) then
    begin
      if (not FWithList[i].CompileToTempVar(Offset, ResVarList[i], BigLock)) or
         (ResVarList[i].VarPos.MemPos in [mpNone, mpStack])
      then
        LapeException(lpeInvalidCondition, FWithList[i].DocPos);
      FVarList[i] := ResVarList[i];
    end;

  if (FBody <> nil) then
    FBody.CompileToTempVar(Offset, Result);

  for i := 0 to High(ResVarList) do
    ResVarList[i].Spill(BigLock);

  if NewStack then
    FCompiler.DecStackInfo(False, True, True);
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

  if (not FCondition.CompileToBoolVar(Offset, ConditionVar)) then
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
    Result.Spill(1);
    FElse.CompileToTempVar(Offset, Result);

    if (FBody <> nil) then
      FCompiler.Emitter._JmpR(Offset - if_e, if_e, @_DocPos)
    else
      FCompiler.Emitter._JmpRIf(Offset - if_o, ConditionVar, if_o, @_DocPos);
  end;

  ConditionVar.Spill(1);
end;

procedure TLapeTree_MultiIf.DeleteChild(Node: TLapeTree_Base);
begin
  inherited;
  if (FValues <> nil) then
    FValues.DeleteItem(Node);
end;

constructor TLapeTree_MultiIf.Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);
  setCondition(Ident);
  FValues := TLapeStatementList.Create(nil, dupAccept, False);
end;

constructor TLapeTree_MultiIf.Create(Ident: TLapeTree_ExprBase; OpenArray: TLapeTree_OpenArray; FreeArray: Boolean = True);
begin
  Assert(OpenArray <> nil);
  Create(Ident, OpenArray.Compiler, @OpenArray._DocPos);
  while (OpenArray.Values.Count > 0) do
    addValue(OpenArray.Values[0]);
  if FreeArray then
    OpenArray.Free();
end;

destructor TLapeTree_MultiIf.Destroy;
var
  i: Integer;
begin
  for i := FValues.Count - 1 downto 0 do
    if (FValues[i] <> nil) and (FValues[i].Parent = Self) then
      FValues[i].Free();
  FreeAndNil(FValues);
  inherited;
end;

function TLapeTree_MultiIf.addValue(p: TLapeTree_Base): Integer;
begin
  Result := FValues.Add(p);
  if (p <> nil) then
    p.Parent := Self;
end;

function TLapeTree_MultiIf.Compile(var Offset: Integer): TResVar;

  function CreateOp(op: EOperator; Condition: TResVar; var Cmp: TLapeTree_ExprBase): TLapeTree_Operator;
  var
    tmpRes: TLapeTree_Operator;
  begin
    tmpRes := TLapeTree_Operator.Create(op, Cmp);
    with tmpRes do
    begin
      Left := TLapeTree_ResVar.Create(Condition.IncLock(), Cmp);
      Right := Cmp;
      Cmp := FRight;
    end;

    Result := TLapeTree_Operator(tmpRes.FoldConstants(False));
    if (tmpRes <> Result) then
    begin
      Cmp.Parent := nil;
      tmpRes.Free();
    end;
  end;

var
  i: Integer;
  ConditionVar: TResVar;
  CheckField, opOR: TLapeTree_Operator;
  Vals: TLapeStatementList.TTArray;
  tmpExpr: TLapeTree_ExprBase;
begin
  Result := NullResVar;
  Assert((FValues.Count > 0) or (FElse <> nil));
  Assert(FCondition <> nil);

  CheckField := nil;
  opOR := nil;
  if (not FCondition.CompileToTempVar(Offset, ConditionVar)) then
    LapeException(lpeInvalidCondition, DocPos);

  try

    Vals := FValues.ExportToArray();
    for i := 0 to High(Vals) do
    begin
      if (Vals[i] is TLapeTree_Range) then
        with TLapeTree_Range(Vals[i]) do
        begin
          CheckField := TLapeTree_Operator.Create(op_AND, Vals[i]);
          CheckField.Left := CreateOp(op_cmp_GreaterThanOrEqual, ConditionVar, FLo);
          CheckField.Right := CreateOp(op_cmp_LessThanOrEqual, ConditionVar, FHi);
          Parent := Vals[i];
        end
      else if (Vals[i] is TLapeTree_ExprBase) then
        CheckField := CreateOp(op_cmp_Equal, ConditionVar, TLapeTree_ExprBase(Vals[i]))
      else
        LapeException(lpeInvalidEvaluation, DocPos);

      if (opOR = nil) then
        opOR := CheckField
      else
      begin
        tmpExpr := opOR;
        opOR := TLapeTree_Operator.Create(op_OR, CheckField);
        opOR.Left := CheckField;
        opOR.Right := tmpExpr;
      end;
      CheckField := nil;
    end;

    tmpExpr := FCondition;
    FCondition := opOR;
    try
      if (FCondition <> nil) then
        Result := inherited
      else
        FElse.CompileToTempVar(Offset, Result);
    finally
      FCondition := tmpExpr;
    end;
  finally
    for i := 0 to High(Vals) do
      if (Vals[i].Parent <> Self) then
      begin
        if (Vals[i] is TLapeTree_Range) then
          with TLapeTree_Range(Vals[i]) do
          begin
            Lo := FLo;
            Hi := FHi;
          end;
        addValue(Vals[i]);
      end;

    if (CheckField <> nil) then
      CheckField.Free();
    if (opOR <> nil) and (opOr <> CheckField) then
      opOR.Free();
  end;

  ConditionVar.Spill(1);
end;

procedure TLapeTree_Case.CompileContinue(Sender: TLapeTree_Callback);
var
  Offset, co: Integer;
begin
  Offset := FCompiler.Emitter.CheckOffset();

  while (FContinueStatements.Count > 0) do
    with FContinueStatements.Delete(0) do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;
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

procedure TLapeTree_Case.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FElse = Node) then
    FElse := nil
  else
    FFields.DeleteItem(TLapeTree_MultiIf(Node));
end;

constructor TLapeTree_Case.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FElse := nil;

  FFields := TLapeCaseFieldList.Create(nil, dupAccept, False);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore, True);
end;

destructor TLapeTree_Case.Destroy;
var
  i: Integer;
begin
  for i := FFields.Count - 1 downto 0 do
    if (FFields[i] <> nil) and (FFields[i].Parent = Self) then
      FFields[i].Free();
  FreeAndNil(FFields);
  FreeAndNil(FContinueStatements);

  setCondition(nil);
  setElse(nil);
  inherited;
end;

function TLapeTree_Case.addField(p: TLapeTree_MultiIf): Integer;
begin
  Result := FFields.Add(p);
  if (p <> nil) then
    p.Parent := Self;
end;

function TLapeTree_Case.Compile(var Offset: Integer): TResVar;

  function addCallback(Statement: TLapeTree_Base; i: Integer): TLapeTree_StatementList;
  begin
    Result := nil;
    if (Statement <> nil) then
    begin
      Result := EnsureCompound(FCompiler, Statement);
      TLapeTree_StatementList(Result).addStatement(
        TLapeTree_Callback.Create(FCompiler, {$IFDEF FPC}@{$ENDIF}CompileContinue, Pointer(i)),
        True
      );
    end;
  end;

var
  i: Integer;
  ConditionVar: TResVar;
begin
  Assert(FCondition <> nil);
  Result := NullResVar;
  FElse := addCallback(FElse, FFields.Count);

  if (FFields.Count > 0) then
  begin
    if (not FCondition.CompileToTempVar(Offset, ConditionVar)) then
      LapeException(lpeInvalidCondition, DocPos);

    for i := 0 to FFields.Count - 1 do
    begin
      FFields[i].Condition := TLapeTree_ResVar.Create(ConditionVar.IncLock(), FCondition);
      FFields[i].Body := addCallback(FFields[i].Body, i);
    end;

    FFields[FFields.Count - 1].ElseBody := FElse;
    for i := FFields.Count - 1 downto 1 do
      FFields[i - 1].ElseBody := FFields[i];

    FFields[0].CompileToTempVar(Offset, Result);
    ConditionVar.Spill(1);
  end
  else if (FElse <> nil) then
    FElse.CompileToTempVar(Offset, Result);

  CompileContinue(nil);
end;

function TLapeTree_Case.addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean;
begin
  Result := lcoContinueCase in CompilerOptions;
  if Result then
    if JumpSafe then
      FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
    else
      FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
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

  if (not FCondition.CompileToBoolVar(Offset, ConditionVar)) then
    LapeException(lpeInvalidCondition, [FCondition, Self]);

  FCompiler.Emitter._JmpRIf(FStartBodyOffset - Offset, ConditionVar, Offset, @_DocPos);
  ConditionVar.Spill(1);
end;

constructor TLapeTree_While.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FBreakStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore, True);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore, True);
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

function TLapeTree_While.addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean;
begin
  if JumpSafe then
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
  Result := True;
end;

function TLapeTree_While.addContinueStatement(JumpSafe: Boolean;var Offset: Integer; Pos: PDocPos = nil): Boolean;
begin
  if JumpSafe then
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
  Result := True;
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

  if WalkDown then
    IncDec := TLapeTree_InternalMethod_Dec.Create(Self)
  else
    IncDec := TLapeTree_InternalMethod_Inc.Create(Self);
  try
    IncDec.addParam(TLapeTree_ResVar.Create(TLapeTree_Operator(FCondition).Left.Compile(Offset).IncLock(), FCondition));
    if (FStep <> nil) then
    begin
      IncDec.addParam(FStep);
      IncDec.Compile(Offset).Spill(1);
      Step := IncDec.Params.Delete(1);
    end
    else
      IncDec.Compile(Offset).Spill(1);
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

constructor TLapeTree_For.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCounter := nil;
  FLimit := nil;
  FStep := nil;
  WalkDown := False;
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

  CounterVar := nil;
  Count := FCounter.Compile(Offset).IncLock();
  try
    if (not FLimit.CompileToTempVar(Offset, Lim)) or (not Lim.HasType()) or (not Lim.VarType.IsOrdinal(True)) then
      LapeException(lpeInvalidEvaluation, FLimit.DocPos);

    if Count.HasType() and (not Count.Writeable) then
    begin
      CounterVar := FCompiler.getTempVar(Count.VarType);
      Count := CounterVar.VarType.Eval(op_Assign, Result, _ResVar.New(CounterVar), Count, [], Offset, @FCounter._DocPos);
    end;
    if (not Count.HasType()) or (not Count.Writeable) or (not Count.VarType.IsOrdinal(True)) then
      LapeException(lpeInvalidIterator, FCounter.DocPos);

    if WalkDown then
      FCondition := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, Self)
    else
      FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
    TLapeTree_Operator(FCondition).Left := TLapeTree_ResVar.Create(Count.IncLock(), FCounter);
    TLapeTree_Operator(FCondition).Right := TLapeTree_ResVar.Create(Lim.IncLock(), FLimit);

    Result := inherited;
  finally
    setCondition(nil);
    Count.Spill(1);
    Lim.Spill(1);
  end;
end;

procedure TLapeTree_Repeat.setCondition(Node: TLapeTree_ExprBase);
begin
  if (FCondition <> nil) and (FCondition <> Node) then
    FCondition.Free();
  FCondition := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Repeat.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Repeat.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCondition = Node) then
    FCondition := nil
  else if (FBody = Node) then
    FBody := nil;
end;

constructor TLapeTree_Repeat.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FCondition := nil;
  FBody := nil;

  FBreakStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore, True);
  FContinueStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupIgnore, True);
end;

destructor TLapeTree_Repeat.Destroy;
begin
  setCondition(nil);
  setBody(nil);
  FBreakStatements.Free();
  FContinueStatements.Free();
  inherited;
end;

function TLapeTree_Repeat.Compile(var Offset: Integer): TResVar;
var
  i, co, StartOffset: Integer;
  ConditionVar: TResVar;
begin
  FBreakStatements.Clear();
  FContinueStatements.Clear();

  Result := NullResVar;
  StartOffset := FCompiler.Emitter.CheckOffset(Offset);
  if (FBody <> nil) then
    FBody.CompileToTempVar(Offset, Result);

  for i := 0 to FContinueStatements.Count - 1 do
    with FContinueStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  if (not FCondition.CompileToBoolVar(Offset, ConditionVar)) then
    LapeException(lpeInvalidCondition, [FCondition, Self]);
  FCompiler.Emitter._JmpRIfNot(StartOffset - Offset, ConditionVar, Offset, @_DocPos);

  for i := 0 to FBreakStatements.Count - 1 do
    with FBreakStatements[i] do
    begin
      co := CodeOffset;
      if JumpSafe then
        FCompiler.Emitter._JmpSafeR(Offset - co, co, @DocPos)
      else
        FCompiler.Emitter._JmpR(Offset - co, co, @DocPos);
    end;

  ConditionVar.Spill(1);
end;

function TLapeTree_Repeat.addBreakStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean;
begin
  if JumpSafe then
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
  Result := True;
end;

function TLapeTree_Repeat.addContinueStatement(JumpSafe: Boolean; var Offset: Integer; Pos: PDocPos = nil): Boolean;
begin
  if JumpSafe then
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0, Offset, Pos), Pos, JumpSafe))
  else
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0, Offset, Pos), Pos, JumpSafe));
  Result := True;
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
    tmp.Spill(1);

    FCompiler.Emitter._JmpR(Offset - o_jmp, o_jmp, @FExcept._DocPos);
  end;
  if (FFinally <> nil) then
  begin
    if (o_except <> 0) then
      FCompiler.Emitter._IncTry(o_except - o_try, Offset - o_except, o_try, @_DocPos)
    else
      FCompiler.Emitter._IncTry(Offset - o_try, Try_NoExcept, o_try, @_DocPos);

    FFinally.CompileToTempVar(Offset, tmp);
    tmp.Spill(1);
  end;
  FCompiler.Emitter._EndTry(Offset, @_DocPos);
end;

end.

