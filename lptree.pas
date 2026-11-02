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
  end;

  TLapeExpressionList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_ExprBase>;
  TLapeStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeTree_Base>;
  TLapeFlowStatementList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeFlowStatement>;

  ILapeTree_CanBreak = interface(IInterface)
    ['{F63EF9FB-2DD0-4C2C-B564-1333995EE9AF}']
    function canBreak: Boolean;
    procedure addBreakStatement(JumpSafe: Boolean);
  end;

  ILapeTree_CanContinue = interface(IInterface)
    ['{411AF7EA-9D38-48DE-90E9-362E2D0D09E9}']
    function canContinue: Boolean;
    procedure addContinueStatement(JumpSafe: Boolean);
  end;

  ILapeTree_CanExit = interface(IInterface)
    ['{D1963640-31F9-4BB3-B03E-0F1DB314D98E}']
    function canExit: Boolean;
    procedure addExitStatement(JumpSafe: Boolean);
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

    procedure setParent(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(ASource: TLapeTree_Base); reintroduce; overload; virtual;
    destructor Destroy; override;

    class function isEmpty(Node: TLapeTree_Base): Boolean; virtual;

    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; virtual;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; virtual;

    function Compile: TResVar; virtual;
    // False: what it gives is not in a variable (it is on the stack)
    function CompileToTempVar(out v: TResVar): Boolean; virtual;
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
    constructor Create(ACompiler: TLapeCompilerBase); override;
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
    function CompileToTempVar(out v: TResVar): Boolean; override;
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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    procedure ClearCache; override;
    function addValue(Val: TLapeTree_Base): Integer; virtual;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function canCastTo(AType: TLapeType; Strict: Boolean): Boolean;
    function canCast: Boolean; virtual;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function isInvoking: Boolean; override;
    function Compile: TResVar; override;

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

    function Evaluate: TLapeGlobalVar; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function isInvoking: Boolean; override;
    function Compile: TResVar; override;

    property CastTo: TLapeTree_ExprBase read FCastTo write setCastTo;
    property Param: TLapeTree_ExprBase read FParam write setParam;
  end;

  TLapeTree_LeftExprBase = class(TLapeTree_DestExprBase)
  protected
    FLeft: TLapeTree_Base;
    FAssigning: TInitBool;
    FInvoked: TInitBool;

    function getLeft: TLapeTree_ExprBase; virtual;
    procedure setLeft(Node: TLapeTree_ExprBase); virtual;
    procedure setParent(Node: TLapeTree_Base); override;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    destructor Destroy; override;

    procedure ClearCache; override;

    function isAssigning: Boolean; virtual;
    function isInvoked: Boolean; virtual;
    function isInvoking: Boolean; override;
    function EvalFlags: ELapeEvalFlags; virtual;

    // Whether it is what a call calls (bUnknown: worked out from where it is, see isInvoked)
    property Invoked: TInitBool read FInvoked write FInvoked;
    property Left: TLapeTree_ExprBase read getLeft write setLeft;
  end;

  TLapeTree_Operator = class(TLapeTree_LeftExprBase)
  protected
    FOperatorType: EOperator;
    FRight: TLapeTree_ExprBase;
    FOverloadOp: Boolean;
    // What p^ of an untyped p is, see setExpectedType
    FDerefType: TLapeType;

    procedure setRight(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(AOperatorType: EOperator; ASource: TLapeTree_Base); overload; virtual;
    destructor Destroy; override;

    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function isInvoking: Boolean; override;
    // An assignment of what calls a method has that worked out first
    function RightFirst: Boolean; virtual;
    function isConstant: Boolean; override;

    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    // `and` or `or` of booleans: the right operand is left alone when the left one says enough
    function isBooleanLogic: Boolean; virtual;
    function isNegation: Boolean; virtual;
    function Compile: TResVar; override;

    property OperatorType: EOperator read FOperatorType write FOperatorType;
    property Right: TLapeTree_ExprBase read FRight write setRight;
  end;

  TLapeTree_Member = class(TLapeTree_LeftExprBase)
  protected
    FName: lpString;
    FPicked: TLapeGlobalVar;
  public
    constructor Create(ALeft: TLapeTree_ExprBase; AName: lpString; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(ALeft: TLapeTree_ExprBase; AName: lpString; ASource: TLapeTree_Base); overload; virtual;
    constructor Create(ALeft: TResVar; AName: lpString; ASource: TLapeTree_Base = nil); overload; virtual;

    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
    function isConstant: Boolean; override;

    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;

    property Name: lpString read FName;
    // The overload of Name a call picked: that method of object is the one compiled
    property Picked: TLapeGlobalVar read FPicked write FPicked;
  end;

  TLapeTree_NoFold = class(TLapeTree_ExprBase)
  public
    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
  end;

  TLapeTree_Var = class(TLapeTree_NoFold)
  protected
    FResVar: TResVar;
    function getGlobalVar: TLapeGlobalVar; virtual;
  public
    constructor Create(AResVar: TResVar; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(AResVar: TResVar; ASource: TLapeTree_Base); overload; virtual;
    constructor Create(AGlobalVar: TLapeGlobalVar; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(AGlobalVar: TLapeGlobalVar; ASource: TLapeTree_Base); overload; virtual;

    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;

    property ResVar: TResVar read FResVar;
    property GlobalVar: TLapeGlobalVar read getGlobalVar;
  end;

  // `inherited`: the method that the one that it is in overrides, of the Self of that one (as Self.Method gives it)
  TLapeTree_Inherited = class(TLapeTree_NoFold)
  protected
    FMethod: TLapeInheritedMethod;
  public
    constructor Create(AMethod: TLapeInheritedMethod; ACompiler: TLapeCompilerBase); reintroduce; virtual;

    function resType: TLapeType; override;
    function Compile: TResVar; override;

    property Method: TLapeInheritedMethod read FMethod;
  end;

  TLapeTree_WithVar = class(TLapeTree_ExprBase)
  protected
    FWithDecl: TLapeWithDeclaration;
  public
    constructor Create(AWithDecl: TLapeWithDeclaration; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(AWithDecl: TLapeWithDeclaration; ASource: TLapeTree_Base); overload; virtual;

    function isConstant: Boolean; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;

    property WithDecl: TLapeWithDeclaration read FWithDecl;
  end;

  TLapeTree_VarType = class(TLapeTree_Var)
  protected
    FVarType: TLapeType;
  public
    constructor Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(AVarType: TLapeType; ASource: TLapeTree_Base); overload; virtual;
    property VarType: TLapeType read FVarType;
  end;

  TLapeTree_Range = class(TLapeTree_Base)
  protected
    FLo: TLapeTree_ExprBase;
    FHi: TLapeTree_ExprBase;

    procedure setLo(Node: TLapeTree_ExprBase); virtual;
    procedure setHi(Node: TLapeTree_ExprBase); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    function addStatement(Statement: TLapeTree_Base; Prepend: Boolean = False): Integer; virtual;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;
    function Compile: TResVar; override;

    property Statements: TLapeStatementList read FStatements;
  end;

  TLapeTree_Method = class(TLapeTree_Base, ILapeTree_CanExit)
  protected
    FStackInfo: TLapeStackInfo;
    FStatements: TLapeTree_StatementList;
    FExitStatements: TLapeFlowStatementList;

    procedure setStatements(Node: TLapeTree_StatementList); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
  public
    // Added by the host: stays when compiling again
    Imported: Boolean;
    Method: TLapeGlobalVar;
    SelfWith: TLapeWithDeclaration;
    FreeStackInfo: Boolean;

    constructor Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase); reintroduce; virtual;
    destructor Destroy; override;

    function Walk(Visit: TLapeTree_Visit): Boolean; override;

    function canExit: Boolean; virtual;
    procedure addExitStatement(JumpSafe: Boolean); virtual;
    function Compile: TResVar; override;

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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    function addVar(AVar: TLapeVarDecl): Integer; virtual;
    function Compile: TResVar; override;

    property Vars: TLapeVarDeclList read FVars;
  end;

  TLapeTree_With = class(TLapeTree_Base)
  protected
    FWithList: TLapeExpressionList;
    FWithDecls: array of TLapeWithDeclaration;
    FBody: TLapeTree_Base;
    procedure setBody(Node: TLapeTree_Base); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    function addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclaration; virtual;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;
    function Compile: TResVar; override;

    property WithList: TLapeExpressionList read FWithList;
    property Body: TLapeTree_Base read FBody write setBody;
  end;

  TLapeTree_If = class(TLapeTree_Base)
  protected
    FStartBody: TCodeLabel;
    FCondition: TLapeTree_ExprBase;
    FBody: TLapeTree_Base;
    FElse: TLapeTree_Base;

    procedure setCondition(Node: TLapeTree_ExprBase); virtual;
    procedure setBody(Node: TLapeTree_Base); virtual;
    procedure setElse(Node: TLapeTree_Base); virtual;
    procedure DeleteChild(Node: TLapeTree_Base); override;
    function CompileBody: TResVar; virtual;
    procedure CompileCondition(JumpWhen: Boolean; Target: TCodeLabel); virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;
    function isInvoking: Boolean; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;
    function Compile: TResVar; override;

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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    function addValue(Value: TLapeTree_Base): Integer; virtual;
    procedure addFallThroughStatement(Offset: Integer); virtual;
    function Compile: TResVar; override;

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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    procedure addBranch(Branch: TLapeTree_CaseBranch); virtual;

    function Walk(Visit: TLapeTree_Visit): Boolean; override;
    function Compile: TResVar; override;

    property Condition: TLapeTree_ExprBase read FCondition write setCondition;
    property ElseBody: TLapeTree_Base read FElse write setElse;
  end;

  TLapeTree_While = class(TLapeTree_If, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FContinueCount: Integer;
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;
    function CompileBody: TResVar; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    function canBreak: Boolean; virtual;
    function canContinue: Boolean; virtual;
    procedure addBreakStatement(JumpSafe: Boolean); virtual;
    procedure addContinueStatement(JumpSafe: Boolean); virtual;
    function Compile: TResVar; override;
  end;

  TLapeTree_Repeat = class(TLapeTree_If, ILapeTree_CanBreak, ILapeTree_CanContinue)
  protected
    FBreakStatements: TLapeFlowStatementList;
    FContinueStatements: TLapeFlowStatementList;
    function CompileBody: TResVar; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;

    function canBreak: Boolean; virtual;
    function canContinue: Boolean; virtual;
    procedure addBreakStatement(JumpSafe: Boolean); virtual;
    procedure addContinueStatement(JumpSafe: Boolean); virtual;
    function Compile: TResVar; override;
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
    function CompileBody: TResVar; override;
    procedure CompileBodyForIn; virtual;
    function CompileForIn: TResVar; virtual;
  public
    LoopType: ELoopType;
    LoopOverWhat: ELoopOverWhat;
    Container: TResVar;
    CheckInVar: TResVar; // loopOver optional: if not (item in CheckInVar) then Continue;

    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;
    function Compile: TResVar; override;

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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    destructor Destroy; override;
    function Walk(Visit: TLapeTree_Visit): Boolean; override;
    function Compile: TResVar; override;

    property Body: TLapeTree_Base read FBody write setBody;
    property ExceptBody: TLapeTree_Base read FExcept write setExcept;
    property FinallyBody: TLapeTree_Base read FFinally write setFinally;
  end;

function getFlowStatement(Offset: Integer; const Pos: TDocPos): TLapeFlowStatement; {$IFDEF Lape_Inline}inline;{$ENDIF}
function EnsureCompound(Compiler: TLapeCompilerBase; Statement: TLapeTree_Base; AllowNil: Boolean = True): TLapeTree_StatementList;
function GetMagicMethodOrNil(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; AResult: TLapeType = nil): TResVar;
function HasMagicMethod(Compiler: TLapeCompilerBase; AName: lpString; AParams: array of TLapeType; ARes: TLapeType): Boolean;

const
  NullFlowStatement: TLapeFlowStatement = (CodeOffset: 0; DocPos: (Line: 0; Col: 0; FileName: ''));

implementation

uses
  Math,
  lpvartypes_ord, lpvartypes_record, lpvartypes_array, lpvartypes_object,
  lpmessages, lpeval, lpinterpreter_types, lpinvoke, lpinternalmethods;

function getFlowStatement(Offset: Integer; const Pos: TDocPos): TLapeFlowStatement;
begin
  Result := NullFlowStatement;
  Result.CodeOffset := Offset;
  Result.DocPos := Pos;
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
    Method := Compiler.getBaseType(ltPointer).EmptyValue;

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

constructor TLapeTree_Base.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create();
  Assert(ACompiler <> nil);

  FParent := nil;
  FCompiler := ACompiler;
  FCompilerOptions := FCompiler.Options;
  FDocPos := FCompiler.DocPos;
end;

constructor TLapeTree_Base.Create(ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(ASource.Compiler);
  FDocPos := ASource.DocPos;
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

function TLapeTree_Base.Compile: TResVar;
begin
  Result := NullResVar;
  LapeException(lpeImpossible);
end;

function TLapeTree_Base.CompileToTempVar(out v: TResVar): Boolean;
begin
  v := Compile();
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

constructor TLapeTree_ExprBase.Create(ACompiler: TLapeCompilerBase);
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

// Cond as a condition: jumps to Target when it is JumpWhen and goes on when it is not
procedure CompileJump(Cond: TLapeTree_ExprBase; JumpWhen: Boolean; Target: TCodeLabel);
var
  Emitter: TLapeCodeEmitter;
  Op: TLapeTree_Operator;
  Res: TResVar;
  Decides: Boolean;
  JumpOver: TCodeLabel;
begin
  Emitter := Cond.Compiler.Emitter;

  if (Cond is TLapeTree_Operator) then
  begin
    Op := TLapeTree_Operator(Cond);

    // `not a`: a, jumping the other way
    if Op.isNegation() then
    begin
      CompileJump(Op.Left, not JumpWhen, Target);
      Exit;
    end;

    // `a and b`, `a or b`: a jump for each side
    if Op.isBooleanLogic() then
    begin
      // (what the left operand says enough with)
      Decides := (Op.OperatorType = op_OR);

      // Either side alone can take the jump
      if (JumpWhen = Decides) then
      begin
        CompileJump(Op.Left, JumpWhen, Target);
        CompileJump(Op.Right, JumpWhen, Target);
      end
      // Only the right side takes it, the left can only jump over the right
      else
      begin
        JumpOver := Emitter.NewLabel();
        CompileJump(Op.Left, Decides, JumpOver);
        CompileJump(Op.Right, JumpWhen, Target);
        Emitter.PlaceLabel(JumpOver);
      end;
      Exit;
    end;
  end;

  // Any other value: a variable, a comparison, a call
  Res := Cond.Compile();
  if (not Res.HasType()) then
    LapeException(lpeExpressionExpected, Cond.DocPos);

  Emitter.DocPos := Cond.DocPos;
  if JumpWhen then
    Emitter.JumpTo(Target, Emitter._JmpRIf(0, Res))
  else
    Emitter.JumpTo(Target, Emitter._JmpRIfNot(0, Res));
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
        Replacement := TLapeTree_Var.Create(t, Self);
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

function TLapeTree_DestExprBase.CompileToTempVar(out v: TResVar): Boolean;
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

constructor TLapeTree_OpenArray.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);
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
    begin
      ToType := FCompiler.addManagedType(TLapeType_StaticArray.Create(FRange, TLapeType_DynArray(FType).PType, FCompiler, ''));
      ToType.DocPos := FDocPos;
    end;
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
          begin
            Result := FCompiler.addManagedType(TLapeType_Set.Create(Res, FCompiler, ''));
            Result.DocPos := FDocPos;
            Exit;
          end;
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
        begin
          Result := FCompiler.addManagedType(TLapeType_Set.Create(Result, FCompiler, ''));
          Result.DocPos := FDocPos;
        end
        else if (FRange.Lo < 0) then
          Result := Result.ArrayOf
        else
        begin
          Result := FCompiler.addManagedType(TLapeType_StaticArray.Create(FRange, Result, FCompiler, ''));
          Result.DocPos := FDocPos;
        end;
    end
    else
      Result := FCompiler.getBaseType(ltVariant).ArrayOf;
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
    FieldVar: TLapeGlobalVar;
  begin
    FieldVar := nil;

    if (FValues.Count > TLapeType_Record(FType).FieldMap.Count) then
      LapeException(lpeInvalidRange, DocPos);
    for i := 0 to FValues.Count - 1 do
      if (FValues[i] is TLapeTree_ExprBase) then
      try
        try
          FieldVar := FType.EvalMemberConst(Result, TLapeType_Record(FType).FieldMap.Key[i], [lefAssigning]);
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
      if (FType <> nil) then
        LapeExceptionFmt(lpeIncompatibleAssignment, ['open array', FType.AsString], DocPos)
      else
        LapeException(lpeInvalidCast, DocPos);

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

function TLapeTree_OpenArray.Compile: TResVar;

  procedure doSetOrDynArray;
  var
    i: Integer;
    tmpVar, HiVar, Val, Dst: TResVar;
    Counter: TLapeVar;
  begin
    tmpVar := NullResVar;
    Counter := nil;

    for i := 0 to FValues.Count - 1 do
      if isEmpty(FValues[i]) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (FValues[i] is TLapeTree_ExprBase) then
      begin
        Val := FValues[i].Compile();
        Dst := Result;
        FCompiler.Emitter.DocPos := FValues[i].DocPos;
        Result := Result.VarType.Eval(op_Plus, Dst, Result, Val, [lefConstRangeCheck]);
        Assert(Dst.VarPos.MemPos <> NullResVar.VarPos.MemPos);
      end
      else if (FValues[i] is TLapeTree_Range) then
      begin
        Assert(FType is TLapeType_Set);
        Counter := FCompiler.getTempVar(TLapeType_Set(FType).Element);
        Val := TLapeTree_Range(FValues[i]).Lo.Compile();
        FCompiler.Emitter.DocPos := FValues[i].DocPos;
        tmpVar := Counter.VarType.Eval(op_Assign, tmpVar, _ResVar.New(Counter), Val, []);
        // (the last is kept in a variable: the loop reads it each round)
        if (not TLapeTree_Range(FValues[i]).Hi.CompileToTempVar(HiVar)) then
          LapeException(lpeInvalidRange, FValues[i].DocPos);

        with TLapeTree_For.Create(FValues[i]) do
        try
          Counter := TLapeTree_Var.Create(tmpVar, FValues[i]);
          Limit := TLapeTree_Var.Create(HiVar, FValues[i]);
          Body := TLapeTree_Operator.Create(op_Plus, FValues[i]);
          with TLapeTree_Operator(Body) do
          begin
            Dest := Result;
            Left := TLapeTree_Var.Create(Result, FValues[i]);
            Right := TLapeTree_Var.Create(tmpVar, FValues[i]);
          end;
          Result := Compile();
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
    Val, Item, tmpVar: TResVar;
  begin
    if (FValues.Count <> TLapeType_StaticArray(FType).Range.Hi - TLapeType_StaticArray(FType).Range.Lo + 1) then
      LapeException(lpeInvalidRange, DocPos);

    for i := 0 to FValues.Count - 1 do
      if (not (FValues[i] is TLapeTree_ExprBase)) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else
      begin
        Val := FValues[i].Compile();
        tmpVar := NullResVar;
        FCompiler.Emitter.DocPos := FValues[i].DocPos;
        Item := Result.VarType.Eval(op_Index, tmpVar, Result, _ResVar.New(FCompiler.getIntConst(i + TLapeType_StaticArray(FType).Range.Lo, ltNativeInt)), [lefConstRangeCheck, lefAssigning]);
        tmpVar := NullResVar;
        Item.VarType.Eval(op_Assign, tmpVar, Item, Val, [lefConstRangeCheck]);
      end;
  end;

  procedure doRecord;
  var
    i: Integer;
    Val, Item, tmpVar: TResVar;
  begin
    if (FValues.Count > TLapeType_Record(FType).FieldMap.Count) then
      LapeException(lpeInvalidRange, DocPos);

    for i := 0 to FValues.Count - 1 do
      if (FValues[i] is TLapeTree_ExprBase) then
      begin
        Val := FValues[i].Compile();
        tmpVar := NullResVar;
        FCompiler.Emitter.DocPos := FValues[i].DocPos;
        Item := Result.VarType.EvalMember(tmpVar, Result, TLapeType_Record(FType).FieldMap.Key[i], [lefConstRangeCheck, lefAssigning]);
        tmpVar := NullResVar;
        Item.VarType.Eval(op_Assign, tmpVar, Item, Val, [lefConstRangeCheck]);
      end
      else if (not isEmpty(FValues[i])) then
        LapeException(lpeInvalidCast, FValues[i].DocPos)
      else if (lcoAlwaysInitialize in FCompilerOptions) then
      begin
        tmpVar := NullResVar;
        FCompiler.Emitter.DocPos := DocPos;
        FCompiler.VarToDefault(Result.VarType.EvalMember(tmpVar, Result, TLapeType_Record(FType).FieldMap.Key[i], [lefConstRangeCheck]));
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
    end
    else if (FType <> nil) then
      LapeExceptionFmt(lpeIncompatibleAssignment, ['open array', FType.AsString], DocPos)
    else
      LapeException(lpeInvalidCast, DocPos);
  end;

  Dest := NullResVar;
  Result := _ResVar.New(FCompiler.getTempVar(FType));

  FCompiler.Emitter.DocPos := DocPos;
  if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    FCompiler.VarToDefault(Result);

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

function TLapeTree_Cast.Compile: TResVar;
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

    Result := FParam.Compile();
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

      FCompiler.Emitter.DocPos := DocPos;
      Result := DestVar.VarType.Eval(op_Assign, tmpVar, DestVar, Result, []);

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

function TLapeTree_LeftExprBase.getLeft: TLapeTree_ExprBase;
begin
  if (FLeft is TLapeTree_ExprBase) then
    Result := TLapeTree_ExprBase(FLeft)
  else
    Result := nil;
end;

procedure TLapeTree_LeftExprBase.setLeft(Node: TLapeTree_ExprBase);
begin
  if (FLeft <> nil) and (FLeft <> Node) then
  begin
    ClearCache();
    FLeft.Free();
  end;
  FLeft := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_LeftExprBase.setParent(Node: TLapeTree_Base);
begin
  if (Node <> FParent) then
    ClearCache();
  inherited;
end;

procedure TLapeTree_LeftExprBase.DeleteChild(Node: TLapeTree_Base);
begin
  if (FLeft = Node) then
    FLeft := nil;
  ClearCache();
end;

destructor TLapeTree_LeftExprBase.Destroy;
begin
  setLeft(nil);
  inherited;
end;

procedure TLapeTree_LeftExprBase.ClearCache;
begin
  FAssigning := bUnknown;
  FInvoked := bUnknown;
  inherited;
end;

function TLapeTree_LeftExprBase.isAssigning: Boolean;
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

function TLapeTree_LeftExprBase.isInvoked: Boolean;
begin
  if (FInvoked = bUnknown) and (not isEmpty(FParent)) then
  begin
    Result := (
      ((FParent is TLapeTree_Invoke) and (TLapeTree_Invoke(FParent).Callee = Self)) or
      ((FParent is TLapeTree_LeftExprBase) and TLapeTree_LeftExprBase(FParent).isInvoked())
    );

    if Result then
      FInvoked := bTrue
    else
      FInvoked := bFalse;
  end;

  Result := (FInvoked = bTrue);
end;

function TLapeTree_LeftExprBase.isInvoking: Boolean;
begin
  Result := (FLeft <> nil) and FLeft.isInvoking();
end;

function TLapeTree_LeftExprBase.EvalFlags: ELapeEvalFlags;
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
  if (FRight = Node) then
    FRight := nil;
  inherited;
end;

constructor TLapeTree_Operator.Create(AOperatorType: EOperator; ACompiler: TLapeCompilerBase);
begin
  // Left.Name is a TLapeTree_Member
  if (AOperatorType = op_Dot) then
    LapeException(lpeImpossible);
  inherited Create(ACompiler);

  FLeft := nil;
  FRight := nil;
  FOperatorType := AOperatorType;
  FOverloadOp := False;
end;

constructor TLapeTree_Operator.Create(AOperatorType: EOperator; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AOperatorType, ASource.Compiler);
  FDocPos := ASource.DocPos;
  FCompilerOptions := ASource.CompilerOptions;
end;

destructor TLapeTree_Operator.Destroy;
begin
  setRight(nil);
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
        // p^ of an untyped p is what is expected (one known while compiling is cast: it folds)
        if (Typ <> nil) and (Typ is TLapeType_Pointer) and (not TLapeType_Pointer(Typ).HasType()) and (not Left.isConstant()) then
        begin
          if (FDerefType = nil) then
            FDerefType := ExpectType;
        end
        else if (Typ <> nil) and (Typ is TLapeType_Pointer) and (not TLapeType_Pointer(Typ).HasType()) then
        begin
          Cast := TLapeTree_Cast.Create(Left);
          Cast.CastTo := TLapeTree_VarType.Create(FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, ExpectType, False)), Self);
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
    else if ((FLeft is TLapeTree_Var) and (TLapeTree_Var(FLeft).GlobalVar <> nil)) and
      TLapeTree_Var(FLeft).GlobalVar.HasType() and
      ((FRight = nil) or ((FRight is TLapeTree_Var) and (TLapeTree_Var(FRight).GlobalVar <> nil)))
    then
      if (FRight <> nil) then
        Result := TLapeTree_Var(FLeft).GlobalVar.VarType.CanEvalConst(
          OperatorType,
          TLapeTree_Var(FLeft).GlobalVar,
          TLapeTree_Var(FRight).GlobalVar)
      else
        Result := TLapeTree_Var(FLeft).GlobalVar.VarType.CanEvalConst(
          OperatorType,
          TLapeTree_Var(FLeft).GlobalVar,
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

function TLapeTree_Operator.resType: TLapeType;
var
  LeftType, RightType: TLapeType;
begin
  if (FResType = nil) then
  try
    if (FLeft <> nil) then
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
        Result := FCompiler.getBaseType(ltBoolean)
    end
    else if (FDerefType <> nil) then
      Result := FDerefType
    else if ((FRight is TLapeTree_Var) and (TLapeTree_Var(FRight).GlobalVar <> nil)) then
      Result := LeftType.EvalRes(FOperatorType, TLapeTree_Var(FRight).GlobalVar, EvalFlags())
    else if (LeftType <> nil) then
      Result := LeftType.EvalRes(FOperatorType, RightType, EvalFlags());

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
end;

function TLapeTree_Operator.isBooleanLogic: Boolean;
begin
  Result := (FOperatorType in [op_AND, op_OR]) and (FLeft <> nil) and (FRight <> nil) and
    (Left.resType() <> nil) and (Left.resType().BaseType in LapeBoolTypes) and
    (FRight.resType() <> nil) and (FRight.resType().BaseType in LapeBoolTypes);
end;

function TLapeTree_Operator.isNegation: Boolean;
begin
  Result := (FOperatorType = op_NOT) and (FLeft <> nil) and (FRight = nil) and
    (Left.resType() <> nil) and (Left.resType().BaseType in LapeBoolTypes);
end;

function TLapeTree_Operator.Evaluate: TLapeGlobalVar;
var
  LeftVar, RightVar, Shared: TLapeGlobalVar;
begin
  if (FRes = nil) then
  begin
    if (FLeft <> nil) then
      if (FLeft is TLapeTree_ExprBase) then
        LeftVar := TLapeTree_ExprBase(FLeft).Evaluate()
      else
        LapeException(lpeImpossible, DocPos)
    else
      LeftVar := nil;

    if (FOperatorType in AssignOperators) and ((LeftVar = nil) or LeftVar.isConstant) then
      LapeException(lpeCannotAssign, [FLeft, Self]);

    if (LeftVar <> nil) and (not isEmpty(FRight)) then
    begin
      FRight := FRight.setExpectedType(LeftVar.VarType) as TLapeTree_ExprBase;
      if (FOperatorType = op_Index) then
        FRight := FRight.setExpectedType(FCompiler.getBaseType(ltSizeInt)) as TLapeTree_ExprBase;
    end;

    // The left side says it: the right side is not worked out, it could fail
    if isBooleanLogic() and ((FOperatorType = op_AND) xor (LeftVar.AsInteger <> 0)) then
      Exit(FCompiler.getBoolConst(FOperatorType = op_OR, resType().BaseType));

    if (FRight <> nil) then
      RightVar := FRight.Evaluate()
    else
      RightVar := nil;

    try
      if (LeftVar <> nil) and LeftVar.HasType() then
        FRes := LeftVar.VarType.EvalConst(FOperatorType, LeftVar, RightVar, EvalFlags())
      else with TLapeType.Create(ltUnknown, FCompiler) do
      try
        FRes := EvalConst(FOperatorType, LeftVar, RightVar, EvalFlags());
      finally
        Free();
      end;

      // a whole number, boolean or char of a base type that was made here is shared
      if (FRes <> nil) and (FRes.DeclarationList = nil) and FRes.isConstant and FRes.Known and FRes.HasType() and
         (FRes.VarType = FCompiler.getBaseType(FRes.VarType.BaseType)) and
         (FRes.VarType.BaseType in LapeIntegerTypes + LapeBoolTypes + LapeCharTypes)
      then
      begin
        Shared := FCompiler.getConst(FRes.Ptr, FRes.VarType.BaseType);
        FRes.Free();
        FRes := Shared;
      end;

      FRes := FCompiler.addManagedDecl(FRes) as TLapeGlobalVar;
    except on E: lpException do
      LapeException(lpString(E.Message), DocPos);
    end;
  end;
  Result := inherited;
end;

function TLapeTree_Operator.Compile: TResVar;

  function doOperatorOverload: TResVar;
  begin
    Dest := NullResVar;
    with TLapeTree_Invoke.Create('!op_' + op_name[FOperatorType], Self) do
    try
      addParam(Left);
      addParam(Right);
      Result := Compile();
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
      if (not Value.CompileToTempVar(Result)) then
        LapeExceptionFmt(lpeExpected, ['Value'], Value.DocPos);
    end;

    // if (Left = Right) then Found := True
    function IfFound(Left, Right: TResVar): TLapeTree_If;
    var
      Condition, SetFound: TLapeTree_Operator;
    begin
      Result := TLapeTree_If.Create(Self);
      Condition := TLapeTree_Operator.Create(op_cmp_Equal, Self);
      Result.Condition := Condition;
      Condition.Left := TLapeTree_Var.Create(Left, Self);
      Condition.Right := TLapeTree_Var.Create(Right, Self);

      SetFound := TLapeTree_Operator.Create(op_Assign, Self);
      Result.Body := SetFound;
      SetFound.Left := TLapeTree_Var.Create(FoundVar, Self);
      SetFound.Right := TLapeTree_Var.Create(FCompiler.getBoolConst(True), Self);
    end;

  var
    i: Integer;
    Value: TLapeTree_Base;
    HiVar, CounterVar, tmpVar: TResVar;
    Statement: TLapeTree_Base;
  begin
    Dest := NullResVar;

    FoundVar := _ResVar.New(FCompiler.getTempVar(ltBoolean));
    tmpVar := NullResVar;
    FCompiler.Emitter.DocPos := DocPos;
    FoundVar.VarType.Eval(op_Assign, tmpVar, FoundVar, _ResVar.New(FCompiler.getBoolConst(False)), []);

    for i := 0 to TLapeTree_OpenArray(FRight).Values.Count - 1 do
    begin
      Value := TLapeTree_OpenArray(FRight).Values[i];
      if (Value is TLapeTree_Range) then
      begin
        CounterVar := _ResVar.New(FCompiler.getTempVar(ltInt64));
        tmpVar := NullResVar;
        HiVar := AsInteger(CompileValue(TLapeTree_Range(Value).Lo));
        FCompiler.Emitter.DocPos := Value.DocPos;
        CounterVar.VarType.Eval(op_Assign, tmpVar, CounterVar, HiVar, []);
        HiVar := AsInteger(CompileValue(TLapeTree_Range(Value).Hi));

        Statement := TLapeTree_For.Create(Self);
        try
          TLapeTree_For(Statement).Counter := TLapeTree_Var.Create(CounterVar, Self);
          TLapeTree_For(Statement).Limit := TLapeTree_Var.Create(HiVar, Self);
          TLapeTree_For(Statement).Body := IfFound(AsInteger(LeftVar), CounterVar);
          Statement.Compile();
        finally
          Statement.Free();
        end;
      end
      else if (Value is TLapeTree_ExprBase) then
      begin
        Statement := IfFound(LeftVar, CompileValue(Value));
        try
          Statement.Compile();
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
  LeftVar, RightVar, tmpVar: TResVar;
  IsFalse, Done: TCodeLabel;

  procedure CompileLeft;
  begin
    if (FLeft <> nil) then
      LeftVar := TLapeTree_ExprBase(FLeft).Compile();

    if (FLeft <> nil) and (not LeftVar.HasType()) then
      LeftVar.VarType := FCompiler.addManagedType(TLapeType.Create(ltUnknown, FCompiler));

    if (FOperatorType in AssignOperators) and LeftVar.isConstant then
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

  // Jumps to IsFalse when it is, and the value is what comes of that
  if isBooleanLogic() then
  begin
    Dest := NullResVar;
    Result := _ResVar.New(FCompiler.getTempVar(resType()));
    Result.isConstant := False;
    IsFalse := FCompiler.Emitter.NewLabel();
    Done := FCompiler.Emitter.NewLabel();

    CompileJump(Self, False, IsFalse);

    FCompiler.Emitter.DocPos := DocPos;
    tmpVar := NullResVar;
    Result.VarType.Eval(op_Assign, tmpVar, Result, _ResVar.New(FCompiler.getBoolConst(True, Result.VarType.BaseType)), [lefConstRangeCheck]);
    FCompiler.Emitter.JumpTo(Done, FCompiler.Emitter._JmpR(0));
    FCompiler.Emitter.PlaceLabel(IsFalse);
    tmpVar := NullResVar;
    Result.VarType.Eval(op_Assign, tmpVar, Result, _ResVar.New(FCompiler.getBoolConst(False, Result.VarType.BaseType)), [lefConstRangeCheck]);
    FCompiler.Emitter.PlaceLabel(Done);

    Result.isConstant := True;
    Exit;
  end;

  if (FLeft <> nil) then
  begin
    resType();
    if FOverloadOp then
      Exit(doOperatorOverload());
  end;

  // x in [...]: a set of x's type when that can be one (enum, AnsiChar, Byte), else a loop over the values
  if (FOperatorType = op_IN) and (FRight is TLapeTree_OpenArray) and
     (not inSet(TLapeTree_OpenArray(FRight), TLapeTree_ExprBase(FLeft).resType())) then
  begin
    CompileLeft();
    Exit(doInList(LeftVar));
  end;

  // (not for a variable by itself: where it is cannot move, and the right side is worked out in it, see below)
  if RightFirst() and (not (FLeft is TLapeTree_Var)) then
  begin
    ExpectRight(TLapeTree_ExprBase(FLeft).resType());
    RightVar := FRight.Compile();
    // (one on the stack is put in a temporary: the left side is worked out after it)
    if (RightVar.VarPos.MemPos = mpStack) and RightVar.HasType() then
    begin
      Typ := RightVar.VarType;
      RightVar := _ResVar.New(FCompiler.getTempVar(Typ));
      FCompiler.Emitter.DocPos := DocPos;
      FCompiler.Emitter._PopStackToVar(Typ.Size, RightVar.VarPos.StackVar.Offset);
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
      if (FOperatorType = op_Assign) and (FLeft <> nil) and (not LeftVar.isConstant) and
         (FRight is TLapeTree_DestExprBase) and (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      begin
        TLapeTree_DestExprBase(FRight).Dest := LeftVar;
        RightVar := FRight.Compile();
        if (TLapeTree_DestExprBase(FRight).Dest.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
        begin
          Dest := NullResVar;
          Result := LeftVar;
          TLapeTree_DestExprBase(FRight).Dest := NullResVar;

          Exit;
        end;
      end else
        RightVar := FRight.Compile();
    end;
  end;

  try
    FCompiler.Emitter.DocPos := DocPos;
    Result := LeftVar.VarType.Eval(FOperatorType, FDest, LeftVar, RightVar, EvalFlags());
    // (writeable, as through a ^FDerefType)
    if (FDerefType <> nil) then
    begin
      Result.VarType := FDerefType;
      Result.isConstant := False;
    end;
  except
    on E: lpException do
      LapeExceptionReraise(E, DocPos);
  end;
end;

constructor TLapeTree_Member.Create(ALeft: TLapeTree_ExprBase; AName: lpString; ACompiler: TLapeCompilerBase);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler);

  FName := AName;
  FLeft := nil;
  setLeft(ALeft);
end;

constructor TLapeTree_Member.Create(ALeft: TLapeTree_ExprBase; AName: lpString; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(ALeft, AName, ASource.Compiler);
  FDocPos := ASource.DocPos;
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Member.Create(ALeft: TResVar; AName: lpString; ASource: TLapeTree_Base = nil);
begin
  if (ASource <> nil) then
    Create(TLapeTree_Var.Create(ALeft, ASource), AName, ASource)
  else
  begin
    Assert(ALeft.HasType() and (ALeft.VarType.Compiler <> nil));
    Create(TLapeTree_Var.Create(ALeft, ALeft.VarType.Compiler), AName, ALeft.VarType.Compiler);
  end;
end;

function TLapeTree_Member.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
begin
  if (not isEmpty(FLeft)) then
    FLeft := Left.FoldConstants() as TLapeTree_ExprBase;

  Result := inherited;
end;

// (when the type of Left works it out while compiling, see EvalMemberConst)
function TLapeTree_Member.isConstant: Boolean;
var
  LeftVar: TLapeGlobalVar;
begin
  if (FConstant = bUnknown) then
  begin
    FConstant := bFalse;
    if (not isEmpty(FLeft)) and (((FLeft is TLapeTree_Var) and (TLapeTree_Var(FLeft).GlobalVar <> nil)) or Left.isConstant()) then
    try
      if ((FLeft is TLapeTree_Var) and (TLapeTree_Var(FLeft).GlobalVar <> nil)) then
        LeftVar := TLapeTree_Var(FLeft).GlobalVar
      else
        LeftVar := Left.Evaluate();

      if (LeftVar <> nil) and LeftVar.HasType() then
        FRes := LeftVar.VarType.EvalMemberConst(LeftVar, FName, EvalFlags());
      if (FRes <> nil) then
      begin
        FRes := FCompiler.addManagedDecl(FRes) as TLapeGlobalVar;
        FConstant := bTrue;
      end;
    except
      on E: lpException do
        FRes := nil;
    end;
  end;
  Result := inherited;
end;

function TLapeTree_Member.resType: TLapeType;
var
  LeftType: TLapeType;
begin
  if (FResType = nil) and (FLeft <> nil) then
  begin
    LeftType := Left.resType();
    if (LeftType <> nil) then
      FResType := LeftType.EvalMemberRes(FName, EvalFlags());
  end;

  Result := inherited;
end;

function TLapeTree_Member.Evaluate: TLapeGlobalVar;
begin
  isConstant();
  Result := inherited;
end;

function TLapeTree_Member.Compile: TResVar;
var
  LeftVar: TResVar;
begin
  Result := NullResVar;

  if (FLeft = nil) then
    LapeException(lpeExpressionExpected, DocPos);
  LeftVar := Left.Compile();
  if (not LeftVar.HasType()) then
    LeftVar.VarType := FCompiler.addManagedType(TLapeType.Create(ltUnknown, FCompiler));

  try
    FCompiler.Emitter.DocPos := DocPos;
    if (FPicked <> nil) then
    begin
      FDest := NullResVar;
      Result := LeftVar.VarType.EvalMethodOfObject(LeftVar, FPicked, EvalFlags());
    end
    else
      Result := LeftVar.VarType.EvalMember(FDest, LeftVar, FName, EvalFlags());
  except
    on E: lpException do
      LapeExceptionReraise(E, DocPos);
  end;
end;

function TLapeTree_NoFold.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
begin
  Result := Self;
end;

constructor TLapeTree_Var.Create(AResVar: TResVar; ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);
  FResVar := AResVar;

  FResType := FResVar.VarType;
  FRes := FResVar.VarPos.GlobalVar;
  if (FResVar.VarPos.MemPos = mpMem) and (not FResVar.VarPos.isPointer) and (FResVar.VarPos.GlobalVar <> nil) and FResVar.VarPos.GlobalVar.Known then
    FConstant := bTrue
  else
    FConstant := bFalse;
end;

constructor TLapeTree_Var.Create(AResVar: TResVar; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AResVar, ASource.Compiler);
  FDocPos := ASource.DocPos;
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Var.Create(AGlobalVar: TLapeGlobalVar; ACompiler: TLapeCompilerBase);
begin
  if (ACompiler <> nil) and (AGlobalVar <> nil) and (AGlobalVar.DeclarationList = nil) then
    AGlobalVar := TLapeGlobalVar(ACompiler.addManagedDecl(AGlobalVar));
  Create(_ResVar.New(AGlobalVar), ACompiler);
end;

constructor TLapeTree_Var.Create(AGlobalVar: TLapeGlobalVar; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AGlobalVar, ASource.Compiler);
  FDocPos := ASource.DocPos;
  FCompilerOptions := ASource.CompilerOptions;
end;

function TLapeTree_Var.getGlobalVar: TLapeGlobalVar;
begin
  Result := nil;
  with FResVar do
    if (VarPos.MemPos = mpMem) and (VarPos.GlobalVar <> nil) and (not VarPos.isPointer) and (VarPos.Offset = 0) and
       (VarType = VarPos.GlobalVar.VarType)
    then
      Result := VarPos.GlobalVar;
end;

function TLapeTree_Var.Evaluate: TLapeGlobalVar;
begin
  if (FResVar.VarPos.MemPos <> mpMem) then
    LapeException(lpeConstantExpected, DocPos)
  else
    Result := inherited;
end;

function TLapeTree_Var.Compile: TResVar;
begin
  Result := FResVar;
end;

constructor TLapeTree_Inherited.Create(AMethod: TLapeInheritedMethod; ACompiler: TLapeCompilerBase);
begin
  Assert(AMethod <> nil);
  inherited Create(ACompiler);
  FMethod := AMethod;
end;

function TLapeTree_Inherited.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FMethod.VarType;
  Result := inherited;
end;

function TLapeTree_Inherited.Compile: TResVar;
var
  SelfVar: TResVar;
begin
  Result := _ResVar.New(FMethod);
  if (FMethod.SelfVar = nil) or (not MethodOfObject(Result.VarType)) then
    Exit;

  // (the method with the Self that it is called on, as Self.Method)
  SelfVar := _ResVar.New(FMethod.SelfVar).InScope(FCompiler.StackInfo);
  FCompiler.Emitter.DocPos := DocPos;
  Result := SelfVar.VarType.EvalMethodOfObject(SelfVar, FMethod, [lefInvoking]);
end;

constructor TLapeTree_WithVar.Create(AWithDecl: TLapeWithDeclaration; ACompiler: TLapeCompilerBase);
begin
  Assert(AWithDecl <> nil);
  inherited Create(ACompiler);
  FWithDecl := AWithDecl;
  FResType := FWithDecl.WithType;
end;

constructor TLapeTree_WithVar.Create(AWithDecl: TLapeWithDeclaration; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AWithDecl, ASource.Compiler);
  FDocPos := ASource.DocPos;
  FCompilerOptions := ASource.CompilerOptions;
end;

function TLapeTree_WithVar.isConstant: Boolean;
begin
  with FWithDecl do
    Result := (WithVar.VarPos.MemPos = mpMem) and (not WithVar.VarPos.isPointer) and (WithVar.VarPos.GlobalVar <> nil) and WithVar.VarPos.GlobalVar.Known;
end;

function TLapeTree_WithVar.Evaluate: TLapeGlobalVar;
begin
  if isConstant() then
    Result := FWithDecl.WithVar.VarPos.GlobalVar
  else
    Result := nil;
end;

function TLapeTree_WithVar.Compile: TResVar;
begin
  if (FWithDecl.WithVar.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    LapeException(lpeInvalidWithReference, DocPos);
  Result := FWithDecl.WithVar.InScope(FCompiler.StackInfo);
end;

constructor TLapeTree_VarType.Create(AVarType: TLapeType; ACompiler: TLapeCompilerBase);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getTypeVar(AVarType), ACompiler);
  FVarType := AVarType;
end;

constructor TLapeTree_VarType.Create(AVarType: TLapeType; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(AVarType, ASource.Compiler);
  FDocPos := ASource.DocPos;
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

constructor TLapeTree_Range.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);
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

constructor TLapeTree_StatementList.Create(ACompiler: TLapeCompilerBase);
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

function CompileStatement(Statement: TLapeTree_Base): TResVar;
var
  Stack: TLapeStackInfo;
  Mark: Integer;
begin
  Stack := Statement.Compiler.StackInfo;
  Mark := 0;
  if (Stack <> nil) then
    Mark := Stack.TempMark();
  Result := Statement.Compile();
  Assert(Statement.Compiler.StackInfo = Stack);
  if (Stack <> nil) then
    Stack.ReleaseTemps(Mark);
end;

function TLapeTree_StatementList.Compile: TResVar;
var
  i: Integer;
begin
  Result := NullResVar;

  i := 0;
  while (i < FStatements.Count) do
  begin
    if (FStatements[i] <> nil) then
      Result := CompileStatement(FStatements[i]);
    Inc(i);
  end;
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

constructor TLapeTree_Method.Create(AMethod: TLapeGlobalVar; AStackInfo: TLapeStackInfo; ACompiler: TLapeCompilerBase);
begin
  Assert(AMethod <> nil);
  Assert(AMethod.VarType is TLapeType_Method);
  Assert(AStackInfo <> nil);
  inherited Create(ACompiler);

  FreeStackInfo := True;
  Method := AMethod;
  SelfWith := nil;
  FStackInfo := AStackInfo;
  FExitStatements := TLapeFlowStatementList.Create(NullFlowStatement, dupAccept, True);
end;

destructor TLapeTree_Method.Destroy;
begin
  setStatements(nil);
  if FreeStackInfo then
    FStackInfo.Free();
  FExitStatements.Free();
  SelfWith.Free();
  inherited;
end;

function TLapeTree_Method.Compile: TResVar;
var
  i: Integer;
begin
  Assert(Method <> nil);

  if (lcoVerboseCompile in FCompilerOptions) then
    FCompiler.Hint('Compiling: "%s"', [GetMethodName(Method.VarType)], DocPos);

  FExitStatements.Clear();

  Result := NullResVar;

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.IncStackInfo(FStackInfo, [lsfEmit]);
  PCodePos(Method.Ptr)^ := FStackInfo.CodePos;
  try
    FStatements.Compile();
    for i := 0 to FExitStatements.Count - 1 do
      FCompiler.Emitter.PatchJump(FExitStatements[i].CodeOffset, FCompiler.Emitter.CodeLen);
  finally
    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.DecStackInfo([lsfFunction, lsfEmit], False);

    // (it starts after what of its entry it did not need)
    PCodePos(Method.Ptr)^ := FStackInfo.CodePos;
    FCompiler.Emitter.addMethod(FStackInfo.CodePos, GetMethodName(Method.VarType));
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

procedure TLapeTree_Method.addExitStatement(JumpSafe: Boolean);
begin
  FExitStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0), FCompiler.DocPos));
end;

procedure TLapeTree_VarList.DeleteChild(Node: TLapeTree_Base);
var
  i: Integer;
begin
  for i := FVars.Count - 1 downto 0 do
    if (FVars[i].Default = Node) then
      FVars.Delete(i);
end;

constructor TLapeTree_VarList.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_VarList.Compile: TResVar;
var
  i: Integer;
  InitVar: TResVar;
begin
  Result := NullResVar;
  i := 0;
  while (i < FVars.Count) do
    if (FVars[i].VarDecl <> nil) and (FVars[i].Default <> nil) then
      with FVars[i], TLapeTree_Operator.Create(op_Assign, Self) do
      try
        InitVar := _ResVar.New(VarDecl);
        InitVar.isConstant := False;
        Left := TLapeTree_Var.Create(InitVar, Self);
        Right := Default;
        Result := Compile();
      finally
        Free();
      end
    else
      Inc(i);
end;

procedure TLapeTree_With.setBody(Node: TLapeTree_Base);
begin
  if (FBody <> nil) and (FBody <> Node) then
    FBody.Free();
  FBody := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

constructor TLapeTree_With.Create(ACompiler: TLapeCompilerBase);
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
  for i := 0 to High(FWithDecls) do
    FreeAndNil(FWithDecls[i]);

  setBody(nil);
  inherited;
end;

function TLapeTree_With.addWith(AWith: TLapeTree_ExprBase): TLapeWithDeclaration;
var
  i: Integer;
begin
  Result := nil;

  i := FWithList.Add(AWith);
  SetLength(FWithDecls, FWithList.Count);
  if (AWith <> nil) and (i > -1) then
  begin
    AWith.Parent := Self;
    FWithDecls[i] := TLapeWithDeclaration.Create(AWith.resType());
    Result := FWithDecls[i];

    // (a constant is known already: what is read of it is folded)
    if ((AWith is TLapeTree_Var) and (TLapeTree_Var(AWith).GlobalVar <> nil)) and AWith.isConstant then
      Result.WithVar := _ResVar.New(TLapeTree_Var(AWith).GlobalVar);
  end;

  if (Result = nil) or (Result.WithType = nil) then
    LapeException(lpeInvalidWithReference, [AWith, Self])
end;

function TLapeTree_With.Compile: TResVar;
var
  i: Integer;
  Compiled: array of Boolean;
begin
  Result := NullResVar;

  if (FCompiler.StackInfo = nil) then
    LapeException(lpeStatementNotAllowed, DocPos);

  SetLength(Compiled, FWithList.Count);
  for i := 0 to FWithList.Count - 1 do
  begin
    Compiled[i] := (FWithDecls[i].WithVar.VarPos.MemPos = NullResVar.VarPos.MemPos);
    if Compiled[i] then
    begin
      FWithDecls[i].WithVar := FWithList[i].Compile();
      if (not FWithDecls[i].WithVar.HasType()) or (FWithDecls[i].WithVar.VarPos.MemPos in [mpNone, mpStack]) then
        LapeException(lpeInvalidWithReference, FWithList[i].DocPos);
    end;
  end;

  if (FBody <> nil) then
    Result := FBody.Compile();

  // (what it compiled is compiled again when it is compiled again)
  for i := 0 to FWithList.Count - 1 do
    if Compiled[i] then
      FWithDecls[i].WithVar := NullResVar;
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

function TLapeTree_If.CompileBody: TResVar;
begin
  if (FStartBody < 0) then
    FStartBody := FCompiler.Emitter.PlaceNewLabel();

  if (FBody <> nil) then
    Result := FBody.Compile()
  else
    Result := NullResVar;
end;

// Jumps to Target when the condition is JumpWhen and goes on when it is not
procedure TLapeTree_If.CompileCondition(JumpWhen: Boolean; Target: TCodeLabel);
var
  Typ: TLapeType;
  Op: TLapeTree_Operator;
  Cast: TLapeTree_Cast;
begin
  Typ := FCondition.resType();

  // No type: compiling it says why
  if (Typ <> nil) then
    case Typ.BaseType of
       ltUInt8, ltInt8, ltUInt16, ltInt16, ltUInt32, ltInt32, ltUInt64, ltInt64,
       ltBoolean, ltByteBool, ltWordBool, ltLongBool,
       ltAnsiChar, ltWideChar,
       ltAnsiString, ltWideString, ltUnicodeString,
       ltSmallEnum, ltLargeEnum,
       ltPointer,
       ltDynArray,
       ltScriptMethod, ltImportedMethod:
         begin
           // (a method of object is two pointers)
           if (Typ.Size <> SizeOf(Pointer)) and (Typ.BaseType in [ltPointer, ltScriptMethod, ltImportedMethod]) then
             LapeExceptionFmt(lpeInvalidCondition, [Typ.AsString], FCondition.DocPos);
         end;

      ltVariant:
        begin
          Cast := TLapeTree_Cast.Create(FCondition);
          Cast.CastTo := TLapeTree_VarType.Create(FCompiler.getBaseType(ltBoolean), FCondition);
          Cast.Param := FCondition;

          Condition := Cast;
        end;

      ltRecord:
        begin
          Op := TLapeTree_Operator.Create(op_cmp_NotEqual, FCondition);
          Op.Left := FCondition;
          Op.Right := TLapeTree_Var.Create(Typ.EmptyValue, Op);

          Condition := Op;
        end;

      else
        LapeExceptionFmt(lpeInvalidCondition, [Typ.AsString], FCondition.DocPos);
    end;

  CompileJump(FCondition, JumpWhen, Target);
end;

constructor TLapeTree_If.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_If.Compile: TResVar;
var
  JumpOver: TCodeLabel;
  if_e: Integer;
begin
  Assert(FCondition <> nil);
  Result := NullResVar;
  FStartBody := -1;
  JumpOver := FCompiler.Emitter.NewLabel();

  // (no body: it jumps over the else instead)
  CompileCondition((FBody = nil) and (FElse <> nil), JumpOver);

  if (FBody <> nil) or (FElse = nil) then
  begin
    Result := CompileBody();

    FCompiler.Emitter.DocPos := DocPos;
    if (FElse <> nil) then
      if_e := FCompiler.Emitter._JmpR(0);
    FCompiler.Emitter.PlaceLabel(JumpOver);
  end;

  if (FElse <> nil) then
  begin
    Result := FElse.Compile();

    if (FBody <> nil) then
      FCompiler.Emitter.PatchJump(if_e, FCompiler.Emitter.CodeLen)
    else
      FCompiler.Emitter.PlaceLabel(JumpOver);
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
  FCompiler.Emitter.PatchJump(FCaseEndJump, Offset);
end;

procedure TLapeTree_CaseBranch.setFallThroughOffset(Offset: Integer);
var
  i: Integer;
begin
  for i := 0 to FFallThroughStatements.Count - 1 do
    FCompiler.Emitter.PatchJump(FFallThroughStatements[i].CodeOffset, Offset);
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

constructor TLapeTree_CaseBranch.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);

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

procedure TLapeTree_CaseBranch.addFallThroughStatement(Offset: Integer);
begin
  FFallThroughStatements.Add(getFlowStatement(Offset, FCompiler.DocPos));
end;

function TLapeTree_CaseBranch.Compile: TResVar;
var
  i: Integer;
  ConditionVar, CompareVar: TResVar;
  Values: TLapeStatementList.TTArray;
  JumpOffsets: TIntegerArray;
begin
  Result := NullResVar;

  Assert(FValues.Count > 0);
  Assert(FCondition <> nil);

  ConditionVar := FCondition.Compile();
  if (not ConditionVar.HasType()) then
    LapeException(lpeExpressionExpected, DocPos);

  SetLength(JumpOffsets, FValues.Count);

  Values := FValues.ExportToArray();

  for i := 0 to High(Values) do
  begin
    if (Values[i] is TLapeTree_ExprBase) then
    begin
      with TLapeTree_Operator.Create(op_cmp_Equal, Self) do
      try
        Right := TLapeTree_ExprBase(Values[i]).FoldConstants(False) as TLapeTree_ExprBase;
        Left := TLapeTree_Var.Create(ConditionVar, Self);

        CompareVar := Compile();
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
          Left := TLapeTree_Var.Create(ConditionVar, Self);
          Right := TLapeTree_Range(Values[i]).Lo.FoldConstants(False) as TLapeTree_ExprBase;
        end;

        Right := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
        with TLapeTree_Operator(Right) do
        begin
          Left := TLapeTree_Var.Create(ConditionVar, Self);
          Right := TLapeTree_Range(Values[i]).Hi.FoldConstants(False) as TLapeTree_ExprBase;
        end;

        CompareVar := Compile();
      finally
        Free();
      end;
    end else
      LapeException(lpeInvalidCaseStatement, DocPos);

    FCompiler.Emitter.DocPos := DocPos;
    if (i = High(Values)) then
      JumpOffsets[i] := FCompiler.Emitter._JmpRIfNot(0, CompareVar) // IF NOT: Jump past statement
    else
      JumpOffsets[i] := FCompiler.Emitter._JmpRIf(0, CompareVar);   // IF: Jump to statement
  end;

  FBodyOffset := FCompiler.Emitter.CodeLen;
  if (FStatement <> nil) then
    Result := FStatement.Compile();

  FCompiler.Emitter.DocPos := DocPos;
  FCaseEndJump := FCompiler.Emitter._JmpR(0); // Jump to case statement end. Set after branches are compiled.

  for i := 0 to High(JumpOffsets) do
    if (i = High(JumpOffsets)) then
      FCompiler.Emitter.PatchJump(JumpOffsets[i], FCompiler.Emitter.CodeLen)
    else
      FCompiler.Emitter.PatchJump(JumpOffsets[i], FBodyOffset);
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

constructor TLapeTree_Case.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_Case.Compile: TResVar;
var
  i: Integer;
  ConditionVar: TResVar;
begin
  Result := NullResVar;

  Assert(FCondition <> nil);
  ConditionVar := FCondition.Compile();
  if (not ConditionVar.HasType()) then
    LapeException(lpeExpressionExpected, DocPos);

  if (FBranches.Count > 0) then
  begin
    for i := 0 to FBranches.Count - 1 do
    begin
      FBranches[i].Condition := TLapeTree_Var.Create(ConditionVar, FCondition);

      Result := FBranches[i].Compile();
    end;

    for i := 1 to FBranches.Count - 1 do
      FBranches[i - 1].FallThroughOffset := FBranches[i].BodyOffset;
    FBranches[FBranches.Count - 1].FallThroughOffset := FCompiler.Emitter.CodeLen;
  end;

  if (FElse <> nil) then
    Result := FElse.Compile();

  for i := 0 to FBranches.Count - 1 do
    FBranches[i].CaseEndOffset := FCompiler.Emitter.CodeLen;

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

function TLapeTree_While.CompileBody: TResVar;
var
  i: Integer;
begin
  Result := inherited;

  if (FContinueStatements <> nil) then
  begin
    FContinueCount := FContinueStatements.Count;
    for i := 0 to FContinueCount - 1 do
      FCompiler.Emitter.PatchJump(FContinueStatements[i].CodeOffset, FCompiler.Emitter.CodeLen);
  end;

  CompileCondition(True, FStartBody);
end;

constructor TLapeTree_While.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_While.Compile: TResVar;
var
  i: Integer;
begin
  FContinueCount := 0;
  FBreakStatements.Clear();
  FContinueStatements.Clear();
  Result := inherited;

  if (FContinueCount < FContinueStatements.Count) then
    LapeException(lpeStatementNotAllowed, FContinueStatements[FContinueCount].DocPos);

  for i := 0 to FBreakStatements.Count - 1 do
    FCompiler.Emitter.PatchJump(FBreakStatements[i].CodeOffset, FCompiler.Emitter.CodeLen);
end;

function TLapeTree_While.canBreak: Boolean;
begin
  Result := True;
end;

function TLapeTree_While.canContinue: Boolean;
begin
  Result := True;
end;

procedure TLapeTree_While.addBreakStatement(JumpSafe: Boolean);
begin
  if JumpSafe then
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0), FCompiler.DocPos))
  else
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0), FCompiler.DocPos));
end;

procedure TLapeTree_While.addContinueStatement(JumpSafe: Boolean);
begin
  if JumpSafe then
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0), FCompiler.DocPos))
  else
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0), FCompiler.DocPos));
end;

function TLapeTree_Repeat.CompileBody: TResVar;
var
  i: Integer;
begin
  Result := inherited;

  for i := 0 to FContinueStatements.Count - 1 do
    FCompiler.Emitter.PatchJump(FContinueStatements[i].CodeOffset, FCompiler.Emitter.CodeLen);

  CompileCondition(False, FStartBody);
end;

constructor TLapeTree_Repeat.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_Repeat.Compile: TResVar;
var
  i: Integer;
begin
  FStartBody := -1;

  FBreakStatements.Clear();
  FContinueStatements.Clear();

  Result := CompileBody();

  for i := 0 to FBreakStatements.Count - 1 do
    FCompiler.Emitter.PatchJump(FBreakStatements[i].CodeOffset, FCompiler.Emitter.CodeLen);
end;

function TLapeTree_Repeat.canBreak: Boolean;
begin
  Result := True;
end;

function TLapeTree_Repeat.canContinue: Boolean;
begin
  Result := True;
end;

procedure TLapeTree_Repeat.addBreakStatement(JumpSafe: Boolean);
begin
  if JumpSafe then
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0), FCompiler.DocPos))
  else
    FBreakStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0), FCompiler.DocPos));
end;

procedure TLapeTree_Repeat.addContinueStatement(JumpSafe: Boolean);
begin
  if JumpSafe then
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpSafeR(0), FCompiler.DocPos))
  else
    FContinueStatements.Add(getFlowStatement(FCompiler.Emitter._JmpR(0), FCompiler.DocPos));
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

function TLapeTree_For.CompileBody: TResVar;
var
  i: Integer;
  IncDec: TLapeTree_InternalMethod;
  tmpBody: TLapeTree_Base;
  tmpContinueStatements: TLapeFlowStatementList;
begin
  Assert((FCondition <> nil) and (FCondition is TLapeTree_Operator));
  Assert((TLapeTree_Operator(FCondition).Right <> nil) and (TLapeTree_Operator(FCondition).Right is TLapeTree_Var));

  FStartBody := FCompiler.Emitter.PlaceNewLabel();

  if (LoopType = loopOver) then
    CompileBodyForIn();

  if (FBody <> nil) then
    Result := FBody.Compile()
  else
    Result := NullResVar;

  FContinueCount := FContinueStatements.Count;
  for i := 0 to FContinueCount - 1 do
    FCompiler.Emitter.PatchJump(FContinueStatements[i].CodeOffset, FCompiler.Emitter.CodeLen);

  if LoopType = loopDown then
    IncDec := TLapeTree_InternalMethod_Dec.Create(Self)
  else
    IncDec := TLapeTree_InternalMethod_Inc.Create(Self);
  
  try
    IncDec.addParam(TLapeTree_Var.Create(TLapeTree_Operator(FCondition).Left.Compile(), FCondition));
    if (FStep <> nil) then
    begin
      IncDec.addParam(FStep);
      IncDec.Compile();
      Step := IncDec.Params.Delete(1);
    end
    else
      IncDec.Compile();
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

procedure TLapeTree_For.CompileBodyForIn;
var
  counterVar, valueVar, numberVar, tmpVar: TResVar;
begin
  Assert(LoopType = loopOver);

  counterVar := FCounter.Compile();

  if (LoopOverWhat = loopOverArray) then
    with TLapeTree_Operator.Create(op_Assign, FCounter) do
    try
      Left := TLapeTree_Var.Create(counterVar, FCounter);
      Right := TLapeTree_Operator.Create(op_Index, FCounter);
      with TLapeTree_Operator(Right) do
      begin
        CompilerOptions := CompilerOptions - [lcoRangeCheck];
        Left := TLapeTree_Var.Create(Container, FLimit);
        Right := TLapeTree_Var.Create(TLapeTree_Operator(FCondition).Left.Compile(), FCondition);
      end;
      Compile();
    finally
      Free();
    end
  else
  begin
    // (the number goes in the counter as it is: as an integer of the counter's size)
    valueVar := counterVar;
    valueVar.VarType := FCompiler.getBaseType(DetermineIntType(counterVar.VarType.Size, counterVar.VarType.BaseType in LapeSignedOrdinalTypes));
    numberVar := TLapeTree_Operator(FCondition).Left.Compile();
    tmpVar := NullResVar;
    FCompiler.Emitter.DocPos := FCounter.DocPos;
    valueVar.VarType.Eval(op_Assign, tmpVar, valueVar, numberVar, []);
  end;

  if (CheckInVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
    with TLapeTree_If.Create(FLimit) do
    try
      Condition := TLapeTree_Operator.Create(op_IN, Self);
      TLapeTree_Operator(Condition).Left := TLapeTree_Var.Create(counterVar, Self);
      TLapeTree_Operator(Condition).Right := TLapeTree_Var.Create(CheckInVar, Self);

      ElseBody := TLapeTree_InternalMethod_Continue.Create(Self);
      ElseBody.FParent := Self;

      Compile();
    finally
      Free();
    end;
end;

constructor TLapeTree_For.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_For.Compile: TResVar;
var
  Count, Lim: TResVar;
  CounterVar: TLapeVar;
begin
  Result := NullResVar;
  Assert(FCondition = nil);
  Assert(FCounter <> nil);
  Assert(FLimit <> nil);

  if (LoopType = loopOver) then
    Exit(CompileForIn());

  CounterVar := nil;
  Count := FCounter.Compile();
  try
    Lim := FLimit.Compile();
    if (not Lim.HasType()) or (not (Lim.VarType.BaseType in LapeOrdinalTypes + [ltPointer])) then
      LapeExceptionFmt(lpeExpected, ['Ordinal or pointer'], FLimit.DocPos);

    if Count.HasType() and Count.isConstant then
    begin
      CounterVar := FCompiler.getTempVar(Count.VarType);
      FCompiler.Emitter.DocPos := DocPos;
      Count := CounterVar.VarType.Eval(op_Assign, Result, _ResVar.New(CounterVar), Count, []);
    end;
    if (not Count.HasType()) or Count.isConstant or (not (Count.VarType.BaseType in LapeOrdinalTypes + [ltPointer])) then
      LapeException(lpeInvalidIterator, FCounter.DocPos);

    if LoopType = loopDown then
      FCondition := TLapeTree_Operator.Create(op_cmp_GreaterThanOrEqual, Self)
    else
      FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
    TLapeTree_Operator(FCondition).Left := TLapeTree_Var.Create(Count, FCounter);
    TLapeTree_Operator(FCondition).Right := TLapeTree_Var.Create(Lim, FLimit);

    Result := inherited;
  finally
    setCondition(nil);
  end;
end;

function TLapeTree_For.CompileForIn: TResVar;
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
      Assignment.Left := TLapeTree_Var.Create(Target, FCounter);
      Assignment.Right := Expr;
      Assignment.Compile();
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

  Container := FLimit.Compile();
  if (not Container.HasType()) then
    LapeException(lpeExpressionExpected, FLimit.DocPos);

  lower := _ResVar.New(FCompiler.getTempVar(ltSizeInt));

  // (of a set or an enum: a number from the first to the last, given to the counter as one of the element)
  if (LoopOverWhat in [loopOverSet, loopOverEnum]) then
  begin
    if (LoopOverWhat = loopOverSet) then
    begin
      if (not (Container.VarType is TLapeType_Set)) then
        LapeExceptionFmt(lpeExpected, ['Set'], FLimit.DocPos);

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
        LapeExceptionFmt(lpeExpected, ['Enum'], FLimit.DocPos);

      Range := TLapeType_SubRange(Element).Range;
      if Enum.HasGap(Range) then
        LapeExceptionFmt(lpeCannotLoopEnumWithGaps, [Element.AsString], FLimit.DocPos);
    end;

    if (FCounter.resType() <> nil) and (FCounter.resType().EvalRes(op_Assign, Element) = nil) then
      LapeExceptionFmt(lpeIncompatibleAssignment, [Element.AsString, FCounter.resType().AsString], FCounter.DocPos);

    AssignTo(lower, TLapeTree_Var.Create(FCompiler.getIntConst(Range.Lo, ltSizeInt), Self));
    upper := _ResVar.New(FCompiler.getIntConst(Range.Hi, ltSizeInt));
  end
  else
  begin
    upper := _ResVar.New(FCompiler.getTempVar(ltSizeInt));

    method := TLapeTree_InternalMethod_Low.Create(FCounter);
    TLapeTree_InternalMethod_Low(method).addParam(TLapeTree_Var.Create(Container, FCounter));
    AssignTo(lower, method.FoldConstants() as TLapeTree_ExprBase);

    method := TLapeTree_InternalMethod_High.Create(FLimit);
    TLapeTree_InternalMethod_High(method).addParam(TLapeTree_Var.Create(Container, FLimit));
    AssignTo(upper, method.FoldConstants() as TLapeTree_ExprBase);
  end;

  try
    FCondition := TLapeTree_Operator.Create(op_cmp_LessThanOrEqual, Self);
    TLapeTree_Operator(FCondition).Left := TLapeTree_Var.Create(lower, FCounter);
    TLapeTree_Operator(FCondition).Right := TLapeTree_Var.Create(upper, FLimit);
    Result := inherited Compile();
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

constructor TLapeTree_Try.Create(ACompiler: TLapeCompilerBase);
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

function TLapeTree_Try.Compile: TResVar;
var
  o_try, o_except, o_jmp: Integer;
begin
  Result := NullResVar;
  Assert((FExcept <> nil) or (FFinally <> nil));

  FCompiler.Emitter.DocPos := DocPos;
  o_try := FCompiler.Emitter._IncTry(0, 0);

  o_except := 0;
  if (FBody <> nil) then
    Result := FBody.Compile();

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._DecTry();
  if (FExcept <> nil) then
  begin
    FCompiler.Emitter.DocPos := DocPos;
    o_jmp := FCompiler.Emitter._JmpR(0);
    if (FFinally = nil) then
      FCompiler.Emitter.PatchTry(o_try, FCompiler.Emitter.CodeLen, Try_NoFinally)
    else
      o_except := FCompiler.Emitter.CodeLen;
    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._CatchException();

    FExcept.Compile();

    FCompiler.Emitter.PatchJump(o_jmp, FCompiler.Emitter.CodeLen);
  end;
  if (FFinally <> nil) then
  begin
    if (o_except <> 0) then
      FCompiler.Emitter.PatchTry(o_try, o_except, FCompiler.Emitter.CodeLen - o_except)
    else
      FCompiler.Emitter.PatchTry(o_try, FCompiler.Emitter.CodeLen, Try_NoExcept);

    FFinally.Compile();
  end;
  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._EndTry();
end;

function TLapeTree_Try.Walk(Visit: TLapeTree_Visit): Boolean;
begin
  Result := ((FBody <> nil) and Visit(FBody)) or ((FExcept <> nil) and Visit(FExcept)) or
            ((FFinally <> nil) and Visit(FFinally));
end;

end.

