{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Script compiler, which turns a script into a syntax tree.
}
unit lpcompiler;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpparser, lptree;

const
  ParserToken_ExpressionEnd = [tk_sym_SemiColon];
  ParserToken_BlockEnd = [tk_kw_End, tk_kw_Finally, tk_kw_Except, tk_kw_Until];

type
  TLapeInternalMethodMap = {$IFDEF FPC}specialize{$ENDIF} TLapeUniqueStringDictionary<TLapeTree_InternalMethodClass>;
  TLapeTypeForwards = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapeType>;
  TLapeFuncForwards = {$IFDEF FPC}specialize{$ENDIF} TLapeList<TLapeGlobalVar>;
  TLapeTree_NodeStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeTree_ExprBase>;
  TLapeTree_OpStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeTree_Operator>;

  TLapeCompiler = class;
  TLapeHandleDirective = function(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean of object;
  TLapeHandleExternal = function(Sender: TLapeCompiler; Header: TLapeGlobalVar): Boolean of object;
  TLapeFindFile = procedure(Sender: TLapeCompiler; var FileName: lpString) of object;
  TLapeCompilerNotification = {$IFDEF FPC}specialize{$ENDIF} TLapeNotifier<TLapeCompiler>;
  TLapeTokenizerArray = array of TLapeTokenizerBase;

  TLapePreprocessorFunc = function(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean of object;
  TLapePreprocessorFuncs = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TLapePreprocessorFunc>;

  TLapeDefine = record Name, Value: lpString; end;
  TLapeDefines = {$IFDEF FPC}specialize{$ENDIF} TLapeArrayBuffer<TLapeDefine>;

  TLapeConditional = record
    Eval: Boolean;
    Pos: TDocPos;
  end;
  TLapeConditionalStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeConditional>;

  TLapeVarStates = array of TLapeGlobalVar;

  PCompilerState = ^TCompilerState;
  TCompilerState = record
    Tokenizer: Integer;
    Tokenizers: array of TLapeTokenizerBase;
    TokStates: array of Pointer;
    Options: ECompilerOptionsSet;
    Options_PackRecords: UInt8;
    Defines: TLapeDefines;
    Conditionals: TLapeConditionalStack.TTArray;
  end;

  PTempTokenizerState = ^TTempTokenizerState;
  TTempTokenizerState = record
    OldStackInfo: TLapeStackInfo;
    OldTokenizer: TLapeTokenizerBase;
    OldTokenizerIndex: Integer;
    OldState: Pointer;
  end;

  TLapeCompiler = class(TLapeCompilerBase)
  private
    __tmp: TDocPos;
    function hasTokenizer: Boolean; inline;
    function hasMoreTokenizers: Boolean; inline;
    function incTokenizerLock(ATokenizer: TLapeTokenizerBase): TLapeTokenizerBase;
    procedure decTokenizerLock(var ATokenizer: TLapeTokenizerBase; DoFree: Boolean = True);
    procedure setTokenizersPeek(Peek: Boolean);
  protected
    FTypeID: Integer;

    FTokenizers: TLapeTokenizerArray;
    FTokenizer: Integer;

    FInternalMethodMap: TLapeInternalMethodMap;
    FTree: TLapeTree_Base;
    FDelayedTree: TLapeTree_DelayedStatementList;
    FImporting: Pointer;

    FIncludes: TStringList;
    FBaseDefines: TLapeDefines;
    FDefines: TLapeDefines;
    FConditionalStack: TLapeConditionalStack;

    FOnHandleDirective: TLapeHandleDirective;
    FOnHandleExternal: TLapeHandleExternal;
    FOnFindFile: TLapeFindFile;
    FAfterParsing: TLapeCompilerNotification;

    FPreprocessorFuncs: TLapePreprocessorFuncs;
    FPreprocessorMacros: TLapePreprocessorFuncs;

    function getPDocPos: PDocPos; inline;
    function getDocPos: TDocPos; override;
    procedure Reset; override;

    function getImporting: Boolean; virtual;
    procedure setImporting(Import: Boolean); virtual;
    procedure setBaseDefines(Defines: TLapeDefines); virtual;
    function getTokenizer: TLapeTokenizerBase; virtual;
    procedure setTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    function popTokenizer: TLapeTokenizerBase; virtual;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); virtual;
    function popConditional: TDocPos; virtual;

    procedure SetUniqueTypeID(Typ: TLapeType); virtual;

    function GetToStringMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType;  AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetObjectifyMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetDisposeMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetDisposeObjectMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    function GetCopyMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    function GetMethod_ArraySort(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayRange(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayMin(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayMax(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayUnique(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayIndexOf(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayIndicesOf(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayMode(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArraySum(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayMean(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayMedian(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayVariance(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayStdev(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayDifference(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArraySymDifference(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayIntersection(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;
    function GetMethod_ArrayEquals(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    procedure InitBaseDefinitions; virtual;
    procedure InitBaseMath; virtual;
    procedure InitBaseString; virtual;
    procedure InitBaseDateTime; virtual;
    procedure InitBaseVariant; virtual;
    procedure InitBaseFile; virtual;

    function EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase; virtual;
    function EnsureTypeExpression(Node: TLapeTree_Base): TLapeTree_Base; virtual;
    function EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range; overload; virtual;
    function EnsureRange(Node: TLapeTree_Base): TLapeTree_Range; overload; virtual;
    function EnsureRange(VarType: TLapeType): TLapeType; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange; overload; virtual;
    function EnsureConstantRange(Node: TLapeTree_Base): TLapeRange; overload; virtual;

    function FindFile(AFileName: lpString): lpString; virtual;
    function HandlePreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean; virtual;
    function HandlePreprocessorMacro(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean; virtual;
    function EvalPreprocessorExpr(Expr: String; ADocPos: TDocPos): Boolean; virtual;
    function EvalPreprocessorMacro(Argument: String; ADocPos: TDocPos): Boolean; virtual;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; virtual;
    function InIgnore: Boolean; virtual;
    function Next: EParserToken; virtual;
    function isNext(Tokens: EParserTokenSet; out Token: EParserToken): Boolean; overload; virtual;
    function isNext(Tokens: EParserTokenSet): Boolean; overload; virtual;
    function Peek: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    procedure ParseDeclHint(Decl: TLapeDeclaration; NextBefore: Boolean = False; NextAfter: Boolean = True); virtual;

    procedure ParseExpressionEnd(Token: EParserToken; NextBefore: Boolean = False; NextAfter: Boolean = True); overload; virtual;
    procedure ParseExpressionEnd(Tokens: EParserTokenSet = ParserToken_ExpressionEnd; NextBefore: Boolean = False; NextAfter: Boolean = True); overload; virtual;
    function ParseIdentifierList(FirstNext: Boolean = False): TStringArray; virtual;

    function ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList; virtual;
    function ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method; virtual;
    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method; overload; virtual;
    function ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString): TLapeTree_Method; overload; virtual;
    function ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method; overload; virtual;
    function ParseType(TypeForwards: TLapeTypeForwards; addToStackOwner: Boolean = False): TLapeType; virtual;
    procedure ParseTypeBlock; virtual;
    procedure ParseLabelBlock; virtual;
    function ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList; virtual;

    function ParseExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_ExprBase; virtual;
    function ParseTypeExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_Base; virtual;
    function ParseStatement(FirstNext: Boolean = True; ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Base; virtual;
    function ParseStatementList: TLapeTree_StatementList; virtual;

    function ParseBeginEnd(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_StatementList; virtual;
    function ParseCase(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Case; virtual;
    function ParseFor(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_For; virtual;
    function ParseIf(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_If; virtual;
    function ParseRepeat(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Repeat; virtual;
    function ParseTry(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Try; virtual;
    function ParseWhile(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_While; virtual;
    function ParseWith(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_With; virtual;
  public
    FreeTokenizer: Boolean;
    FreeTree: Boolean;

    constructor Create(
      ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
      AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True
    ); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    function getState: Pointer; virtual;
    procedure setState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeState(const State: Pointer); virtual;

    function getTempTokenizerState(const ATokenizer: TLapeTokenizerBase; ResetState: Boolean = True): Pointer; overload; virtual;
    function getTempTokenizerState(const AStr: lpString; const AFileName: lpString = ''; ResetState: Boolean = True): Pointer; overload; virtual;
    procedure resetTokenizerState(const State: Pointer; DoFreeState: Boolean = True); virtual;
    procedure freeTempTokenizerState(const State: Pointer); virtual;

    function getVarStates: TLapeVarStates; virtual;
    procedure setVarStates(VarStates: TLapeVarStates); virtual;
    procedure freeVarStates(VarStates: TLapeVarStates); virtual;

    procedure StartImporting; virtual;
    procedure EndImporting; virtual;

    function addDelayedExpression(Node: TLapeTree_Base; AfterCompilation: Boolean = True; IsGlobal: Boolean = False): TLapeTree_Base; virtual;
    function ParseFile: TLapeTree_Base; virtual;

    function Compile: Boolean; virtual;
    procedure CheckAfterCompile; virtual;

    procedure VarToDefault(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil); override;
    procedure FinalizeVar(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil); override;

    function getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration; override;
    function hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; override;
    function hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean; override;
    function getDeclarationNoWith(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration; overload; virtual;
    function getDeclarationNoWith(AName: lpString; LocalOnly: Boolean = False): TLapeDeclaration; overload; virtual;
    function getExpression(AName: lpString; AStackInfo: TLapeStackInfo; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase; overload; virtual;
    function getExpression(AName: lpString; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase; overload; virtual;

    function hasInclude(FileName: lpString): Boolean; virtual;
    function hasBaseDefine(AName: lpString): Boolean; virtual;
    function hasDefine(AName: lpString): Boolean; virtual;
    procedure addBaseDefine(AName: lpString; AValue: lpString = ''); virtual;
    procedure addDefine(AName: lpString; AValue: lpString = ''); virtual;
    procedure addPreprocessorFunc(Name: lpString; Func: TLapePreprocessorFunc); virtual;
    procedure addPreprocessorMacro(Name: lpString; Func: TLapePreprocessorFunc); virtual;

    function addLocalDecl(Decl: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration; override;
    function addLocalVar(AVar: TLapeType; Name: lpString = ''): TLapeVar; virtual;

    function addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: ELapeBaseType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Single; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Double; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType; overload; virtual;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; virtual;

    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AHeader: TLapeType_Method; AName, Body: lpString; Pos: PDocPos = nil): TLapeTree_Method; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar; overload; virtual;

    function addGlobalMethod(AFunc: TLapeGlobalVar; Value: TMethod; FreeFunc: Boolean = True): TLapeGlobalVar; overload; virtual;
    function addGlobalMethod(AHeader: lpString; Value: TMethod): TLapeGlobalVar; overload; virtual;
    function addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar; overload; virtual;

    function addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; virtual;

    property InternalMethodMap: TLapeInternalMethodMap read FInternalMethodMap;
    property Tree: TLapeTree_Base read FTree;
    property DelayedTree: TLapeTree_DelayedStatementList read FDelayedTree;
    property Importing: Boolean read getImporting write setImporting;
    property Tokenizer: TLapeTokenizerBase read getTokenizer write setTokenizer;
    property Defines: TLapeDefines read FDefines write setBaseDefines;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
    property OnHandleExternal: TLapeHandleExternal read FOnHandleExternal write FOnHandleExternal;
    property OnFindFile: TLapeFindFile read FOnFindFile write FOnFindFile;
    property AfterParsing: TLapeCompilerNotification read FAfterParsing;
  end;

  TLapeType_SystemUnit = class(TLapeType)
  public
    constructor Create(ACompiler: TLapeCompilerBase); reintroduce; virtual;
    procedure ClearSubDeclarations; override;

    function CanHaveChild: Boolean; override;
    function HasChild(AName: lpString): Boolean; override;
    function HasChild(ADecl: TLapeDeclaration): Boolean; override;
    function HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean; override;

    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
  end;

implementation

uses
  Variants,
  lpvartypes_ord, lpvartypes_record, lpvartypes_array, lpvartypes_object,
  lpinternalmethods, lpinternalmethods_algorithm,
  lpmessages, lpeval, lpinterpreter_types, lpvartypes_helper;

function TLapeCompiler.getPDocPos: PDocPos;
begin
  __tmp := DocPos;
  Result := @__tmp;
end;

function TLapeCompiler.hasTokenizer: Boolean;
begin
  Result := FTokenizer >= 0;
end;

function TLapeCompiler.hasMoreTokenizers: Boolean;
begin
  Result := FTokenizer > 0;
end;

function TLapeCompiler.incTokenizerLock(ATokenizer: TLapeTokenizerBase): TLapeTokenizerBase;
begin
  Result := ATokenizer;
  if (Result <> nil) then
    Inc(Result.Tag);
end;

procedure TLapeCompiler.decTokenizerLock(var ATokenizer: TLapeTokenizerBase; DoFree: Boolean = True);
begin
  if (ATokenizer <> nil) then
  begin
    Dec(ATokenizer.Tag);
    if (ATokenizer.Tag < 0) and DoFree then
      FreeAndNil(ATokenizer);
  end;
end;

type
  __LapeTokenizerBase = class(TLapeTokenizerBase);
procedure TLapeCompiler.setTokenizersPeek(Peek: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(FTokenizers) do
    if (FTokenizers[i] <> nil) then
      __LapeTokenizerBase(FTokenizers[i]).FInPeek := Peek;
end;

function TLapeCompiler.getDocPos: TDocPos;
begin
  if hasTokenizer() then
    Result := Tokenizer.DocPos
  else
    Result := NullDocPos;
end;

procedure TLapeCompiler.Reset;
begin
  inherited;
  EndImporting();

  FOptions := FBaseOptions;
  FOptions_PackRecords := FBaseOptions_PackRecords;

  FTokenizer := High(FTokenizers);
  while hasMoreTokenizers() do
    popTokenizer();

  if (FTokenizer = 0) and (FTokenizers[0] <> nil) then
    FTokenizers[0].Reset();

  if FreeTree and (FTree <> nil) then
    FTree.Free();
  FTree := nil;

  if (FDelayedTree <> nil) then
    FDelayedTree.Clean();

  if (FIncludes <> nil) then
    FIncludes.Clear();
  if (FConditionalStack <> nil) then
    FConditionalStack.Reset();

  FDefines := FBaseDefines;
end;

function TLapeCompiler.getImporting: Boolean;
begin
  Result := FImporting <> nil;
end;

procedure TLapeCompiler.setImporting(Import: Boolean);
begin
  if Import then
    StartImporting()
  else
    EndImporting();
end;

procedure TLapeCompiler.setBaseDefines(Defines: TLapeDefines);
begin
  FBaseDefines := Defines;
  Reset();
end;

function TLapeCompiler.getTokenizer: TLapeTokenizerBase;
begin
  if hasTokenizer() then
    Result := FTokenizers[FTokenizer]
  else
    Result := nil;
end;

procedure TLapeCompiler.setTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  if hasTokenizer() and (FreeTokenizer or hasMoreTokenizers()) and
    (FTokenizer < Length(FTokenizers)) and (Tokenizer <> ATokenizer)
  then
    decTokenizerLock(FTokenizers[FTokenizer]);

  if (not hasTokenizer()) then
    FTokenizer := 0;
  if (Length(FTokenizers) <= FTokenizer) then
    SetLength(FTokenizers, FTokenizer + 1);
  FTokenizers[FTokenizer] := ATokenizer;

  if (ATokenizer <> nil) then
  begin
    ATokenizer.OnHandleDirective := {$IFDEF FPC}@{$ENDIF}HandleDirective;
    ATokenizer.Reset();
  end;
end;

procedure TLapeCompiler.pushTokenizer(ATokenizer: TLapeTokenizerBase);
var
  InPeek: Boolean;
begin
  InPeek := (Tokenizer <> nil) and Tokenizer.InPeek;
  Inc(FTokenizer);
  setTokenizer(ATokenizer);

  if (ATokenizer <> nil) then
  begin
    ATokenizer.RelativeFileNames := (lcoRelativeFileNames in FOptions);
    if InPeek then
      __LapeTokenizerBase(Tokenizer).FInPeek := True;
  end;
end;

function TLapeCompiler.popTokenizer: TLapeTokenizerBase;
var
  LastTok: EParserToken;
begin
  Result := Tokenizer;
  if (Tokenizer <> nil) then
    LastTok := Tokenizer.LastTok
  else
    LastTok := tk_NULL;

  setTokenizer(nil);
  Dec(FTokenizer);

  if (Tokenizer <> nil) then
    __LapeTokenizerBase(Tokenizer).FLastTok := LastTok;
end;

procedure TLapeCompiler.pushConditional(AEval: Boolean; ADocPos: TDocPos);
var
  Conditional: TLapeConditional;
begin
  Assert(FConditionalStack <> nil);
  with Conditional do
  begin
    Eval := AEval;
    Pos := ADocPos;
  end;
  FConditionalStack.Push(Conditional);
end;

function TLapeCompiler.popConditional: TDocPos;
begin
  Assert(FConditionalStack <> nil);
  if (FConditionalStack.Count > 0) then
    Result := FConditionalStack.Pop().Pos
  else
    LapeException(lpeLostConditional, Tokenizer.DocPos);
end;

procedure TLapeCompiler.SetUniqueTypeID(Typ: TLapeType);
begin
  Typ.TypeID := FTypeID;
  Inc(FTypeID);
end;

function TLapeCompiler.GetToStringMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType;  AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar;
var
  Body: lpString;
  Index: Integer;
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (AParams[0] = nil) or ((AResult <> nil) and (AResult.BaseType <> ltString)) then
    Exit;

  AResult := getBaseType(ltString);
  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptConstRef], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;

  Result := Header.NewGlobalVar(@_LapeToString_Unknown);
  Result.VarType.Name := '_ToString';
  Sender.addMethod(Result);

  Body := AParams[0].VarToStringBody(Sender);
  if (Body = '') and Sender.NeedFullMatch then
  try
    Sender.NeedFullMatch := False;

    Assert(Result.DeclarationList = Sender.ManagedDeclarations);
    Result.DeclarationList := nil;

    Index := Sender.getMethodIndex(AParams, AResult);
    if (Index < 0) then
      Index := Sender.getMethodIndex(getTypeArray([getBaseType(AParams[0].BaseType)]), AResult);

    if (Index >= 0) then
    begin
      Body := 'begin Result := System.ToString[' + lpString(IntToStr(Index)) + '](Param0); end;';
      Result.DeclarationList := Sender.ManagedDeclarations;
    end;
  finally
    Sender.NeedFullMatch := True;
  end;

  if (Body <> '') then
    Result := addGlobalFunc(Header, 'ToString', 'override;' + LapeDelayedFlags + Body + LineEnding).Method
  else
    FreeAndNil(Result);
end;

function TLapeCompiler.GetObjectifyMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType;  AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Invoke: TLapeTree_Invoke;
  Assignment: TLapeTree_Operator;
  Callback: TResVar;
  i: Int32;
  AType: TLapeType_MethodOfObject;
begin
  Result := nil;
  if (AObjectType = nil) or (AObjectType.ClassType <> TLapeType_MethodOfObject) then
    Exit;

  AType := TLapeType_MethodOfObject(AObjectType);
  IncStackInfo();

  try
    Result := addManagedDecl(AType.NewGlobalVar(EndJump)) as TLapeGlobalVar;
    Result.VarType.Name := '_Objectify';

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Method);

    Callback := _ResVar.New(FStackInfo.addSelfVar(lptConstRef, getGlobalType('ConstPointer')));
    Callback.VarType := addManagedType(TLapeType_Method.Create(AType));

    Invoke := TLapeTree_Invoke.Create(TLapeTree_ResVar.Create(Callback.IncLock(), Self), Self);
    for i := 0 to AType.Params.Count - 1 do
      Invoke.addParam(TLapeTree_ResVar.Create(_ResVar.New(FStackInfo.addVar(AType.Params[i].ParType, AType.Params[i].VarType)), Self));

    if (AType.Res <> nil) then
    begin
      Assignment := TLapeTree_Operator.Create(op_Assign, Self);
      Assignment.Left := TLapeTree_ResVar.Create(_ResVar.New(FStackInfo.addVar(lptOut, AType.Res)), Self);
      Assignment.Right := Invoke;

      Method.Statements.addStatement(Assignment);
    end else
      Method.Statements.addStatement(Invoke);

    addDelayedExpression(Method);
  finally
    DecStackInfo(True, False, Method = nil);
  end;
end;

function TLapeCompiler.GetDisposeMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType;  AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Header: TLapeType_Method;
begin
  Result := nil;
  Method := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (AParams[0] = nil) or (AResult <> nil) then
    Exit;
  if (not (lcoFullDisposal in FOptions)) and (not AParams[0].NeedFinalization) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptVar], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;

  IncStackInfo();
  try
    Result := Header.NewGlobalVar(EndJump);
    Result.VarType.Name := '_Dispose';
    Sender.addMethod(Result);

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Self);
    Method.Statements.addStatement(TLapeTree_FinalizeVar.Create(FStackInfo.addVar(lptVar, AParams[0], '!AVar'), Self));
    addDelayedExpression(Method);
  finally
    DecStackInfo(True, False, Method = nil);
  end;
end;

function TLapeCompiler.GetDisposeObjectMethod(
  Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType;
  AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Header: TLapeType_Method;
  ParamVar: TResVar;
  Op: TLapeTree_Operator;
  Dispose: TLapeTree_InternalMethod_Dispose;
  i: Integer;
begin
  Result := nil;
  Method := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_Object)) or (AResult <> nil) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptConstRef], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;

  IncStackInfo();
  try
    Result := Header.NewGlobalVar(EndJump);
    Result.VarType.Name := '_DisposeObject';
    Sender.addMethod(Result);

    ParamVar := _ResVar.New(FStackInfo.addVar(lptConstRef, AParams[0], '!AVar'));
    ParamVar.Writeable := True;

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Self);

    if ParamVar.VarType.HasSubDeclaration('Destroy', bTrue) then
    begin
      Op := TLapeTree_Operator.Create(op_Dot, Self);
      Op.Left := TLapeTree_ResVar.Create(ParamVar.IncLock(), Self);
      Op.Right := TLapeTree_Field.Create('Destroy', Self);

      Method.Statements.addStatement(TLapeTree_Invoke.Create(Op, Self));
    end;

    with TLapeType_Object(AParams[0]) do
      for i := 0 to FieldMap.Count - 1 do
      begin
        if not FieldMap.ItemsI[i].FieldType.NeedFinalization then
          Continue;

        Op := TLapeTree_Operator.Create(op_Dot, Self);
        Op.Left := TLapeTree_ResVar.Create(ParamVar.IncLock(), Self);
        Op.Right := TLapeTree_Field.Create(FieldMap.Key[i], Self);

        Dispose := TLapeTree_InternalMethod_Dispose.Create(Method.Statements);
        Dispose.FunctionOnly := True;
        Dispose.addParam(Op);

        Method.Statements.addStatement(Dispose);
      end;

    addDelayedExpression(Method);
  finally
    DecStackInfo(True, False, Method = nil);
  end;
end;

function TLapeCompiler.GetCopyMethod(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType;  AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Assignment: TLapeTree_Operator;
  Header: TLapeType_Method;
begin
  Result := nil;
  Method := nil;
  if (Sender = nil) or (Length(AParams) <> 2) or (AParams[0] = nil) or (AParams[1] = nil) or (AResult <> nil) or (not AParams[0].CompatibleWith(AParams[1])) then
    Exit;
  if (not AParams[0].NeedFinalization) and (not AParams[1].NeedInitialization) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptConstRef, lptOut], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;

  IncStackInfo();
  try
    Result := Header.NewGlobalVar(EndJump);
    Result.VarType.Name := '_Assign';
    Sender.addMethod(Result);

    Assignment := TLapeTree_Operator.Create(op_Assign, Self);
    Assignment.Right := TLapeTree_ResVar.Create(_ResVar.New(FStackInfo.addVar(lptConstRef, AParams[0], '!Src')), Self);
    Assignment.Left  := TLapeTree_ResVar.Create(_ResVar.New(FStackInfo.addVar(lptOut, AParams[1], '!Dst')), Self);

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Self);
    Method.Statements.addStatement(Assignment);
    addDelayedExpression(Method);
  finally
    DecStackInfo(True, False, Method = nil);
  end;
end;

function TLapeCompiler.GetMethod_ArraySort(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (not (Length(AParams) in [1, 2])) then
    Exit;

  if (Length(AParams) = 1) then
    Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptVar], [TLapeGlobalVar(nil)])) as TLapeType_Method
  else
    Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptVar, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)])) as TLapeType_Method;

  Method := addGlobalFunc(Header, '!ArraySort',
    '{$RANGECHECKS OFF}                                       ' + LineEnding +
    'type                                                     ' + LineEnding +
    '  TType = PType(Param0);                                 ' + LineEnding +
    'const                                                    ' + LineEnding +
    '  Gaps: TIntegerArray = [                                ' + LineEnding +
    '    835387, 392925, 184011, 85764, 39744, 18298, 8359,   ' + LineEnding +
    '    3785, 1695, 701, 301, 132, 57, 23, 10, 4, 1          ' + LineEnding +
    '  ];                                                     ' + LineEnding +
    'var                                                      ' + LineEnding +
    '  i, j, Gap, GapLo, Len, Lo, Hi: Int32;                  ' + LineEnding +
    '  Temp: TType;                                           ' + LineEnding +
    'begin                                                    ' + LineEnding +
    '  Len := Length(Param0);                                 ' + LineEnding +
    '  Lo := Low(Param0);                                     ' + LineEnding +
    '  Hi := High(Param0);                                    ' + LineEnding +
    '                                                         ' + LineEnding +
    '  for Gap in Gaps do                                     ' + LineEnding +
    '  begin                                                  ' + LineEnding +
    '    // skip gaps larger than the subsection size         ' + LineEnding +
    '    if (Gap > Len) then                                  ' + LineEnding +
    '      Continue;                                          ' + LineEnding +
    '                                                         ' + LineEnding +
    '    GapLo := Lo + Gap;                                   ' + LineEnding +
    '    for i := GapLo to Hi do                              ' + LineEnding +
    '    begin                                                ' + LineEnding +
    '      Temp := Param0[i];                                 ' + LineEnding +
    '      j := i;                                            ' + LineEnding +
    '      while (j >= GapLo) and                             ' + LineEnding +
    '        {$IFPARAM Param1}                                ' + LineEnding +
    '        (Param1(Param0[j - Gap], Temp) > 0)              ' + LineEnding +
    '        {$ELSE}                                          ' + LineEnding +
    '        (Param0[j - Gap] > Temp)                         ' + LineEnding +
    '        {$ENDIF} do                                      ' + LineEnding +
    '      begin                                              ' + LineEnding +
    '        Param0[j] := Param0[j - Gap];                    ' + LineEnding +
    '        j := j - Gap;                                    ' + LineEnding +
    '      end;                                               ' + LineEnding +
    '      Param0[j] := Temp;                                 ' + LineEnding +
    '    end;                                                 ' + LineEnding +
    '  end;                                                   ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArraySort';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayRange(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 3) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1], AParams[2]], [lptNormal, lptOut, lptOut], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], getBaseType(ltInt32))) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayRange',
    'begin                                    ' + LineEnding +
    '  Result := Length(Param0);              ' + LineEnding +
    '  if (Result > 0) then                   ' + LineEnding +
    '  begin                                  ' + LineEnding +
    '    Param1 := @Param0[Low(Param0)];      ' + LineEnding +
    '    Param2 := @Param0[High(Param0)];     ' + LineEnding +
    '  end;                                   ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayRange';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayMin(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], TLapeType_DynArray(AParams[0]).PType)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayMin',
    '{$RANGECHECKS OFF}                              ' + LineEnding +
    'type                                            ' + LineEnding +
    '  TType = PType(Param0);                        ' + LineEnding +
    'var                                             ' + LineEnding +
    '  Ptr, Upper: ^TType;                           ' + LineEnding +
    'begin                                           ' + LineEnding +
    '  case _ArrayRange(Param0, Ptr, Upper) of       ' + LineEnding +
    '    0: Result := Default(TType);                ' + LineEnding +
    '    1: Result := Param0[0];                     ' + LineEnding +
    '    else                                        ' + LineEnding +
    '    begin                                       ' + LineEnding +
    '      Result := Param0[0];                      ' + LineEnding +
    '      while (PtrUInt(Ptr) <= PtrUInt(Upper)) do ' + LineEnding +
    '      begin                                     ' + LineEnding +
    '        if (Ptr^ < Result) then                 ' + LineEnding +
    '          Result := Ptr^;                       ' + LineEnding +
    '        Inc(Ptr);                               ' + LineEnding +
    '      end;                                      ' + LineEnding +
    '    end;                                        ' + LineEnding +
    '  end;                                          ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayMin';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayMax(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], TLapeType_DynArray(AParams[0]).PType)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayMax',
    '{$RANGECHECKS OFF}                              ' + LineEnding +
    'type                                            ' + LineEnding +
    '  TType = PType(Param0);                        ' + LineEnding +
    'var                                             ' + LineEnding +
    '  Ptr, Upper: ^TType;                           ' + LineEnding +
    'begin                                           ' + LineEnding +
    '  case _ArrayRange(Param0, Ptr, Upper) of       ' + LineEnding +
    '    0: Result := Default(TType);                ' + LineEnding +
    '    1: Result := Param0[0];                     ' + LineEnding +
    '    else                                        ' + LineEnding +
    '    begin                                       ' + LineEnding +
    '      Result := Param0[0];                      ' + LineEnding +
    '      while (PtrUInt(Ptr) <= PtrUInt(Upper)) do ' + LineEnding +
    '      begin                                     ' + LineEnding +
    '        if (Ptr^ > Result) then                 ' + LineEnding +
    '          Result := Ptr^;                       ' + LineEnding +
    '        Inc(Ptr);                               ' + LineEnding +
    '      end;                                      ' + LineEnding +
    '    end;                                        ' + LineEnding +
    '  end;                                          ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayMax';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayUnique(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  // Copy() creates dynamic arrays from static arrays
  if (AParams[0] is TLapeType_StaticArray) then
    AResult := addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(AParams[0]).PType, Self))
  else
    AResult := AParams[0];

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayUnique',
    '{$RANGECHECKS OFF}                                 ' + LineEnding +
    'type                                               ' + LineEnding +
    '  TType = PType(Param0);                           ' + LineEnding +
    'var                                                ' + LineEnding +
    '  Ptr, Upper: ^TType;                              ' + LineEnding +
    '  I, NewLen: Int32;                                ' + LineEnding +
    'begin                                              ' + LineEnding +
    '  Result := Copy(Param0);                          ' + LineEnding +
    '  if (_ArrayRange(Result, Ptr, Upper) <= 1) then   ' + LineEnding +
    '    Exit;                                          ' + LineEnding +
    '                                                   ' + LineEnding +
    '  while (PtrUInt(Ptr) <= PtrUInt(Upper)) do        ' + LineEnding +
    '  begin                                            ' + LineEnding +
    '    I := 0;                                        ' + LineEnding +
    '    while (I < NewLen) and (Ptr^ <> Result[I]) do  ' + LineEnding +
    '      Inc(I);                                      ' + LineEnding +
    '                                                   ' + LineEnding +
    '    if (I = NewLen) then                           ' + LineEnding +
    '    begin                                          ' + LineEnding +
    '      Result[NewLen] := Ptr^;                      ' + LineEnding +
    '      Inc(NewLen);                                 ' + LineEnding +
    '    end;                                           ' + LineEnding +
    '                                                   ' + LineEnding +
    '    Inc(Ptr);                                      ' + LineEnding +
    '  end;                                             ' + LineEnding +
    '                                                   ' + LineEnding +
    '  SetLength(Result, NewLen);                       ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayUnique';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayIndexOf(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 2) or (not (AParams[1] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], getBaseType(ltInt32))) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayIndexOf',
    '{$RANGECHECKS OFF}                        ' + LineEnding +
    'var                                       ' + LineEnding +
    '  i: Int32;                               ' + LineEnding +
    'begin                                     ' + LineEnding +
    '  for i := Low(Param1) to High(Param1) do ' + LineEnding +
    '    if (Param1[i] = Param0) then          ' + LineEnding +
    '      Exit(i);                            ' + LineEnding +
    '  Result := -1;                           ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayIndexOf';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayIndicesOf(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 2) or (not (AParams[1] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], getGlobalType('TIntegerArray'))) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayIndicesOf',
    '{$RANGECHECKS OFF}                        ' + LineEnding +
    'var                                       ' + LineEnding +
    '  i, Count, Size: Int32;                  ' + LineEnding +
    'begin                                     ' + LineEnding +
    '  Result := [];                           ' + LineEnding +
    '  Count := Size := 0;                     ' + LineEnding +
    '  for i := Low(Param1) to High(Param1) do ' + LineEnding +
    '    if (Param1[i] = Param0) then          ' + LineEnding +
    '    begin                                 ' + LineEnding +
    '      if (Count = Size) then              ' + LineEnding +
    '      begin                               ' + LineEnding +
    '        Size := 4 + (Size * 2);           ' + LineEnding +
    '        SetLength(Result, Size);          ' + LineEnding +
    '      end;                                ' + LineEnding +
    '                                          ' + LineEnding +
    '      Result[Count] := i;                 ' + LineEnding +
    '      Inc(Count);                         ' + LineEnding +
    '    end;                                  ' + LineEnding +
    '  SetLength(Result, Count);               ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayIndicesOf';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayMode(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], TLapeType_DynArray(AParams[0]).PType)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayMode',
    '{$RANGECHECKS OFF}                                      ' + LineEnding +
    'type                                                    ' + LineEnding +
    '  TType = PType(Param0);                                ' + LineEnding +
    'var                                                     ' + LineEnding +
    '  Arr: array of TType;                                  ' + LineEnding +
    '  Best, Current: record Value: TType; Hits: Int32; end; ' + LineEnding +
    '  i: Int32;                                             ' + LineEnding +
    'begin                                                   ' + LineEnding +
    '  if (Length(Param0) = 0) then                          ' + LineEnding +
    '    Exit(Default(Result));                              ' + LineEnding +
    '                                                        ' + LineEnding +
    '  Arr := Sorted(Param0);                                ' + LineEnding +
    '  Best := [];                                           ' + LineEnding +
    '  Current := [Arr[0], 1];                               ' + LineEnding +
    '                                                        ' + LineEnding +
    '  for i := 1 to High(Arr) do                            ' + LineEnding +
    '  begin                                                 ' + LineEnding +
    '    if (Arr[i] <> Current.Value) then                   ' + LineEnding +
    '    begin                                               ' + LineEnding +
    '      if (Current.Hits > Best.Hits) then                ' + LineEnding +
    '        Best := Current;                                ' + LineEnding +
    '      Current := [Arr[i], 1];                           ' + LineEnding +
    '    end else                                            ' + LineEnding +
    '      Current.Hits := Current.Hits + 1;                 ' + LineEnding +
    '  end;                                                  ' + LineEnding +
    '                                                        ' + LineEnding +
    '  if (Current.Hits > Best.Hits) then                    ' + LineEnding +
    '     Best := Current;                                   ' + LineEnding +
    '  Result := Best.Value;                                 ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayMode';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArraySum(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeRealTypes) then
    AResult := getBaseType(ltDouble)
  else
  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeIntegerTypes) then
    AResult := getBaseType(ltInt64)
  else
    AResult := TLapeType_DynArray(AParams[0]).PType;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArraySum',
    '{$RANGECHECKS OFF}                                 ' + LineEnding +
    'type                                               ' + LineEnding +
    '  TType = PType(Param0);                           ' + LineEnding +
    'var                                                ' + LineEnding +
    '  Ptr, Upper: ^TType;                              ' + LineEnding +
    'begin                                              ' + LineEnding +
    '  Result := Default(Result);                       ' + LineEnding +
    '  if (_ArrayRange(Param0, Ptr, Upper) > 0) then    ' + LineEnding +
    '    while (PtrUInt(Ptr) <= PtrUInt(Upper)) do      ' + LineEnding +
    '    begin                                          ' + LineEnding +
    '      Result := Result + Ptr^;                     ' + LineEnding +
    '      Inc(Ptr);                                    ' + LineEnding +
    '    end;                                           ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArraySum';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayMean(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    AResult := getBaseType(ltDouble)
  else
    AResult := TLapeType_DynArray(AParams[0]).PType;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayMean',
    '{$RANGECHECKS OFF}                                ' + LineEnding +
    'begin                                             ' + LineEnding +
    '  if (Length(Param0) > 0) then                    ' + LineEnding +
    '    Result := ArraySum(Param0) / Length(Param0)   ' + LineEnding +
    '  else                                            ' + LineEnding +
    '    Result := Default(Result);                    ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayMean';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayMedian(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    AResult := getBaseType(ltDouble)
  else
    AResult := TLapeType_DynArray(AParams[0]).PType;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], AResult)) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayMedian',
    '{$RANGECHECKS OFF}                                     ' + LineEnding +
    'type                                                   ' + LineEnding +
    '  TType = PType(Param0);                               ' + LineEnding +
    'var                                                    ' + LineEnding +
    '  Arr: array of TType;                                 ' + LineEnding +
    '  Mid: Double;                                         ' + LineEnding +
    'begin                                                  ' + LineEnding +
    '  if (Length(Param0) < 2) then                         ' + LineEnding +
    '    raise "ArrayMedian requires at least two values";  ' + LineEnding +
    '  Arr := Sorted(Param0);                               ' + LineEnding +
    '  Mid := (Length(Arr) - 1) / 2;                        ' + LineEnding +
    '  Result := (Arr[Trunc(Mid)] + Arr[Ceil(Mid)]) / 2;    ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayMedian';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayVariance(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltDouble))) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayVariance',
    '{$RANGECHECKS OFF}                             ' + LineEnding +
    'type                                           ' + LineEnding +
    '  TType = PType(Param0);                       ' + LineEnding +
    'var                                            ' + LineEnding +
    '  Mean, Square: Double;                        ' + LineEnding +
    '  i: Int32;                                    ' + LineEnding +
    'begin                                          ' + LineEnding +
    '  if Length(Param0) = 0 then                   ' + LineEnding +
    '    Exit(Default(Result));                     ' + LineEnding +
    '                                               ' + LineEnding +
    '  Mean := ArrayMean(Param0);                   ' + LineEnding +
    '  for i := Low(Param0) to High(Param0) do      ' + LineEnding +
    '    Square := Square + Sqr(Param0[i] - Mean);  ' + LineEnding +
    '  Result := Square / Length(Param0);           ' + LineEnding +
    'end;                                           '
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayVariance';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayStdev(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptNormal], [TLapeGlobalVar(nil)], getBaseType(ltDouble))) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayStdev',
    'begin                                     ' + LineEnding +
    '  Result := Sqrt(ArrayVariance(Param0));  ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayStdev';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayDifference(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 2) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], AParams[0])) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayDifference',
    '{$RANGECHECKS OFF}                    ' + LineEnding +
    'var                                   ' + LineEnding +
    '  Val: PType(Param0);                 ' + LineEnding +
    '  Count: Int32 = 0;                   ' + LineEnding +
    'begin                                 ' + LineEnding +
    '  SetLength(Result, Length(Param0));  ' + LineEnding +
    '  for Val in Param0 do                ' + LineEnding +
    '    if IndexOf(Val, Param1) = -1 then ' + LineEnding +
    '    begin                             ' + LineEnding +
    '      Result[Count] := Val;           ' + LineEnding +
    '      Count := Count + 1;             ' + LineEnding +
    '    end;                              ' + LineEnding +
    '  SetLength(Result, Count);           ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayDifference';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArraySymDifference(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 2) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], AParams[0])) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArraySymDifference',
    '{$RANGECHECKS OFF}                                    ' + LineEnding +
    'var                                                   ' + LineEnding +
    '  Val: PType(Param0);                                 ' + LineEnding +
    '  Count: Int32 = 0;                                   ' + LineEnding +
    'begin                                                 ' + LineEnding +
    '  SetLength(Result, Length(Param0) + Length(Param1)); ' + LineEnding +
    '  for Val in Param0 do                                ' + LineEnding +
    '    if IndexOf(val, Param1) = -1 then                 ' + LineEnding +
    '    begin                                             ' + LineEnding +
    '      Result[Count] := Val;                           ' + LineEnding +
    '      Count := Count + 1;                             ' + LineEnding +
    '    end;                                              ' + LineEnding +
    '  for Val in Param1 do                                ' + LineEnding +
    '    if IndexOf(val, Param0) = -1 then                 ' + LineEnding +
    '    begin                                             ' + LineEnding +
    '      Result[Count] := Val;                           ' + LineEnding +
    '      Count := Count + 1;                             ' + LineEnding +
    '    end;                                              ' + LineEnding +
    '  SetLength(Result, Count);                           ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArraySymDifference';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayIntersection(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Method: TLapeTree_Method;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 2) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], AParams[0])) as TLapeType_Method;
  Method := addGlobalFunc(Header, '!ArrayIntersection',
    '{$RANGECHECKS OFF}                                        ' + LineEnding +
    'var                                                       ' + LineEnding +
    '  Val: PType(Param0);                                     ' + LineEnding +
    '  Count: Int32 = 0;                                       ' + LineEnding +
    'begin                                                     ' + LineEnding +
    '  SetLength(Result, Min(Length(Param0), Length(Param1))); ' + LineEnding +
    '  for Val in Param0 do                                    ' + LineEnding +
    '    if IndexOf(Val, Param1) > -1 then                     ' + LineEnding +
    '    begin                                                 ' + LineEnding +
    '      Result[Count] := Val;                               ' + LineEnding +
    '      Count := Count + 1;                                 ' + LineEnding +
    '    end;                                                  ' + LineEnding +
    '  SetLength(Result, Count);                               ' + LineEnding +
    'end;'
  );

  Result := Method.Method;
  Result.VarType.Name := '_ArrayIntersection';

  Sender.addMethod(Result);
end;

function TLapeCompiler.GetMethod_ArrayEquals(Sender: TLapeType_OverloadedMethod; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;

  function IsMultiDimensional: Boolean;
  begin
    Result := TLapeType_DynArray(AParams[0]).PType is TLapeType_DynArray;
  end;

  function CanCompareMem: Boolean;
  var
    ElementType: TLapeType;
  begin
    Result := False;
    if (AParams[0].ClassType = TLapeType_DynArray) then
    begin
      ElementType := TLapeType_DynArray(AParams[0]).PType;
      if (ElementType <> nil) and (not ElementType.NeedInitialization) then
        if (ElementType.BaseType in LapeStackTypes + [ltPointer]) then
          Result := True
        else if (ElementType is TLapeType_Record) and (TLapeType_Record(ElementType).Padding = 0) then
          Result := True;
    end;
  end;

var
  Header: TLapeType_Method;
  Body: lpString;
begin
  Result := nil;
  if (Sender = nil) or (Length(AParams) <> 2) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], getBaseType(ltBoolean))) as TLapeType_Method;

  if IsMultiDimensional() then
    Body := '{$RANGECHECKS OFF}'                                   + LineEnding +
            'var i: SizeInt;'                                      + LineEnding +
            'begin'                                                + LineEnding +
            '  Result := False;'                                   + LineEnding +
            '  if (Length(Param0) = Length(Param1)) then'          + LineEnding +
            '  begin'                                              + LineEnding +
            '    for i := 0 to High(Param0) do'                    + LineEnding +
            '      if not ArrayEquals(Param0[i], Param1[i]) then'  + LineEnding +
            '        Exit;'                                        + LineEnding +
            '    Result := True;'                                  + LineEnding +
            '  end;'                                               + LineEnding +
            'end;'
  else if CanCompareMem() then
    Body := '{$RANGECHECKS OFF}'                                                                          + LineEnding +
            'var Len1 := Length(Param0);'                                                                 + LineEnding +
            'var Len2 := Length(Param1);'                                                                 + LineEnding +
            'begin'                                                                                       + LineEnding +
            '  if (Len1 = Len2) then'                                                                     + LineEnding +
            '    Result := (Len1 = 0) or CompareMem(Param0[0], Param1[0], Len1 * SizeOf(PType(Param0)))'  + LineEnding +
            '  else'                                                                                      + LineEnding +
            '    Result := False;'                                                                        + LineEnding +
            'end;'
  else
    Body := '{$RANGECHECKS OFF}'                                 + LineEnding +
            'type'                                               + LineEnding +
            '  TType = PType(Param0);'                           + LineEnding +
            '  TOtherType = PType(Param1);'                      + LineEnding +
            'var'                                                + LineEnding +
            '  Ptr, Upper: ^TType;'                              + LineEnding +
            '  OtherPtr: ^TOtherType;'                           + LineEnding +
            'begin'                                              + LineEnding +
            '  Result := False;'                                 + LineEnding +
            ''                                                   + LineEnding +
            '  if (Length(Param0) = Length(Param1)) then'        + LineEnding +
            '  begin'                                            + LineEnding +
            '    if (_ArrayRange(Param0, Ptr, Upper) > 0) then'  + LineEnding +
            '    begin'                                          + LineEnding +
            '      OtherPtr := @Param1[Low(Param1)];'            + LineEnding +
            '      while (PtrUInt(Ptr) <= PtrUInt(Upper)) do'    + LineEnding +
            '      begin'                                        + LineEnding +
            '        if (Ptr^ <> OtherPtr^) then'                + LineEnding +
            '          Exit;'                                    + LineEnding +
            '        Inc(Ptr);'                                  + LineEnding +
            '        Inc(OtherPtr);'                             + LineEnding +
            '      end;'                                         + LineEnding +
            '    end;'                                           + LineEnding +
            ''                                                   + LineEnding +
            '    Result := True;'                                + LineEnding +
            '  end;'                                             + LineEnding +
            'end;';

  Result := addGlobalFunc(Header, '!ArrayEquals', Body).Method;
  Result.VarType.Name := '_ArrayEquals';

  Sender.addMethod(Result);
end;

procedure TLapeCompiler.InitBaseDefinitions;

  procedure addCompilerFuncs;
  const
    pn = lptNormal;
    pv = lptVar;
  var
    i, b, n: TLapeType;
    gn: TLapeGlobalVar;
  begin
    i := getBaseType(ltSizeInt);
    b := getBaseType(ltEvalBool);

    n := nil;
    gn := nil;

    addGlobalFunc([n, n, i], [pv, pv, pn], [gn, gn, gn], b, @_LapeCompareMem, '!cmp');
    addGlobalFunc([n, n, i], [pv, pv, pn], [gn, gn, gn], @_LapeMove, '!move');
  end;

  function NewMagicMethod(GetMethod: TLapeGetOverloadedMethod; NeedFullMatch: Boolean = True): TLapeType_OverloadedMethod;
  begin
    Result := TLapeType_OverloadedMethod.Create(Self);
    Result.OnFunctionNotFound := GetMethod;
    Result.NeedFullMatch := NeedFullMatch;
    addManagedDecl(Result);
  end;

  procedure addToString;
  var
    OLMethod: TLapeType_OverloadedMethod;
    BaseType: ELapeBaseType;
  begin
    OLMethod := NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetToStringMethod);
    for BaseType := Low(ELapeBaseType) to High(ELapeBaseType) do
      if ({$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType] <> nil) then
        OLMethod.addMethod(
          TLapeType_Method(addManagedType(
            TLapeType_Method.Create(
              Self,
              [getBaseType(BaseType)],
              [lptConstRef],
              [TLapeGlobalVar(nil)],
              getBaseType(ltString)
            )
          )).NewGlobalVar(
            {$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType]
          )
        );

    addGlobalVar(OLMethod.NewGlobalVar('ToString'));
  end;
begin
  StartImporting();

  addBaseDefine('Lape');
  addBaseDefine('Sesquipedalian');

  {$IFDEF Lape_CaseSensitive}
  addBaseDefine('Lape_CaseSensitive');
  {$ELSE}
  addBaseDefine('Lape_CaseInsensitive');
  {$ENDIF}
  {$IFDEF Lape_PascalLabels}
  addBaseDefine('Lape_PascalLabels');
  {$ELSE}
  addBaseDefine('Lape_LapeLabels');
  {$ENDIF}
  {$IFDEF Lape_Unicode}
  addBaseDefine('Lape_Unicode');
  {$ELSE}
  addBaseDefine('Lape_Ansi');
  {$ENDIF}
  {$IFDEF Lape_NoExtended}
  addBaseDefine('Lape_NoExtended');
  {$ENDIF}

  addCompilerFuncs();
  addGlobalVar(addManagedType(TLapeType_SystemUnit.Create(Self)).NewGlobalVarP(nil), 'System').isConstant := True;
  addGlobalVar(addManagedType(TLapeType_NilPointer.Create(Self, nil, False)).NewGlobalVarP(), 'nil').isConstant := True;

  addGlobalType(TLapeType_Label.Create(Self), '!label');
  addGlobalType(TLapeType_Pointer.Create(Self, nil, True), 'ConstPointer');
  addGlobalType(getBaseType(ltString).createCopy(), 'string');
  addGlobalType(getBaseType(ltChar).createCopy(), 'Char');
  addGlobalType(getBaseType(ltEvalBool).createCopy(), 'EvalBool');
  addGlobalType('packed record Method, Self: Pointer; end;', 'TMethod');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(SizeInt), True)).createCopy(), 'SizeInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(SizeUInt), False)).createCopy(), 'SizeUInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(NativeInt), True)).createCopy(), 'NativeInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(NativeUInt), False)).createCopy(), 'NativeUInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(PtrInt), True)).createCopy(), 'PtrInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(PtrUInt), False)).createCopy(), 'PtrUInt');

  addGlobalType('array of String', 'TStringArray');
  addGlobalType('array of Boolean', 'TBooleanArray');
  addGlobalType('array of Int32', 'TIntegerArray');
  addGlobalType('array of Single', 'TSingleArray');
  addGlobalType('array of Double', 'TDoubleArray');
  {$IFNDEF Lape_NoExtended}
  addGlobalType('array of Extended', 'TExtendedArray');
  {$ENDIF}

  addDelayedCode(
    'const                 ' + LineEnding +
    '  True  = EvalBool(1);' + LineEnding +
    '  False = EvalBool(0);' + LineEnding +
    '',
    '!InitBaseDefinitions'
  );

  addGlobalFunc('function _LocationToStr(DocPos: Pointer): string;', @_LapeLocationToStr);

  addGlobalFunc('procedure _Write(s: string);', @_LapeWrite);
  addGlobalFunc('procedure _WriteLn();', @_LapeWriteLn);

  addGlobalFunc('procedure _Assert(Expr: EvalBool); overload;', @_LapeAssert);
  addGlobalFunc('procedure _Assert(Expr: EvalBool; Msg: string); overload;', @_LapeAssertMsg);
  addGlobalFunc('procedure _RangeCheck(Idx, Lo, Hi: SizeInt);', @_LapeRangeCheck);

  addGlobalFunc('procedure _SStr_SetLen(var s; l, m: UInt8);', @_LapeSStr_SetLen);
  addGlobalFunc('procedure _AStr_SetLen(var s: AnsiString; l: SizeInt);', @_LapeAStr_SetLen);
  addGlobalFunc('procedure _WStr_SetLen(var s: WideString; l: SizeInt);', @_LapeWStr_SetLen);
  addGlobalFunc('procedure _UStr_SetLen(var s: UnicodeString; l: SizeInt);', @_LapeUStr_SetLen);

  addGlobalFunc('procedure _SStr_Copy(s: ShortString; Start: UInt8 = 1; Count: UInt8 = High(UInt8); out Result: AnsiString);', @_LapeSStr_Copy);
  addGlobalFunc('procedure _AStr_Copy(s: AnsiString; Start: SizeInt = 1; Count: SizeInt = High(SizeInt); out Result: AnsiString);', @_LapeAStr_Copy);
  addGlobalFunc('procedure _WStr_Copy(s: WideString; Start: SizeInt = 1; Count: SizeInt = High(SizeInt); out Result: WideString);', @_LapeWStr_Copy);
  addGlobalFunc('procedure _UStr_Copy(s: UnicodeString; Start: SizeInt = 1; Count: SizeInt = High(SizeInt); out Result: UnicodeString);', @_LapeUStr_Copy);

  addGlobalFunc('procedure _SStr_Delete(var s; Start: UInt8; Count: UInt8 = High(UInt8));', @_LapeSStr_Delete);
  addGlobalFunc('procedure _AStr_Delete(var s: AnsiString; Start: SizeInt; Count: SizeInt = High(SizeInt));', @_LapeAStr_Delete);
  addGlobalFunc('procedure _WStr_Delete(var s: WideString; Start: SizeInt; Count: SizeInt = High(SizeInt));', @_LapeWStr_Delete);
  addGlobalFunc('procedure _UStr_Delete(var s: UnicodeString; Start: SizeInt; Count: SizeInt = High(SizeInt));', @_LapeUStr_Delete);

  addGlobalFunc('procedure _AStr_Insert(Src: AnsiString; var Dst: AnsiString; Pos: SizeInt = 1; Count: SizeInt = 0);', @_LapeAStr_Insert);
  addGlobalFunc('procedure _WStr_Insert(Src: WideString; var Dst: WideString; Pos: SizeInt = 1; Count: SizeInt = 0);', @_LapeWStr_Insert);
  addGlobalFunc('procedure _UStr_Insert(Src: UnicodeString; var Dst: UnicodeString; Pos: SizeInt = 1; Count: SizeInt = 0);', @_LapeUStr_Insert);

  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int32; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_Int32);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of UInt32; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_UInt32);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int64; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_Int64);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of UInt64; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_UInt64);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Single; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_Single);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Double; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_Double);
  {$IFNDEF Lape_NoExtended}
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Extended; SortUp: EvalBool); overload;', @_LapeArraySortWeighted_Extended);
  {$ENDIF}

  addGlobalFunc('procedure _ArrayReverse(A: Pointer; ElSize, Len: SizeInt); overload;', @_LapeArrayReverse);

  addGlobalFunc('function GetMem(i: SizeInt): Pointer;', @_LapeGetMem);
  addGlobalFunc('function AllocMem(i: SizeInt): Pointer;', @_LapeAllocMem);
  addGlobalFunc('procedure FreeMem(p: Pointer);', @_LapeFreeMem);
  addGlobalFunc('procedure ReallocMem(var p: Pointer; s: SizeInt);', @_LapeReallocMem);

  addGlobalFunc('procedure FillMem(var p; s: SizeInt; b: UInt8 = 0);', @_LapeFillMem);
  addGlobalFunc('procedure Move(constref Src; var Dst; s: SizeInt);', @_LapeMove);
  addGlobalFunc('function CompareMem(constref p1, p2; Length: SizeInt): EvalBool;', @_LapeCompareMem);

  addGlobalFunc('function Assigned(constref p): EvalBool;', @_LapeAssigned);

  addGlobalFunc('function BitCount(constref Value; Size: UInt8): UInt32;', @_LapeBitCount);

  addGlobalFunc('procedure RaiseException(Message: string); overload;', @_LapeRaiseString);
  addGlobalFunc('procedure RaiseException(Message: String; DocPos: Pointer); overload;', @_LapeRaiseStringWithDocPos);

  addGlobalFunc('procedure UniqueString(var Str: AnsiString); overload;', @_LapeAStr_Unique);
  addGlobalFunc('procedure UniqueString(var Str: WideString); overload;', @_LapeWStr_Unique);
  addGlobalFunc('procedure UniqueString(var Str: UnicodeString); overload;', @_LapeUStr_Unique);

  addToString();
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetDisposeMethod).NewGlobalVar('_Dispose'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetDisposeObjectMethod).NewGlobalVar('_DisposeObject'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetCopyMethod).NewGlobalVar('_Assign'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetObjectifyMethod).NewGlobalVar('_Objectify'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArraySort).NewGlobalVar('_ArraySort'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayRange).NewGlobalVar('_ArrayRange'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayMin).NewGlobalVar('_ArrayMin'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayMax).NewGlobalVar('_ArrayMax'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayUnique).NewGlobalVar('_ArrayUnique'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayIndexOf).NewGlobalVar('_ArrayIndexOf'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayIndicesOf).NewGlobalVar('_ArrayIndicesOf'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayMode).NewGlobalVar('_ArrayMode'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArraySum).NewGlobalVar('_ArraySum'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayMean).NewGlobalVar('_ArrayMean'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayMedian).NewGlobalVar('_ArrayMedian'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayVariance).NewGlobalVar('_ArrayVariance'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayStdev).NewGlobalVar('_ArrayStdev'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayDifference).NewGlobalVar('_ArrayDifference'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArraySymDifference).NewGlobalVar('_ArraySymDifference'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayIntersection).NewGlobalVar('_ArrayIntersection'));
  addGlobalVar(NewMagicMethod({$IFDEF FPC}@{$ENDIF}GetMethod_ArrayEquals).NewGlobalVar('_ArrayEquals'));

  InitBaseMath();
  InitBaseString();
  InitBaseDateTime();
  InitBaseVariant();
  InitBaseFile();

  addDelayedCode(
    LapeDelayedFlags +
    _LapeToString_Enum +
    Format(_LapeToString_Set, ['Small', Ord(High(ELapeSmallEnum))]) +
    Format(_LapeToString_Set, ['Large', Ord(High(ELapeLargeEnum))]) +
    _LapeToString_Array +
    _LapeSwap +
    _LapeSetLength +
    _LapeObjectSetLength +
    _LapeCopy +
    _LapeDelete +
    _LapeInsert +
    _LapeArraySlice,
    '!addDelayedCore'
  );

  EndImporting();
end;

procedure TLapeCompiler.InitBaseMath;
begin
  {$I lpeval_import_math.inc}
end;

procedure TLapeCompiler.InitBaseString;
begin
  {$I lpeval_import_string.inc}
end;

procedure TLapeCompiler.InitBaseDateTime;
begin
 {$I lpeval_import_datetime.inc}
end;

procedure TLapeCompiler.InitBaseVariant;
begin
  {$I lpeval_import_variant.inc}
end;

procedure TLapeCompiler.InitBaseFile;
begin
  {$I lpeval_import_file.inc}
end;

function TLapeCompiler.EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
begin
 if (Node <> nil) then
   Result := Node
 else
   Result := TLapeTree_ExprBase.Create(Self, getPDocPos());
end;

function TLapeCompiler.EnsureTypeExpression(Node: TLapeTree_Base): TLapeTree_Base;
begin
 if (Node <> nil) then
   Result := Node
 else
   Result := TLapeTree_Base.Create(Self, getPDocPos());
end;

function TLapeCompiler.EnsureRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeTree_Range;
begin
  VarType := nil;
  if (Node <> nil) and (Node is TLapeTree_Range) then
    Result := TLapeTree_Range(Node)
  else if (Node <> nil) and (Node is TLapeTree_VarType) and (TLapeTree_VarType(Node).VarType <> nil) then
  begin
    Result := TLapeTree_Range.Create(Self, @Node._DocPos);
    with TLapeTree_VarType(Node).VarType do
    begin
      Result.Lo := TLapeTree_GlobalVar.Create(VarLo(), Self, @Node._DocPos);
      Result.Hi := TLapeTree_GlobalVar.Create(VarHi(), Self, @Node._DocPos);
    end;
  end
  else
    LapeException(lpeInvalidRange, [Node, Self]);

  if (Result.Lo <> nil) and (Result.Hi <> nil) then
    VarType := Result.Hi.resType();
  if (VarType = nil) or (not VarType.CompatibleWith(Result.Lo.resType())) then
    LapeException(lpeInvalidRange, Node.DocPos);
end;

function TLapeCompiler.EnsureRange(Node: TLapeTree_Base): TLapeTree_Range;
var
  VarType: TLapeType;
begin
  Result := EnsureRange(Node, VarType);
end;

function TLapeCompiler.EnsureRange(VarType: TLapeType): TLapeType;
var
  Node: TLapeTree_VarType;
  Range: TLapeRange;
begin
  if (VarType = nil) or (VarType is TLapeType_SubRange) then
    Exit(VarType);

  Node := TLapeTree_VarType.Create(VarType, Self);
  try
    Range := EnsureConstantRange(Node, VarType);
    Result := addManagedType(TLapeType_SubRange.Create(Range, Self, VarType));
  finally
    Node.Free();
  end;
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange;
var
  Range: TLapeTree_Range;
  Lo, Hi: TLapeGlobalVar;
begin
  Range := EnsureRange(Node, VarType);
  try
    if (Node = nil) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);

    Lo := Range.Lo.Evaluate();
    Hi := Range.Hi.Evaluate();
    if (Lo = nil) or (not Lo.HasType()) or (not Lo.VarType.IsOrdinal()) or
       (Hi = nil) or (not Hi.HasType()) or (not Hi.VarType.IsOrdinal())
    then
      LapeException(lpeInvalidRange, Node.DocPos)
    else if (not Lo.Readable) or (not Hi.Readable) then
      LapeException(lpeConstantExpected, Node.DocPos);

    Result.Lo := Lo.AsInteger;
    Result.Hi := Hi.AsInteger;
    if (Result.Hi < Result.Lo) then
      LapeException(lpeInvalidRange, Node.DocPos);
  finally
    if (Range <> Node) then
      Range.Free();
  end;
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base): TLapeRange;
var
  VarType: TLapeType;
begin
  Result := EnsureConstantRange(Node, VarType);
end;

function TLapeCompiler.FindFile(AFileName: lpString): lpString;
var
  i: Integer;
  Dir: lpString;
begin
  if (AFileName = '') then
    Exit('');

  if ({$IFNDEF FPC}@{$ENDIF}FOnFindFile <> nil) then
    FOnFindFile(Self, AFileName);

  AFileName := StringReplace(AFileName, '\', '/', [rfReplaceAll]);
  if FileExists(AFileName) then
    Exit(AFileName);

  for i := FTokenizer downto 0 do
    if (FTokenizers[i] <> nil) then
    begin
      Dir := ExtractFilePath(FTokenizers[i].FileName);
      if FileExists(string(Dir + AFileName)) then
        Exit(Dir + AFileName);
    end;

  if FileExists(string(AFileName)) then
    Result := AFileName
  else
    Result := '';
end;

procedure TLapeCompiler.addPreprocessorFunc(Name: lpString; Func: TLapePreprocessorFunc);
begin
  FPreprocessorFuncs.Add(UpperCase(Name), Func);
end;

procedure TLapeCompiler.addPreprocessorMacro(Name: lpString; Func: TLapePreprocessorFunc);
begin
  FPreprocessorMacros.Add(UpperCase(Name), Func);
end;

function TLapeCompiler.HandlePreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
begin
  Result := True;

  if (Name = 'DEFINED') then
    Value := BoolToStr(hasDefine(Argument), True)
  else
  if (Name = 'DECLARED') then
    Value := BoolToStr(hasDeclaration(Argument), True)
  else
  if (Name = 'FILEEXISTS') then
    Value := BoolToStr(FileExists(Argument) or FileExists(IncludeTrailingPathDelimiter(ExtractFileDir(Tokenizer.FileName)) + Argument), True)
  else
  if (Name = 'INCLUDED') then
    Value := BoolToStr(FIncludes.IndexOf(FindFile(Argument)) > -1, True)
  else
    Result := False;
end;

function TLapeCompiler.HandlePreprocessorMacro(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
var
  i: Integer;
begin
  Result := True;

  if (Name = 'FILE') then
    Value := #39 + Tokenizer.FileName + #39
  else
  if (Name = 'DIR') then
    Value := #39 + IncludeTrailingPathDelimiter(ExtractFileDir(Tokenizer.FileName)) + #39
  else
  if (Name = 'LINE') then
    Value := 'UInt32(' + IntToStr(Tokenizer.DocPos.Line) + ')'
  else
  if (Name = 'NOW') then
    Value := 'TDateTime(' + FloatToStrDot(Now()) + ')'
  else
  if (Name = 'ENV') then
    Value := #39 + GetEnvironmentVariable(Argument) + #39
  else
  if (Name = 'TICKCOUNT') then
    Value := 'UInt64(' + UIntToStr(TThread.GetTickCount64()) + ')'
  else
  if (Name = 'FUNC') then
    Value := #39 + FStackInfo.FuncName + #39
  else
  if (Name = 'INCLUDEDFILE') then
  begin
    for i := 0 to FIncludes.Count - 1 do
      if (Argument = ExtractFileName(FIncludes[i])) then
      begin
        Value := #39 + FIncludes[i] + #39;
        Exit;
      end;
  end
  else
  if (Name = 'INCLUDEDFILES') then
  begin
    Value := 'TStringArray([';
    for i := 0 to FIncludes.Count - 1 do
    begin
      if (i > 0) then
        Value := Value + ', ';
      Value := Value + #39 + FIncludes[i] + #39;
    end;
    Value := Value + '])';
  end
  else
    Result := False;
end;

function TLapeCompiler.EvalPreprocessorExpr(Expr: String; ADocPos: TDocPos): Boolean;

  function hasFunc(const Expr, Name: lpString; out StartPos, EndPos: Integer; out Param: lpString): Boolean;
  var
    Upper: lpString;
  begin
    Upper := UpperCase(Expr);

    StartPos := Pos(Name + '(', Upper);
    if (StartPos > 0) then
      EndPos := Pos(')', Upper, StartPos);

    Result := (StartPos > 0) and (EndPos > 0);
    if Result then
      Param := lpString(Copy(Expr, StartPos + Length(Name) + 1, (EndPos - StartPos) - (Length(Name) + 1)));
  end;

var
  OldState: Pointer;
  ExprBase: TLapeTree_ExprBase;
  Res: TLapeGlobalVar;
  StartPos, EndPos: Integer;
  Param, Value: lpString;
  i: Integer;
begin
  Result := False;

  for i := 0 to FPreprocessorFuncs.Count - 1 do
    while hasFunc(Expr, FPreprocessorFuncs.Key[i], StartPos, EndPos, Param) do
    begin
      if not FPreprocessorFuncs.ItemsI[i](Self, FPreprocessorFuncs.Key[i], Param, Value) then
        LapeExceptionFmt(lpeCannotEvalPreprocessorFunc, [FPreprocessorFuncs.Key[i]], ADocPos);

      // replace `Defined(hello)` with `Value`
      Delete(Expr, startPos, (endPos - startPos) + 1);
      Insert(Value, Expr, startPos);
    end;

  try
    OldState := getTempTokenizerState(Expr);
    Tokenizer.OverridePos := @ADocPos;

    ExprBase := ParseExpression();
    if (ExprBase = nil) then
      LapeException(lpeCannotEvalConst, Tokenizer.DocPos);
    Res := ExprBase.Evaluate();
    if (Res = nil) or (not (Res.BaseType in LapeBoolTypes)) then
      LapeException(lpeExpectedBoolExpression, Tokenizer.DocPos);

    Result := Res.AsInteger <> 0;
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.EvalPreprocessorMacro(Argument: String; ADocPos: TDocPos): Boolean;
var
  Param, Value: lpString;
  StartPos, EndPos: Integer;
  i: Integer;
begin
  Result := False;

  StartPos := Pos('(', Argument);
  if (StartPos > 0) then
    EndPos := Pos(')', Argument, StartPos);
  if (StartPos > 0) and (EndPos > 0) then
  begin
    Param := Copy(Argument, StartPos + 1, (EndPos - StartPos) - 1);
    Argument := UpperCase(Trim(Copy(Argument, 1, StartPos - 1)));
  end else
  begin
    Param := '';
    Argument := Trim(UpperCase(Argument));
  end;

  for i := 0 to FPreprocessorMacros.Count - 1 do
    if (Argument = FPreprocessorMacros.Key[i]) then
    begin
      if FPreprocessorMacros.ItemsI[i](Self, FPreprocessorMacros.Key[i], Param, Value) and (Value <> '') then
        pushTokenizer(TLapeTokenizerString.Create(Value));

      Result := True;
      Exit;
    end;

    for i := 0 to FDefines.Count - 1 do
      if (FDefines[i].Name = Argument) then
      begin
        pushTokenizer(TLapeTokenizerString.Create(FDefines[i].Value));

        Result := True;
        Exit;
      end;

  LapeExceptionFmt(lpeUnknownDeclaration, [string(Argument)], ADocPos);
end;

function TLapeCompiler.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;

  function hasDefineOrOption(Name: lpString): Boolean;
  begin
    Result := hasDefine(Name) or hasBaseDefine(Name);
    if Result then
      Exit;

    Name := LowerCase(Name);
    if (Name = 'assertions') then
      Result := (lcoAssertions in FOptions)
    else
    if (Name = 'rangechecks') then
      Result := (lcoRangeCheck in FOptions)
    else
    if (Name = 'booleval') then
      Result := (lcoShortCircuit in FOptions)
    else
    if (Name = 'memoryinit') then
      Result := (lcoAlwaysInitialize in FOptions)
    else
    if (Name = 'fulldisposal') then
      Result := (lcoFullDisposal in FOptions)
    else
    if (Name = 'loosesemicolon') then
      Result := (lcoLooseSemicolon in FOptions)
    else
    if (Name = 'extendedsyntax') then
      Result := (lcoLooseSyntax in FOptions)
    else
    if (Name = 'autoinvoke') then
      Result := (lcoAutoInvoke in FOptions)
    else
    if (Name = 'scopedenums') then
      Result := (lcoScopedEnums in FOptions)
    else
    if (Name = 'constaddress') then
      Result := (lcoConstAddress in FOptions)
    else
    if (Name = 'coperators') then
      Result := (lcoCOperators in FOptions)
    else
    if (Name = 'hints') then
      Result := (lcoHints in FOptions)
    else
    if (Name = 'autoobjectify') then
      Result := (lcoAutoObjectify in FOptions)
    else
    if (Name = 'explicitself') then
      Result := (lcoExplicitSelf in FOptions)
    else
    if (Name = 'duplicatelocalnamehints') then
      Result := (lcoDuplicateLocalNameHints in FOptions)
    else
    if (Name = 'verbosecompile') then
      Result := (lcoVerboseCompile in FOptions)
    else
    if (Name = 'relativefilenames') then
      Result := (lcoRelativeFileNames in FOptions)
    else
    if (Name = 'methoddeclarationparentheses') then
      Result := (lcoMethodDeclarationParentheses in FOptions);
  end;

  procedure switchConditional;
  var
    Conditional: TLapeConditional;
  begin
    if (FConditionalStack.Count <= 0) then
      LapeException(lpeLostConditional, Sender.DocPos)
    else
    begin
      Conditional := FConditionalStack.Pop();
      Conditional.Eval := (not InIgnore()) and (not Conditional.Eval);
      FConditionalStack.Push(Conditional);
    end;
  end;

  procedure setOption(Option: ECompilerOption);
  begin
    Argument := LowerCase(Argument);

    if (Argument = 'on') or (Argument = '+') then
      Include(FOptions, Option)
    else if (Argument = 'off') or (Argument = '-') then
      Exclude(FOptions, Option)
    else
      LapeException(lpeInvalidCondition, Sender.DocPos);
  end;

  procedure setAlignment(Argument: lpString);
  begin
    Argument := LowerCase(Argument);

    if (Argument = 'on') then
      FOptions_PackRecords := Lape_PackRecordsDef
    else if (Argument = 'off') then
      FOptions_PackRecords := 1
    else
    begin
      FOptions_PackRecords := StrToIntDef(string(Argument), 0);
      if (not (FOptions_PackRecords in [1, 2, 4, 8, 16])) then
        LapeException(lpeInvalidCondition, Sender.DocPos);
    end;
  end;

  procedure handleDefine(Directive, Argument: lpString);
  var
    p, i: Integer;
    Name, Value: lpString;
  begin
    if (Directive = 'define') then
    begin
      p := Pos(':=', lpString(Argument));
      if (p > 0) then
      begin
        Name := TrimRight(Copy(Argument, 1, p - 1));
        Value := TrimLeft(Copy(Argument, p + 2));
      end else
      begin
        Name := Argument;
        Value := '';
      end;

      if (Name = '') then
        LapeException(lpeInvalidEvaluation, Sender.DocPos);

      addDefine(Name, Value);
    end else
    if (Directive = 'undef') then
    begin
      Name := UpperCase(Argument);
      for i := FDefines.Count - 1 downto 0 do
        if (FDefines[i].Name = Name) then
          FDefines.Delete(i);
    end;
  end;

  procedure pushInclude(Directive, Argument: lpString);
  var
    IncludeFile: lpString;
    NewTokenizer: TLapeTokenizerBase;
  begin
    IncludeFile := FindFile(Argument);
    if (IncludeFile = '') or (not FileExists(IncludeFile)) then
      LapeExceptionFmt(lpeFileNotFound, [Argument], Sender.DocPos);
    IncludeFile := ExpandFileName(IncludeFile);

    if (Directive = 'include_once') and (FIncludes.IndexOf(string(IncludeFile)) > -1) then
      Exit;

    if (not Sender.InPeek) then
      FIncludes.Add(string(IncludeFile));

    if (FTokenizer + 1 < Length(FTokenizers)) and (FTokenizers[FTokenizer + 1] <> nil) and (FTokenizers[FTokenizer + 1].FileName = IncludeFile) then
    begin
      NewTokenizer := FTokenizers[FTokenizer + 1];
      NewTokenizer.Reset();
    end else
      NewTokenizer := TLapeTokenizerFile.Create(IncludeFile);

    pushTokenizer(NewTokenizer);
  end;

begin
  Assert(Sender = Tokenizer);

  Result := True;

  if ({$IFNDEF FPC}@{$ENDIF}FOnHandleDirective <> nil) then
    if FOnHandleDirective(Self, Directive, Argument, Sender.InPeek, InIgnore()) then
      Exit;

  Directive := LowerCase(Directive);
  Argument := Trim(Argument);

  if InIgnore() then
  begin
    if (Directive = 'else') then
      switchConditional()
    else
    if (Directive = 'endif') then
      popConditional()
    else
    if (Directive = 'ifdef') or (Directive = 'ifndef') or (Directive = 'ifdecl') or (Directive = 'ifndecl') or (Directive = 'ifparam') or (Directive = 'if') then
      pushConditional(False, Sender.DocPos);
  end else
  begin
    // Conditionals
    if (Directive = 'else') then
      switchConditional()
    else
    if (Directive = 'endif') then
      popConditional()
    else
    if (Directive = 'if') then
      pushConditional(EvalPreprocessorExpr(Argument, Sender.DocPos), Sender.DocPos)
    else
    if (Directive = 'ifdef') then
      pushConditional(HasDefineOrOption(Argument), Sender.DocPos)
    else
    if (Directive = 'ifndef') then
      pushConditional(not HasDefineOrOption(Argument), Sender.DocPos)
    else
    if (Directive = 'ifdecl') then
      pushConditional(hasDeclaration(Argument), Sender.DocPos)
    else
    if (Directive = 'ifndecl') then
      pushConditional(not hasDeclaration(Argument), Sender.DocPos)
    else
    if (Directive = 'ifparam') then
      pushConditional(getDeclaration(Argument, True) is TLapeParameterVar, Sender.DocPos)
    else
    if (Directive = 'define') or (Directive = 'undef') then
      handleDefine(Directive, Argument)
    else
    // Includes
    if (Directive = 'macro') then
      EvalPreprocessorMacro(Argument, Sender.DocPos)
    else
    if (Directive = 'i') or (Directive = 'include') or (Directive = 'include_once') then
      pushInclude(Directive, Argument)
    else
    // Options
    if (Directive = 'a') or (Directive = 'align') then
      setAlignment(Argument)
    else
    if (Directive = 'c') or (Directive = 'assertions') then
      setOption(lcoAssertions)
    else
    if (Directive = 'r') or (Directive = 'rangechecks') then
      setOption(lcoRangeCheck)
    else
    if (Directive = 'b') or (Directive = 'booleval') then
      setOption(lcoShortCircuit)
    else
    if (Directive = 'm') or (Directive = 'memoryinit') then
      setOption(lcoAlwaysInitialize)
    else
    if (Directive = 'd') or (Directive = 'fulldisposal') then
      setOption(lcoFullDisposal)
    else
    if (Directive = 'l') or (Directive = 'loosesemicolon') then
      setOption(lcoLooseSemicolon)
    else
    if (Directive = 'x') or (Directive = 'extendedsyntax') then
      setOption(lcoLooseSyntax)
    else
    if (Directive = 'f') or (Directive = 'autoinvoke') then
      setOption(lcoAutoInvoke)
    else
    if (Directive = 's') or (Directive = 'scopedenums') then
      setOption(lcoScopedEnums)
    else
    if (Directive = 'j') or (Directive = 'constaddress') then
      setOption(lcoConstAddress)
    else
    if (Directive = 'h') or (Directive = 'hints') then
      setOption(lcoHints)
    else
    if (Directive = 'coperators') then
      setOption(lcoCOperators)
    else
    if (Directive = 'autoobjectify') then
      setOption(lcoAutoObjectify)
    else
    if (Directive = 'explicitself') then
      setOption(lcoExplicitSelf)
    else
    if (Directive = 'duplicatelocalnamehints') then
      setOption(lcoDuplicateLocalNameHints)
    else
    if (Directive = 'verbosecompile') then
      setOption(lcoVerboseCompile)
    else
    if (Directive = 'relativefilenames') then
      setOption(lcoRelativeFileNames)
    else
    if (Directive = 'methoddeclarationparentheses') then
      setOption(lcoMethodDeclarationParentheses)
    else
    // messages
    if (Directive = 'hint') then
      Hint(lphUserDefined, [Argument], Sender.DocPos)
    else
    if (Directive = 'error') then
      LapeExceptionFmt(lpeUserDefinedError, [Argument], Sender.DocPos)
    else
      Result := False;
  end;
end;

function TLapeCompiler.InIgnore: Boolean;
begin
  Result := (FConditionalStack.Cur >= 0) and (not FConditionalStack.Top.Eval);
end;

function TLapeCompiler.Next: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := Tokenizer.Tok;
  repeat
    Result := Tokenizer.Next{NoWhiteSpace}();
    if (Result = tk_NULL) and hasMoreTokenizers() then
    begin
      if Tokenizer.InPeek then
        Dec(FTokenizer)
      else
        popTokenizer();
      Result := Tokenizer.Next{NoWhiteSpace}();
    end;
  until (Result = tk_NULL) or ((not (Result in TokJunk)) and (not InIgnore()));
  __LapeTokenizerBase(Tokenizer).FLastTok := PrevTok;
end;

function TLapeCompiler.isNext(Tokens: EParserTokenSet; out Token: EParserToken): Boolean;
var
  OldState: Pointer;
begin
  Result := False;
  OldState := getState();
  try
    setTokenizersPeek(True);
    Token := Next();
    Result := Token in Tokens;
  finally
    if (not Result) then
      setState(OldState)
    else
    begin
      freeState(OldState);
      setTokenizersPeek(False);
    end;
  end;
end;

function TLapeCompiler.isNext(Tokens: EParserTokenSet): Boolean;
var
  Token: EParserToken;
begin
  Result := isNext(Tokens, Token);
end;

function TLapeCompiler.Peek: EParserToken;
var
  OldState: Pointer;
begin
  OldState := getState();
  try
    setTokenizersPeek(True);
    Result := Next();
  finally
    setState(OldState);
  end;
end;

function TLapeCompiler.Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    Next();
  Result := Tokenizer.Tok;
  if (Result <> Token) then
    LapeExceptionFmt(lpeExpectedOther, [Tokenizer.TokString, LapeTokenToString(Token)], Tokenizer.DocPos);
  if NextAfter then
    Next();
end;

function TLapeCompiler.Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken;
begin
  if NextBefore then
    Next();
  Result := Tokenizer.Tok;
  if (not (Result in Tokens)) then
    LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(Result)], Tokenizer.DocPos);
  if NextAfter then
    Next();
end;

procedure TLapeCompiler.ParseDeclHint(Decl: TLapeDeclaration; NextBefore: Boolean; NextAfter: Boolean);
begin
  if NextBefore then
    Next();
  Assert(Tokenizer.Tok in [tk_kw_Deprecated, tk_kw_Experimental, tk_kw_UnImplemented]);

  case Tokenizer.Tok of
    tk_kw_Deprecated:
      begin
        if (Peek() in [tk_typ_String, tk_typ_HereString]) then
        begin
          Next();
          Decl.AddHint(ldhDeprecated, Copy(Tokenizer.TokString, 2, Tokenizer.TokLen - 2));
        end else
          Decl.AddHint(ldhDeprecated);

        if NextAfter then
          Next();
      end;

    tk_kw_Experimental:
      begin
        Decl.AddHint(ldhExperimental);
        if NextAfter then
          Next();
      end;

    tk_kw_UnImplemented:
      begin
        Decl.AddHint(ldhUnImplemented);
        if NextAfter then
          Next();
      end;
  end;
end;

procedure TLapeCompiler.ParseExpressionEnd(Token: EParserToken; NextBefore: Boolean = False; NextAfter: Boolean = True);
begin
  if (not (lcoLooseSemicolon in FOptions)) then
    Expect(Token, NextBefore, NextAfter)
  else
    ParseExpressionEnd([Token], NextBefore, NextAfter);
end;

procedure TLapeCompiler.ParseExpressionEnd(Tokens: EParserTokenSet = ParserToken_ExpressionEnd; NextBefore: Boolean = False; NextAfter: Boolean = True);
begin
  if (not (lcoLooseSemicolon in FOptions)) then
    Expect(Tokens, NextBefore, NextAfter)
  else
  begin
    if NextBefore then
      isNext(Tokens);
    if NextAfter and (Tokenizer.Tok in Tokens) then
      Next();
  end;
end;

function TLapeCompiler.ParseIdentifierList(FirstNext: Boolean = False): TStringArray;
begin
  Result := nil;
  repeat
    Expect(tk_Identifier, FirstNext, False);
    FirstNext := True;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Tokenizer.TokString;
  until (Next() <> tk_sym_Comma);
end;

function TLapeCompiler.ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList;
var
  FuncForwards: TLapeFuncForwards;
  Statement: TLapeTree_Base;
  DoBreak: Boolean;
begin

  Result := TLapeTree_StatementList.Create(Self, getPDocPos());
  try
    FuncForwards := TLapeFuncForwards.Create(nil, dupError, True);

    try
      DoBreak := False;
      repeat
        Statement := nil;
        case Tokenizer.Tok of
          tk_NULL: Break;
          tk_kw_Begin:
            begin
              if StopAfterBeginEnd then
                Statement := ParseBeginEnd()
              else
                Statement := ParseBeginEnd(ParserToken_ExpressionEnd + [tk_sym_Dot]);
              DoBreak := (Tokenizer.LastTok = tk_sym_Dot) or StopAfterBeginEnd;
            end;
          tk_kw_Const, tk_kw_Var: Statement := ParseVarBlock();
          tk_kw_Function, tk_kw_Procedure, tk_kw_Operator, tk_kw_Property:
            addDelayedExpression(ParseMethod(FuncForwards));
          tk_kw_Type: ParseTypeBlock();

          {$IFDEF Lape_PascalLabels}
          tk_kw_Label: ParseLabelBlock();
          {$ENDIF}

          else if (lcoLooseSyntax in FOptions) and (not StopAfterBeginEnd) and (not (Tokenizer.Tok in ParserToken_BlockEnd)) then
            Statement := ParseStatement(False)
          else
            LapeException(lpeBlockExpected, Tokenizer.DocPos);
        end;

        if (Statement <> nil) then
          Result.addStatement(Statement);
      until DoBreak;

      if (FuncForwards.Count > 0) then
        LapeExceptionFmt(lpeInvalidForward, [FuncForwards[0].Name], FuncForwards[0].VarType.DocPos);
    finally
      while (FuncForwards.Count > 0) do
        FuncForwards.Delete(0).Free();
      FuncForwards.Free();
    end;

  except
    Result.Free();
    raise;
  end;

  if (Result.Statements.Count < 1) then
    FreeAndNil(Result);
end;

function TLapeCompiler.ParseMethodHeader(out Name: lpString; addToScope: Boolean = True): TLapeType_Method;

  procedure addVar(ParType: ELapeParameterType; VarType: TLapeType; AVar: lpString);
  var
    StackVar: TLapeStackVar;
  begin
    if addToScope then
      if (FStackInfo = nil) then
        LapeException(lpeImpossible, Tokenizer.DocPos)
      else if (LapeCase(Name) = LapeCase(AVar)) or hasDeclaration(AVar, True) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [AVar], Tokenizer.DocPos)
      else
      begin
        StackVar := FStackInfo.addVar(ParType, VarType, AVar);
        if (lcoHints in FOptions) then
          StackVar.Used := duFalse;
      end;
  end;

var
  i: Integer;
  Pos: TDocPos;
  ExpectResult: Boolean;
  methodDef: EMethodDef;
  Typ: TLapeDeclaration;
  Identifiers: TStringArray;
  Param: TLapeParameter;
  Token: EParserToken;
  Default: TLapeTree_ExprBase;
  op: EOperator;
  ltyp,rtyp:TLapeType;
begin
  Pos := Tokenizer.DocPos;
  Result := TLapeType_Method.Create(Self, nil, nil, '', @Pos);
  case Tokenizer.Tok of
    tk_kw_Operator: Result.MethodDef := mdOperator;
    tk_kw_Property: Result.MethodDef := mdProperty;
    else            Result.MethodDef := mdUnspecified;
  end;
  ExpectResult := Tokenizer.Tok in [tk_kw_Function, tk_kw_Operator];

  try
    if (isNext([tk_Identifier, tk_sym_ParenthesisOpen], Token) and (Token = tk_Identifier)) or
       ((Result.MethodDef = mdOperator) and isNext(ParserToken_Operators, Token)) then
    begin
      if Result.MethodDef <> mdOperator then
        Name := Tokenizer.TokString
      else begin
        op := ParserTokenToOperator(Tokenizer.Tok);
        if (op in OverloadableOperators) then
          Name := '!op_'+op_name[op]
        else
          LapeExceptionFmt(lpeCannotOverloadOperator, [Tokenizer.TokString], Tokenizer.DocPos);
      end;

      if isNext([tk_sym_Dot, tk_sym_ParenthesisOpen], Token) and (Token = tk_sym_Dot) then
      begin
        Expect(tk_Identifier, True, False);
        Token := tk_NULL;

        Typ := getDeclaration(Name);
        Name := Tokenizer.TokString;
        if (Typ is TLapeVar) then
          Typ := TLapeVar(Typ).VarType;

        if (Typ is TLapeType) then
        begin
          if addToScope then
            FStackInfo.addSelfVar(Lape_SelfParam, TLapeType(Typ));
          methodDef := Result.MethodDef;
          Result.Free();
          Result := TLapeType_MethodOfType.Create(Self, TLapeType(Typ), nil, nil, '', @Pos);
          Result.MethodDef := methodDef;
        end
        else if (not (Typ is TLapeType_SystemUnit)) then
          LapeException(lpeTypeExpected, [Tokenizer]);
      end
      else if (Result.MethodDef = mdProperty) then
        LapeException(lpeExpectedProperty, Tokenizer.DocPos);
    end;

    if (Token = tk_sym_ParenthesisOpen) or ((Token = tk_NULL) and isNext([tk_sym_ParenthesisOpen])) then
    begin
      repeat
        Param := NullParameter;

        case Next() of
          tk_NULL: Break;
          tk_sym_ParenthesisClose:
            begin
              if (Tokenizer.LastTok <> tk_sym_ParenthesisOpen) then
                Expect(tk_sym_SemiColon, False, False);
              Break;
            end;
          tk_kw_Const:    Param.ParType := lptConst;
          tk_kw_ConstRef: Param.ParType := lptConstRef;
          tk_kw_Out:   Param.ParType := lptOut;
          tk_kw_Var:   Param.ParType := lptVar;
        end;

        Identifiers := ParseIdentifierList(Param.ParType <> NullParameter.ParType);
        Expect([tk_sym_Colon, tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
        if (Tokenizer.Tok = tk_sym_Colon) then
        begin
          Param.VarType := ParseType(nil, True);
          Param.VarType._DocPos := Tokenizer.DocPos;
          if (Param.VarType = nil) then
            LapeException(lpeTypeExpected, Tokenizer.DocPos);
          Expect([tk_sym_Equals, tk_sym_SemiColon, tk_sym_ParenthesisClose], True, False);

          if (Tokenizer.Tok = tk_sym_Equals) then
          begin
             if (Result.MethodDef = mdProperty) then
              LapeException(lpeDefaultParamInProperties, Tokenizer.DocPos);
            Default := EnsureExpression(ParseExpression([tk_sym_ParenthesisClose], True, False)).setExpectedType(Param.VarType) as TLapeTree_ExprBase;
            try
              Param.Default := Default.Evaluate();
              if (not (Param.ParType in Lape_ValParams)) and ((Param.Default = nil) or (not Param.Default.Writeable)) then
                LapeException(lpeVariableExpected, Default.DocPos);
            finally
              Default.Free();
            end;
            Expect([tk_sym_SemiColon, tk_sym_ParenthesisClose], False, False);
          end;
        end
        else if (not (Param.ParType in Lape_RefParams)) then
          Expect(tk_sym_Colon, False, False);

        for i := 0 to High(Identifiers) do
        begin
          addVar(Param.ParType, Param.VarType, Identifiers[i]);
          Result.addParam(Param);
        end;
      until (Tokenizer.Tok = tk_sym_ParenthesisClose);
    end
    else if lcoMethodDeclarationParentheses in FOptions then
    begin
      Next();
      LapeException(lpeMethodDeclarationParenthesesExpected, Tokenizer.DocPos);
    end;

    if (Result.MethodDef = mdOperator) then
    begin
      if (Result.Params.Count <> 2) then
        LapeExceptionFmt(lpeInvalidOperator, [op_name[op], 2], Pos);
      ltyp := Result.Params[0].VarType;
      rtyp := Result.Params[1].VarType;
      if ltyp.EvalRes(op, rtyp) <> nil then
        LapeExceptionFmt(lpeCannotOverrideOperator, [op_name[op], ltyp.AsString, rtyp.AsString], Pos);
    end;

    if ExpectResult or ((Result.MethodDef = mdProperty) and (Peek() = tk_sym_Colon)) then
    begin
      Expect(tk_sym_Colon, True, False);
      Result.Res := ParseType(nil);
      Result.Res._DocPos := Tokenizer.DocPos;
      if (Result.Res = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);

      addVar(lptOut, Result.Res, 'Result');
    end;

    Result.Name := Name;
    Result := addManagedType(Result) as TLapeType_Method;

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString; isExternal: Boolean): TLapeTree_Method;
var
  Pos: TDocPos;
  SelfWith: TLapeWithDeclRec;
  OldDeclaration: TLapeDeclaration;
  LocalDecl, ResetStack: Boolean;

  procedure RemoveSelfVar;
  begin
    if (not isExternal) then
    begin
      Assert(FStackInfo.Vars[0].Name = 'Self');
      FStackInfo.Delete(FStackInfo.Vars[0], True);

      // Add reference to the to reference static methods or class variables
      if (FuncHeader is TLapeType_MethodOfType) and TLapeType_MethodOfType(FuncHeader).ObjectType.CanHaveChild() then
        FStackInfo.addDeclaration(
          addManagedDecl(TLapeType_Type.Create(TLapeType_MethodOfType(FuncHeader).ObjectType, Self, 'Self'))
        );
    end;
  end;

  procedure AddSelfVar(Param: ELapeParameterType; VarType: TLapeType);
  begin
    if (not isExternal) then
    begin
      FStackInfo.addSelfVar(Param, VarType);
      FStackInfo.VarStack.MoveItem(FStackInfo.VarStack.Count - 1, 0);
    end;
  end;

  procedure SwapMethodTree(varFrom, varTo: TLapeGlobalVar);
  var
    i: Integer;
  begin
    for i := 0 to FDelayedTree.Statements.Count - 1 do
      if (FDelayedTree.Statements[i] is TLapeTree_Method) and (TLapeTree_Method(FDelayedTree.Statements[I]).Method = varFrom) then
      begin
        TLapeTree_Method(FDelayedTree.Statements[I]).Method := varTo;

        Exit;
      end;
  end;

  procedure InheritVarStack(AMethod: TLapeType_Method; AStackInfo: TLapeStackInfo; Count: Integer; addToScope: Boolean);
  var
    i: Integer;
    Param: TLapeParameter;
    SVar: TLapeStackVar;
  begin
    if (AMethod = nil) or (AStackInfo = nil) or (AStackInfo.VarCount < Count) then
      Exit;

    for i := 0 to Count - 1 do
    begin
      SVar := AStackInfo[i];
      if (SVar is TLapeStackTempVar) or (SVar.Name = '') then
        Continue;

      Param := NullParameter;
      Param.VarType := SVar.VarType;
      Param.Default := SVar;

      if SVar.isConstant then
        Param.ParType := lptConstRef
      else
        Param.ParType := lptVar;

      AMethod.Params.Insert(Param, i);
      Inc(AMethod.ImplicitParams);

      if addToScope then
      begin
        FStackInfo.inheritVar(SVar);
        FStackInfo.VarStack.MoveItem(FStackInfo.VarStack.Count - 1, i);
      end;
    end;
  end;

  function InheritMethodStack(AMethod: TLapeType_Method; AStackInfo: TLapeStackInfo): TLapeType_Method;
  begin
    if (AMethod = nil) or (AStackInfo = nil) or (AStackInfo.VarCount < 1) then
      Exit(AMethod);

    AMethod := TLapeType_Method.Create(AMethod);
    InheritVarStack(AMethod, AStackInfo, AStackInfo.VarCount, True);
    Result := TLapeType_Method(addManagedType(AMethod));
  end;

  procedure InheritMethodDefaults(AVar: TLapeGlobalVar; AMethod: TLapeType_Method);
  var
    i: Integer;
    Param: TLapeParameter;
    NewMethod: TLapeType_Method;
  begin
    if (AMethod = nil) or (FStackInfo = nil) or (AMethod.ParamSize <= 0) or (FStackInfo.TotalParamSize <> AMethod.ParamSize) then
      Exit;

    NewMethod := TLapeType_Method.Create(AMethod);

    if MethodOfObject(AMethod) then
      InheritVarStack(NewMethod, FStackInfo, 1, False);

    for i := 0 to NewMethod.Params.Count - 1 do
    begin
      Param := NewMethod.Params[i];
      Param.Default := FStackInfo[i];
      NewMethod.Params[i] := Param;
    end;

    AVar.VarType := addManagedType(NewMethod);
  end;

  function SetStackOwner(AStackInfo: TLapeStackInfo): TLapeStackInfo;
  begin
    Result := FStackInfo.Owner;
    if (AStackInfo = nil) then
      FStackInfo.Owner := Result.Owner
    else
    begin
      AStackInfo.Owner := FStackInfo.Owner;
      FStackInfo.Owner := AStackInfo;
    end;
  end;


begin
  Result := nil;
  Pos := Tokenizer.DocPos;

  if (FuncHeader = nil) or (FuncName = '') then
    LapeException(lpeBlockExpected, Tokenizer.DocPos)
  else if (FuncHeader is TLapeType_MethodOfType) and (not hasDeclaration(TLapeType_MethodOfType(FuncHeader).ObjectType, FStackInfo.Owner, True, False)) then
    LapeException(lpeParentOutOfScope, Tokenizer.DocPos);

  if (FuncHeader is TLapeType_MethodOfType) then
  begin
    SetStackOwner(TLapeDeclStack.Create(TLapeType_MethodOfType(FuncHeader).ObjectType));
    ResetStack := True;
  end
  else
    ResetStack := False;

  StackInfo.FuncName := GetMethodName(FuncHeader);

  try
    isNext([tk_kw_External, tk_kw_Forward, tk_kw_Overload, tk_kw_Override, tk_kw_Static] + ParserToken_Hints);
    OldDeclaration := getDeclarationNoWith(FuncName, FStackInfo.Owner);
    LocalDecl := (OldDeclaration <> nil) and hasDeclaration(OldDeclaration, FStackInfo.Owner, True, False);

    if (OldDeclaration <> nil) and (FuncHeader is TLapeType_MethodOfType) and (not LocalDecl) and (not TLapeType_MethodOfType(FuncHeader).ObjectType.HasSubDeclaration(OldDeclaration, bTrue)) then
      OldDeclaration := nil;

    if (Tokenizer.Tok = tk_kw_Static) then
    begin
      ParseExpressionEnd(tk_sym_SemiColon, True, False);
      if MethodOfObject(FuncHeader) then
      begin
        RemoveSelfVar();
        FuncHeader := TLapeType_Method(addManagedType(TLapeType_Method.Create(FuncHeader)));
      end;
      isNext([tk_kw_External, tk_kw_Forward, tk_kw_Overload, tk_kw_Override] + ParserToken_Hints);
    end
    else if (not isExternal) and (not MethodOfObject(FuncHeader)) then
      FuncHeader := InheritMethodStack(FuncHeader, FStackInfo.Owner);

    if isExternal then
      Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(FuncHeader.NewGlobalVar(nil), FStackInfo.Owner)), FStackInfo, Self, @Pos)
    else
    begin
      Result := TLapeTree_Method.Create(TLapeGlobalVar(addLocalDecl(FuncHeader.NewGlobalVar(EndJump), FStackInfo.Owner)), FStackInfo, Self, @Pos);

      if (FuncHeader is TLapeType_MethodOfType) then
      begin
        Result.SelfVar := _ResVar.New(FStackInfo.Vars[0]);

        if (not (lcoExplicitSelf in FOptions)) then
        begin
          SelfWith.WithType := TLapeType_MethodOfType(FuncHeader).ObjectType;
          SelfWith.WithVar := @Result.SelfVar;

          FStackInfo.Owner.addWith(SelfWith);
        end;
      end;
    end;

    Result.Method.setReadWrite(False, False);

    try
      if (Tokenizer.Tok = tk_kw_Overload) or ((FuncHeader.MethodDef in [mdOperator, mdProperty]) and (Tokenizer.Tok <> tk_kw_Override)) then
      begin
        if not(FuncHeader.MethodDef in [mdOperator, mdProperty]) then
          ParseExpressionEnd(tk_sym_SemiColon, True, False)
        else if (Tokenizer.Tok = tk_kw_Overload) then
          LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(Tokenizer.Tok)], DocPos);

        if (OldDeclaration = nil) or (not LocalDecl) or ((OldDeclaration is TLapeGlobalVar) and (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_Method)) then
          with TLapeType_OverloadedMethod(addLocalDecl(TLapeType_OverloadedMethod.Create(Self, '', @Pos), FStackInfo.Owner)) do
          begin
            if (OldDeclaration <> nil) then
            begin
              if (not LocalDecl) and (OldDeclaration.DeclarationList <> nil) then
                OldDeclaration := TLapeGlobalVar(OldDeclaration).CreateCopy(False);
              addMethod(OldDeclaration as TLapeGlobalVar);
            end;

            MethodDef := FuncHeader.MethodDef;
            OldDeclaration := addLocalDecl(NewGlobalVar('', @_DocPos), FStackInfo.Owner);
            OldDeclaration.Name := FuncName;
          end
        else if (not (OldDeclaration is TLapeGlobalVar)) or (not (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_OverloadedMethod)) or (TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).getMethod(FuncHeader) <> nil) then
          LapeException(lpeCannotOverload, Tokenizer.DocPos);

        try
          if TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).MethodDef <> FuncHeader.MethodDef then
             LapeExceptionFmt(lpeCannotOverload2, [LapeMethodDefToString(TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).MethodDef), LapeMethodDefToString(FuncHeader.MethodDef)], Tokenizer.DocPos);

          TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).addMethod(Result.Method, not LocalDecl);
        except on E: lpException do
          LapeException(lpString(E.Message), Tokenizer.DocPos);
        end;

        isNext([tk_kw_External, tk_kw_Forward] + ParserToken_Hints);
      end
      else if (Tokenizer.Tok = tk_kw_Override) then
      begin
        ParseExpressionEnd(tk_sym_SemiColon, True, False);

        if (OldDeclaration <> nil) and (OldDeclaration is TLapeGlobalVar) and (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_OverloadedMethod) then
        begin
          if (not LocalDecl) then
            with TLapeType_OverloadedMethod(addLocalDecl(TLapeType_OverloadedMethod.Create(Self, '', @Pos), FStackInfo.Owner)) do
            begin
              addMethod(OldDeclaration as TLapeGlobalVar);
              OldDeclaration := addLocalDecl(NewGlobalVar('', @_DocPos), FStackInfo.Owner);
              OldDeclaration.Name := FuncName;
            end;

          if LocalDecl then
            OldDeclaration := TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).getMethod(FuncHeader)
          else
            OldDeclaration := TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).overrideMethod(Result.Method)
        end;

        if (OldDeclaration = nil) or (not (OldDeclaration is TLapeGlobalVar)) or TLapeGlobalVar(OldDeclaration).Readable or TLapeGlobalVar(OldDeclaration).Writeable or (not (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_Method)) then
          LapeException(lpeUnknownParent, Tokenizer.DocPos);
        if (not TLapeType_Method(TLapeGlobalVar(OldDeclaration).VarType).EqualParams(FuncHeader, False)) then
          LapeException(lpeNoForwardMatch, Tokenizer.DocPos);
        if hasDeclaration('inherited', FStackInfo, True) then
          LapeExceptionFmt(lpeDuplicateDeclaration, ['inherited'], Tokenizer.DocPos);

        if (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_MethodOfObject) and
           (not (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_MethodOfType))
        then
        begin
          Result.Method.VarType := addManagedType(TLapeType_MethodOfObject.Create(Result.Method.VarType as TLapeType_Method));
          AddSelfVar(lptConstRef, getGlobalType('ConstPointer'));
        end;

        if (MethodOfObject(TLapeGlobalVar(OldDeclaration).VarType) <> MethodOfObject(Result.Method.VarType)) then
          LapeException(lpeNoForwardMatch, Tokenizer.DocPos);

        if LocalDecl then
        begin
          if (TLapeType_Method(TLapeGlobalVar(OldDeclaration).VarType).BaseType = ltScriptMethod) then
            SwapMethodTree(TLapeGlobalVar(OldDeclaration), Result.Method);

          TLapeType_Method(Result.Method.VarType).setImported(Result.Method, TLapeType_Method(TLapeGlobalVar(OldDeclaration).VarType).BaseType = ltImportedMethod);
          Move(TLapeGlobalVar(OldDeclaration).Ptr^, Result.Method.Ptr^, FuncHeader.Size);

          Result.Method.Name := 'inherited';
          Result.Method.DeclarationList := nil;
          InheritMethodDefaults(Result.Method, Result.Method.VarType as TLapeType_Method);
          addLocalDecl(Result.Method, FStackInfo);

          Result.Method := OldDeclaration as TLapeGlobalVar;
        end
        else
        begin
          if (OldDeclaration.DeclarationList <> nil) then
          begin
            Result.Method.Name := FuncName;
            OldDeclaration := TLapeGlobalVar(OldDeclaration).CreateCopy(False);
          end;

          OldDeclaration.Name := 'inherited';
          InheritMethodDefaults(OldDeclaration as TLapeGlobalVar, TLapeGlobalVar(OldDeclaration).VarType as TLapeType_Method);
          OldDeclaration := addLocalDecl(OldDeclaration);
        end;

        TLapeType_Method(Result.Method.VarType).setImported(Result.Method, isExternal);
      end
      else
      begin
        OldDeclaration := getDeclarationNoWith(FuncName, FStackInfo.Owner, True);
        if (OldDeclaration <> nil) and (OldDeclaration is TLapeGlobalVar) and (TLapeGlobalVar(OldDeclaration).VarType is TLapeType_OverloadedMethod) then
        begin
          OldDeclaration := TLapeType_OverloadedMethod(TLapeGlobalVar(OldDeclaration).VarType).getMethod(FuncHeader);
          if (OldDeclaration = nil) then
            LapeExceptionFmt(lpeDuplicateDeclaration, [FuncName], Tokenizer.DocPos)
        end;
        if (OldDeclaration <> nil) then
          if (FuncForwards <> nil) and FuncForwards.ExistsItem(OldDeclaration as TLapeGlobalVar) then
          begin
            if (not TLapeGlobalVar(OldDeclaration).VarType.Equals(Result.Method.VarType, False)) then
              LapeException(lpeNoForwardMatch, Tokenizer.DocPos);

            Result.Method.Free();
            Result.Method := TLapeGlobalVar(OldDeclaration);
          end
          else
            LapeExceptionFmt(lpeDuplicateDeclaration, [FuncName], Tokenizer.DocPos);

        Result.Method.Name := FuncName;
      end;

      if (Tokenizer.Tok = tk_kw_External) then
        if ({$IFNDEF FPC}@{$ENDIF}FOnHandleExternal = nil) then
          LapeException(lpeBlockExpected, Tokenizer.DocPos)
        else
        begin
          if (not FOnHandleExternal(Self, Result.Method)) then
            LapeException(lpeUnknownParent, Tokenizer.DocPos);

          ParseExpressionEnd(tk_sym_SemiColon, False, True);

          Result.FreeStackInfo := False;
          FreeAndNil(Result);
          Exit;
        end;

      if (Tokenizer.Tok = tk_kw_Forward) then
      begin
        ParseExpressionEnd(tk_sym_SemiColon, True, True);
        if (FuncForwards = nil) then
          LapeException(lpeBlockExpected, Tokenizer.DocPos)
        else if FuncForwards.ExistsItem(Result.Method) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [FuncName], Tokenizer.DocPos)
        else
          FuncForwards.Add(Result.Method);

        Result.FreeStackInfo := False;
        FreeAndNil(Result);
        Exit;
      end;

      while (Tokenizer.Tok in ParserToken_Hints) do
      begin
        ParseDeclHint(Result.Method.VarType);
        if (Peek() in ParserToken_Hints) then
          Next();
      end;

      if isExternal then
        Exit;

      if (FuncForwards <> nil) and (OldDeclaration is TLapeGlobalVar) then
        FuncForwards.DeleteItem(TLapeGlobalVar(OldDeclaration));

      if (lcoExplicitSelf in FOptions) and ResetStack then
      begin
        SetStackOwner(nil).Free();

        ResetStack := False;
      end;

      Next();
      Result.Statements := ParseBlockList();

      if (Result.Statements = nil) or (not (Result.Statements.Statements[Result.Statements.Statements.Count - 1] is TLapeTree_StatementList)) then
        Expect(tk_kw_Begin, False, False);
    except
      Result.FreeStackInfo := False;
      FreeAndNil(Result);
      raise;
    end;

  finally
    if ResetStack then
      SetStackOwner(nil).Free();
  end;
end;

function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; FuncHeader: TLapeType_Method; FuncName: lpString): TLapeTree_Method;
var
  i: Integer;
begin
  Result := nil;
  IncStackInfo();

  try
    if (FuncHeader <> nil) then
    begin
      if (FuncHeader is TLapeType_MethodOfType) then
        FStackInfo.addSelfVar(TLapeType_MethodOfType(FuncHeader).SelfParam, TLapeType_MethodOfType(FuncHeader).ObjectType)
      else if (FuncHeader is TLapeType_MethodOfObject) then
        FStackInfo.addSelfVar(lptConstRef, getGlobalType('ConstPointer'));
      for i := 0 to FuncHeader.Params.Count - 1 do
        FStackInfo.addVar(FuncHeader.Params[i].ParType, FuncHeader.Params[i].VarType, lpString('Param'+IntToStr(i)));
      if (FuncHeader.Res <> nil) then
        FStackInfo.addVar(lptOut, FuncHeader.Res, 'Result');
    end;
    Result := ParseMethod(FuncForwards, FuncHeader, FuncName, False);
  finally
    DecStackInfo(True, False, (Result = nil));
  end;
end;

function TLapeCompiler.ParseMethod(FuncForwards: TLapeFuncForwards; isExternal: Boolean = False): TLapeTree_Method;
var
  FuncHeader: TLapeType_Method;
  FuncName: lpString;
begin
  Result := nil;
  IncStackInfo();

  try
    FuncHeader := ParseMethodHeader(FuncName, not isExternal);
    ParseExpressionEnd(tk_sym_SemiColon, True, False);
    Result := ParseMethod(FuncForwards, FuncHeader, FuncName, isExternal);
  finally
    DecStackInfo(True, False, (Result = nil));
  end;
end;

function TLapeCompiler.ParseType(TypeForwards: TLapeTypeForwards; addToStackOwner: Boolean = False): TLapeType;

  procedure ParseArray;
  var
    TypeExpr: TLapeTree_Base;
    Range: TLapeRange;
    DocPos: TDocPos;
  begin
    DocPos := Tokenizer.DocPos;

    Expect([tk_sym_BracketOpen, tk_kw_Of], True, False);
    if (Tokenizer.Tok = tk_sym_BracketOpen) then
    begin
      TypeExpr := ParseTypeExpression();
      try
        Range := EnsureConstantRange(TypeExpr);
        Expect(tk_sym_BracketClose, False, False);
        Expect(tk_kw_Of, True, False);
        Result := addManagedType(TLapeType_StaticArray.Create(Range, ParseType(nil, addToStackOwner), Self, '', @DocPos));
      finally
        if (TypeExpr <> nil) then
          TypeExpr.Free();
      end;
    end
    else
      Result := addManagedType(TLapeType_DynArray.Create(ParseType(nil, addToStackOwner), Self, '', @DocPos));
  end;

  procedure ParseFields(IsPacked: Boolean);
  var
    i: Integer;
    FieldType: TLapeType;
    Identifiers: TStringArray;
    Expression: TLapeTree_ExprBase;
    FieldValue: TLapeGlobalVar;
  begin
    if (Tokenizer.Tok <> tk_kw_End) then
    repeat
      if (Tokenizer.Tok = tk_kw_Const) then
      begin
        Expression := nil;
        FieldValue := nil;
        FieldType := nil;

        Identifiers := ParseIdentifierList(True);
        if Result.HasChild(Identifiers[0]) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[0]], DocPos);

        if Expect([tk_sym_Colon, tk_sym_Equals], False, False) = tk_sym_Colon then
          FieldType := ParseType(nil);

        if Expect([tk_sym_SemiColon, tk_sym_Equals], FieldType <> nil, False) = tk_sym_Equals then
        begin
          Expression := EnsureExpression(ParseExpression([tk_sym_SemiColon], True, False)).setExpectedType(FieldType) as TLapeTree_ExprBase;
          if (Expression <> nil) and (not Expression.isConstant()) then
            LapeException(lpeConstantExpected, Expression.DocPos);

          FieldValue := Expression.Evaluate();
          if (FieldType = nil) then
            FieldType := FieldValue.VarType;
          Expression.Free();
        end;

        if (Result is TLapeType_Record) then
          TLapeType_Record(Result).addConstField(FieldType, FieldValue, Identifiers[0])
        else if (Result is TLapeType_Object) then
          TLapeType_Object(Result).addConstField(FieldType, FieldValue, Identifiers[0]);
      end else
      begin
        Identifiers := ParseIdentifierList();
        Expect(tk_sym_Colon, False, False);
        FieldType := ParseType(nil, addToStackOwner);
        Expect(tk_sym_SemiColon, True, False);

        for i := 0 to High(Identifiers) do
        begin
          if Result.HasChild(Identifiers[i]) then
            LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], DocPos);

          if (Result is TLapeType_Record) then
          begin
            if IsPacked then
              TLapeType_Record(Result).addField(FieldType, Identifiers[i], 1)
            else
              TLapeType_Record(Result).addField(FieldType, Identifiers[i], FOptions_PackRecords);
          end else if (Result is TLapeType_Object) then
            TLapeType_Object(Result).addField(FieldType, Identifiers[i]);
        end;
      end;
      Expect(tk_sym_SemiColon, False, True);
    until (Tokenizer.Tok in [tk_NULL, tk_kw_End]);
  end;

  procedure ParseRecord(IsPacked: Boolean = False);
  var
    IsRecord: Boolean;
    FieldType: TLapeType;
  begin
    IsRecord := Tokenizer.Tok = tk_kw_Record;

    Next();

    if (lcoInheritableRecords in FOptions) and (Tokenizer.Tok = tk_sym_ParenthesisOpen) then
    begin
      FieldType := ParseType(nil, addToStackOwner);
      if (not (FieldType is TLapeType_Record)) or (IsRecord <> (FieldType.BaseType = ltRecord)) then
        LapeException(lpeUnknownParent, Tokenizer.DocPos);
      Tokenizer.Expect(tk_sym_ParenthesisClose, True, False);
      Result := FieldType.CreateCopy(True) as TLapeType_Record;
      if (Peek() = tk_sym_SemiColon) then
      begin
        Result := addManagedType(Result);
        Exit;
      end else
        Next();
    end
    else if IsRecord then
      Result := TLapeType_Record.Create(Self, nil, '', getPDocPos())
    else
      Result := TLapeType_Union.Create(Self, nil, '', getPDocPos());

    ParseFields(IsPacked);

    Result := addManagedType(Result);
  end;

  procedure ParseObject;
  begin
    Result := TLapeType_Object.Create(Self, nil, '', getPDocPos());
    SetUniqueTypeID(Result); // important for Dispose
    Next();
    ParseFields(True);
    Result := addManagedType(Result);
  end;

  procedure ParseSet;
  var
    SetType: TLapeType;
  begin
    Expect(tk_kw_Of, True, False);
    SetType := EnsureRange(ParseType(nil, addToStackOwner));
    if (not (SetType is TLapeType_SubRange)) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);

    try
      Result := addManagedType(TLapeType_Set.Create(TLapeType_SubRange(SetType), Self, '', getPDocPos));
    except on E: lpException do
      LapeException(lpString(E.Message), Tokenizer.DocPos);
    end;
  end;

  procedure ParsePointer;
  var
    PointerType: TLapeType;
    ConstPointer: Boolean;
    DocPos: TDocPos;
  begin
    DocPos := Tokenizer.DocPos;

    if (Tokenizer.Tok = tk_kw_Strict) then
    begin
      Expect(tk_Identifier);
      PointerType := TLapeType(getDeclarationNoWith(Tokenizer.TokString));
      if not (PointerType is TLapeType_Pointer) then
        LapeException(lpeExpectedPointerType, DocPos);

      if (PointerType.ClassType = TLapeType_Pointer) then
        Result := addManagedType(TLapeType_StrictPointer.Create(Self, nil, False, '', @DocPos))
      else
        Result := addManagedType(PointerType.CreateCopy(True));

      Exit;
    end;

    Expect([tk_kw_Const, tk_Identifier], True, False);

    if (Tokenizer.Tok = tk_kw_Const) then
    begin
      ConstPointer := True;
      Expect(tk_Identifier, True, False);
    end
    else
      ConstPointer := False;

    PointerType := TLapeType(getDeclarationNoWith(Tokenizer.TokString));
    if ((PointerType = nil) and (TypeForwards = nil)) or ((PointerType <> nil) and (not (PointerType is TLapeType))) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    if (PointerType <> nil) then
      Result := addManagedType(TLapeType_Pointer.Create(Self, PointerType, ConstPointer, '', @DocPos))
    else
    begin
      Result := TLapeType_Pointer.Create(Self, PointerType, ConstPointer, '', @DocPos);
      TypeForwards.Add(Tokenizer.TokString, Result);
    end;
  end;

  procedure ParseEnum;
  var
    Enum: TLapeType_Enum absolute Result;
    Name: lpString;
    Member: TLapeTree_ExprBase;
    Default: TLapeGlobalVar;
    StackOwner: TLapeStackInfo;
    Val: Int64;
    Scoped: Boolean;
  begin
    Scoped := (lcoScopedEnums in FOptions) or (Tokenizer.Tok = tk_kw_Enum);
    if Scoped and (Tokenizer.Tok = tk_kw_Enum) then
      Expect(tk_sym_ParenthesisOpen, True, False);

    Enum := TLapeType_Enum.Create(Self, nil, Scoped, '', getPDocPos());
    if (FStackInfo = nil) or (not addToStackOwner) then
      StackOwner := FStackInfo
    else
      StackOwner := FStackInfo.Owner;

    if Scoped then
      Result := addManagedType(Enum)
    else
      Result := addManagedDecl(Enum) as TLapeType;

    repeat
      Expect(tk_Identifier, True, False);
      Name := Tokenizer.TokString;
      if (Scoped and Enum.hasMember(Name)) or
         ((not Scoped) and hasDeclaration(Name, StackOwner, True))
      then
      begin
        Result := nil; // Do not free type, because enum members rely on it

        LapeExceptionFmt(lpeDuplicateDeclaration, [Name], Tokenizer.DocPos);
      end;

      Expect([tk_sym_Comma, tk_sym_ParenthesisClose, tk_sym_Equals], True, False);
      if (Tokenizer.Tok = tk_cmp_Equal) then
      try
        Member := ParseExpression([tk_sym_Comma, tk_sym_ParenthesisClose], True, False);
        try
          if (Member <> nil) then
            Default := Member.Evaluate()
          else
            Default := nil;

          if (Default = nil) or (not Default.HasType()) or (not Default.VarType.IsOrdinal()) or (not Default.Readable) then
            LapeException(lpeExpressionExpected, Tokenizer.DocPos);
          Val := Enum.addMember(Default.AsInteger, Name);
        finally
          Member.Free();
        end;
      except
        on E: lpException do
        begin
          Result := nil; // Do not free type, because enum members rely on it

          LapeException(lpString(E.Message), Tokenizer.DocPos);
        end
      end
      else
        Val := Enum.addMember(Name);

      if (not Scoped) then
        TLapeGlobalVar(addLocalDecl(Enum.NewGlobalVar(Val, Name), StackOwner)).isConstant := True;

    until (Tokenizer.Tok in [tk_NULL, tk_sym_ParenthesisClose]);
  end;

  procedure ParseMethodType;
  var
    BaseType: ELapeBaseType;
    Name: lpString;
  begin
    BaseType := ltPointer;
    if (Tokenizer.Tok in [tk_kw_Property, tk_kw_Operator]) then
       LapeException(lpeTypeExpected);
    if (Tokenizer.Tok in [tk_kw_External, {tk_kw_Export,} tk_kw_Private]) then
    begin
      if (Tokenizer.Tok = tk_kw_External) then
        BaseType := ltImportedMethod
      //else if (Tokenizer.Tok = tk_kw_Export) then
      else if (Tokenizer.Tok = tk_kw_Private) then
        BaseType := ltScriptMethod;

      Expect([tk_kw_Function, tk_kw_Procedure{, tk_kw_Property, tk_kw_Operator}], True, False);
    end;

    Result := ParseMethodHeader(Name, False);
    if (Name <> '') or (Result = nil) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    Result := Result.CreateCopy();
    Result.BaseType := BaseType;
    Result := addManagedType(Result);

    if isNext([tk_kw_Of]) then
    begin
      Expect(tk_kw_Object, True, False);
      Result := addManagedType(TLapeType_MethodOfObject.Create(Result as TLapeType_Method));
    end;
  end;

  procedure ParseTypeType;
  var
    TypeExpr: TLapeTree_Base;
  begin
    TypeExpr := ParseTypeExpression([tk_sym_Equals, tk_op_Assign, tk_sym_ParenthesisClose, tk_sym_SemiColon]);
    try
      if (TypeExpr <> nil) and (TypeExpr is TLapeTree_VarType) and (TLapeTree_VarType(TypeExpr).VarType <> nil) then
      begin
        Result := TLapeTree_VarType(TypeExpr).VarType.CreateCopy(True);
        SetUniqueTypeID(Result);
        addManagedType(Result);
      end
      else
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
    finally
      if (TypeExpr <> nil) then
      begin
        Tokenizer.tempRollBack();
        TypeExpr.Free();
      end;
    end;
  end;

  procedure ParseDef;
  var
    TypeExpr: TLapeTree_Base;
    Range: TLapeRange;
    RangeType: TLapeType;
  begin
    TypeExpr := ParseTypeExpression([tk_sym_Equals, tk_op_Assign, tk_op_In, tk_sym_ParenthesisClose, tk_sym_SemiColon], False);
    try
      if (TypeExpr <> nil) and (TypeExpr is TLapeTree_Range) then
      begin
        Range := EnsureConstantRange(TypeExpr, RangeType);
        Result := addManagedType(TLapeType_SubRange.Create(Range, Self, RangeType, '', getPDocPos()))
      end
      else if (TypeExpr <> nil) and (TypeExpr is TLapeTree_Operator) and (TLapeTree_Operator(TypeExpr).OperatorType = op_Index) then
        with TLapeTree_Operator(TypeExpr) do
        begin
          Range.Hi := -1;
          try
            if (Right <> nil) and (Right is TLapeTree_ExprBase) then
              if TLapeTree_ExprBase(Right).isConstant() then
                Range.Hi := TLapeTree_ExprBase(Right).Evaluate().AsInteger
              else
                LapeException(lpeConstantExpected, Right.DocPos);
          finally
            if (Range.Hi < 0) or (Range.Hi > High(UInt8)) then
              LapeException(lpeInvalidRange, [Right, Self]);
          end;

          if (Left <> nil) and (Left is TLapeTree_VarType) and
            (TLapeTree_VarType(Left).VarType <> nil) and
            (TLapeTree_VarType(Left).VarType.BaseType = ltString)
          then
            Result := addManagedType(TLapeType_ShortString.Create(Self, Range.Hi, '', @Left._DocPos))
          else
            LapeException(lpeOutOfTypeRange, Tokenizer.DocPos);
        end
      else if (TypeExpr <> nil) and (TypeExpr is TLapeTree_InternalMethod_Label) then
        Result := getGlobalType('!label')
      else if (TypeExpr <> nil) and (TypeExpr is TLapeTree_VarType) and (TLapeTree_VarType(TypeExpr).VarType <> nil) then
        Result := TLapeTree_VarType(TypeExpr).VarType
      else
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
    finally
      if (TypeExpr <> nil) then
      begin
        Tokenizer.tempRollBack();
        TypeExpr.Free();
      end;
    end;
  end;

begin
  Result := nil;
  try

    case Next() of
      tk_kw_Array: ParseArray();
      tk_kw_Record, tk_kw_Union: ParseRecord();
      tk_kw_Object: ParseObject();
      tk_kw_Set: ParseSet();
      tk_kw_Packed:
        begin
          Expect(tk_kw_Record, True, False);
          ParseRecord(True);
        end;
      tk_sym_Caret, tk_kw_Strict: ParsePointer();
      tk_kw_Enum, tk_sym_ParenthesisOpen: ParseEnum();
      tk_kw_Function, tk_kw_Procedure, tk_kw_Operator, tk_kw_Property, tk_kw_External, tk_kw_Private: ParseMethodType();
      tk_kw_Type: ParseTypeType();
      else ParseDef();
    end;

    if (Result <> nil) then
      if (TypeForwards = nil) and (lcoHints in FOptions) and Result.HasHints() then
        Result.WriteHints({$IFDEF FPC}@{$ENDIF}Hint, Tokenizer.DocPos)
      else
      // TypeForwards<>nil means ParseTypeBlock
      // type TPoint = record X,Y: Integer deprecated;
      if (TypeForwards <> nil) and (Peek() in ParserToken_Hints) then
        ParseDeclHint(Result, True, False);
  except
    if (Result <> nil) then
      FreeAndNil(Result);
    raise;
  end;
end;

type
  __LapeType_Pointer = class(TLapeType_Pointer);
procedure TLapeCompiler.ParseTypeBlock;
var
  TypeForwards: TLapeTypeForwards;
  Typ: TLapeType;
  Name: lpString;
begin
  TypeForwards := TLapeTypeForwards.Create(nil, dupAccept, False);
  try

    Next();
    repeat
      Expect(tk_Identifier, False, False);
      Name := Tokenizer.TokString;
      if hasDeclaration(Name, True) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [Name], Tokenizer.DocPos);
      Expect(tk_sym_Equals, True, False);

      Typ := ParseType(TypeForwards, False);
      if ((Typ.DeclarationList <> nil) or (Typ.Name <> '')) and (not TypeForwards.ExistsItem(Typ)) then
        Typ := Typ.CreateCopy();
      Typ.Name := Name;
      addLocalDecl(Typ);

      ParseExpressionEnd(tk_sym_SemiColon, True, False);
    until (Next() <> tk_Identifier);

    while (TypeForwards.Count > 0) do
      with __LapeType_Pointer(TypeForwards.ItemsI[0]) do
      begin
        FPType := TLapeType(getDeclarationNoWith(TypeForwards.Key[0]));
        if (not HasType()) then
          LapeExceptionFmt(lpeInvalidForward, [TypeForwards.Key[0]], DocPos);
        TypeForwards.Delete(0);
      end;
  finally
    while (TypeForwards.Count > 0) do
      TypeForwards.Delete(0).Free();
    TypeForwards.Free();
  end;
end;

procedure TLapeCompiler.ParseLabelBlock;
var
  i: Integer;
  Identifiers: TStringArray;
  VarType: TLapeType;
begin
  Identifiers := ParseIdentifierList(True);
  ParseExpressionEnd(tk_sym_SemiColon, False, True);

  VarType := getGlobalType('!label');
  for i := 0 to High(Identifiers) do
    if hasDeclaration(Identifiers[i], True) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], Tokenizer.DocPos)
    else
      addLocalDecl(VarType.NewGlobalVarP(nil, Identifiers[i]));
end;

function TLapeCompiler.ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList;
var
  i: Integer;
  isConst, wasShortCircuit: Boolean;
  Identifiers: TStringArray;
  VarType: TLapeType;
  DefExpr: TLapeTree_ExprBase;
  DefConst: TLapeGlobalVar;
  VarDecl: TLapeVarDecl;
begin
  ValidEnd := ValidEnd + [tk_kw_Deprecated, tk_kw_UnImplemented, tk_kw_Experimental];

  Result := TLapeTree_VarList.Create(Self, getPDocPos());
  try
    isConst := (Tokenizer.Tok = tk_kw_Const);
    wasShortCircuit := isConst and (lcoShortCircuit in FOptions);
    if wasShortCircuit then
      FOptions := FOptions - [lcoShortCircuit];

    Next();
    repeat
      VarType := nil;
      DefExpr := nil;
      DefConst := nil;

      Identifiers := ParseIdentifierList();
      Expect([tk_sym_Colon, tk_op_Assign, tk_sym_Equals], False, False);

      if (Tokenizer.Tok = tk_sym_Colon) then
      begin
        VarType := ParseType(nil);
        if isConst then
          Expect([tk_op_Assign, tk_sym_Equals], True, False)
        else
          ParseExpressionEnd([tk_op_Assign, tk_sym_Equals] + ValidEnd, True, False);
      end;

      if (Tokenizer.Tok = tk_sym_Equals) then
      begin
        DefExpr := EnsureExpression(ParseExpression(ValidEnd, True, False)).setExpectedType(VarType) as TLapeTree_ExprBase;
        if (DefExpr <> nil) and (not DefExpr.isConstant()) then
          LapeException(lpeConstantExpected, DefExpr.DocPos);

        try
          Expect(ValidEnd, False, False);
          DefConst := DefExpr.Evaluate();
        finally
          if (DefExpr <> nil) then
            FreeAndNil(DefExpr);
        end;
      end
      else if (Tokenizer.Tok = tk_op_Assign) then
      begin
        if (Length(Identifiers) <> 1) then
          LapeException(lpeDefaultToMoreThanOne, Tokenizer.DocPos);

        DefExpr := ParseExpression();
        Expect(ValidEnd, False, False);
      end;

      if (VarType = nil) then
        if (DefConst <> nil) then
          VarType := DefConst.VarType
        else if (DefExpr <> nil) then
          VarType := DefExpr.resType()
        else
          LapeException(lpeCannotAssign, Tokenizer.DocPos);
      if (VarType = nil) then
        LapeException(lpeTypeExpected, Tokenizer.DocPos);

      for i := 0 to High(Identifiers) do
      begin
        if hasDeclaration(Identifiers[i], True) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], Tokenizer.DocPos);

        if isConst or VarType.IsStatic then
          VarDecl.VarDecl := TLapeVar(addLocalDecl(VarType.NewGlobalVarP(nil, Identifiers[i])))
        else
          VarDecl.VarDecl := addLocalVar(VarType, Identifiers[i]);

        VarDecl.Default := nil;
        VarDecl.VarDecl._DocPos := Tokenizer.DocPos;
        if (lcoHints in FOptions) then
          VarDecl.VarDecl.Used := duFalse;
        if (DefConst <> nil) then
          if (not (VarDecl.VarDecl is TLapeGlobalVar)) or (VarType is TLapeType_Method) then
            VarDecl.Default := TLapeTree_GlobalVar.Create(DefConst, Self, GetPDocPos())
          else
            VarDecl.VarDecl.VarType.EvalConst(op_Assign, TLapeGlobalVar(VarDecl.VarDecl), DefConst, [])
        else if (DefExpr <> nil) then
        begin
          VarDecl.Default := DefExpr;
          DefExpr := nil;
        end;

        if (VarDecl.VarDecl is TLapeVar) then
          TLapeVar(VarDecl.VarDecl).setReadWrite(isConst and (DefConst <> nil), not isConst);

        // var a: Integer deprecated
        if (Tokenizer.Tok in ParserToken_Hints) then
          ParseDeclHint(VarDecl.VarDecl);

        Result.addVar(VarDecl);
      end;
    until (Next() <> tk_Identifier) or OneOnly;

    if wasShortCircuit then
      FOptions := FOptions + [lcoShortCircuit];
  except
    Result.Free();
    if (DefExpr <> nil) then
      DefExpr.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseExpression(ReturnOn: EParserTokenSet; FirstNext: Boolean; DoFold: Boolean): TLapeTree_ExprBase;
const
  ParenthesisOpen = Pointer(-1);
var
  VarStack: TLapeTree_NodeStack;
  OpStack: TLapeTree_OpStack;
  Precedence: Byte;
  _LastNode: (_None, _Var, _Op);
  DoNext: Boolean;

  procedure PopOpNode;
  var
    OpNode: TLapeTree_Operator;
    DocPos: TDocPos;
  begin
    OpNode := OpStack.Pop();
    try
      with OpNode do
      begin
        if (not (OperatorType in UnaryOperators)) then
          Right := VarStack.Pop();
        Left := VarStack.Pop();
      end;
      VarStack.Push(OpNode);
    except
      DocPos := OpNode._DocPos;
      OpNode.Free();
      LapeException(lpeInvalidEvaluation, DocPos);
    end;
  end;

  procedure PopOpStack(op: EOperator = op_Unknown);
  var
    Associative: ShortInt;
  begin
    if (OpStack.Cur >= 0) and (op <> op_Unknown) then
    begin
      case OperatorAssociative[op] of
        assocLeft: Associative := 0;
        assocRight: Associative := 1;
        assocNone: LapeException(lpeImpossible, Tokenizer.DocPos);
      end;
      Precedence := OperatorPrecedence[op];

      while (OpStack.Cur >= 0) and (OpStack.Top <> TLapeTree_Operator(ParenthesisOpen)) do
        if (Precedence >= OperatorPrecedence[OpStack.Top.OperatorType] + Associative) then
          PopOpNode()
        else
          Break;
    end;
  end;

  function PushVarStack(Item: TLapeTree_ExprBase): Integer;
  begin
    try
      if (_LastNode = _Var) then
        LapeException(lpeOperatorExpected, Tokenizer.DocPos)
      else
        _LastNode := _Var;
      Result := VarStack.Push(Item);
    except
      Item.Free();
      raise;
    end;
  end;

  function PushOpStack(Item: TLapeTree_Operator): Integer;
  begin
    try
      if (Item = TLapeTree_Operator(ParenthesisOpen)) or (Item.OperatorType in AssignOperators) then
        _LastNode := _None
      else if (_LastNode <> _Var) and (not (Item.OperatorType in UnaryOperators)) then
        LapeException(lpeExpressionExpected, Tokenizer.DocPos)
      else if (Item.OperatorType = op_Deref) then
        _LastNode := _Var
      else
        _LastNode := _Op;
      Result := OpStack.Push(Item);
    except
      if (Item <> TLapeTree_Operator(ParenthesisOpen)) then
        Item.Free();
      raise;
    end;
  end;

  function getString: lpString;
  begin
    Result := Tokenizer.TokString;
    if (Length(Result) > 1) then
    begin
      Delete(Result, 1, 1);
      Delete(Result, Length(Result), 1);
    end;
  end;

  procedure ParseAndPushString(ForceString: Boolean = False);
  var
    Str: lpString;
    DocPos: TDocPos;
    Token: EParserToken;
  begin
    DocPos := Tokenizer.DocPos;
    case Tokenizer.Tok of
      tk_typ_String, tk_typ_HereString: Str := getString();
      tk_typ_Char: Str := lpString(Tokenizer.TokChar);
      else
        LapeException(lpeImpossible);
    end;

    while isNext([tk_typ_String, tk_typ_HereString, tk_typ_Char], Token) do
    begin
      case Token of
        tk_typ_String:
          if (Tokenizer.LastTok = tk_typ_String) then
            Str := Str + #39 + getString()
          else
            Str := Str + getString();
        tk_typ_HereString:
          if (Tokenizer.LastTok = tk_typ_HereString) then
            Str := Str + #34 + getString()
          else
            Str := Str + getString();
        tk_typ_Char:
          Str := Str + lpString(Tokenizer.TokChar);
        else
          LapeException(lpeImpossible);
      end;
      ForceString := True;
    end;

    if (Length(Str) = 1) and (not ForceString) then
      PushVarStack(TLapeTree_Char.Create(WideChar(Str[1]), Self, @DocPos))
    else
      PushVarStack(TLapeTree_String.Create(Str, Self, @DocPos));
  end;

  procedure ParseAndPushArray;
  var
    OpenArr: TLapeTree_OpenArray;
  begin
    OpenArr := TLapeTree_OpenArray.Create(Self, getPDocPos());
    try
      if (Next() <> tk_sym_BracketClose) then
      begin
        OpenArr.addValue(EnsureTypeExpression(ParseTypeExpression([tk_sym_BracketClose, tk_sym_Comma], False)));
        while True do
          case Tokenizer.Tok of
            tk_sym_BracketClose: Break;
            tk_sym_Comma: OpenArr.addValue(EnsureTypeExpression(ParseTypeExpression([tk_sym_BracketClose, tk_sym_Comma])));
            else Expect(tk_sym_BracketClose, False, False);
          end;
      end;
    except
      OpenArr.Free();
      raise;
    end;
    PushVarStack(OpenArr);
  end;

  procedure ParseOperator(op: EOperator = op_Unknown);
  begin
    if (op = op_Unknown) and (_LastNode = _Var) then
      if (Tokenizer.Tok = tk_sym_ParenthesisOpen) then
      begin
        PushOpStack(TLapeTree_Operator(ParenthesisOpen));
        Exit;
      end;
    if (op = op_Unknown) then
      op := ParserTokenToOperator(Tokenizer.Tok);

    if (op = op_Index) and (_LastNode <> _Var) then
    begin
      ParseAndPushArray();
      Exit;
    end;

    //Unary minus and double negation
    if (op in [op_Minus, op_Plus]) then
      if (_LastNode = _None) or
           ((Tokenizer.LastTok in ParserToken_Operators) and
           (not (ParserTokenToOperator(Tokenizer.LastTok) in UnaryOperators)) and
           (OperatorPrecedence[ParserTokenToOperator(Tokenizer.LastTok)] <> OperatorPrecedence[op]))
      then
        if (op = op_Minus) then
          op := op_UnaryMinus
        else
          Exit
      else if (op = op_Minus) and (Tokenizer.LastTok in [tk_op_Plus, tk_op_Minus]) then
      begin
        case opStack.Top.OperatorType of
          op_Plus: opStack.Top.OperatorType := op_Minus;
          op_Minus: opStack.Top.OperatorType := op_Plus;
          op_UnaryPlus: opStack.Top.OperatorType := op_UnaryMinus;
          op_UnaryMinus: opStack.Top.OperatorType := op_UnaryPlus;
        end;
        Exit;
      end;

    if (op = op_UnaryPlus) then
      Exit;

    if (_LastNode <> _Var) and (not (op in UnaryOperators)) then
      LapeException(lpeExpressionExpected, Tokenizer.DocPos);

    PopOpStack(op);
    PushOpStack(TLapeTree_Operator.Create(op, Self, getPDocPos()));

    if (op = op_Dot) then
    begin
      if (Next() <> tk_Identifier) then
        LapeExceptionFmt(lpeExpected, [LapeTokenToString(tk_Identifier)], Tokenizer.DocPos);
      PushVarStack(TLapeTree_Field.Create(Tokenizer.TokString, Self, getPDocPos()));
    end
    else if (op = op_Index) then
    begin
      PushVarStack(ParseExpression());
      if (Tokenizer.Tok = tk_sym_Comma) then
        ParseOperator(op_Index)
      else
        Expect(tk_sym_BracketClose, False, False);
    end;
  end;

  function ResolveMethods(Node: TLapeTree_Base; SkipTop: Boolean): TLapeTree_Base;

    function Resolve(Node: TLapeTree_Base; Top, Recurse: Boolean; out HasChanged: Boolean): TLapeTree_Base;

      function ResolveProperty(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
      var
        Changed: Boolean;
      begin
        if IsProperty(Node.resType()) then
        begin
          Result := TLapeTree_InvokeProperty.Create(Node, Node);
          TLapeTree_InvokeProperty(Result).PropertyType := ptRead;
        end else
        begin
          Result := TLapeTree_InvokeProperty.Create(TLapeTree_Operator(Node).Left, Node);
          if (TLapeTree_Operator(Node).Right is TLapeTree_Operator) and (TLapeTree_Operator(TLapeTree_Operator(Node).Right).OperatorType = op_Addr) then
            TLapeTree_InvokeProperty(Result).addParam(Resolve(TLapeTree_Operator(Node).Right, True,True, Changed) as TLapeTree_ExprBase)
          else
            TLapeTree_InvokeProperty(Result).addParam(TLapeTree_Operator(Node).Right);
          TLapeTree_InvokeProperty(Result).PropertyType := ptWrite;
          TLapeTree_InvokeProperty(Result).AssignOp := TLapeTree_Operator(Node).OperatorType;

          Node.Free();
        end;
      end;

      function ResolveMethod(Node: TLapeTree_ExprBase): TLapeTree_ExprBase;
      var
        Op: EOperator;
      begin
        Node := Node.FoldConstants() as TLapeTree_ExprBase;

        if (Node is TLapeTree_Operator) then
          Op := TLapeTree_Operator(Node).OperatorType
        else
          Op := op_Unknown;

        if IsProperty(Node.resType) or ((Op in AssignOperators) and IsProperty(TLapeTree_Operator(Node).Left.resType())) then
          Result := ResolveProperty(Node)
        else if (lcoAutoInvoke in Node.CompilerOptions) and (not (Op in AssignOperators)) and IsMethod(Node.resType()) then
          Result := TLapeTree_Invoke.Create(Node, Node)
        else if (Op = op_Addr) and IsMethod(TLapeTree_Operator(Node).Left.resType()) then
        begin
          Result := TLapeTree_Operator(Node).Left;
          Result.Parent := nil;
          Node.Free();
        end else
          Result := Node;
      end;

    var
      LeftChanged, RightChanged: Boolean;
    begin
      Result := Node;
      HasChanged := False;

      if TLapeTree_Base.isEmpty(Node) or (not (Node is TLapeTree_ExprBase)) or (Node is TLapeTree_Invoke) then
        Exit;

      if Top then
        Result := ResolveMethod(TLapeTree_ExprBase(Node))
      else
        Result := Node;

      if (Result <> Node) then
        HasChanged := Recurse
      else if (Node is TLapeTree_Operator) then
        with TLapeTree_Operator(Node) do
        begin
          if (not (OperatorType in AssignOperators)) or (not IsMethod(Left.resType())) then
            Left := TLapeTree_ExprBase(Resolve(Left, True, not (OperatorType in [op_Addr, op_Assign]), LeftChanged))
          else
            LeftChanged := False;
          Right := TLapeTree_ExprBase(Resolve(Right, True, True, RightChanged));

          HasChanged := LeftChanged or RightChanged;
          if HasChanged and Recurse then
            Result := ResolveMethod(TLapeTree_ExprBase(Node));
        end;
    end;

  var
    HasChanged: Boolean;
  begin
    Result := Resolve(Node, not SkipTop, not SkipTop, HasChanged);
  end;

  // return false = need parseOperator
  function ParseIndex: Boolean;
  var
    Expr: TLapeTree_ExprBase;
    IndexableProp: Boolean;
    PropInvoke: TLapeTree_InvokeProperty;
  begin
    Result := False;

    if (_LastNode = _Var) then
    begin
      PopOpStack(op_Index);

      VarStack.Top := TLapeTree_ExprBase(VarStack.Top.FoldConstants());
      Expr := ResolveMethods(VarStack.Top, True) as TLapeTree_ExprBase;
      if IsProperty(Expr.resType(), IndexableProp) then
      begin
        PropInvoke := TLapeTree_InvokeProperty.Create(Expr, Self, getPDocPos());

        if IndexableProp then
        begin
          if (Next() <> tk_sym_BracketClose) then
          begin
            PropInvoke.addParam(EnsureExpression(ParseExpression([tk_sym_BracketClose, tk_sym_Comma], False)));
            while True do
              case Tokenizer.Tok of
                tk_sym_BracketClose: Break;
                tk_sym_Comma:        PropInvoke.addParam(EnsureExpression(ParseExpression([tk_sym_BracketClose, tk_sym_Comma])));
              else
                LapeException(lpeClosingBracketExpected, Tokenizer.DocPos);
              end;

            if (ParserTokenToOperator(Peek()) in AssignOperators) then
            begin
              Next();
              PropInvoke.PropertyType := ptWrite;
              PropInvoke.AssignOp := ParserTokenToOperator(Tokenizer.Tok);
              PropInvoke.addParam(EnsureExpression(ParseExpression(ParserToken_ExpressionEnd, True)));
              DoNext := False;
            end else
              PropInvoke.PropertyType := ptRead;
          end else
            LapeException(lpeExpectedIndexValue, Tokenizer.DocPos);

          Result := True;
        end;

        VarStack.Pop();
        VarStack.Push(PropInvoke);
      end;
    end;
  end;

  procedure ParseSpecialParam(Method: TLapeTree_InternalMethod);
  begin
    case Method.SpecialParam of
      spForce:
        if not (Tokenizer.Tok in ReturnOn) then
        begin
          Method.addParam(EnsureExpression(ParseExpression(ReturnOn, Tokenizer.Tok = tk_kw_At)));
          if (Method is TLapeTree_InternalMethod_Raise) and (Tokenizer.Tok = tk_kw_At) then
            Method.addParam(EnsureExpression(ParseExpression(ReturnOn, True)));
        end;

      spTypeThenParams:
        begin
          Method.addParam(EnsureExpression(getExpression(Tokenizer.TokString)));
          if Expect([tk_sym_ParenthesisOpen, tk_sym_SemiColon]) = tk_sym_ParenthesisOpen then
          begin
            if (Next() <> tk_sym_ParenthesisClose) then
            begin
              Method.AddParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], False, True)));
              while True do
                case Tokenizer.Tok of
                  tk_sym_ParenthesisClose: Break;
                  tk_sym_Comma: Method.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], True, True)));
                  else
                    LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
                end;
            end;
            Expect(tk_sym_ParenthesisClose, False, True);
          end;
        end;

      spType:
        begin
          Expect(tk_cmp_LessThan, False, False);
          Method.AddParam(EnsureExpression(ParseExpression([tk_cmp_GreaterThan, tk_sym_Comma], True, True)));
          while True do
            case Tokenizer.Tok of
              tk_cmp_GreaterThan: Break;
              tk_sym_Comma:
                Method.AddParam(EnsureExpression(ParseExpression([tk_cmp_GreaterThan, tk_sym_Comma], True, True)));
              else
                LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
            end;
          Expect(tk_cmp_GreaterThan, False, True);
        end;
    end;
  end;

var
  Signed: Boolean;
  Cast: TLapeTree_Cast;
  InExpr: Integer;
  Expr: TLapeTree_ExprBase;
  Method: TLapeTree_Invoke;
begin
  Result := nil;
  Method := nil;
  VarStack := TLapeTree_NodeStack.Create(8);
  OpStack := TLapeTree_OpStack.Create(16);
  _LastNode := _None;
  InExpr := 0;
  DoNext := FirstNext;

  try
    while True do
    begin
      if ((DoNext and (Next() in ReturnOn)) or ((not DoNext) and (Tokenizer.Tok in ReturnOn))) and (InExpr <= 0) then
        Break;
      DoNext := True;

      case Tokenizer.Tok of
        tk_typ_Float, tk_typ_Integer, tk_typ_Integer_Hex, tk_typ_Integer_Bin:
          begin
            // FCachedDeclarations are used - Need to know if signed or not. Remove op_UnaryMinus node too.
            Signed := (OpStack.Cur >= 0) and (OpStack.Top <> TLapeTree_Operator(ParenthesisOpen)) and (opStack.Top.OperatorType = op_UnaryMinus);

            if Signed then
            begin
              case Tokenizer.Tok of
                tk_typ_Integer:
                  PushVarStack(TLapeTree_Integer.Create(lpString('-' + Tokenizer.TokString), Self, getPDocPos()));
                tk_typ_Integer_Hex, tk_typ_Integer_Bin:
                  PushVarStack(TLapeTree_Integer.Create(lpString('-' + UIntToStr(Tokenizer.TokUInt64)), Self, getPDocPos()));
                tk_typ_Float:
                  PushVarStack(TLapeTree_Float.Create(lpString('-' + Tokenizer.TokString), Self, getPDocPos()));
              end;

              OpStack.Pop.Free();
            end else
            begin
              case Tokenizer.Tok of
                tk_typ_Integer:
                  PushVarStack(TLapeTree_Integer.Create(lpString(Tokenizer.TokString), Self, getPDocPos()));
                tk_typ_Integer_Hex, tk_typ_Integer_Bin:
                  PushVarStack(TLapeTree_Integer.Create(lpString(UIntToStr(Tokenizer.TokUInt64)), Self, getPDocPos()));
                tk_typ_Float:
                  PushVarStack(TLapeTree_Float.Create(lpString(Tokenizer.TokString), Self, getPDocPos()));
              end;
            end;
          end;

        tk_typ_String, tk_typ_HereString, tk_typ_Char:
          ParseAndPushString();

        tk_kw_Type:
          PushVarStack(TLapeTree_VarType.Create(parseType(nil), Self));

        tk_Identifier:
          begin
            Expr := getExpression(Tokenizer.TokString, getPDocPos());
            if (Expr = nil) then
              LapeExceptionFmt(lpeUnknownDeclaration, [Tokenizer.TokString], Tokenizer.DocPos);

            // cast
            if (Expr is TLapeTree_VarType) and (Peek() = tk_sym_ParenthesisOpen) then
            begin
              _LastNode := _Var;
              Expect(tk_sym_ParenthesisOpen, True, True);
              Cast := TLapeTree_Cast.Create(Self, getPDocPos());
              Cast.CastTo := Expr;
              Cast.Param := EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], False, True));
              VarStack.Push(Cast);
              Expect(tk_sym_ParenthesisClose, False, True);
              DoNext := False;
            end
            // method
            else if (Expr is TLapeTree_Invoke) then
            begin
              Method := Expr as TLapeTree_Invoke;
              DoNext := False;
              if (Next() = tk_sym_ParenthesisOpen) then
                _LastNode := _Var
              else
              begin
                if (Method is TLapeTree_InternalMethod) and (TLapeTree_InternalMethod(Method).SpecialParam <> spNo) then
                  ParseSpecialParam(Method as TLapeTree_InternalMethod);
                VarStack.Push(Method);
                Method := nil;
              end;
            end
            else
              PushVarStack(Expr);
          end;

        tk_sym_ParenthesisOpen:
          begin
            if (_LastNode = _Var) then
            begin
              PopOpStack(op_Invoke);

              if (Method = nil) then
              begin
                if IsProperty(VarStack.Top.resType()) then
                  LapeException(lpeCannotInvokeProperty, Tokenizer.DocPos);

                Expr := ResolveMethods(VarStack.Top.FoldConstants(), True) as TLapeTree_ExprBase;
                if (Expr <> VarStack.Pop()) and (Expr is TLapeTree_InternalMethod) then
                  Method := TLapeTree_Invoke(Expr)
                else
                  Method := TLapeTree_Invoke.Create(Expr, Self, getPDocPos());
              end;

              if (Next() <> tk_sym_ParenthesisClose) then
              begin
                Method.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], False, True)));
                while True do
                  case Tokenizer.Tok of
                    tk_sym_ParenthesisClose: Break;
                    tk_sym_Comma: Method.addParam(EnsureExpression(ParseExpression([tk_sym_ParenthesisClose, tk_sym_Comma], True, True)));
                    else
                      LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);
                  end;
              end;
              VarStack.Push(Method);
              Method := nil;
            end
            else
            begin
              PushOpStack(TLapeTree_Operator(ParenthesisOpen));
              Inc(InExpr);
            end;
          end;

        tk_sym_ParenthesisClose:
          begin
            while (OpStack.Cur >= 0) and (OpStack.Top <> TLapeTree_Operator(ParenthesisOpen)) do
              PopOpNode();
            if (OpStack.Cur < 0) or (OpStack.Pop() <> TLapeTree_Operator(ParenthesisOpen)) then
              LapeException(lpeLostClosingParenthesis, Tokenizer.DocPos);
            Dec(InExpr);
          end;

        tk_sym_BracketOpen:
          if (not ParseIndex()) then
            ParseOperator();

        {$IFDEF Lape_PascalLabels}
        tk_sym_Colon:
          begin
            PopOpStack(op_Label);
            if (VarStack.Count <> 1) then
              LapeException(lpeInvalidLabel, Tokenizer.DocPos);
            if (Method <> nil) then
              LapeException(lpeCannotEvalConstProc, Tokenizer.DocPos);

            Method := TLapeTree_InternalMethod_Label.Create(Self, getPDocPos());
            Method.addParam(VarStack.Pop());

            VarStack.Push(Method);
            Method := nil;

            Next();
            Break;
          end;
        {$ENDIF}

        else
          if Tokenizer.Tok in [ParserToken_FirstOperator..ParserToken_LastOperator] then
            ParseOperator()
          else
            Break;
      end;
    end;

    while (OpStack.Cur >= 0) do
    begin
      if (OpStack.Top = TLapeTree_Operator(ParenthesisOpen)) then
        LapeException(lpeClosingParenthesisExpected, Tokenizer.DocPos);

      PopOpNode();
    end;

    if (VarStack.Cur <> 0) then
      if (VarStack.Cur < 0) and (Tokenizer.Tok in ReturnOn) then
        Exit(nil)
      else
        LapeException(lpeInvalidEvaluation, Tokenizer.DocPos);

    Result := TLapeTree_ExprBase(ResolveMethods(VarStack.Pop(), False));
    if DoFold and (not TLapeTree_Base.isEmpty(Result)) then
      Result := TLapeTree_ExprBase(Result.FoldConstants());
  finally
    if (Method <> nil) then
      Method.Free();
    while (VarStack.Cur >= 0) do
      VarStack.Pop().Free();
    while (OpStack.Cur >= 0) do
      if (OpStack.Top = TLapeTree_Operator(ParenthesisOpen)) then
        OpStack.Pop()
      else
        OpStack.Pop().Free();

    VarStack.Free();
    OpStack.Free();
  end;
end;

function TLapeCompiler.ParseTypeExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_Base;
var
  ExprLo: TLapeTree_ExprBase;
  TypeHi: TLapeType;
begin
  Result := nil;
  TypeHi := nil;

  ExprLo := ParseExpression(ReturnOn, FirstNext, DoFold);
  if (Tokenizer.Tok <> tk_sym_DotDot) then
    Result := ExprLo
  else
  try
    Result := TLapeTree_Range.Create(Self, getPDocPos());
    TLapeTree_Range(Result).Lo := ExprLo;
    TLapeTree_Range(Result).Hi := ParseExpression(ReturnOn, True, DoFold);

    if (ExprLo <> nil) and (TLapeTree_Range(Result).Hi <> nil) then
      TypeHi := TLapeTree_Range(Result).Hi.resType();
    if (TypeHi = nil) or (not TypeHi.CompatibleWith(ExprLo.resType())) then
      LapeException(lpeInvalidRange, Tokenizer.DocPos);
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseStatement(FirstNext: Boolean = True; ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Base;
var
  AcceptTokens: EParserTokenSet;
begin
  Result := nil;
  AcceptTokens := [tk_NULL, tk_kw_Begin, tk_kw_Case, tk_kw_For, tk_kw_If, tk_kw_Repeat, tk_kw_While, tk_kw_With, tk_kw_Try, tk_sym_SemiColon, tk_kw_Else];
  if (lcoLooseSyntax in FOptions) then
    AcceptTokens := AcceptTokens + [tk_kw_Const, tk_kw_Var];

  if FirstNext then
    Next();

  if (Tokenizer.Tok in AcceptTokens) then
    case Tokenizer.Tok of
      tk_NULL:;
      tk_kw_Begin: Result := ParseBeginEnd(ExprEnd);
      tk_kw_Case: Result := ParseCase(ExprEnd);
      tk_kw_Const, tk_kw_Var: Result := ParseVarBlock(True);
      tk_kw_For: Result := ParseFor(ExprEnd);
      tk_kw_If: Result := ParseIf(ExprEnd);
      tk_kw_Repeat: Result := ParseRepeat(ExprEnd);
      tk_kw_While: Result := ParseWhile(ExprEnd);
      tk_kw_With: Result := ParseWith(ExprEnd);
      tk_kw_Try: Result := ParseTry(ExprEnd);
      else ParseExpressionEnd(ExprEnd);
    end
  else if (not (Tokenizer.Tok in ParserToken_BlockEnd)) then
  begin
    Result := ParseExpression(ExprEnd, False);
    {$IFDEF Lape_PascalLabels}
    if (Result is TLapeTree_InternalMethod_Label) then
    begin
      if (Tokenizer.LastTok <> tk_sym_Colon) then
        LapeExceptionFmt(lpeExpectedOther, [LapeTokenToString(Tokenizer.LastTok), LapeTokenToString(tk_sym_Colon)], Tokenizer.DocPos);
      Exit;
    end;
    {$ENDIF}

    try
      ParseExpressionEnd(ExprEnd);
    except
      Result.Free();
      raise;
    end;
  end;
end;

function TLapeCompiler.ParseStatementList: TLapeTree_StatementList;
var
  Statement: TLapeTree_Base;
  Recover: Integer;
begin
  Result := TLapeTree_StatementList.Create(Self, getPDocPos());
  Recover := 0;

  try

    Next();
    repeat
      Statement := ParseStatement(False);
      // Fix a simple statement of `something=1` when assignment operator is probably wanted.
      if (Statement is TLapeTree_Operator) and (TLapeTree_Operator(Statement).OperatorType = op_cmp_Equal) then
        LapeException(lpeExpectedAssignOperator, DocPos);

      if (Statement <> nil) then
      begin
        Result.addStatement(Statement);
        Recover := 0;
      end
      else if (Statement = nil) and
        (lcoLooseSemicolon in FOptions) and
        (Tokenizer.LastTok = tk_sym_SemiColon) and
        (not (Tokenizer.Tok in ParserToken_BlockEnd + [tk_NULL]))
      then
      begin
        PtrUInt(Statement) := 1; //Ensure another iteration
        Inc(Recover);
      end;
    until (Statement = nil) or (Recover > 10);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseBeginEnd(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_StatementList;
begin
  Result := ParseStatementList();

  try

    Expect(tk_kw_End, False, True);
    ParseExpressionEnd(ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseCase(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Case;
var
  Expr: TLapeTree_Base;
  Branch: TLapeTree_CaseBranch;
begin
  Result := TLapeTree_Case.Create(Self, getPDocPos());
  Expr := nil;
  Branch := nil;
  try
    Result.Condition := ParseExpression();
    Expect(tk_kw_Of, False, True);

    while (not (Tokenizer.Tok in [tk_Null, tk_kw_Else, tk_kw_End])) do
    begin
      Expr := ParseTypeExpression([tk_sym_Comma, tk_sym_Colon], False);
      if (Expr = nil) then
        LapeException(lpeInvalidCaseStatement, Tokenizer.DocPos);

      Branch := TLapeTree_CaseBranch.Create(Self, @Expr._DocPos);
      repeat
        Branch.addValue(Expr);
        Expr := nil;
        Expect([tk_sym_Comma, tk_sym_Colon], False, False);
        if (Tokenizer.Tok = tk_sym_Colon) then
          Break
        else
          Expr := ParseTypeExpression([tk_sym_Comma, tk_sym_Colon]);
      until False;

      Branch.Statement := ParseStatement();
      Result.addBranch(Branch);
      Branch := nil;
    end;

    Expect([tk_kw_Else, tk_kw_End], False, True);
    if (Tokenizer.LastTok = tk_kw_Else) then
    begin
      Result.ElseBody := ParseStatement(False);
      Expect(tk_kw_End, False, True);
    end;

    ParseExpressionEnd(ExprEnd);
  except
    if (Expr <> nil) then
      Expr.Free();
    if (Branch <> nil) then
      Branch.Free();
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseFor(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_For;

  // for i:=0 to 1000 do/downto
  // for 1 to 10 do/downto
  procedure parseForTo(counterExpr: TLapeTree_ExprBase);
  begin
    case Tokenizer.Tok of
      tk_kw_DownTo: Result.LoopType := lptypes.loopDown;
      tk_kw_To    : Result.LoopType := lptypes.loopUp;
    end;
    Next();

    Result.Counter := counterExpr;
    Result.Limit := EnsureExpression(ParseExpression([], False)).setExpectedType(Result.Counter.resType) as TLapeTree_ExprBase;
  end;

  // for i in arr
  // "i in arr" is TLapeTree_Operator (op=in, left=i, right=arr);
  procedure parseForOver(counterExpr: TLapeTree_ExprBase);
  var
    LimitType: TLapeType;
  begin
    Result.Counter := TLapeTree_Operator(counterExpr).Left;
    Result.LoopType := loopOver;
    Result.LoopOverWhat := loopOverArray;

    if (TLapeTree_Operator(counterExpr).Right.resType() is TLapeType_Set) then
      Result.LoopOverWhat := loopOverSet
    else
    if (TLapeTree_Operator(counterExpr).Right.resType() is TLapeType_SubRange) or
       (TLapeTree_Operator(counterExpr).Right.resType() is TLapeType_TypeEnum) then
      Result.LoopOverWhat := loopOverEnum;

    if Result.LoopOverWhat in [loopOverEnum, loopOverSet] then
      Result.Limit := TLapeTree_Operator(counterExpr).Right
    else
    begin
      LimitType := addManagedType(TLapeType_DynArray.Create(Result.Counter.resType(), Self, '', getPDocPos()));
      with TLapeTree_Operator(counterExpr).Right do
      begin
        Parent := Result;
        Result.Limit := TLapeTree_ExprBase(setExpectedType(LimitType));
      end;
    end;
    counterExpr.Free();
  end;

var
  Expr: TLapeTree_ExprBase;
begin
  Result := TLapeTree_For.Create(Self, getPDocPos());

  try
    if (lcoLooseSyntax in FOptions) and isNext([tk_kw_Var]) then
    begin
      // TODO: make ParseExpression work with var/const decls so this can be handled more generally
      with ParseVarBlock(True, [tk_op_In, tk_kw_To, tk_kw_DownTo]) do
      try
        if (Vars.Count <> 1) then
          LapeException(lpeVariableExpected, DocPos);

        if (Vars[0].Default <> nil) then
        begin
          Result.Counter := TLapeTree_Operator.Create(op_Assign, Compiler, @_DocPos);
          with TLapeTree_Operator(Result.Counter) do
          begin
            Left := TLapeTree_ResVar.Create(_ResVar.New(Vars[0].VarDecl), Compiler, @_DocPos);
            Right := Vars[0].Default;
          end;
        end
        else
          Result.Counter := TLapeTree_ResVar.Create(_ResVar.New(Vars[0].VarDecl), Compiler, @_DocPos);
      finally
        Free();
      end;
    end else
    begin
      Expr := ParseExpression();

      if (Tokenizer.Tok in [tk_kw_To, tk_kw_DownTo]) then
        parseForTo(Expr)
      else
      if (Expr is TLapeTree_Operator) and (TLapeTree_Operator(Expr).OperatorType = op_In) then
        parseForOver(Expr)
      else
        LapeException(lpeVariableExpected, DocPos);
    end;

    Expect([tk_kw_With, tk_kw_Do], False, False);
    if (Tokenizer.Tok = tk_kw_With) then
    begin
      Result.Step := ParseExpression();
      Expect(tk_kw_Do, False, False);
    end;

    Result.Body := ParseStatement(True, ExprEnd + [tk_kw_Else]);
    if (Tokenizer.LastTok = tk_kw_Else) then
      Result.ElseBody := ParseStatement(False, ExprEnd);
  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseIf(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_If;
begin
  Result := TLapeTree_If.Create(Self, getPDocPos());

  try

    Result.Condition := ParseExpression();
    Expect(tk_kw_Then, False, False);
    Result.Body := ParseStatement(True, ExprEnd + [tk_kw_Else]);
    if (Tokenizer.LastTok = tk_kw_Else) then
      Result.ElseBody := ParseStatement(False, ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseRepeat(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Repeat;
begin
  Result := TLapeTree_Repeat.Create(Self, getPDocPos());

  try

    Result.Body := ParseStatementList();
    Expect(tk_kw_Until, False, False);
    Result.Condition := ParseExpression();
    ParseExpressionEnd(ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseTry(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Try;
begin
  Result := TLapeTree_Try.Create(Self, getPDocPos());

  try

    Result.Body := ParseStatementList();
    Expect([tk_kw_Except, tk_kw_Finally], False, False);
    if (Tokenizer.Tok = tk_kw_Except) then
    begin
      Result.ExceptBody := ParseStatementList();
      Expect([tk_kw_Finally, tk_kw_End], False, False);
    end;
    if (Tokenizer.Tok = tk_kw_Finally) then
      Result.FinallyBody := ParseStatementList();

    Expect(tk_kw_End, False, True);
    ParseExpressionEnd(ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseWhile(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_While;
begin
  Result := TLapeTree_While.Create(Self, getPDocPos());

  try

    Result.Condition := ParseExpression();
    Expect(tk_kw_Do, False, False);
    Result.Body := ParseStatement(True, ExprEnd + [tk_kw_Else]);
    if (Tokenizer.LastTok = tk_kw_Else) then
      Result.ElseBody := ParseStatement(False, ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseWith(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_With;
var
  Count: Integer;
begin
  Result := TLapeTree_With.Create(Self, getPDocPos());
  Count := 0;

  try

    repeat
      if (Tokenizer.Tok in [tk_kw_With, tk_sym_Comma]) then
      begin
        FStackInfo.addWith(Result.addWith(ParseExpression([tk_sym_Comma])));
        Inc(Count);
      end
      else
      begin
        Expect(tk_kw_Do, False, False);
        Break;
      end;
    until False;

    Result.Body := ParseStatement(True, ExprEnd);
    FStackInfo.delWith(Count);

  except
    Result.Free();
    raise;
  end;
end;

constructor TLapeCompiler.Create(
  ATokenizer: TLapeTokenizerBase; ManageTokenizer: Boolean = True;
  AEmitter: TLapeCodeEmitter = nil; ManageEmitter: Boolean = True);
begin
  inherited Create(AEmitter, ManageEmitter);

  FTokenizer := -1;
  FImporting := nil;
  FDelayedTree := TLapeTree_DelayedStatementList.Create(Self);
  FreeTokenizer := ManageTokenizer;
  FreeTree := True;

  FIncludes := TStringList.Create();
  FIncludes.Duplicates := dupIgnore;
  FIncludes.CaseSensitive := {$IFDEF DARWIN}False{$ELSE}LapeSystemCaseSensitive{$ENDIF};
  FConditionalStack := TLapeConditionalStack.Create(0);

  FOnHandleDirective := nil;
  FOnHandleExternal := nil;
  FOnFindFile := nil;
  FAfterParsing := TLapeCompilerNotification.Create();

  FPreprocessorFuncs := TLapePreprocessorFuncs.Create(nil, dupError, False);
  FPreprocessorFuncs.Add('DEFINED', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);
  FPreprocessorFuncs.Add('DECLARED', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);
  FPreprocessorFuncs.Add('FILEEXISTS', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);
  FPreprocessorFuncs.Add('INCLUDED', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);

  FPreprocessorMacros := TLapePreprocessorFuncs.Create(nil, dupError, False);
  FPreprocessorMacros.Add('FILE', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('DIR', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('ENV', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('INCLUDEDFILE', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('INCLUDEDFILES', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('TICKCOUNT', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('LINE', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('NOW', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('FUNC', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);

  FInternalMethodMap := TLapeInternalMethodMap.Create(nil);
  FInternalMethodMap['Write'] := TLapeTree_InternalMethod_Write;
  FInternalMethodMap['WriteLn'] := TLapeTree_InternalMethod_WriteLn;
  FInternalMethodMap['ToStr'] := TLapeTree_InternalMethod_ToStr;

  FInternalMethodMap['Assert'] := TLapeTree_InternalMethod_Assert;
  FInternalMethodMap['IsScriptMethod'] := TLapeTree_InternalMethod_IsScriptMethod;
  FInternalMethodMap['GetExceptionMessage'] := TLapeTree_InternalMethod_GetExceptionMessage;
  FInternalMethodMap['GetExceptionLocation'] := TLapeTree_InternalMethod_GetExceptionLocation;
  FInternalMethodMap['GetExceptionLocationStr'] := TLapeTree_InternalMethod_GetExceptionLocationStr;
  FInternalMethodMap['GetCallerAddress'] := TLapeTree_InternalMethod_GetCallerAddress;
  FInternalMethodMap['GetCallerName'] := TLapeTree_InternalMethod_GetCallerName;
  FInternalMethodMap['GetCallerLocation'] := TLapeTree_InternalMethod_GetCallerLocation;
  FInternalMethodMap['GetCallerLocationStr'] := TLapeTree_InternalMethod_GetCallerLocationStr;
  FInternalMethodMap['GetScriptMethodName'] := TLapeTree_InternalMethod_GetScriptMethodName;
  FInternalMethodMap['DumpCallStack'] := TLapeTree_InternalMethod_DumpCallStack;
  FInternalMethodMap['Break'] := TLapeTree_InternalMethod_Break;
  FInternalMethodMap['Continue'] := TLapeTree_InternalMethod_Continue;
  FInternalMethodMap['FallThrough'] := TLapeTree_InternalMethod_FallThrough;
  FInternalMethodMap['Exit'] := TLapeTree_InternalMethod_Exit;
  FInternalMethodMap['Halt'] := TLapeTree_InternalMethod_Halt;

  FInternalMethodMap['New'] := TLapeTree_InternalMethod_New;
  FInternalMethodMap['Dispose'] := TLapeTree_InternalMethod_Dispose;
  FInternalMethodMap['Default'] := TLapeTree_InternalMethod_Default;

  FInternalMethodMap['Swap'] := TLapeTree_InternalMethod_Swap;
  FInternalMethodMap['SizeOf'] := TLapeTree_InternalMethod_SizeOf;

  FInternalMethodMap['Low'] := TLapeTree_InternalMethod_Low;
  FInternalMethodMap['High'] := TLapeTree_InternalMethod_High;
  FInternalMethodMap['Length'] := TLapeTree_InternalMethod_Length;

  FInternalMethodMap['SetLength'] := TLapeTree_InternalMethod_SetLength;
  FInternalMethodMap['Copy'] := TLapeTree_InternalMethod_Copy;
  FInternalMethodMap['Delete'] := TLapeTree_InternalMethod_Delete;
  FInternalMethodMap['Insert'] := TLapeTree_InternalMethod_Insert;

  FInternalMethodMap['Sort'] := TLapeTree_InternalMethod_Sort;
  FInternalMethodMap['Sorted'] := TLapeTree_InternalMethod_Sorted;
  FInternalMethodMap['Unique'] := TLapeTree_InternalMethod_Unique;
  FInternalMethodMap['Reverse'] := TLapeTree_InternalMethod_Reverse;
  FInternalMethodMap['Reversed'] := TLapeTree_InternalMethod_Reversed;
  FInternalMethodMap['IndexOf'] := TLapeTree_InternalMethod_IndexOf;
  FInternalMethodMap['IndicesOf'] := TLapeTree_InternalMethod_IndicesOf;
  FInternalMethodMap['Contains'] := TLapeTree_InternalMethod_Contains;
  FInternalMethodMap['Remove'] := TLapeTree_InternalMethod_Remove;

  FInternalMethodMap['Ord'] := TLapeTree_InternalMethod_Ord;
  FInternalMethodMap['Succ'] := TLapeTree_InternalMethod_Succ;
  FInternalMethodMap['Pred'] := TLapeTree_InternalMethod_Pred;
  FInternalMethodMap['Inc'] := TLapeTree_InternalMethod_Inc;
  FInternalMethodMap['Dec'] := TLapeTree_InternalMethod_Dec;

  FInternalMethodMap['label'] := TLapeTree_InternalMethod_Label;
  FInternalMethodMap['goto'] := TLapeTree_InternalMethod_GoTo;

  FInternalMethodMap['raise'] := TLapeTree_InternalMethod_Raise;

  FInternalMethodMap['Objectify'] := TLapeTree_InternalMethod_Objectify;

  FInternalMethodMap['ArrayMin'] := TLapeTree_InternalMethod_ArrayMin;
  FInternalMethodMap['ArrayMax'] := TLapeTree_InternalMethod_ArrayMax;
  FInternalMethodMap['ArrayMedian'] := TLapeTree_InternalMethod_ArrayMedian;
  FInternalMethodMap['ArrayMode'] := TLapeTree_InternalMethod_ArrayMode;
  FInternalMethodMap['ArraySum'] := TLapeTree_InternalMethod_ArraySum;
  FInternalMethodMap['ArrayMean'] := TLapeTree_InternalMethod_ArrayMean;
  FInternalMethodMap['ArrayVariance'] := TLapeTree_InternalMethod_ArrayVariance;
  FInternalMethodMap['ArrayStdev'] := TLapeTree_InternalMethod_ArrayStdev;
  FInternalMethodMap['ArrayDifference'] := TLapeTree_InternalMethod_ArrayDifference;
  FInternalMethodMap['ArraySymDifference'] := TLapeTree_InternalMethod_ArraySymDifference;
  FInternalMethodMap['ArrayIntersection'] := TLapeTree_InternalMethod_ArrayIntersection;
  FInternalMethodMap['ArrayEquals'] := TLapeTree_InternalMethod_ArrayEquals;

  FInternalMethodMap['Slice'] := TLapeTree_InternalMethod_Slice;

  FInternalMethodMap['PType'] := TLapeTree_InternalMethod_PType;

  setTokenizer(ATokenizer);
  Reset();

  LapeCreateArrayHelpers(Self);

  InitBaseDefinitions();
end;

destructor TLapeCompiler.Destroy;
begin
  EndImporting();
  setTokenizer(nil);
  if FreeTree and (FDelayedTree <> nil) then
    FreeAndNil(FDelayedTree);
  FreeAndNil(FPreprocessorFuncs);
  FreeAndNil(FPreprocessorMacros);
  FreeAndNil(FIncludes);
  FreeAndNil(FConditionalStack);
  FreeAndNil(FAfterParsing);
  FreeAndNil(FInternalMethodMap);
  inherited;
end;

procedure TLapeCompiler.Clear;
begin
  inherited;
  FTypeID := TypeID_User;
end;

function TLapeCompiler.getState: Pointer;
var
  i: Integer;
begin
  New(PCompilerState(Result));
  with PCompilerState(Result)^ do
  begin
    Tokenizer := FTokenizer;

    SetLength(Tokenizers, Length(FTokenizers));
    SetLength(TokStates, Length(FTokenizers));
    for i := 0 to High(FTokenizers) do
    begin
      Tokenizers[i] := incTokenizerLock(FTokenizers[i]);
      if (Tokenizers[i] <> nil) then
        TokStates[i] := Tokenizers[i].getState()
      else
        TokStates[i] := nil;
    end;

    Options := FOptions;
    Options_PackRecords := FOptions_PackRecords;
    Defines := FDefines;
    Conditionals := FConditionalStack.ExportToArray();
  end;
end;

procedure TLapeCompiler.setState(const State: Pointer; DoFreeState: Boolean = True);
var
  i: Integer;
begin
  with PCompilerState(State)^ do
  begin
    Assert(Length(FTokenizers) >= Length(Tokenizers));
    Assert(Length(Tokenizers) = Length(TokStates));
    FTokenizer := Tokenizer;

    for i := 0 to High(Tokenizers) do
    begin
      if (TokStates[i] <> nil) then
        Tokenizers[i].setState(TokStates[i], False);
      if (FTokenizers[i] <> Tokenizers[i]) then
      begin
        decTokenizerLock(FTokenizers[i]);
        FTokenizers[i] := incTokenizerLock(Tokenizers[i]);
      end;
    end;

    FOptions := Options;
    FOptions_PackRecords := Options_PackRecords;
    FDefines := Defines;
    FConditionalStack.ImportFromArray(Conditionals);
  end;
  if DoFreeState then
    freeState(State);
end;

procedure TLapeCompiler.freeState(const State: Pointer);
var
  i: Integer;
begin
  with PCompilerState(State)^ do
    for i := 0 to High(Tokenizers) do
    begin
      if (TokStates[i] <> nil) then
        Tokenizers[i].freeState(TokStates[i]);
      decTokenizerLock(Tokenizers[i]);
    end;

  Dispose(PCompilerState(State));
end;

function TLapeCompiler.getTempTokenizerState(const ATokenizer: TLapeTokenizerBase; ResetState: Boolean = True): Pointer;
begin
  if (not hasTokenizer()) then
    SetLength(FTokenizers, 1);

  if Importing and (FTokenizers[0] = nil) then
    Result := nil
  else
  begin
    New(PTempTokenizerState(Result));
    with PTempTokenizerState(Result)^ do
    begin
      OldStackInfo := FStackInfo;
      OldTokenizerIndex := FTokenizer;
      OldTokenizer := FTokenizers[0];
      OldState := getState();
    end;
  end;

  FTokenizer := 0;
  FTokenizers[0] := nil;
  Tokenizer := ATokenizer;

  if ResetState then
  begin
    FConditionalStack.Reset();
    FStackInfo := nil;
    FOptions := FBaseOptions;
    FOptions_PackRecords := FBaseOptions_PackRecords;
    FDefines := FBaseDefines;
  end;

  if (FStackInfo = nil) then
    FStackInfo := EmptyStackInfo;
end;

function TLapeCompiler.getTempTokenizerState(const AStr: lpString; const AFileName: lpString = ''; ResetState: Boolean = True): Pointer;
begin
  Result := getTempTokenizerState(TLapeTokenizerString.Create(AStr, AFileName), ResetState);
end;

procedure TLapeCompiler.resetTokenizerState(const State: Pointer; DoFreeState: Boolean = True);
begin
  if (State <> nil) then
    with PTempTokenizerState(State)^ do
    begin
      Swap(Pointer(FTokenizers[0]), Pointer(OldTokenizer));
      FTokenizer := OldTokenizerIndex;
      FStackInfo := OldStackInfo;
      setState(OldState, False);
    end;
  if DoFreeState then
    freeTempTokenizerState(State);
end;

procedure TLapeCompiler.freeTempTokenizerState(const State: Pointer);
begin
  if (State = nil) then
  begin
    if Importing and (FTokenizers[0] <> nil) then
      decTokenizerLock(FTokenizers[0]);
    Exit;
  end;

  with PTempTokenizerState(State)^ do
  begin
    if (OldTokenizer <> nil) then
      decTokenizerLock(OldTokenizer);
    freeState(OldState);
  end;
  Dispose(PTempTokenizerState(State));
end;

function TLapeCompiler.getVarStates: TLapeVarStates;
var
  Decls: TLapeDeclArray;
  i: Integer;
begin
  Decls := FGlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue);
  SetLength(Result, Length(Decls));
  for i := 0 to High(Decls) do
    if TLapeGlobalVar(Decls[i]).VarType.CompatibleWith(TLapeGlobalVar(Decls[i]).VarType) then
    begin
      with TLapeTree_InternalMethod_DeepCopy.Create(Self) do
      try
        addParam(TLapeTree_GlobalVar.Create(TLapeGlobalVar(Decls[i]), Self));
        Result[I] := Evaluate();
      finally
        Free();
      end;
    end;
end;

procedure TLapeCompiler.setVarStates(VarStates: TLapeVarStates);
var
  Decls: TLapeDeclArray;
  i: Integer;
  VarCopy: TLapeGlobalVar;
  wasConstant: Boolean;
begin
  Decls := FGlobalDeclarations.GetByClass(TLapeGlobalVar, bTrue);
  if (Length(Decls) <> Length(VarStates)) then
    LapeException(lpeImpossible);

  for i := 0 to High(VarStates) do
    if (VarStates[i] <> nil) then
    begin
      wasConstant := not TLapeGlobalVar(Decls[i]).Writeable;
      if wasConstant then
        TLapeGlobalVar(Decls[i]).Writeable := True;

      with TLapeTree_InternalMethod_DeepCopy.Create(Self) do
      try
        addParam(TLapeTree_GlobalVar.Create(TLapeGlobalVar(VarStates[i]), Self));

        VarCopy := Evaluate();
        VarStates[i].VarType.EvalConst(op_Assign, TLapeGlobalVar(Decls[i]), VarCopy, []);
      finally
        VarCopy.Free();
        Free();

        if wasConstant then
          TLapeGlobalVar(Decls[i]).Writeable := False;
      end;
    end;
end;

procedure TLapeCompiler.freeVarStates(VarStates: TLapeVarStates);
var
  i: Integer;
begin
  for i := 0 to High(VarStates) do
    if (VarStates[i] <> nil) then
      FreeAndNil(VarStates[i]);
end;

procedure TLapeCompiler.StartImporting;
begin
  if (not Importing) then
    FImporting := getTempTokenizerState(nil);
end;

procedure TLapeCompiler.EndImporting;
begin
  if Importing then
  begin
    resetTokenizerState(FImporting);
    FImporting := nil;
  end;
end;

function TLapeCompiler.addDelayedExpression(Node: TLapeTree_Base; AfterCompilation: Boolean = True; IsGlobal: Boolean = False): TLapeTree_Base;
begin
  Result := Node;
  if (Node = nil) or (FDelayedTree = nil) then
    Exit;
  FDelayedTree.addStatement(Node, AfterCompilation, IsGlobal);
end;

function TLapeCompiler.ParseFile: TLapeTree_Base;
begin
  Result := nil;
  Assert(Tokenizer <> nil);

  try
    FDefines := FBaseDefines;

    if (Next() = tk_kw_Program) then
    begin
      Expect(tk_Identifier, True, True);
      ParseExpressionEnd(tk_sym_SemiColon);
      Result := ParseBlockList(False);
    end
    else
      Result := ParseBlockList(False);

    CheckAfterCompile();
  except
    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.Compile: Boolean;

  procedure GlobalHints;
  var
    Decls: TLapeDeclArray;
    Decl: TLapeGlobalVar;
    i: Integer;
  begin
    Decls := GlobalDeclarations.GetByClass(TLapeGlobalVar, bFalse);
    for i := 0 to High(Decls) do
    begin
      Decl := TLapeGlobalVar(Decls[i]);
      if (Decl.Used <> duFalse) or (Decl.Name = '') or (Decl.Name[1] = '!') or
         (not Decl.Writeable) or ((Decl._DocPos.Line = NullDocPos.Line) and (Decl._DocPos.Col = NullDocPos.Line)) then
        Continue;

      Hint(lphVariableNotUsed, [Decl.Name], Decl._DocPos);
    end;
  end;

  procedure GlobalFinalize;
  var
    Decls: TLapeDeclArray;
    Decl: TLapeGlobalVar;
    i: Integer;
  begin
    Decls := GlobalDeclarations.GetByClass(TLapeGlobalVar, bFalse);
    for i := 0 to High(Decls) do
    begin
      Decl := TLapeGlobalVar(Decls[i]);
      if Decl.NeedFinalization then
        FinalizeVar(_ResVar.New(Decl));
    end;
  end;

begin
  Result := False;
  try
    Reset();
    IncStackInfo(True);
    FTree := ParseFile();
    if (FTree = nil) and (FDelayedTree.GlobalCount(False) <= 0) then
      LapeException(lpeExpressionExpected);
    GlobalHints();

    FAfterParsing.Notify(Self);

    FDelayedTree.Compile(False).Spill(1);
    if (FTree <> nil) then
      FTree.Compile().Spill(1);

    FDelayedTree.Compile(True, ldfStatements).Spill(1);

    FStackInfo.FullDisposal := lcoFullDisposal in FOptions;
    DecStackInfo(False, True, True);
    GlobalFinalize();

    FDelayedTree.Compile(True, ldfMethods).Spill(1);

    FEmitter._op(ocNone);
    Result := True;
  except
    Reset();
    raise;
  end;
end;

procedure TLapeCompiler.CheckAfterCompile;
begin
  Assert(Tokenizer <> nil);

  if (FConditionalStack.Cur >= 0) then
    LapeException(lpeConditionalNotClosed, popConditional());
end;

procedure TLapeCompiler.VarToDefault(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil);
begin
  Assert(AVar.Writeable);
  if (AVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) and AVar.HasType() then
    with TLapeTree_InternalMethod_Default.Create(Self, Pos) do
    try
      addParam(TLapeTree_ResVar.Create(AVar.IncLock(), Self, Pos));
      Compile(Offset).Spill(1);
    finally
      Free();
    end;
end;

procedure TLapeCompiler.FinalizeVar(AVar: TResVar; var Offset: Integer; Pos: PDocPos = nil);
var
  wasConstant: Boolean;
begin
  wasConstant := False;
  if (AVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) and AVar.HasType()  then
    with TLapeTree_InternalMethod_Dispose.Create(Self, Pos) do
    try
      wasConstant := not AVar.Writeable;
      if wasConstant then
        AVar.Writeable := True;

      FunctionOnly := True;
      addParam(TLapeTree_ResVar.Create(AVar.IncLock(), Self, Pos));
      Compile(Offset).Spill(1);
    finally
      if wasConstant then
        AVar.Writeable := False;
      Free();
    end;
end;

function TLapeCompiler.getDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): TLapeDeclaration;
begin
  Result := inherited;
  if (Result = nil) and LocalOnly and (AStackInfo <> nil) and (AStackInfo.Owner = nil) then
    Result := inherited getDeclaration(AName, nil, Localonly, CheckWith);
end;

function TLapeCompiler.hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean;
begin
  Result := inherited;
  if (not Result) and LocalOnly and (AStackInfo <> nil) and (AStackInfo.Owner = nil) then
    Result := inherited hasDeclaration(AName, nil, Localonly, CheckWith);
  if (not Result) and ((AStackInfo = nil) or (not LocalOnly)) then
    Result := FInternalMethodMap[AName] <> FInternalMethodMap.InvalidVal;
end;

function TLapeCompiler.hasDeclaration(ADecl: TLapeDeclaration; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False; CheckWith: Boolean = True): Boolean;
begin
  Result := inherited;
  if (not Result) and LocalOnly and (AStackInfo <> nil) and (AStackInfo.Owner = nil) then
    Result := inherited hasDeclaration(ADecl, nil, Localonly, CheckWith);
end;

function TLapeCompiler.getDeclarationNoWith(AName: lpString; AStackInfo: TLapeStackInfo; LocalOnly: Boolean = False): TLapeDeclaration;
begin
  Result := getDeclaration(AName, AStackInfo, LocalOnly);
  if (Result is TLapeWithDeclaration) then
    with TLapeWithDeclaration(Result), TLapeTree_Operator.Create(op_Dot, Self) do
    try
      Left := TLapeTree_VarType.Create(WithDeclRec.WithType, Self);
      Right := TLapeTree_Field.Create(AName, Self);

      Result.Free();
      if isConstant() then
        Result := Evaluate()
      else
        Result := nil;
    finally
      Free();
    end;
end;

function TLapeCompiler.getDeclarationNoWith(AName: lpString; LocalOnly: Boolean = False): TLapeDeclaration;
begin
  Result := getDeclarationNoWith(AName, FStackInfo, LocalOnly);
end;

function TLapeCompiler.getExpression(AName: lpString; AStackInfo: TLapeStackInfo; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase;
var
  Decl: TLapeDeclaration;
begin
  Result := nil;

  try
    Decl := getDeclaration(AName, AStackInfo, LocalOnly);
  except
    on E: lpException do
      if (Pos = nil) then
        LapeException(lpString(E.Message))
      else
        LapeException(lpString(E.Message), Pos^)
  end;

  if (Decl <> nil) then
  begin
    Decl.Used := duTrue;
    if (lcoHints in FOptions) and (Decl is TLapeVar) and TLapeVar(Decl).HasHints() then
      TLapeVar(Decl).WriteHints({$IFDEF FPC}@{$ENDIF}Hint, Tokenizer.DocPos);

    if (Decl is TLapeWithDeclaration) then
      with TLapeWithDeclaration(Decl) do
      try
        Result := TLapeTree_Operator.Create(op_Dot, Self, Pos);
        TLapeTree_Operator(Result).Left := TLapeTree_WithVar.Create(WithDeclRec, Self, Pos);
        TLapeTree_Operator(Result).Right := TLapeTree_Field.Create(AName, Self, Pos);
        Result := Result.FoldConstants() as TLapeTree_ExprBase;
      finally
        Free();
      end
    else if (Decl is TLapeGlobalVar) then
      Result := TLapeTree_GlobalVar.Create(TLapeGlobalVar(Decl), Self, Pos)
    else if (Decl is TLapeVar) then
      Result := TLapeTree_ResVar.Create(_ResVar.New(TLapeVar(Decl)), Self, Pos)
    else if (Decl is TLapeType) then
      Result := TLapeTree_VarType.Create(TLapeType(Decl), Self, Pos)
    else
      {nothing}
  end else if FInternalMethodMap[AName] <> nil then
    Result := FInternalMethodMap[AName].Create(Self, Pos);
end;

function TLapeCompiler.getExpression(AName: lpString; Pos: PDocPos = nil; LocalOnly: Boolean = False): TLapeTree_ExprBase;
begin
  Result := getExpression(AName, FStackInfo, Pos, LocalOnly);
end;

function TLapeCompiler.hasBaseDefine(AName: lpString): Boolean;
var
  S: lpString;
  i: Integer;
begin
  S := UpperCase(AName);
  for i := 0 to FBaseDefines.Count - 1 do
    if (FBaseDefines[i].Name = S) then
      Exit(True);

  Result := False;
end;

function TLapeCompiler.hasDefine(AName: lpString): Boolean;
var
  S: lpString;
  i: Integer;
begin
  S := UpperCase(AName);
  for i := 0 to FDefines.Count - 1 do
    if (FDefines[I].Name = S) then
      Exit(True);

  Result := False;
end;

procedure TLapeCompiler.addDefine(AName: lpString; AValue: lpString);
var
  S: lpString;
  i: Integer;
  Def: TLapeDefine;
begin
  S := UpperCase(AName);
  if (AValue = '') and hasDefine(S) then
    Exit;

  // modify value if already exists
  if (AValue <> '') then
  begin
    for i := 0 to FDefines.Count - 1 do
      if (FDefines[i].Name = S) then
      begin
        Def.Name  := S;
        Def.Value := AValue;
        FDefines[I] := Def;

        Exit;
      end;
  end;

  Def.Name  := S;
  Def.Value := AValue;

  FDefines.Add(Def);
end;

procedure TLapeCompiler.addBaseDefine(AName: lpString; AValue: lpString);
var
  Def: TLapeDefine;
begin
  if not hasBaseDefine(AName) then
  begin
    Def.Name  := UpperCase(AName);
    Def.Value := AValue;

    FBaseDefines.Add(Def);
  end;
end;

function TLapeCompiler.hasInclude(FileName: lpString): Boolean;
begin
  Result := FIncludes.IndexOf(FileName) > -1;
end;

function TLapeCompiler.addLocalDecl(Decl: TLapeDeclaration; AStackInfo: TLapeStackInfo): TLapeDeclaration;
begin
  if (Decl = nil) then
    Exit(nil);

  if (Decl.Name <> '') then
  begin
    if hasDeclaration(Decl.Name, AStackInfo, True) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Decl.Name]);

    if (lcoDuplicateLocalNameHints in Options) and
       (FStackInfo <> nil) and hasDeclaration(Decl.Name, FStackInfo, False, False) then
      Hint(lphDuplicateLocalName, [Decl.Name], DocPos);
  end;

  Result := Decl;
  if (AStackInfo = nil) or (AStackInfo.Owner = nil) then
    FGlobalDeclarations.addDeclaration(Decl)
  else
    AStackInfo.addDeclaration(addManagedDecl(Decl));
end;

function TLapeCompiler.addLocalVar(AVar: TLapeType; Name: lpString = ''): TLapeVar;
begin
  if (AVar = nil) then
    Exit(nil);

  if (Name <> '') and hasDeclaration(Name, True) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [Name]);

  if (Name <> '') then
  begin
    if hasDeclaration(Name, True) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Name]);

    if (lcoDuplicateLocalNameHints in Options) and
       (FStackInfo <> nil) and hasDeclaration(Name, FStackInfo, False, False) then
      Hint(lphDuplicateLocalName, [Name], DocPos);
  end;

  if (FStackInfo = nil) or (FStackInfo.Owner = nil) then
    Result := addGlobalVar(AVar.NewGlobalVarP(), Name)
  else
    Result := addStackVar(AVar, Name);
end;

function TLapeCompiler.addGlobalVar(AVar: TLapeGlobalVar; AName: lpString = ''): TLapeGlobalVar;
var
  Decl: TLapeDeclaration;
begin
  if (AVar <> nil) then
  begin
    if (AName <> '') then
      AVar.Name := AName;
    if FGlobalDeclarations.Get(AVar.Name, Decl, bTrue) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [AVar.Name]);
    AVar.isConstant := False;
    FGlobalDeclarations.addDeclaration(AVar);
  end;
  Result := AVar;
end;

function TLapeCompiler.addGlobalVar(Typ: lpString; Value: lpString; AName: lpString): TLapeGlobalVar;
var
  OldState: Pointer;
begin
  Typ := AName + ': ' + Typ;
  if (Value <> '') then
    Typ := Typ + ' = ' + Value;
  OldState := getTempTokenizerState(Typ + ';', '!addGlobalVar::' + AName);

  try
    ParseVarBlock().Free();
    Result := getGlobalVar(AName);
    CheckAfterCompile();
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalVar(Typ: lpString; Value: Pointer; AName: lpString): TLapeGlobalVar;
var
  OldState: Pointer;
  VarType: TLapeType;
begin
  OldState := getTempTokenizerState(Typ + ';', '!addGlobalVar::' + AName);
  try
    VarType := ParseType(nil);
    Result := VarType.NewGlobalVarP(Value);
    addGlobalVar(Result, AName);
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalVar(Typ: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(Typ.NewGlobalVarP(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Typ: ELapeBaseType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(FBaseTypes[Typ].NewGlobalVarP(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Int32(FBaseTypes[ltInt32]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_UInt32(FBaseTypes[ltUInt32]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Int64; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Int64(FBaseTypes[ltInt64]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: UInt64; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_UInt64(FBaseTypes[ltUInt64]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Single; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Single(FBaseTypes[ltSingle]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Double; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Double(FBaseTypes[ltDouble]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_AnsiString(FBaseTypes[ltAnsiString]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: UnicodeString; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_UnicodeString(FBaseTypes[ltUnicodeString]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Variant; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Variant(FBaseTypes[ltVariant]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalVar(Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Pointer(FBaseTypes[ltPointer]).NewGlobalVar(Value), AName);
end;

function TLapeCompiler.addGlobalType(Typ: TLapeType; AName: lpString = ''; ACopy: Boolean = True): TLapeType;
var
  Decl: TLapeDeclaration;
begin
  if (Typ <> nil) then
  begin
    if (not ACopy) then
      SetUniqueTypeID(Typ);
    if (AName <> '') then
      Typ.Name := AName;
    if FGlobalDeclarations.Get(Typ.Name, Decl, bTrue) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Typ.Name]);
    FGlobalDeclarations.addDeclaration(Typ);
  end;
  Result := Typ;
end;

function TLapeCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
var
  OldState: Pointer;
begin
  Result := nil;
  OldState := getTempTokenizerState(AName + ' = ' + Str + ';', '!addGlobalType::' + AName);

  try
    ParseTypeBlock();
    Result := getGlobalType(AName);
    CheckAfterCompile();
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  OldState: Pointer;
  p: TDocPos;
begin
  Result := nil;
  OldState := getTempTokenizerState(AHeader + ';', '!addGlobalFuncHdr');

  try
    try
      Expect([tk_kw_Function, tk_kw_Procedure, tk_kw_Property, tk_kw_Operator]);
      Method := ParseMethod(nil, True);
      CheckAfterCompile();

      try
        if (Method.Method = nil) or (not Method.Method.HasType()) or
           (Method.Method.VarType.BaseType <> ltImportedMethod)
        then
          LapeException(lpeInvalidEvaluation);

        Result := Method.Method;
        PPointer(Result.Ptr)^ := Value;
      finally
        FreeAndNil(Method);
      end;
    finally
      resetTokenizerState(OldState);
    end;
  except
    on e: Exception do
    begin
      if (e is lpException) then
        with lpException(e) do
          if (DocPos.FileName <> '') and (DocPos.FileName[1] = '!') then
          begin
            p := DocPos;
            p.FileName := '';

            LapeExceptionFmt(lpeExceptionIn, [Error, AHeader], p);
          end;

      raise;
    end;
  end;
end;

function TLapeCompiler.addGlobalFunc(AHeader: TLapeType_Method; AName, Body: lpString; Pos: PDocPos): TLapeTree_Method;
var
  OldState: Pointer;
begin
  OldState := getTempTokenizerState(Body, '!addGlobalFuncBdy::' + AName);
  Tokenizer.OverridePos := Pos;

  try
    Result := ParseMethod(nil, AHeader, AName);
    CheckAfterCompile();
    addDelayedExpression(Result, True, True);
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; ARes: TLapeType; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalVar(TLapeType_Method(addManagedType(TLapeType_Method.Create(Self, AParams, AParTypes, AParDefaults, ARes))).NewGlobalVar(Value), AName);
  Result.setReadWrite(False, False);
end;

function TLapeCompiler.addGlobalFunc(AParams: array of TLapeType; AParTypes: array of ELapeParameterType; AParDefaults: array of TLapeGlobalVar; Value: Pointer; AName: lpString): TLapeGlobalVar;
begin
  Result := addGlobalFunc(AParams, AParTypes, AParDefaults, nil, Value, AName);
end;

function TLapeCompiler.addGlobalMethod(AFunc: TLapeGlobalVar; Value: TMethod; FreeFunc: Boolean = True): TLapeGlobalVar;
begin
  Assert(AFunc <> nil);

  Result := TLapeType_MethodOfObject(addManagedType(TLapeType_MethodOfObject.Create(AFunc.VarType as TLapeType_Method))).NewGlobalVar(Value, AFunc.Name);
  Result.setReadWrite(False, False);
  TLapeType_MethodOfObject(Result.VarType).HiddenSelf := not (AFunc.VarType is TLapeType_MethodOfObject);

  if (AFunc.DeclarationList <> nil) then
    Result.DeclarationList := AFunc.DeclarationList
  else
  begin
    AFunc.Name := '';
    Result := addGlobalVar(Result);
  end;

  if FreeFunc then
    AFunc.Free();
end;

function TLapeCompiler.addGlobalMethod(AHeader: lpString; Value: TMethod): TLapeGlobalVar;
begin
  Result := addGlobalMethod(addGlobalFunc(AHeader, @Value.Code), Value);
end;

function TLapeCompiler.addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar;
var
  Val: TMethod;
begin
  Val.Code := AMethod;
  Val.Data := ASelf;
  Result := addGlobalMethod(AHeader, Val);
end;

function TLapeCompiler.addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base;
var
  Index: Integer;
  OldState: Pointer;
begin
  if (AFileName = '') then
  begin
    AFileName := '!addDelayedCode';
    if hasTokenizer() and (not Importing) then
      AFileName := AFileName + '::' + Tokenizer.FileName;
  end;

  Index := FDelayedTree.Statements.Count;
  OldState := getTempTokenizerState(ACode, AFileName);

  try
    Result := ParseFile();
    CheckAfterCompile();

    addDelayedExpression(Result);
    FDelayedTree.OverrideDelayGlobal(AfterCompilation, IsGlobal, Index, -1);
  finally
    resetTokenizerState(OldState);
  end;
end;

constructor TLapeType_SystemUnit.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ltUnknown, ACompiler);

  if (ACompiler <> nil) then
    ManagedDeclarations := ACompiler.GlobalDeclarations;
end;

procedure TLapeType_SystemUnit.ClearSubDeclarations;
begin
  //Nothing
end;

function TLapeType_SystemUnit.CanHaveChild: Boolean;
begin
  Result := (FCompiler <> nil);
end;

function TLapeType_SystemUnit.HasChild(AName: lpString): Boolean;
begin
  Result := CanHaveChild() and FCompiler.hasDeclaration(AName, nil);
end;

function TLapeType_SystemUnit.HasChild(ADecl: TLapeDeclaration): Boolean;
begin
  Result := CanHaveChild() and FCompiler.hasDeclaration(ADecl, nil);
end;

function TLapeType_SystemUnit.HasConstantChild(Left: TLapeGlobalVar; AName: lpString): Boolean;
begin
  Result := HasChild(AName);
end;

function TLapeType_SystemUnit.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType;

  function getType(d: TLapeDeclaration): TLapeType;
  begin
    Result := nil;
    if (d is TLapeVar) then
      Result := TLapeVar(d).VarType
    else if (d is TLapeType) then
      Result := TLapeType(d);
  end;

begin
  if (Op = op_Dot) and CanHaveChild() and (Right <> nil) and (Right.BaseType = ltString) then
    Result := getType(FCompiler.getDeclaration(PlpString(Right.Ptr)^, nil))
  else
    Result := nil;
end;

function TLapeType_SystemUnit.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  FieldName: lpString;
  Decl: TLapeTree_ExprBase;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType = Self));

  if (Op = op_Dot) and (Right <> nil) and Right.HasType() and (Right.VarType.BaseType = ltString) then
  begin
    Assert(Right.Ptr <> nil);
    FieldName := PlpString(Right.Ptr)^;

    Result := FCompiler[FieldName];
    if (Result = nil) and (FCompiler is TLapeCompiler) then
    begin
      Decl := TLapeCompiler(FCompiler).getExpression(FieldName, TLapeStackInfo(nil), TLapeCompiler(FCompiler).getPDocPos());
      if (Decl <> nil) then
        Result := TLapeTreeType.Create(Decl).NewGlobalVarP();
    end;
    if (Result = nil) then
      LapeExceptionFmt(lpeUnknownDeclaration, [FieldName])
  end
  else
    Result := inherited;
end;

end.
