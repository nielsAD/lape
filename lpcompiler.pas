{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Script compiler, which turns a script into a syntax tree.
}
unit lpcompiler;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lptokenizer, lptree, lpinvoke;

const
  ParserToken_ExpressionEnd = [tk_sym_SemiColon];
  ParserToken_BlockEnd = [tk_kw_End, tk_kw_Finally, tk_kw_Except, tk_kw_Until];

type
  TLapeTypeForwards = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TLapeType>;

  TLapeCompiler = class;
  TLapeHandleDirective = function(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean of object;
  TLapeHandleExternal = function(Sender: TLapeCompiler; Header: TLapeGlobalVar): Boolean of object;
  TLapeFindFile = procedure(Sender: TLapeCompiler; var FileName: lpString) of object;
  TLapeTokenizerArray = array of TLapeTokenizerBase;

  TLapePreprocessorFunc = function(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean of object;
  TLapePreprocessorFuncs = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TLapePreprocessorFunc>;

  TLapeDefineStack = class(TLapeBaseClass)
  protected type
    TEntry = record
      Name, Value: lpString;
      Hash: UInt32;  // of its name
      Defined: Boolean;
      Prev: Integer; // the entry of its name that was before it
    end;
    TTop = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<Integer>;
  protected
    FEntries: array of TEntry;
    FCount: Integer;
    FFloor: Integer;
    FTop: TTop; // by name: its entry that is on top
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure Push(const AName, AValue: lpString; ADefined: Boolean); virtual;
    function Find(const AName: lpString; out AValue: lpString): TInitBool; virtual;
    procedure Restore(ACount, AFloor: Integer); virtual;
    procedure Hide; virtual;
    function Clone: TLapeDefineStack; virtual;

    property Count: Integer read FCount;
    property Floor: Integer read FFloor;
  end;

  TLapeBaseDefines = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<lpString>;
  TLapeInternalMethods = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TLapeTree_InternalMethodClass>;

  // Parsed after what is declared: the body of Method, or what goes in Block where it was written. Its directives run
  // then, so it goes back to the place it starts (State) with what was defined there, and reads to where the skip ended
  EDeferred = (dkBody, dkBeginEnd, dkVars, dkStatement);
  TLapeDeferredBody = record
    Kind: EDeferred;
    Method: TLapeTree_Method;
    Block: TLapeTree_StatementList;
    Stack: TLapeStackInfo;
    State: Pointer;
    Defines: TLapeDefineStack;
    StopAt: Integer;
  end;

  TLapeConditional = record
    Eval: Boolean;
    Pos: TDocPos;
  end;
  TLapeConditionalStack = {$IFDEF FPC}specialize{$ENDIF} TLapeStack<TLapeConditional>;

  PCompilerState = ^TCompilerState;
  TCompilerState = record
    Tokenizer: Integer;
    Tokenizers: array of TLapeTokenizerBase;
    TokStates: array of Pointer;
    InPeek: Boolean;
    LastTok: EParserToken;
    Options: ECompilerOptionsSet;
    Options_PackRecords: UInt8;
    DefineCount, DefineFloor: Integer;
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
    function HasTokenizer: Boolean; inline;
    function HasMoreTokenizers: Boolean; inline;
    function incTokenizerLock(ATokenizer: TLapeTokenizerBase): TLapeTokenizerBase;
    procedure decTokenizerLock(var ATokenizer: TLapeTokenizerBase; DoFree: Boolean = True);
  protected
    FTokenizers: TLapeTokenizerArray;
    FTokenizer: Integer;
    FInPeek: Boolean;
    FLastTok: EParserToken;

    // What is compiled, in this field order
    FPreTree: TLapeTree_StatementList;
    FTree: TLapeTree_Base; // the script
    FPostTree: TLapeTree_StatementList;
    FMethods: TLapeTree_StatementList; // last: one jump over them all (so methods aren't called once execution ends) and what compiling adds joins the end

    FIncludes: TStringList;
    FBaseDefines: TLapeBaseDefines;
    // By name: what a script declares goes before them, see getExpression
    FInternalMethods: TLapeInternalMethods;
    // What is declared comes first: bodies are parsed at the end of the file, where all of it is known
    FDeferring: Boolean;
    FDeferred: array of TLapeDeferredBody;
    // Reading what is parsed later: directives wait for that
    FSkipping: Boolean;
    FDefines: TLapeDefineStack;
    FConditionalStack: TLapeConditionalStack;

    FOnHandleDirective: TLapeHandleDirective;
    FOnHandleExternal: TLapeHandleExternal;
    FOnFindFile: TLapeFindFile;

    FPreprocessorFuncs: TLapePreprocessorFuncs;
    FPreprocessorMacros: TLapePreprocessorFuncs;

    function getDocPos: TDocPos; override;
    procedure Reset; override;

    // Internal methods are declarations of the global scope, by name
    procedure setInternalMethod(AName: lpString; AClass: TLapeTree_InternalMethodClass); virtual;
    procedure addInternalMethods; virtual;

    function getTokenizer: TLapeTokenizerBase; virtual;
    procedure setTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    procedure pushTokenizer(ATokenizer: TLapeTokenizerBase); virtual;
    function popTokenizer: TLapeTokenizerBase; virtual;
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos); virtual;
    function popConditional: TDocPos; virtual;


    procedure InitBaseDefinitions; virtual;
    procedure InitBaseMath; virtual;
    procedure InitBaseString; virtual;
    procedure InitBaseDateTime; virtual;
    procedure InitBaseVariant; virtual;
    procedure InitBaseFile; virtual;

    function EnsureExpression(Node: TLapeTree_ExprBase): TLapeTree_ExprBase; virtual;
    // Lo..Hi of a range of constants, or of an ordinal type (VarType: that of Hi, or the type)
    function EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange; virtual;

    function FindFile(AFileName: lpString): lpString; virtual;
    function HandlePreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean; virtual;
    function HandlePreprocessorMacro(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean; virtual;
    function EvalPreprocessorExpr(Expr: String; ADocPos: TDocPos): Boolean; virtual;
    function EvalPreprocessorMacro(Argument: String; ADocPos: TDocPos): Boolean; virtual;

    function HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean; virtual;
    function InIgnore: Boolean; virtual;
    procedure UnknownCharacter; virtual;
    function Next: EParserToken; virtual;
    function isNext(Tokens: EParserTokenSet; out Token: EParserToken): Boolean; overload; virtual;
    function isNext(Tokens: EParserTokenSet): Boolean; overload; virtual;
    function Peek: EParserToken; virtual;
    function Expect(Token: EParserToken; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;
    function Expect(Tokens: EParserTokenSet; NextBefore: Boolean = True; NextAfter: Boolean = False): EParserToken; overload; virtual;

    procedure ParseDeclHint(ADecl: TLapeDeclaration); virtual;

    procedure ParseExpressionEnd(Token: EParserToken; NextBefore: Boolean = False; NextAfter: Boolean = True); overload; virtual;
    procedure ParseExpressionEnd(Tokens: EParserTokenSet = ParserToken_ExpressionEnd; NextBefore: Boolean = False; NextAfter: Boolean = True); overload; virtual;
    function ParseIdentifierList(FirstNext: Boolean = False): TStringArray; overload; virtual;
    function ParseIdentifierList(out Positions: TDocPosArray; FirstNext: Boolean = False): TStringArray; overload; virtual;

    function ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList; virtual;
    function StartSkip: TLapeDeferredBody; virtual;
    procedure EndSkip(var Body: TLapeDeferredBody); virtual;
    procedure FreeDeferred(var Body: TLapeDeferredBody); virtual;
    procedure SkipBody(Depth: Integer); virtual;
    procedure SkipVars; virtual;
    procedure SkipStatement; virtual;
    procedure addDeferred(const Body: TLapeDeferredBody; Kind: EDeferred; Method: TLapeTree_Method; Block: TLapeTree_StatementList); virtual;
    function ParseDeferred(var Body: TLapeDeferredBody; Kind: EDeferred; Method: TLapeTree_Method): TLapeTree_Base; virtual;
    function DeferStatement: TLapeTree_Base; virtual;
    procedure DeferBody(Method: TLapeTree_Method; Block: TLapeTree_StatementList); virtual;
    function DeferVars: TLapeTree_Base; virtual;
    procedure ParseDeferredBodies; virtual;
    function ParseType(TypeForwards: TLapeTypeForwards; addToStackOwner: Boolean = False): TLapeType; virtual;
    procedure ParseTypeBlock; virtual;
    procedure ParseLabelBlock; virtual;
    function ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList; virtual;

    function ParseExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True; DoFold: Boolean = True): TLapeTree_ExprBase; virtual;
    function ParseTypeExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True): TLapeTree_Base; virtual;
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
    constructor Create(ATokenizer: TLapeTokenizerBase); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; override;

    function getState: Pointer; virtual;
    procedure setState(const State: Pointer); virtual;
    procedure freeState(const State: Pointer); virtual;

    function getTempTokenizerState(const AStr: lpString; const AFileName: lpString = ''): Pointer; virtual;
    procedure resetTokenizerState(const State: Pointer); virtual;

    function ParseFile: TLapeTree_Base; virtual;

    function Compile: Boolean; virtual;

    procedure VarToDefault(AVar: TResVar); override;

    procedure checkDeclHints(ADecl: TLapeDeclaration); overload; virtual;
    procedure checkDeclHints(AStack: TLapeStackInfo); overload; virtual;
    function hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False): Boolean; overload; override;
    function getExpression(AName: lpString): TLapeTree_ExprBase; virtual;

    function HasBaseDefine(AName: lpString): Boolean; virtual;
    function HasDefine(AName: lpString): Boolean; virtual;
    function getDefine(AName: lpString; out AValue: lpString): Boolean; virtual;
    procedure addBaseDefine(AName: lpString; AValue: lpString = ''); virtual;
    procedure addDefine(AName: lpString; AValue: lpString = ''); virtual;
    procedure addPreprocessorFunc(AName: lpString; Func: TLapePreprocessorFunc); virtual;
    procedure addPreprocessorMacro(AName: lpString; Func: TLapePreprocessorFunc); virtual;


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

    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; virtual;

    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; overload; virtual;
    function addGlobalFunc(AHeader: TLapeType_Method; Body: lpString): TLapeTree_Method; overload; virtual;

    function addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar; virtual;
    procedure addCode(ACode: lpString; AFileName: lpString = ''; AfterScript: Boolean = True); virtual;

    property InternalMethodMap[AName: lpString]: TLapeTree_InternalMethodClass write setInternalMethod;
    property Tokenizer: TLapeTokenizerBase read getTokenizer write setTokenizer;
    property OnHandleDirective: TLapeHandleDirective read FOnHandleDirective write FOnHandleDirective;
    property OnHandleExternal: TLapeHandleExternal read FOnHandleExternal write FOnHandleExternal;
    property OnFindFile: TLapeFindFile read FOnFindFile write FOnFindFile;
  end;

implementation

uses
  Variants,
  lpvartypes_ord, lpvartypes_record, lpvartypes_array, lpvartypes_object,
  lpinternalmethods, lpinternalmethods_algorithm,
  lpmessages, lpeval, lpinterpreter_types, lpvartypes_helper, lpmethodparser, lpexpressionparser, lpgeneratedmethods;

constructor TLapeDefineStack.Create;
begin
  inherited Create();

  FTop := TTop.Create(-1);
end;

destructor TLapeDefineStack.Destroy;
begin
  FTop.Free();

  inherited;
end;

procedure TLapeDefineStack.Clear;
begin
  FEntries := nil;
  FCount := 0;
  FFloor := 0;
  FTop.Clear();
end;

procedure TLapeDefineStack.Push(const AName, AValue: lpString; ADefined: Boolean);
var
  Hash: UInt32;
  Prev: Integer;
begin
  Hash := LapeHashAlpha(AName);
  if FTop.Find(AName, Hash, Prev) then
    FTop.Remove(AName, Hash, Prev)
  else
    Prev := -1;
  FTop.Add(AName, Hash, FCount);

  if (FCount = Length(FEntries)) then
    SetLength(FEntries, 4 + FCount * 2);
  FEntries[FCount].Name := AName;
  FEntries[FCount].Value := AValue;
  FEntries[FCount].Hash := Hash;
  FEntries[FCount].Defined := ADefined;
  FEntries[FCount].Prev := Prev;
  Inc(FCount);
end;

function TLapeDefineStack.Find(const AName: lpString; out AValue: lpString): TInitBool;
var
  i: Integer;
begin
  Result := bUnknown;
  AValue := '';
  if (FCount = 0) or (not FTop.Find(AName, LapeHashAlpha(AName), i)) or (i < FFloor) then
    Exit;

  if FEntries[i].Defined then
  begin
    AValue := FEntries[i].Value;
    Result := bTrue;
  end
  else
    Result := bFalse;
end;

procedure TLapeDefineStack.Restore(ACount, AFloor: Integer);
begin
  Assert(ACount <= FCount);
  while (FCount > ACount) do
  begin
    Dec(FCount);
    FTop.Remove(FEntries[FCount].Name, FEntries[FCount].Hash, FCount);
    if (FEntries[FCount].Prev > -1) then
      FTop.Add(FEntries[FCount].Name, FEntries[FCount].Hash, FEntries[FCount].Prev);
  end;
  FFloor := AFloor;
end;

procedure TLapeDefineStack.Hide;
begin
  FFloor := FCount;
end;

function TLapeDefineStack.Clone: TLapeDefineStack;
var
  i: Integer;
begin
  Result := TLapeDefineStack.Create();
  for i := 0 to FCount - 1 do
    Result.Push(FEntries[i].Name, FEntries[i].Value, FEntries[i].Defined);
  Result.FFloor := FFloor;
end;

function TLapeCompiler.HasTokenizer: Boolean;
begin
  Result := FTokenizer >= 0;
end;

function TLapeCompiler.HasMoreTokenizers: Boolean;
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

// Where the node being compiled is, or else the token being parsed
function TLapeCompiler.getDocPos: TDocPos;
begin
  if (FEmitter <> nil) and (not FEmitter.DocPos.isEmpty) then
    Result := FEmitter.DocPos
  else if HasTokenizer() then
    Result := Tokenizer.DocPos
  else
    Result := NullDocPos;
end;

procedure TLapeCompiler.Reset;
var
  i: Integer;
begin
  inherited;

  FOptions := FBaseOptions;
  FOptions_PackRecords := FBaseOptions_PackRecords;

  FTokenizer := High(FTokenizers);
  while HasMoreTokenizers() do
    popTokenizer();

  if (FTokenizer = 0) and (FTokenizers[0] <> nil) then
    FTokenizers[0].Reset();

  FreeAndNil(FTree);

  if (FMethods <> nil) then
    for i := FMethods.Statements.Count - 1 downto 0 do
      if (not TLapeTree_Method(FMethods.Statements[i]).Imported) then
        FMethods.Statements[i].Free();

  if (FIncludes <> nil) then
    FIncludes.Clear();
  if (FConditionalStack <> nil) then
    FConditionalStack.Clear();
  if (FDefines <> nil) then
    FDefines.Clear();
end;

function TLapeCompiler.getTokenizer: TLapeTokenizerBase;
begin
  if HasTokenizer() then
    Result := FTokenizers[FTokenizer]
  else
    Result := nil;
end;

procedure TLapeCompiler.setTokenizer(ATokenizer: TLapeTokenizerBase);
begin
  if HasTokenizer() and (FTokenizer < Length(FTokenizers)) and (Tokenizer <> ATokenizer) then
    decTokenizerLock(FTokenizers[FTokenizer]);

  if (not HasTokenizer()) then
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
begin
  Inc(FTokenizer);
  setTokenizer(ATokenizer);

  if (ATokenizer <> nil) then
    ATokenizer.RelativeFileNames := (lcoRelativeFileNames in FOptions);
end;

function TLapeCompiler.popTokenizer: TLapeTokenizerBase;
begin
  Result := Tokenizer;
  setTokenizer(nil);
  Dec(FTokenizer);
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

procedure TLapeCompiler.InitBaseDefinitions;
begin

  addBaseDefine('Lape');
  addBaseDefine('Sesquipedalian');

  addBaseDefine('Lape_CaseInsensitive');
  addBaseDefine('Lape_PascalLabels');
  {$IFDEF Lape_Unicode}
  addBaseDefine('Lape_Unicode');
  {$ELSE}
  addBaseDefine('Lape_Ansi');
  {$ENDIF}
  {$IFDEF Lape_NoExtended}
  addBaseDefine('Lape_NoExtended');
  {$ENDIF}

  addGlobalVar(addManagedType(TLapeType_NilPointer.Create(Self, nil, False)).NewGlobalVarP(), 'nil').isConstant := True;

  addGlobalType(TLapeType_Label.Create(Self), '!label');
  addGlobalType(TLapeType_Pointer.Create(Self, nil, True), 'ConstPointer');
  addGlobalType(getBaseType(ltString).CreateAlias(), 'string');
  addGlobalType(getBaseType(ltChar).CreateAlias(), 'Char');
  addGlobalType('packed record Method, Self: Pointer; end;', 'TMethod');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(SizeInt), True)).CreateAlias(), 'SizeInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(SizeUInt), False)).CreateAlias(), 'SizeUInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(NativeInt), True)).CreateAlias(), 'NativeInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(NativeUInt), False)).CreateAlias(), 'NativeUInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(PtrInt), True)).CreateAlias(), 'PtrInt');
  addGlobalType(getBaseType(DetermineIntType(SizeOf(PtrUInt), False)).CreateAlias(), 'PtrUInt');

  addGlobalType('array of String', 'TStringArray');
  addGlobalType('array of Boolean', 'TBooleanArray');
  addGlobalType('array of Int32', 'TIntegerArray');
  addGlobalType('array of Single', 'TSingleArray');
  addGlobalType('array of Double', 'TDoubleArray');
  {$IFNDEF Lape_NoExtended}
  addGlobalType('array of Extended', 'TExtendedArray');
  {$ENDIF}

  addGlobalVar(getBaseType(ltBoolean).NewGlobalVarStr('1'), 'True').isConstant := True;
  addGlobalVar(getBaseType(ltBoolean).NewGlobalVarStr('0'), 'False').isConstant := True;

  addGlobalFunc('function _LocationToStr(DocPos: Pointer): string;', @_LapeLocationToStr);

  addGlobalFunc('procedure _Write(s: string);', @_LapeWrite);
  addGlobalFunc('procedure _WriteLn();', @_LapeWriteLn);

  addGlobalFunc('procedure _Assert(Expr: Boolean); overload;', @_LapeAssert);
  addGlobalFunc('procedure _Assert(Expr: Boolean; Msg: string); overload;', @_LapeAssertMsg);
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

  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int32; SortUp: Boolean); overload;', @_LapeArraySortWeighted_Int32);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of UInt32; SortUp: Boolean); overload;', @_LapeArraySortWeighted_UInt32);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int64; SortUp: Boolean); overload;', @_LapeArraySortWeighted_Int64);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of UInt64; SortUp: Boolean); overload;', @_LapeArraySortWeighted_UInt64);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Single; SortUp: Boolean); overload;', @_LapeArraySortWeighted_Single);
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Double; SortUp: Boolean); overload;', @_LapeArraySortWeighted_Double);
  {$IFNDEF Lape_NoExtended}
  addGlobalFunc('procedure _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Extended; SortUp: Boolean); overload;', @_LapeArraySortWeighted_Extended);
  {$ENDIF}

  addGlobalFunc('procedure _ArrayReverse(A: Pointer; ElSize, Len: SizeInt); overload;', @_LapeArrayReverse);

  addGlobalFunc('function GetMem(i: SizeInt): Pointer;', @_LapeGetMem);
  addGlobalFunc('function AllocMem(i: SizeInt): Pointer;', @_LapeAllocMem);
  addGlobalFunc('procedure FreeMem(p: Pointer);', @_LapeFreeMem);
  addGlobalFunc('procedure ReallocMem(var p: Pointer; s: SizeInt);', @_LapeReallocMem);

  addGlobalFunc('procedure FillMem(var p; s: SizeInt; b: UInt8 = 0);', @_LapeFillMem);
  addGlobalFunc('procedure Move(constref Src; var Dst; s: SizeInt);', @_LapeMove);
  addGlobalFunc('function CompareMem(constref p1, p2; Length: SizeInt): Boolean;', @_LapeCompareMem);
  addGlobalFunc('procedure _Move(var Src, Dst; s: SizeInt);', @_LapeMove);
  addGlobalFunc('function _CompareMem(var p1, p2; s: SizeInt): Boolean;', @_LapeCompareMem);

  addGlobalFunc('function Assigned(constref p): Boolean;', @_LapeAssigned);

  addGlobalFunc('function BitCount(constref Value; Size: UInt8): UInt32;', @_LapeBitCount);

  addGlobalFunc('procedure RaiseException(Message: string); overload;', @_LapeRaiseString);

  addGlobalFunc('procedure UniqueString(var Str: AnsiString); overload;', @_LapeAStr_Unique);
  addGlobalFunc('procedure UniqueString(var Str: WideString); overload;', @_LapeWStr_Unique);
  addGlobalFunc('procedure UniqueString(var Str: UnicodeString); overload;', @_LapeUStr_Unique);

  LapeAddGeneratedMethods(Self);

  InitBaseMath();
  InitBaseString();
  InitBaseDateTime();
  InitBaseVariant();
  InitBaseFile();

  addCode(
    LapeCodeFlags +
    Format(_LapeToString_Set, ['Small', High(TLapeSmallSetItem)]) +
    Format(_LapeToString_Set, ['Large', High(TLapeLargeSetItem)]) +
    _LapeToString_Array +
    _LapeSwap +
    _LapeSetLength +
    _LapeObjectSetLength +
    _LapeCopy +
    _LapeDelete +
    _LapeInsert +
    _LapeArraySlice,
    '!addCore'
  );

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
   Result := TLapeTree_ExprBase.Create(Self);
end;

function TLapeCompiler.EnsureConstantRange(Node: TLapeTree_Base; out VarType: TLapeType): TLapeRange;
var
  Lo, Hi: TLapeGlobalVar;
begin
  VarType := nil;
  if (Node is TLapeTree_Range) and (TLapeTree_Range(Node).Lo <> nil) and (TLapeTree_Range(Node).Hi <> nil) then
  begin
    VarType := TLapeTree_Range(Node).Hi.resType();
    if (VarType = nil) or (not VarType.CompatibleWith(TLapeTree_Range(Node).Lo.resType())) then
      LapeException(lpeInvalidRange, Node.DocPos);
    Lo := TLapeTree_Range(Node).Lo.Evaluate();
    Hi := TLapeTree_Range(Node).Hi.Evaluate();
  end
  // (array[TEnum]: the first to the last of a type)
  else if (Node is TLapeTree_VarType) and (TLapeTree_VarType(Node).VarType <> nil) then
  begin
    VarType := TLapeTree_VarType(Node).VarType;
    Lo := VarType.VarLo();
    Hi := VarType.VarHi();
  end
  else
    LapeException(lpeInvalidRange, [Node, Self]);

  if (Lo = nil) or (not Lo.HasType()) or (not (Lo.VarType.BaseType in LapeOrdinalTypes)) or
     (Hi = nil) or (not Hi.HasType()) or (not (Hi.VarType.BaseType in LapeOrdinalTypes))
  then
    LapeException(lpeInvalidRange, Node.DocPos)
  else if (not Lo.Known) or (not Hi.Known) then
    LapeException(lpeConstantExpected, Node.DocPos);

  Result.Lo := Lo.AsInteger;
  Result.Hi := Hi.AsInteger;
  if (Result.Hi < Result.Lo) then
    LapeException(lpeInvalidRange, Node.DocPos);
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

  Result := '';
end;

procedure TLapeCompiler.addPreprocessorFunc(AName: lpString; Func: TLapePreprocessorFunc);
begin
  if FPreprocessorFuncs.ExistsKey(AName) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName]);
  FPreprocessorFuncs.Add(LapeUpperCase(AName), Func);
end;

procedure TLapeCompiler.addPreprocessorMacro(AName: lpString; Func: TLapePreprocessorFunc);
begin
  if FPreprocessorMacros.ExistsKey(AName) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName]);
  FPreprocessorMacros.Add(LapeUpperCase(AName), Func);
end;

function TLapeCompiler.HandlePreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
begin
  Result := True;

  if (Name = 'DEFINED') then
    Value := BoolToStr(HasDefine(Argument), True)
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
    Value := #39 + GetMethodName(FStackInfo.Method) + #39
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

  function HasFunc(const Expr, Name: lpString; out StartPos, EndPos: Integer; out Param: lpString): Boolean;
  var
    Upper: lpString;
  begin
    Upper := LapeUpperCase(Expr);

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
    while HasFunc(Expr, FPreprocessorFuncs.Key[i], StartPos, EndPos, Param) do
    begin
      if not FPreprocessorFuncs.ItemsI[i](Self, FPreprocessorFuncs.Key[i], Param, Value) then
        LapeExceptionFmt(lpeCannotEvalPreprocessorFunc, [FPreprocessorFuncs.Key[i]], ADocPos);

      // replace `Defined(hello)` with `Value`
      Delete(Expr, startPos, (endPos - startPos) + 1);
      Insert(Value, Expr, startPos);
    end;

  ExprBase := nil;
  OldState := getTempTokenizerState(Expr);
  try
    try
      ExprBase := ParseExpression();
      if (ExprBase = nil) then
        LapeException(lpeCannotEvalConst, ADocPos);
      Res := ExprBase.Evaluate();
      if (Res = nil) or (not (Res.BaseType in LapeBoolTypes)) then
        LapeException(lpeExpectedBoolExpression, ADocPos);

      Result := Res.AsInteger <> 0;
    finally
      ExprBase.Free();
      resetTokenizerState(OldState);
    end;
  except
    on E: lpException do
      LapeExceptionReraise(E, ADocPos);
  end;
end;

function TLapeCompiler.EvalPreprocessorMacro(Argument: String; ADocPos: TDocPos): Boolean;
var
  Name, Param, Value: lpString;
  StartPos, EndPos: Integer;
  Macro: TLapePreprocessorFunc;
  MacroTokenizer: TLapeTokenizerBase;
begin
  Result := False;

  StartPos := Pos('(', Argument);
  if (StartPos > 0) then
    EndPos := Pos(')', Argument, StartPos);
  if (StartPos > 0) and (EndPos > 0) then
  begin
    Param := Copy(Argument, StartPos + 1, (EndPos - StartPos) - 1);
    Name := Trim(Copy(Argument, 1, StartPos - 1));
  end else
  begin
    Param := '';
    Name := Trim(Argument);
  end;
  Argument := LapeUpperCase(Name);

  // (what a built-in macro gives is in a file of its own, named after it)
  if FPreprocessorMacros.Find(Argument, Macro) then
  begin
    if Macro(Self, Argument, Param, Value) and (Value <> '') then
      pushTokenizer(TLapeTokenizerString.Create(Value, 'macro ' + Name));

    Result := True;
    Exit;
  end;

  // (a define's value is where the macro is used)
  if getDefine(Argument, Value) then
  begin
    MacroTokenizer := TLapeTokenizerString.Create(Value, Tokenizer.FileName);
    MacroTokenizer.FixedPos := ADocPos;
    pushTokenizer(MacroTokenizer);

    Result := True;
    Exit;
  end;

  LapeExceptionFmt(lpeUnknownDeclaration, [string(Argument)], ADocPos);
end;

function TLapeCompiler.HandleDirective(Sender: TLapeTokenizerBase; Directive, Argument: lpString): Boolean;

  function HasDefineOrOption(Name: lpString): Boolean;
  begin
    Result := HasDefine(Name) or HasBaseDefine(Name);
    if Result then
      Exit;

    Name := LapeLowerCase(Name);
    if (Name = 'assertions') then
      Result := (lcoAssertions in FOptions)
    else
    if (Name = 'rangechecks') then
      Result := (lcoRangeCheck in FOptions)
    else
    if (Name = 'memoryinit') then
      Result := (lcoAlwaysInitialize in FOptions)
    else
    if (Name = 'loosesemicolon') then
      Result := (lcoLooseSemicolon in FOptions)
    else
    if (Name = 'extendedsyntax') then
      Result := (lcoLooseSyntax in FOptions)
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
    Argument := LapeLowerCase(Argument);

    if (Argument = 'on') or (Argument = '+') then
      Include(FOptions, Option)
    else if (Argument = 'off') or (Argument = '-') then
      Exclude(FOptions, Option)
    else
      LapeExceptionFmt(lpeExpected, ['On or off'], Sender.DocPos);
  end;

  procedure setAlignment(Argument: lpString);
  begin
    Argument := LapeLowerCase(Argument);

    if (Argument = 'on') then
      FOptions_PackRecords := Lape_PackRecordsDef
    else if (Argument = 'off') then
      FOptions_PackRecords := 1
    else
    begin
      FOptions_PackRecords := StrToIntDef(string(Argument), 0);
      if (not (FOptions_PackRecords in [1, 2, 4, 8, 16])) then
        LapeExceptionFmt(lpeExpected, ['1, 2, 4, 8 or 16'], Sender.DocPos);
    end;
  end;

  procedure handleDefine(Directive, Argument: lpString);
  var
    p: Integer;
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
        LapeExceptionFmt(lpeExpected, ['Directive name'], Sender.DocPos);

      addDefine(Name, Value);
    end else
    if (Directive = 'undef') and HasDefine(Argument) then
      FDefines.Push(Argument, '', False);
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

    if (not FInPeek) then
      if (FIncludes.IndexOf(string(IncludeFile)) < 0) then
        FIncludes.Add(string(IncludeFile));

    // (a macro's tokenizer can have the file's name too)
    if (FTokenizer + 1 < Length(FTokenizers)) and (FTokenizers[FTokenizer + 1] is TLapeTokenizerFile) and (FTokenizers[FTokenizer + 1].FileName = IncludeFile) then
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
    if FOnHandleDirective(Self, Directive, Argument, FInPeek, InIgnore()) then
      Exit;

  Directive := LapeLowerCase(Directive);
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
      pushConditional(getDeclaration(Argument, nil, True) is TLapeParameterVar, Sender.DocPos)
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
    if (Directive = 'm') or (Directive = 'memoryinit') then
      setOption(lcoAlwaysInitialize)
    else
    if (Directive = 'l') or (Directive = 'loosesemicolon') then
      setOption(lcoLooseSemicolon)
    else
    if (Directive = 'x') or (Directive = 'extendedsyntax') then
      setOption(lcoLooseSyntax)
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
  Result := (FConditionalStack.Count > 0) and (not FConditionalStack.Top.Eval);
end;

procedure TLapeCompiler.UnknownCharacter;
var
  Text: lpString;
begin
  Text := Tokenizer.TokString;
  if (Length(Text) = 1) and ((Text[1] < #33) or (Text[1] > #126)) then
    Text := '#' + lpString(IntToStr(Ord(Text[1])));
  LapeExceptionFmt(lpeUnknownCharacter, [Text], Tokenizer.DocPos);
end;

function TLapeCompiler.Next: EParserToken;
var
  PrevTok: EParserToken;
begin
  PrevTok := Tokenizer.Tok;
  repeat
    Tokenizer.KeepDirectives := FSkipping and (not FInPeek);
    Result := Tokenizer.Next{NoWhiteSpace}();
    if (Result = tk_NULL) and HasMoreTokenizers() and (Tokenizer.StopAt = 0) then
    begin
      if FInPeek then
        Dec(FTokenizer)
      else
        popTokenizer();
      Tokenizer.KeepDirectives := FSkipping and (not FInPeek);
      Result := Tokenizer.Next{NoWhiteSpace}();
    end;
  until (Result = tk_NULL) or ((not (Result in TokJunk)) and (not InIgnore()));
  FLastTok := PrevTok;

  if (Result = tk_Unknown) then
    UnknownCharacter();
end;

function TLapeCompiler.isNext(Tokens: EParserTokenSet; out Token: EParserToken): Boolean;
var
  OldState: Pointer;
begin
  Result := False;
  OldState := getState();
  try
    FInPeek := True;
    Token := Next();
    Result := Token in Tokens;
  finally
    if (not Result) then
      setState(OldState)
    else
    begin
      freeState(OldState);
      FInPeek := False;
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
    FInPeek := True;
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

procedure TLapeCompiler.ParseDeclHint(ADecl: TLapeDeclaration);
begin
  Assert(Tokenizer.Tok in [tk_kw_Deprecated, tk_kw_Experimental, tk_kw_UnImplemented]);

  case Tokenizer.Tok of
    tk_kw_Deprecated:
      if (Peek() in [tk_typ_String, tk_typ_HereString]) then
      begin
        Next();
        ADecl.AddHint(ldhDeprecated, Copy(Tokenizer.TokString, 2, Length(Tokenizer.TokString) - 2));
      end else
        ADecl.AddHint(ldhDeprecated);

    tk_kw_Experimental:
      ADecl.AddHint(ldhExperimental);

    tk_kw_UnImplemented:
      ADecl.AddHint(ldhUnImplemented);
  end;

  Next();
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
var
  Positions: TDocPosArray;
begin
  Result := ParseIdentifierList(Positions, FirstNext);
end;

function TLapeCompiler.ParseIdentifierList(out Positions: TDocPosArray; FirstNext: Boolean = False): TStringArray;
var
  Count: Integer;
begin
  SetLength(Result, 4);
  SetLength(Positions, 4);
  Count := 0;
  repeat
    Expect(tk_Identifier, FirstNext, False);
    FirstNext := True;

    if (Count = Length(Result)) then
    begin
      SetLength(Result, Count * 2);
      SetLength(Positions, Count * 2);
    end;
    Result[Count] := Tokenizer.TokString;
    Positions[Count] := Tokenizer.DocPos;
    Inc(Count);
  until (Next() <> tk_sym_Comma);

  SetLength(Result, Count);
  SetLength(Positions, Count);
end;

function TLapeCompiler.ParseBlockList(StopAfterBeginEnd: Boolean = True): TLapeTree_StatementList;
var
  Statement: TLapeTree_Base;
  Method: TLapeTree_Method;
  DoBreak: Boolean;
begin

  Result := TLapeTree_StatementList.Create(Self);
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
            begin
              Statement := TLapeTree_StatementList.Create(Self);
              Result.addStatement(Statement);
              DeferBody(nil, TLapeTree_StatementList(Statement));
              Statement := nil;
            end;
            DoBreak := (Tokenizer.Tok = tk_sym_Dot) or StopAfterBeginEnd;
          end;
        tk_kw_Const: Statement := ParseVarBlock();
        tk_kw_Var:
          if (not StopAfterBeginEnd) then
            Statement := DeferVars()
          else
            Statement := ParseVarBlock();
        tk_kw_Function, tk_kw_Procedure, tk_kw_Operator, tk_kw_Property:
          begin
            // (nil: external)
            Method := TLapeMethodParser(Self).ParseMethod();
            if (Method <> nil) then
              FMethods.addStatement(Method);
          end;
        tk_kw_Type: ParseTypeBlock();

        tk_kw_Label: ParseLabelBlock();

        else if (lcoLooseSyntax in FOptions) and (not StopAfterBeginEnd) and (not (Tokenizer.Tok in ParserToken_BlockEnd)) then
          Statement := DeferStatement()
        else
          LapeException(lpeBlockExpected, Tokenizer.DocPos);
      end;

      if (Statement <> nil) then
        Result.addStatement(Statement);
    until DoBreak;

  except
    Result.Free();
    raise;
  end;

  if (Result.Statements.Count < 1) then
    FreeAndNil(Result);
end;

function TLapeCompiler.ParseType(TypeForwards: TLapeTypeForwards; addToStackOwner: Boolean = False): TLapeType;

  procedure ParseArray;
  var
    TypeExpr: TLapeTree_Base;
    Range: TLapeRange;
    RangeType: TLapeType;
    DocPos: TDocPos;
  begin
    DocPos := Tokenizer.DocPos;

    Expect([tk_sym_BracketOpen, tk_kw_Of], True, False);
    if (Tokenizer.Tok = tk_sym_BracketOpen) then
    begin
      TypeExpr := ParseTypeExpression();
      try
        Range := EnsureConstantRange(TypeExpr, RangeType);
        Expect(tk_sym_BracketClose, False, False);
        Expect(tk_kw_Of, True, False);
        Result := addManagedType(TLapeType_StaticArray.Create(Range, ParseType(nil, addToStackOwner), Self, ''));
        Result.DocPos := DocPos;
      finally
        if (TypeExpr <> nil) then
          TypeExpr.Free();
      end;
    end
    else
    begin
      Result := addManagedType(TLapeType_DynArray.Create(ParseType(nil, addToStackOwner), Self, ''));
      Result.DocPos := DocPos;
    end;
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
        if Result.HasMember(Identifiers[0]) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[0]], DocPos);

        if Expect([tk_sym_Colon, tk_sym_Equals], False, False) = tk_sym_Colon then
          FieldType := ParseType(nil);

        if Expect([tk_sym_SemiColon, tk_sym_Equals], False, False) = tk_sym_Equals then
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
        Expect(tk_sym_SemiColon, False, False);

        for i := 0 to High(Identifiers) do
        begin
          if Result.HasMember(Identifiers[i]) then
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
      Expect(tk_sym_ParenthesisClose, False, True);
      Result := FieldType.CreateCopy() as TLapeType_Record;
      // record(TParent); is the parent's fields only
      if (Tokenizer.Tok = tk_sym_SemiColon) then
      begin
        Result := addManagedType(Result);
        Exit;
      end;
    end
    else
    begin
      if IsRecord then
        Result := TLapeType_Record.Create(Self, nil, '')
      else
        Result := TLapeType_Union.Create(Self, nil, '');
      Result.DocPos := getDocPos();
    end;

    ParseFields(IsPacked);
    Next();

    Result := addManagedType(Result);
  end;

  procedure ParseObject;
  begin
    Result := TLapeType_Object.Create(Self, nil, '');
    Result.DocPos := getDocPos();
    SetUniqueTypeID(Result); // important for Dispose
    Next();
    ParseFields(True);
    Next();
    Result := addManagedType(Result);
  end;

  procedure ParseSet;
  var
    Given: TLapeType;
    TypePos: TDocPos;
  begin
    TypePos := Tokenizer.DocPos;
    Expect(tk_kw_Of, True, False);
    Given := ParseType(nil, addToStackOwner);
    if (not (Given.BaseType in [ltSmallEnum, ltLargeEnum, ltAnsiChar, ltUInt8])) then
      LapeExceptionFmt(lpeInvalidSetType, [Given.AsString], TypePos);

    try
      Result := addManagedType(TLapeType_Set.Create(Given, Self, ''));
      Result.DocPos := getDocPos();
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
      PointerType := TLapeType(getDeclaration(Tokenizer.TokString, nil, False, False));
      if not (PointerType is TLapeType_Pointer) then
        LapeException(lpeExpectedPointerType, DocPos);

      if (PointerType.ClassType = TLapeType_Pointer) then
      begin
        Result := addManagedType(TLapeType_StrictPointer.Create(Self, nil, False, ''));
        Result.DocPos := DocPos;
      end
      else
        Result := addManagedType(PointerType.CreateCopy());

      Next();
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

    PointerType := TLapeType(getDeclaration(Tokenizer.TokString, nil, False, False));
    if ((PointerType = nil) and (TypeForwards = nil)) or ((PointerType <> nil) and (not (PointerType is TLapeType))) then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    if (PointerType <> nil) then
    begin
      Result := addManagedType(TLapeType_Pointer.Create(Self, PointerType, ConstPointer, ''));
      Result.DocPos := DocPos;
    end
    else
    begin
      Result := TLapeType_Pointer.Create(Self, PointerType, ConstPointer, '');
      Result.DocPos := DocPos;
      TypeForwards.Add(Tokenizer.TokString, Result);
    end;
    Next();
  end;

  procedure ParseEnum;
  var
    Enum: TLapeType_Enum absolute Result;
    Name: lpString;
    NamePos: TDocPos;
    Element: TLapeTree_ExprBase;
    Default, ElementVar: TLapeGlobalVar;
    StackOwner: TLapeStackInfo;
    Val: Int64;
    Scoped: Boolean;
  begin
    Scoped := (lcoScopedEnums in FOptions) or (Tokenizer.Tok = tk_kw_Enum);
    if Scoped and (Tokenizer.Tok = tk_kw_Enum) then
      Expect(tk_sym_ParenthesisOpen, True, False);

    Enum := TLapeType_Enum.Create(Self, Scoped, '');
    Enum.DocPos := getDocPos();
    if (not addToStackOwner) then
      StackOwner := FStackInfo
    else
      StackOwner := FStackInfo.Owner;
    if (StackOwner = nil) then
      StackOwner := GlobalScope;

    if Scoped then
      Result := addManagedType(Enum)
    else
      Result := addManagedDecl(Enum) as TLapeType;

    repeat
      Expect(tk_Identifier, True, False);
      Name := Tokenizer.TokString;
      NamePos := Tokenizer.DocPos;
      if (Scoped and Enum.HasElement(Name)) or ((not Scoped) and hasDeclaration(Name, StackOwner, True)) then
      begin
        Result := nil; // Do not free type, because enum elements rely on it

        LapeExceptionFmt(lpeDuplicateDeclaration, [Name], Tokenizer.DocPos);
      end;

      Expect([tk_sym_Comma, tk_sym_ParenthesisClose, tk_sym_Equals], True, False);
      if (Tokenizer.Tok = tk_cmp_Equal) then
      try
        Element := ParseExpression([tk_sym_Comma, tk_sym_ParenthesisClose], True, False);
        try
          if (Element <> nil) then
            Default := Element.Evaluate()
          else
            Default := nil;

          if (Default = nil) or (not Default.HasType()) or (not (Default.VarType.BaseType in LapeOrdinalTypes)) or (not Default.Known) then
            LapeException(lpeExpressionExpected, Tokenizer.DocPos);
          Val := Enum.addElement(Default.AsInteger, Name);
        finally
          Element.Free();
        end;
      except
        on E: lpException do
        begin
          Result := nil; // Do not free type, because enum elements rely on it

          LapeException(lpString(E.Message), Tokenizer.DocPos);
        end
      end
      else
        Val := Enum.addElement(Name);

      if (not Scoped) then
      begin
        ElementVar := Enum.NewGlobalVar(Val, Name);
        ElementVar.DocPos := NamePos;
        ElementVar.isConstant := True;
        StackOwner.addDeclaration(ElementVar);
      end;

    until (Tokenizer.Tok in [tk_NULL, tk_sym_ParenthesisClose]);
    Next();
  end;

  procedure ParseMethodType;
  var
    BaseType: ELapeBaseType;
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

    Result := TLapeMethodParser(Self).ParseMethodHeader();
    if (Result.Name <> '') then
      LapeException(lpeTypeExpected, Tokenizer.DocPos);

    Result := Result.CreateCopy();
    Result.BaseType := BaseType;
    Result := addManagedType(Result);

    if (Tokenizer.Tok = tk_kw_Of) then
    begin
      Expect(tk_kw_Object, True, True);
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
        Result := TLapeTree_VarType(TypeExpr).VarType.CreateCopy();
        SetUniqueTypeID(Result);
        addManagedType(Result);
      end
      else
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
    finally
      if (TypeExpr <> nil) then
        TypeExpr.Free();
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
        if (RangeType.ClassType <> TLapeType_Enum) then
          LapeException(lpeEnumRangeExpected, TypeExpr.DocPos);
        Result := addManagedType(TLapeType_SubRange.Create(Range, Self, RangeType, ''));
        Result.DocPos := getDocPos();
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
          begin
            Result := addManagedType(TLapeType_ShortString.Create(Self, Range.Hi, ''));
            Result.DocPos := Left.DocPos;
          end
          else
            LapeException(lpeOutOfTypeRange, Tokenizer.DocPos);
        end
      else if (TypeExpr <> nil) and (TypeExpr is TLapeTree_VarType) and (TLapeTree_VarType(TypeExpr).VarType <> nil) then
        Result := TLapeTree_VarType(TypeExpr).VarType
      else
        LapeException(lpeTypeExpected, Tokenizer.DocPos);
    finally
      if (TypeExpr <> nil) then
        TypeExpr.Free();
    end;
  end;

var
  TypePos: TDocPos;
begin
  Result := nil;
  try

    Next();
    TypePos := Tokenizer.DocPos;
    case Tokenizer.Tok of
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
        Result.WriteHints({$IFDEF FPC}@{$ENDIF}Hint, TypePos)
      else
      // TypeForwards<>nil means ParseTypeBlock
      // type TPoint = record X,Y: Integer deprecated;
      if (TypeForwards <> nil) and (Tokenizer.Tok in ParserToken_Hints) then
        ParseDeclHint(Result);
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
  NamePos: TDocPos;

  // a pointer that still waits for its type
  function isForward(AType: TLapeType): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to TypeForwards.Count - 1 do
      if (TypeForwards.ItemsI[i] = AType) then
        Exit(True);
    Result := False;
  end;

begin
  TypeForwards := TLapeTypeForwards.Create(nil);
  try

    Next();
    repeat
      Expect(tk_Identifier, False, False);
      Name := Tokenizer.TokString;
      NamePos := Tokenizer.DocPos;
      if hasDeclaration(Name, nil, True) then
        LapeExceptionFmt(lpeDuplicateDeclaration, [Name], Tokenizer.DocPos);
      Expect(tk_sym_Equals, True, False);

      Typ := ParseType(TypeForwards, False);
      if ((Typ.DeclarationList <> nil) or (Typ.Name <> '')) and (not isForward(Typ)) then
        Typ := Typ.CreateAlias();
      Typ.Name := Name;
      Typ.DocPos := NamePos;
      FStackInfo.addDeclaration(Typ);

      ParseExpressionEnd(tk_sym_SemiColon, False, False);
    until (Next() <> tk_Identifier);

    while (TypeForwards.Count > 0) do
      with __LapeType_Pointer(TypeForwards.ItemsI[0]) do
      begin
        FPType := TLapeType(Self.getDeclaration(TypeForwards.Key[0], nil, False, False));
        if (not HasType()) then
          LapeExceptionFmt(lpeInvalidForward, [TypeForwards.Key[0]], DocPos);
        TypeForwards.Delete(0);
      end;
  finally
    while (TypeForwards.Count > 0) do
    begin
      TypeForwards.ItemsI[0].Free();
      TypeForwards.Delete(0);
    end;
    TypeForwards.Free();
  end;
end;

procedure TLapeCompiler.ParseLabelBlock;
var
  i: Integer;
  Identifiers: TStringArray;
  Positions: TDocPosArray;
  VarType: TLapeType;
  Decl: TLapeGlobalVar;
begin
  Identifiers := ParseIdentifierList(Positions, True);
  ParseExpressionEnd(tk_sym_SemiColon, False, True);

  VarType := getGlobalType('!label');
  for i := 0 to High(Identifiers) do
    if hasDeclaration(Identifiers[i], nil, True) then
      LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], Tokenizer.DocPos)
    else
    begin
      Decl := VarType.NewGlobalVarP(nil, Identifiers[i]);
      Decl.DocPos := Positions[i];
      FStackInfo.addDeclaration(Decl);
    end;
end;

function TLapeCompiler.ParseVarBlock(OneOnly: Boolean = False; ValidEnd: EParserTokenSet = [tk_sym_SemiColon]): TLapeTree_VarList;
var
  i: Integer;
  isConst: Boolean;
  Identifiers: TStringArray;
  Positions: TDocPosArray;
  VarType: TLapeType;
  DefExpr: TLapeTree_ExprBase;
  DefConst: TLapeGlobalVar;
  DefPos: TDocPos;
  VarDecl: TLapeVarDecl;
begin
  ValidEnd := ValidEnd + [tk_kw_Deprecated, tk_kw_UnImplemented, tk_kw_Experimental];

  Result := TLapeTree_VarList.Create(Self);
  try
    isConst := (Tokenizer.Tok = tk_kw_Const);

    Next();
    repeat
      VarType := nil;
      DefExpr := nil;
      DefConst := nil;

      Identifiers := ParseIdentifierList(Positions);
      Expect([tk_sym_Colon, tk_op_Assign, tk_sym_Equals], False, False);

      if (Tokenizer.Tok = tk_sym_Colon) then
      begin
        VarType := ParseType(nil);
        if isConst then
          Expect([tk_op_Assign, tk_sym_Equals], False, False)
        else
          ParseExpressionEnd([tk_op_Assign, tk_sym_Equals] + ValidEnd, False, False);
      end;

      if (Tokenizer.Tok = tk_sym_Equals) then
      begin
        DefExpr := EnsureExpression(ParseExpression(ValidEnd, True, False)).setExpectedType(VarType) as TLapeTree_ExprBase;
        if (DefExpr <> nil) and (not DefExpr.isConstant()) then
          LapeException(lpeConstantExpected, DefExpr.DocPos);

        DefPos := DefExpr.DocPos;
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
        if hasDeclaration(Identifiers[i], nil, True) then
          LapeExceptionFmt(lpeDuplicateDeclaration, [Identifiers[i]], Tokenizer.DocPos);

        if isConst then
        begin
          VarDecl.VarDecl := VarType.NewGlobalVarP(nil, Identifiers[i]);
          FStackInfo.addDeclaration(VarDecl.VarDecl);
        end
        else
          VarDecl.VarDecl := FStackInfo.addVar(VarType, Identifiers[i]);

        VarDecl.Default := nil;
        VarDecl.VarDecl.DocPos := Positions[i];
        // (a variable is tracked for the hint that it is not used, see checkDeclHints)
        if (lcoHints in FOptions) and (not isConst) then
          VarDecl.VarDecl.IsUsed := False;
        if (DefConst <> nil) then
          if (not (VarDecl.VarDecl is TLapeGlobalVar)) or (VarType is TLapeType_Method) then
            VarDecl.Default := TLapeTree_Var.Create(DefConst, Self)
          else
          try
            VarDecl.VarDecl.VarType.EvalConst(op_Assign, TLapeGlobalVar(VarDecl.VarDecl), DefConst, []);
          except
            on E: lpException do
              LapeExceptionReraise(E, DefPos);
          end
        else if (DefExpr <> nil) then
        begin
          VarDecl.Default := DefExpr;
          DefExpr := nil;
        end;

        if (VarDecl.VarDecl is TLapeVar) then
        begin
          TLapeVar(VarDecl.VarDecl).isConstant := isConst;
          if (VarDecl.VarDecl is TLapeGlobalVar) then
            TLapeGlobalVar(VarDecl.VarDecl).Known := isConst and (DefConst <> nil);
        end;

        // var a: Integer deprecated
        if (Tokenizer.Tok in ParserToken_Hints) then
          ParseDeclHint(VarDecl.VarDecl);

        Result.addVar(VarDecl);
      end;
    until (Next() <> tk_Identifier) or OneOnly;

  except
    Result.Free();
    if (DefExpr <> nil) then
      DefExpr.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseExpression(ReturnOn: EParserTokenSet; FirstNext: Boolean; DoFold: Boolean): TLapeTree_ExprBase;
begin
  Result := TLapeExpressionParser(Self).Parse(ReturnOn, FirstNext, DoFold);
end;

function TLapeCompiler.ParseTypeExpression(ReturnOn: EParserTokenSet = []; FirstNext: Boolean = True): TLapeTree_Base;
begin
  Result := TLapeExpressionParser(Self).ParseRange(ReturnOn, FirstNext);
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
    if (Result is TLapeTree_InternalMethod_Label) then
    begin
      if (FLastTok <> tk_sym_Colon) then
        LapeExceptionFmt(lpeExpectedOther, [LapeTokenToString(FLastTok), LapeTokenToString(tk_sym_Colon)], Tokenizer.DocPos);
      Exit;
    end;

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
  Result := TLapeTree_StatementList.Create(Self);
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
        (FLastTok = tk_sym_SemiColon) and
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
    if (tk_sym_Dot in ExprEnd) and (Tokenizer.Tok = tk_sym_Dot) then
      Exit;
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
  Result := TLapeTree_Case.Create(Self);
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

      Branch := TLapeTree_CaseBranch.Create(Self);
      Branch.DocPos := Expr.DocPos;
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
    if (FLastTok = tk_kw_Else) then
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

    LimitType := TLapeTree_Operator(counterExpr).Right.resType();
    if (LimitType is TLapeType_Set) and (TLapeTree_Operator(counterExpr).Right is TLapeTree_VarType) then
      Result.LoopOverWhat := loopOverEnum
    else if (LimitType is TLapeType_Set) then
      Result.LoopOverWhat := loopOverSet
    else
    // (a range, or the name of an enum or of a range)
    if (LimitType is TLapeType_SubRange) then
      Result.LoopOverWhat := loopOverEnum;

    if Result.LoopOverWhat in [loopOverEnum, loopOverSet] then
      Result.Limit := TLapeTree_Operator(counterExpr).Right
    else
    begin
      LimitType := Result.Counter.resType().ArrayOf;
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
  Result := TLapeTree_For.Create(Self);

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
          Result.Counter := TLapeTree_Operator.Create(op_Assign, Compiler);
          Result.Counter.DocPos := DocPos;
          with TLapeTree_Operator(Result.Counter) do
          begin
            Left := TLapeTree_Var.Create(_ResVar.New(Vars[0].VarDecl), Compiler);
            Left.DocPos := DocPos;
            Right := Vars[0].Default;
          end;
        end
        else
        begin
          Result.Counter := TLapeTree_Var.Create(_ResVar.New(Vars[0].VarDecl), Compiler);
          Result.Counter.DocPos := DocPos;
        end;
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
    if (FLastTok = tk_kw_Else) then
      Result.ElseBody := ParseStatement(False, ExprEnd);
  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseIf(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_If;
begin
  Result := TLapeTree_If.Create(Self);

  try

    Result.Condition := ParseExpression();
    Expect(tk_kw_Then, False, False);
    Result.Body := ParseStatement(True, ExprEnd + [tk_kw_Else]);
    if (FLastTok = tk_kw_Else) then
      Result.ElseBody := ParseStatement(False, ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseRepeat(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_Repeat;
begin
  Result := TLapeTree_Repeat.Create(Self);

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
  Result := TLapeTree_Try.Create(Self);

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
  Result := TLapeTree_While.Create(Self);

  try

    Result.Condition := ParseExpression();
    Expect(tk_kw_Do, False, False);
    Result.Body := ParseStatement(True, ExprEnd + [tk_kw_Else]);
    if (FLastTok = tk_kw_Else) then
      Result.ElseBody := ParseStatement(False, ExprEnd);

  except
    Result.Free();
    raise;
  end;
end;

function TLapeCompiler.ParseWith(ExprEnd: EParserTokenSet = ParserToken_ExpressionEnd): TLapeTree_With;
var
  Count: Integer;
  Expr: TLapeTree_ExprBase;
begin
  Result := TLapeTree_With.Create(Self);
  Count := 0;

  try

    repeat
      if (Tokenizer.Tok in [tk_kw_With, tk_sym_Comma]) then
      begin
        Expr := ParseExpression([tk_sym_Comma]);
        if (FStackInfo.addWith(Result.addWith(Expr)) < 0) then
          LapeException(lpeInvalidWithReference, [Expr, Result]);
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
    FStackInfo.delWith(Count);
    Result.Free();
    raise;
  end;
end;

constructor TLapeCompiler.Create(ATokenizer: TLapeTokenizerBase);
begin
  FInternalMethods := TLapeInternalMethods.Create(nil);
  inherited Create();

  FTokenizer := -1;
  FPreTree := TLapeTree_StatementList.Create(Self);
  FPostTree := TLapeTree_StatementList.Create(Self);
  FMethods := TLapeTree_StatementList.Create(Self);

  FIncludes := TStringList.Create();
  FIncludes.CaseSensitive := {$IFDEF DARWIN}False{$ELSE}LapeSystemCaseSensitive{$ENDIF};
  FConditionalStack := TLapeConditionalStack.Create();
  FBaseDefines := TLapeBaseDefines.Create('');
  FDefines := TLapeDefineStack.Create();

  FOnHandleDirective := nil;
  FOnHandleExternal := nil;
  FOnFindFile := nil;

  FPreprocessorFuncs := TLapePreprocessorFuncs.Create(nil);
  FPreprocessorFuncs.Add('DEFINED', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);
  FPreprocessorFuncs.Add('DECLARED', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);
  FPreprocessorFuncs.Add('FILEEXISTS', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);
  FPreprocessorFuncs.Add('INCLUDED', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorFunc);

  FPreprocessorMacros := TLapePreprocessorFuncs.Create(nil);
  FPreprocessorMacros.Add('FILE', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('DIR', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('ENV', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('INCLUDEDFILE', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('INCLUDEDFILES', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('TICKCOUNT', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('LINE', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('NOW', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);
  FPreprocessorMacros.Add('FUNC', {$IFDEF FPC}@{$ENDIF}HandlePreprocessorMacro);

  setTokenizer(ATokenizer);
  Reset();

  LapeCreateArrayHelpers(Self);

  InitBaseDefinitions();
end;

procedure TLapeCompiler.setInternalMethod(AName: lpString; AClass: TLapeTree_InternalMethodClass);
begin
  FInternalMethods[AName] := AClass;
end;

procedure TLapeCompiler.addInternalMethods;
begin
  InternalMethodMap['Write'] := TLapeTree_InternalMethod_Write;
  InternalMethodMap['WriteLn'] := TLapeTree_InternalMethod_WriteLn;
  InternalMethodMap['ToStr'] := TLapeTree_InternalMethod_ToStr;

  InternalMethodMap['Assert'] := TLapeTree_InternalMethod_Assert;
  InternalMethodMap['IsScriptMethod'] := TLapeTree_InternalMethod_IsScriptMethod;
  InternalMethodMap['GetExceptionMessage'] := TLapeTree_InternalMethod_GetExceptionMessage;
  InternalMethodMap['GetExceptionLocation'] := TLapeTree_InternalMethod_GetExceptionLocation;
  InternalMethodMap['GetExceptionLocationStr'] := TLapeTree_InternalMethod_GetExceptionLocationStr;
  InternalMethodMap['GetCallerAddress'] := TLapeTree_InternalMethod_GetCallerAddress;
  InternalMethodMap['GetCallerName'] := TLapeTree_InternalMethod_GetCallerName;
  InternalMethodMap['GetScriptMethodName'] := TLapeTree_InternalMethod_GetScriptMethodName;
  InternalMethodMap['DumpCallStack'] := TLapeTree_InternalMethod_DumpCallStack;
  InternalMethodMap['Break'] := TLapeTree_InternalMethod_Break;
  InternalMethodMap['Continue'] := TLapeTree_InternalMethod_Continue;
  InternalMethodMap['Exit'] := TLapeTree_InternalMethod_Exit;
  InternalMethodMap['Halt'] := TLapeTree_InternalMethod_Halt;

  InternalMethodMap['New'] := TLapeTree_InternalMethod_New;
  InternalMethodMap['Dispose'] := TLapeTree_InternalMethod_Dispose;
  InternalMethodMap['Default'] := TLapeTree_InternalMethod_Default;

  InternalMethodMap['Swap'] := TLapeTree_InternalMethod_Swap;
  InternalMethodMap['SizeOf'] := TLapeTree_InternalMethod_SizeOf;

  InternalMethodMap['Low'] := TLapeTree_InternalMethod_Low;
  InternalMethodMap['High'] := TLapeTree_InternalMethod_High;
  InternalMethodMap['Length'] := TLapeTree_InternalMethod_Length;

  InternalMethodMap['SetLength'] := TLapeTree_InternalMethod_SetLength;
  InternalMethodMap['Copy'] := TLapeTree_InternalMethod_Copy;
  InternalMethodMap['Delete'] := TLapeTree_InternalMethod_Delete;
  InternalMethodMap['Insert'] := TLapeTree_InternalMethod_Insert;

  InternalMethodMap['Sort'] := TLapeTree_InternalMethod_Sort;
  InternalMethodMap['Sorted'] := TLapeTree_InternalMethod_Sorted;
  InternalMethodMap['Unique'] := TLapeTree_InternalMethod_Unique;
  InternalMethodMap['Reverse'] := TLapeTree_InternalMethod_Reverse;
  InternalMethodMap['Reversed'] := TLapeTree_InternalMethod_Reversed;
  InternalMethodMap['IndexOf'] := TLapeTree_InternalMethod_IndexOf;
  InternalMethodMap['IndicesOf'] := TLapeTree_InternalMethod_IndicesOf;
  InternalMethodMap['Contains'] := TLapeTree_InternalMethod_Contains;
  InternalMethodMap['Remove'] := TLapeTree_InternalMethod_Remove;

  InternalMethodMap['Ord'] := TLapeTree_InternalMethod_Ord;
  InternalMethodMap['Succ'] := TLapeTree_InternalMethod_Succ;
  InternalMethodMap['Pred'] := TLapeTree_InternalMethod_Pred;
  InternalMethodMap['Inc'] := TLapeTree_InternalMethod_Inc;
  InternalMethodMap['Dec'] := TLapeTree_InternalMethod_Dec;

  InternalMethodMap['goto'] := TLapeTree_InternalMethod_GoTo;

  InternalMethodMap['raise'] := TLapeTree_InternalMethod_Raise;

  InternalMethodMap['Objectify'] := TLapeTree_InternalMethod_Objectify;

  InternalMethodMap['ArrayMin'] := TLapeTree_InternalMethod_ArrayMin;
  InternalMethodMap['ArrayMax'] := TLapeTree_InternalMethod_ArrayMax;
  InternalMethodMap['ArrayMedian'] := TLapeTree_InternalMethod_ArrayMedian;
  InternalMethodMap['ArrayMode'] := TLapeTree_InternalMethod_ArrayMode;
  InternalMethodMap['ArraySum'] := TLapeTree_InternalMethod_ArraySum;
  InternalMethodMap['ArrayMean'] := TLapeTree_InternalMethod_ArrayMean;
  InternalMethodMap['ArrayVariance'] := TLapeTree_InternalMethod_ArrayVariance;
  InternalMethodMap['ArrayStdev'] := TLapeTree_InternalMethod_ArrayStdev;
  InternalMethodMap['ArrayDifference'] := TLapeTree_InternalMethod_ArrayDifference;
  InternalMethodMap['ArraySymDifference'] := TLapeTree_InternalMethod_ArraySymDifference;
  InternalMethodMap['ArrayIntersection'] := TLapeTree_InternalMethod_ArrayIntersection;
  InternalMethodMap['ArrayEquals'] := TLapeTree_InternalMethod_ArrayEquals;

  InternalMethodMap['Slice'] := TLapeTree_InternalMethod_Slice;

  InternalMethodMap['PType'] := TLapeTree_InternalMethod_PType;
end;

procedure TLapeCompiler.Clear;
begin
  inherited;
  FInternalMethods.Clear();
  addInternalMethods();
end;

destructor TLapeCompiler.Destroy;
begin
  setTokenizer(nil);
  FreeAndNil(FPreTree);
  FreeAndNil(FPostTree);
  FreeAndNil(FMethods);
  FreeAndNil(FPreprocessorFuncs);
  FreeAndNil(FPreprocessorMacros);
  FreeAndNil(FIncludes);
  FreeAndNil(FConditionalStack);
  FreeAndNil(FBaseDefines);
  FreeAndNil(FDefines);
  inherited;
  FreeAndNil(FInternalMethods);
end;

function TLapeCompiler.getState: Pointer;
var
  i: Integer;
begin
  New(PCompilerState(Result));
  with PCompilerState(Result)^ do
  begin
    Tokenizer := FTokenizer;
    InPeek := FInPeek;
    LastTok := FLastTok;

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
    DefineCount := FDefines.Count;
    DefineFloor := FDefines.Floor;
    Conditionals := FConditionalStack.ExportToArray();
  end;
end;

procedure TLapeCompiler.setState(const State: Pointer);
var
  i: Integer;
begin
  with PCompilerState(State)^ do
  begin
    Assert(Length(FTokenizers) >= Length(Tokenizers));
    Assert(Length(Tokenizers) = Length(TokStates));
    FTokenizer := Tokenizer;
    FInPeek := InPeek;
    FLastTok := LastTok;

    for i := 0 to High(Tokenizers) do
    begin
      if (TokStates[i] <> nil) then
        Tokenizers[i].setState(TokStates[i]);
      if (FTokenizers[i] <> Tokenizers[i]) then
      begin
        decTokenizerLock(FTokenizers[i]);
        FTokenizers[i] := incTokenizerLock(Tokenizers[i]);
      end;
    end;

    FOptions := Options;
    FOptions_PackRecords := Options_PackRecords;
    FDefines.Restore(DefineCount, DefineFloor);
    FConditionalStack.ImportFromArray(Conditionals);
  end;
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

function TLapeCompiler.getTempTokenizerState(const AStr: lpString; const AFileName: lpString = ''): Pointer;
begin
  if (not HasTokenizer()) then
    SetLength(FTokenizers, 1);

  New(PTempTokenizerState(Result));
  with PTempTokenizerState(Result)^ do
  begin
    OldStackInfo := FStackInfo;
    OldTokenizerIndex := FTokenizer;
    OldTokenizer := FTokenizers[0];
    OldState := getState();
    // (what it parses is at its own place, not where the compiling node is)
    FEmitter.DocPos := NullDocPos;
  end;

  FTokenizer := 0;
  FTokenizers[0] := nil;
  Tokenizer := TLapeTokenizerString.Create(AStr, AFileName);

  FConditionalStack.Clear();
  FStackInfo := FStackInfo.Root;
  FOptions := FBaseOptions;
  FOptions_PackRecords := FBaseOptions_PackRecords;
  FDefines.Hide();
end;

procedure TLapeCompiler.resetTokenizerState(const State: Pointer);
begin
  with PTempTokenizerState(State)^ do
  begin
    Swap(Pointer(FTokenizers[0]), Pointer(OldTokenizer));
    FTokenizer := OldTokenizerIndex;
    FStackInfo := OldStackInfo;
    setState(OldState);

    if (OldTokenizer <> nil) then
      decTokenizerLock(OldTokenizer);
  end;
  Dispose(PTempTokenizerState(State));
end;

// What Tok does to how deep a skip is
function SkipDepth(Tok, Prev: EParserToken): Integer;
begin
  case Tok of
    tk_kw_Begin, tk_kw_Case, tk_kw_Try, tk_kw_Repeat, tk_kw_Record, tk_kw_Union, tk_sym_ParenthesisOpen, tk_sym_BracketOpen:
      Result := 1;
    tk_kw_End, tk_kw_Until, tk_sym_ParenthesisClose, tk_sym_BracketClose:
      Result := -1;
    tk_kw_Object:
      if (Prev = tk_kw_Of) then
        Result := 0
      else
        Result := 1;
    else
      Result := 0;
  end;
end;

// To the `end` of a body, with Depth of what it has open. (A method in it has a begin..end of its own: what is not one
// is a type, what comes after `:` or `=`)
procedure TLapeCompiler.SkipBody(Depth: Integer);
var
  Tok, Prev: EParserToken;
  Nested: Integer;
  InBegin: Boolean;
begin
  Nested := 0;
  InBegin := (Depth > 0);
  Tok := tk_NULL;

  repeat
    Prev := Tok;
    Tok := Next();

    if (Depth = 0) then
      InBegin := (Tok = tk_kw_Begin);
    Inc(Depth, SkipDepth(Tok, Prev));

    case Tok of
      tk_NULL:
        Exit;
      tk_kw_Function, tk_kw_Procedure, tk_kw_Operator, tk_kw_Property:
        if (Depth = 0) and (not (Prev in [tk_sym_Colon, tk_sym_Equals, tk_kw_Of, tk_kw_Type])) then
          Inc(Nested);
      tk_kw_External:
        if (Depth = 0) and (Nested > 0) then
          Dec(Nested);
      tk_kw_End:
        if (Depth = 0) and InBegin then
          if (Nested = 0) then
            Exit
          else
            Dec(Nested);
    end;
  until False;
end;

// To the `;` that ends a var block
procedure TLapeCompiler.SkipVars;
var
  Tok, Prev: EParserToken;
  Depth: Integer;
begin
  Depth := 0;
  Tok := tk_NULL;

  repeat
    Prev := Tok;
    Tok := Next();
    Inc(Depth, SkipDepth(Tok, Prev));

    case Tok of
      tk_NULL:
        Exit;
      tk_sym_SemiColon:
        if (Depth = 0) and (Peek() <> tk_Identifier) then
          Exit;
    end;
  until False;
end;

// To the `;` that ends a statement of its own, the current token is its first
procedure TLapeCompiler.SkipStatement;
var
  Tok, Prev: EParserToken;
  Depth: Integer;
begin
  Depth := 0;
  Prev := tk_NULL;
  Tok := Tokenizer.Tok;

  while (Tok <> tk_NULL) do
  begin
    Inc(Depth, SkipDepth(Tok, Prev));
    if (Tok = tk_sym_SemiColon) and (Depth = 0) then
      Exit;

    Prev := Tok;
    Tok := Next();
  end;
end;

// (the current token is the first of what is skipped)
function TLapeCompiler.StartSkip: TLapeDeferredBody;
begin
  Result.Stack := FStackInfo;
  Result.State := getState();
  Result.Defines := FDefines.Clone();
  FSkipping := True;
end;

// (a file has what is skipped from start to end: the place to go back to is in it)
procedure TLapeCompiler.EndSkip(var Body: TLapeDeferredBody);
begin
  FSkipping := False;
  if (FTokenizer <> PCompilerState(Body.State)^.Tokenizer) then
    LapeExceptionFmt(lpeExpected, [LapeTokenToString(tk_sym_SemiColon)], Tokenizer.DocPos);
  Body.StopAt := Tokenizer.Pos;
end;

procedure TLapeCompiler.FreeDeferred(var Body: TLapeDeferredBody);
begin
  if (Body.State <> nil) then
    freeState(Body.State);
  Body.State := nil;
  FreeAndNil(Body.Defines);
end;

procedure TLapeCompiler.addDeferred(const Body: TLapeDeferredBody; Kind: EDeferred; Method: TLapeTree_Method; Block: TLapeTree_StatementList);
var
  i: Integer;
begin
  i := Length(FDeferred);
  SetLength(FDeferred, i + 1);
  FDeferred[i] := Body;
  FDeferred[i].Kind := Kind;
  FDeferred[i].Method := Method;
  FDeferred[i].Block := Block;
end;

function TLapeCompiler.DeferStatement: TLapeTree_Base;
var
  Body: TLapeDeferredBody;
begin
  Body := StartSkip();
  try
    SkipStatement();
    EndSkip(Body);
    Next();
  except
    FSkipping := False;
    FreeDeferred(Body);
    raise;
  end;

  Result := TLapeTree_StatementList.Create(Self);
  addDeferred(Body, dkStatement, nil, TLapeTree_StatementList(Result));
end;

function TLapeCompiler.DeferVars: TLapeTree_Base;
var
  Body: TLapeDeferredBody;
begin
  Body := StartSkip();
  try
    SkipVars();
    EndSkip(Body);
    Next();
  except
    FSkipping := False;
    FreeDeferred(Body);
    raise;
  end;

  Result := TLapeTree_StatementList.Create(Self);
  addDeferred(Body, dkVars, nil, TLapeTree_StatementList(Result));
end;

procedure TLapeCompiler.DeferBody(Method: TLapeTree_Method; Block: TLapeTree_StatementList);
var
  Body: TLapeDeferredBody;
begin
  Body := StartSkip();
  try
    SkipBody(Ord(Method = nil));
    Expect(tk_kw_End, False, True);
    EndSkip(Body);

    // (what comes after the body is not of it: it runs as it is read)
    if (Method <> nil) or (Tokenizer.Tok <> tk_sym_Dot) then
      ParseExpressionEnd(ParserToken_ExpressionEnd);
  except
    FSkipping := False;
    FreeDeferred(Body);
    raise;
  end;

  if (Method <> nil) then
    addDeferred(Body, dkBody, Method, nil)
  else
    addDeferred(Body, dkBeginEnd, nil, Block);
end;

// (Body is gone after this)
function TLapeCompiler.ParseDeferred(var Body: TLapeDeferredBody; Kind: EDeferred; Method: TLapeTree_Method): TLapeTree_Base;
var
  OldState: Pointer;
  OldDefines: TLapeDefineStack;
  OldStack: TLapeStackInfo;
  Stopped: TLapeTokenizerBase;
begin
  Result := nil;
  Stopped := nil;
  OldState := getState();
  OldDefines := FDefines;
  OldStack := FStackInfo;
  // (what it parses is at its own place, not where the compiling node is)
  FEmitter.DocPos := NullDocPos;

  try
    FDefines := Body.Defines;
    setState(Body.State);
    Body.State := nil;
    FConditionalStack.Clear();
    FStackInfo := Body.Stack;
    Stopped := Tokenizer;
    Stopped.StopAt := Body.StopAt;

    case Kind of
      dkBody:
        begin
          IncStackInfo(Body.Stack, []);
          try
            TLapeMethodParser(Self).ParseBody(Method);
          finally
            DecStackInfo([lsfFunction]);
          end;

          // (where the skip ended and where the body did are one place)
          if (Tokenizer.Tok <> tk_NULL) and (not (lcoLooseSemicolon in FOptions)) then
            LapeExceptionFmt(lpeUnexpectedToken, [LapeTokenToString(Tokenizer.Tok)], Tokenizer.DocPos);
        end;
      dkVars:      Result := ParseVarBlock();
      dkStatement: Result := ParseStatement(False);
      dkBeginEnd:  Result := ParseBeginEnd(ParserToken_ExpressionEnd + [tk_sym_Dot]);
    end;

    if (FConditionalStack.Count > 0) then
      LapeException(lpeConditionalNotClosed, popConditional());
  finally
    if (Stopped <> nil) then
      Stopped.StopAt := 0;
    FDefines := OldDefines;
    FStackInfo := OldStack;
    setState(OldState);
    FreeDeferred(Body);
  end;
end;

// (global variables first: a body can use them. A method in a body joins the list, so it sees all of what is around it)
procedure TLapeCompiler.ParseDeferredBodies;

  procedure ParseOne(Index: Integer);
  var
    Body: TLapeDeferredBody;
    Node: TLapeTree_Base;
  begin
    Body := FDeferred[Index];
    FDeferred[Index].State := nil;
    FDeferred[Index].Defines := nil;

    Node := ParseDeferred(Body, Body.Kind, Body.Method);
    if (Body.Kind <> dkBody) then
      Body.Block.addStatement(Node);
  end;

var
  i: Integer;
begin
  for i := 0 to High(FDeferred) do
    if (FDeferred[i].Kind = dkVars) then
      ParseOne(i);

  i := 0;
  while (i <= High(FDeferred)) do
  begin
    if (FDeferred[i].Kind <> dkVars) then
      ParseOne(i);
    Inc(i);
  end;
end;

function TLapeCompiler.ParseFile: TLapeTree_Base;
var
  OldDeferring: Boolean;
  OldDeferred: array of TLapeDeferredBody;
  i: Integer;
begin
  Result := nil;
  Assert(Tokenizer <> nil);

  OldDeferring := FDeferring;
  OldDeferred := FDeferred;
  FDeferring := True;
  FDeferred := nil;

  try
    FDefines.Hide();

    if (Next() = tk_kw_Program) then
    begin
      Expect(tk_Identifier, True, True);
      ParseExpressionEnd(tk_sym_SemiColon);
    end;

    Result := ParseBlockList(False);

    if (FConditionalStack.Count > 0) then
      LapeException(lpeConditionalNotClosed, popConditional());

    ParseDeferredBodies();
    FDeferring := OldDeferring;
    FDeferred := OldDeferred;
  except
    for i := 0 to High(FDeferred) do
      FreeDeferred(FDeferred[i]);
    FDeferring := OldDeferring;
    FDeferred := OldDeferred;

    if (Result <> nil) then
      Result.Free();
    raise;
  end;
end;

function TLapeCompiler.Compile: Boolean;
var
  ToEnd, Imported: Integer;
begin
  Result := False;
  try
    Reset();

    GlobalScope.ForceInitialization := (lcoAlwaysInitialize in FOptions);
    IncStackInfo(GlobalScope, [lsfEmit]);
    Imported := FMethods.Statements.Count;
    FTree := ParseFile();
    if (FTree = nil) and (FMethods.Statements.Count = Imported) then
      LapeException(lpeExpressionExpected);
    checkDeclHints(GlobalScope);

    FPreTree.Compile();
    if (FTree <> nil) then
      FTree.Compile();
    FPostTree.Compile();
    DecStackInfo([lsfGlobal, lsfEmit]);
    Assert(GlobalScope.CodePos = 0);

    // (every method after the program's end, which jumps past them)
    ToEnd := FEmitter._JmpR(0);
    FMethods.Compile();
    FEmitter.PatchJump(ToEnd, FEmitter.CodeLen);

    FEmitter._op(ocNone);
    FEmitter.WriteTables();
    FEmitter.DocPos := NullDocPos;
    Result := True;
  except
    Reset();
    raise;
  end;
end;

procedure TLapeCompiler.VarToDefault(AVar: TResVar);
var
  tmpVar: TResVar;
begin
  Assert(not AVar.isConstant);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) or (not AVar.HasType()) then
    Exit;

  if (AVar.VarType is TLapeType_Object) then
    with TLapeTree_InternalMethod_Default.Create(Self) do
    try
      addParam(TLapeTree_Var.Create(AVar, Self));
      Compile();
    finally
      Free();
    end
  else
  begin
    tmpVar := NullResVar;
    AVar.VarType.Eval(op_Assign, tmpVar, AVar, _ResVar.New(AVar.VarType.EmptyValue), [lefConstRangeCheck]);
  end;
end;

procedure TLapeCompiler.checkDeclHints(ADecl: TLapeDeclaration);
var
  Stack: TLapeStackInfo;
begin
  // only hints on what a script declared
  if ADecl.isHidden() or ADecl.DocPos.isEmpty or (ADecl is TLapeParameterVar) or (not (ADecl.DeclarationList is TLapeStackInfo)) then
    Exit;

  if (lcoDuplicateLocalNameHints in FOptions) then
  begin
    Stack := TLapeStackInfo(ADecl.DeclarationList).Owner;
    while (Stack <> nil) and (not Stack.hasDeclaration(ADecl.Name, False)) do
      Stack := Stack.Owner;

    if (Stack <> nil) then
      Hint(lphDuplicateLocalName, [ADecl.Name], ADecl.DocPos);
  end;

  // (only a variable is tracked, see ParseVarBlock)
  if (lcoHints in FOptions) and (not ADecl.IsUsed) then
    Hint(lphVariableNotUsed, [ADecl.Name], ADecl.DocPos);
end;

procedure TLapeCompiler.checkDeclHints(AStack: TLapeStackInfo);
var
  i: Integer;
begin
  for i := 0 to AStack.Count - 1 do
    checkDeclHints(AStack.Items[i]);

  // (set by an assignment, or Exit(x))
  if (lcoHints in FOptions) and (AStack.ResultVar <> nil) and (not AStack.ResultVar.IsUsed) then
    Hint(lphResultNotSet, [], AStack.ResultVar.DocPos);
end;


function TLapeCompiler.hasDeclaration(AName: lpString; AStackInfo: TLapeStackInfo = nil; LocalOnly: Boolean = False): Boolean;
begin
  Result := inherited or ((not LocalOnly) and (FInternalMethods[AName] <> nil));
end;

function TLapeCompiler.getExpression(AName: lpString): TLapeTree_ExprBase;
var
  Decl: TLapeDeclaration;
begin
  Result := nil;

  try
    Decl := getDeclaration(AName);
  except
    on E: lpException do
      LapeException(lpString(E.Message), getDocPos());
  end;

  if (Decl <> nil) then
  begin
    Decl.IsUsed := True;
    if (lcoHints in FOptions) and (Decl is TLapeVar) and TLapeVar(Decl).HasHints() then
      TLapeVar(Decl).WriteHints({$IFDEF FPC}@{$ENDIF}Hint, Tokenizer.DocPos);

    Result := DeclNode(Decl, AName, Self);
  end
  else if (FInternalMethods[AName] <> nil) then
    Result := FInternalMethods[AName].Create(Self);
end;

function TLapeCompiler.HasBaseDefine(AName: lpString): Boolean;
begin
  Result := FBaseDefines.ExistsKey(AName);
end;

function TLapeCompiler.HasDefine(AName: lpString): Boolean;
var
  Value: lpString;
begin
  Result := getDefine(AName, Value);
end;

function TLapeCompiler.getDefine(AName: lpString; out AValue: lpString): Boolean;
begin
  case FDefines.Find(AName, AValue) of
    bTrue:  Result := True;
    bFalse: Result := False;
    else    Result := FBaseDefines.Find(AName, AValue);
  end;
end;

procedure TLapeCompiler.addDefine(AName: lpString; AValue: lpString);
begin
  // (defined again without a value: it keeps the one it has)
  if (AValue = '') and HasDefine(AName) then
    Exit;
  FDefines.Push(AName, AValue, True);
end;

procedure TLapeCompiler.addBaseDefine(AName: lpString; AValue: lpString);
begin
  if (not HasBaseDefine(AName)) then
    FBaseDefines.Add(AName, AValue);
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

function TLapeCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
var
  OldState: Pointer;
begin
  Result := nil;
  OldState := getTempTokenizerState(AName + ' = ' + Str + ';', '!addGlobalType::' + AName);

  try
    ParseTypeBlock();
    Result := getGlobalType(AName);
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar;
var
  OldState: Pointer;
  p: TDocPos;
begin
  Result := nil;
  OldState := getTempTokenizerState(AHeader + ';', '!addGlobalFuncHdr');

  try
    try
      Expect([tk_kw_Function, tk_kw_Procedure, tk_kw_Property, tk_kw_Operator]);
      Result := TLapeMethodParser(Self).ParseImportedMethod();

      Result.VarType.BaseType := ltImportedMethod;
      PPointer(Result.Ptr)^ := Value;
    finally
      resetTokenizerState(OldState);
    end;
  except
    on e: Exception do
    begin
      if (e is lpException) then
        with lpException(e) do
          if DocPos.isInternal then
          begin
            p := DocPos;
            p.FileName := '';

            LapeExceptionFmt(lpeExceptionIn, [Error, AHeader], p);
          end;

      raise;
    end;
  end;
end;

function TLapeCompiler.addGlobalFunc(AHeader: TLapeType_Method; Body: lpString): TLapeTree_Method;
var
  OldState: Pointer;
begin
  // (Body is in an internal file of the method's name)
  OldState := getTempTokenizerState(Body, '!' + AHeader.Name);

  try
    Result := TLapeMethodParser(Self).ParseMethodBody(AHeader);
    Result.Imported := True;
    FMethods.addStatement(Result);

    if (FConditionalStack.Count > 0) then
      LapeException(lpeConditionalNotClosed, popConditional());
  finally
    resetTokenizerState(OldState);
  end;
end;

function TLapeCompiler.addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar;
var
  Func: TLapeGlobalVar;
  Value: TMethod;
begin
  Func := addGlobalFunc(AHeader, AMethod);
  Assert(Func <> nil);

  Value.Code := AMethod;
  Value.Data := ASelf;

  Result := TLapeType_MethodOfObject(addManagedType(TLapeType_MethodOfObject.Create(Func.VarType as TLapeType_Method))).NewGlobalVar(Value);
  Result.isConstant := True;
  Result.Known := False;

  Func.DeclarationList.addDeclaration(Result);
  Func.Free();
end;

procedure TLapeCompiler.addCode(ACode: lpString; AFileName: lpString = ''; AfterScript: Boolean = True);
var
  Index, i: Integer;
  OldState: Pointer;
  Statements: TLapeTree_Base;
begin
  if (AFileName = '') then
  begin
    AFileName := '!addCode';
    if HasTokenizer() then
      AFileName := AFileName + '::' + Tokenizer.FileName;
  end;

  Index := FMethods.Statements.Count;
  OldState := getTempTokenizerState(ACode, AFileName);

  try
    Statements := ParseFile();
    if AfterScript then
      FPostTree.addStatement(Statements)
    else
      FPreTree.addStatement(Statements);

    for i := Index to FMethods.Statements.Count - 1 do
      TLapeTree_Method(FMethods.Statements[i]).Imported := True;
  finally
    resetTokenizerState(OldState);
  end;
end;

end.
