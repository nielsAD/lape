{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Calls in the syntax tree of methods
}
unit lpinvoke;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lptree;

type
  TLapeTree_Invoke = class(TLapeTree_DestExprBase)
  protected
    FCallee: TLapeTree_ExprBase;
    FPicked: TLapeGlobalVar;
    FParams: TLapeExpressionList;

    procedure CheckHints(VarType: TLapeType; Pos: PDocPos);
    procedure CheckPickedMethod(VarType: TLapeType); virtual;

    // TLapeTree_OpenArray casting for overloaded.getMethodIndex
    function CastsExactly(Index: Integer; ParamType: TLapeType): Boolean;
    function CastsLoosely(Index: Integer; ParamType: TLapeType): Boolean;

    function PickOverload(Overloaded: TLapeType_OverloadedMethod; ExpectType: TLapeType): Integer; virtual;
    function PickedMethod: TLapeGlobalVar; virtual;

    procedure setCallee(Node: TLapeTree_ExprBase); virtual;

    procedure DeleteChild(Node: TLapeTree_Base); override;
    function getParamTypes: TLapeTypeArray; virtual;
    function getParamTypesStr: lpString; virtual;

    // AVar on the stack: its address (Addr), or a copy
    function pushToStack(AVar: TResVar; APos: TDocPos; var Offset: Integer; Addr: Boolean = True): TResVar; virtual;
    procedure pushRefParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar; var Offset: Integer); virtual;
    procedure pushValueParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar; var Offset: Integer); virtual;
    // The Self that a method of an object holds, on the stack
    procedure pushSelf(Called: TResVar; var Offset: Integer); virtual;
    procedure pushParams(Method: TLapeType_Method; var Args: TResVarArray; Imported: Boolean; var Offset: Integer); virtual;

    function CallScript(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar; var Offset: Integer): TResVar; virtual;
    function CallImported(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar; var Offset: Integer): TResVar; virtual;
    // Use IsScriptMethod to determine what above to use (for function pointers etc)
    function CallEither(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar; var Offset: Integer): TResVar; virtual;
  public
    constructor Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(Ident: TLapeTree_ExprBase; ASource: TLapeTree_Base); reintroduce; overload; virtual;
    constructor Create(Ident: lpString; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); reintroduce;overload; virtual;
    constructor Create(Ident: lpString; ASource: TLapeTree_Base); reintroduce; overload; virtual;
    constructor Create(Ident: TLapeGlobalVar; ASource: TLapeTree_Base); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure ClearCache; override;
    function addParam(p: TLapeTree_ExprBase): Integer; virtual;
    // If the result type is known for overloads
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function resType: TLapeType; override;
    // Compile into a var
    function CompileInto(ResultVar: TResVar; var Offset: Integer): TResVar; virtual;
    // Compiled into a new var
    function Compile(var Offset: Integer): TResVar; override;
    function isInvoking: Boolean; override;

    // callee: what method is being called
    property Callee: TLapeTree_ExprBase read FCallee write setCallee;
    // what params to invoke with
    property Params: TLapeExpressionList read FParams;
  end;

  // object `new` constructor
  TLapeTree_InvokeConstruct = class(TLapeTree_Invoke)
  protected
    FObjectType: TLapeType;
    FHasConstruct: Boolean;

    procedure CheckPickedMethod(VarType: TLapeType); override;
  public
    constructor Create(ObjType: TLapeType; ASource: TLapeTree_Base); reintroduce; overload; virtual;
    function CompileInto(ResultVar: TResVar; var Offset: Integer): TResVar; override;
    property HasConstruct: Boolean read FHasConstruct;
  end;

  TLapeTree_InvokeProperty = class(TLapeTree_Invoke)
  protected
    FPropertyType: EPropertyType;
    FAssignOp: EOperator;

    procedure CheckPickedMethod(VarType: TLapeType); override;
    function PickOverload(Overloaded: TLapeType_OverloadedMethod; ExpectType: TLapeType): Integer; override;
  public
    function Compile(var Offset: Integer): TResVar; override;

    property PropertyType: EPropertyType read FPropertyType write FPropertyType;
    property AssignOp: EOperator read FAssignOp write FAssignOp;
  end;

  TLapeTree_InternalMethodClass = class of TLapeTree_InternalMethod;
  TLapeTree_InternalMethod = class(TLapeTree_Invoke)
  public type
    ESpecialParam = (
      spNo,             // normal parameters
      spForce,          // force the next expression to be the parameter even without parentheses
      spTypeThenParams, // type then parameters like: TMyType(1, 'abc')
      spType            // type parameter in <> like generics
    );
  protected
    FSpecialParam: ESpecialParam;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    procedure ClearCache; override;
    property SpecialParam: ESpecialParam read FSpecialParam;
  end;

implementation

uses
  lpvartypes_object, lpmessages, lpeval, lpinternalmethods;

procedure TLapeTree_Invoke.CheckHints(VarType: TLapeType; Pos: PDocPos);
begin
  if not (lcoHints in FCompilerOptions) then
    Exit;

  if (VarType is TLapeType_Method) and TLapeType_Method(VarType).HasHints() then
    with TLapeType_Method(VarType).Hints do
    begin
      if (ldhDeprecated in Types) then
        if (Message <> '') then
          FCompiler.Hint(lphDeprecatedMethodHint, [GetMethodName(VarType), Message], Pos^)
        else
          FCompiler.Hint(lphDeprecatedMethod, [GetMethodName(VarType)], Pos^);

      if (ldhExperimental  in Types) then FCompiler.Hint(lphExperimentalMethod, [GetMethodName(VarType)], Pos^);
      if (ldhUnImplemented in Types) then FCompiler.Hint(lphUnImplementedMethod, [GetMethodName(VarType)], Pos^);
    end;
end;

procedure TLapeTree_Invoke.CheckPickedMethod(VarType: TLapeType);
var
  i: Integer;
  Overloaded: TLapeType_OverloadedMethod;
  Msg, Available: String;
begin
  if (not (VarType is TLapeType_Method)) and (not (VarType is TLapeType_OverloadedMethod)) then
    LapeException(lpeCannotInvoke, FCallee._DocPos);

  // disallow calling object construct methods normally: only `new` or the inherited of a Construct, which give the object
  if (VarType is TLapeType_Method) and (TLapeType_Method(VarType).Res is TLapeType_Object) and
     (VarType.Name = 'Construct') and (not (Self is TLapeTree_InvokeConstruct)) then
    LapeException(lpeCannonInvokeConstruct, FCallee._DocPos);

  if (VarType is TLapeType_OverloadedMethod) then
  begin
    Overloaded := TLapeType_OverloadedMethod(VarType);

    if (Overloaded.MethodCount > 0) then
    begin
      Available := Format(lphAvailableMethods, [GetMethodName(VarType)]);
      for i := 0 to Overloaded.MethodCount - 1 do
      begin
        if (i > 0) then
          Available := Available + LineEnding;
        Available := Available + '> ' + MethodToString(Overloaded.Methods[i].VarType);
      end;
    end else
      Available := '';

    if (Overloaded.MethodDef = mdProperty) then
      Msg := lpeNoProperty
    else
      Msg := lpeNoOverloadedMethod;

    if (Available <> '') then
      LapeExceptionFmt(Msg, [getParamTypesStr()], FCallee._DocPos, Available)
    else
      LapeExceptionFmt(Msg, [getParamTypesStr()], FCallee._DocPos);
  end;
end;

function TLapeTree_Invoke.CastsExactly(Index: Integer; ParamType: TLapeType): Boolean;
begin
  Result := (FParams[Index] is TLapeTree_OpenArray) and TLapeTree_OpenArray(FParams[Index]).canCastTo(ParamType, True);
end;

function TLapeTree_Invoke.CastsLoosely(Index: Integer; ParamType: TLapeType): Boolean;
begin
  Result := (FParams[Index] is TLapeTree_OpenArray) and TLapeTree_OpenArray(FParams[Index]).canCastTo(ParamType, False);
end;

function TLapeTree_Invoke.PickOverload(Overloaded: TLapeType_OverloadedMethod; ExpectType: TLapeType): Integer;
var
  ObjectType: TLapeType;
  ParamTypes: TLapeTypeArray;
begin
  ObjectType := nil;
  if (FCallee is TLapeTree_Operator) and (TLapeTree_Operator(FCallee).Left <> nil) then
    ObjectType := TLapeTree_Operator(FCallee).Left.resType();

  // as they are, then with open array casting
  ParamTypes := getParamTypes();
  Result := Overloaded.getMethodIndex(ParamTypes, ExpectType, ObjectType, @_DocPos);
  if (Result < 0) then
    Result := Overloaded.getMethodIndex(ParamTypes, ExpectType, ObjectType, @_DocPos, {$IFDEF FPC}@{$ENDIF}CastsExactly);
  if (Result < 0) then
    Result := Overloaded.getMethodIndex(ParamTypes, ExpectType, ObjectType, @_DocPos, {$IFDEF FPC}@{$ENDIF}CastsLoosely);
end;

function TLapeTree_Invoke.PickedMethod: TLapeGlobalVar;
var
  Typ: TLapeType;
  Index: Integer;
begin
  Result := nil;
  if isEmpty(FCallee) then
    Exit;
  Typ := FCallee.resType();
  if (not (Typ is TLapeType_OverloadedMethod)) then
    Exit;

  if (FPicked = nil) then
  begin
    Index := PickOverload(TLapeType_OverloadedMethod(Typ), nil);
    if (Index >= 0) then
      FPicked := TLapeType_OverloadedMethod(Typ).Methods[Index];
  end;
  Result := FPicked;
end;

procedure TLapeTree_Invoke.setCallee(Node: TLapeTree_ExprBase);
begin
  if (FCallee <> nil) and (FCallee <> Node) then
  begin
    ClearCache();
    FCallee.Free();
  end;

  FCallee := Node;
  if (Node <> nil) then
    Node.Parent := Self;
end;

procedure TLapeTree_Invoke.DeleteChild(Node: TLapeTree_Base);
begin
  if (FCallee = Node) then
    FCallee := nil
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
  ParamTypes: TLapeTypeArray;
  ParamType: TLapeType;
begin
  Result := '';
  ParamTypes := getParamTypes();
  for i := 0 to High(ParamTypes) do
  begin
    if (i > 0) then
      Result := Result + ', ';
    ParamType := ParamTypes[i];
    if (ParamType <> nil) then
      if (not (ParamType.BaseType in [ltImportedMethod, ltScriptMethod, ltUnknown])) and (ParamType.Name <> '') then
        Result := Result + ParamType.Name
      else
        Result := Result + ParamType.AsString
    else
      Result := Result + '*unknown*';
  end;
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ADocPos);

  setCallee(Ident);
  FParams := TLapeExpressionList.Create(nil, dupAccept, False);
  FConstant := bFalse;
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

destructor TLapeTree_Invoke.Destroy;
var
  i: Integer;
begin
  setCallee(nil);

  for i := FParams.Count - 1 downto 0 do
    if (FParams[i] <> nil) and (FParams[i].Parent = Self) then
      FParams[i].Free();
  FreeAndNil(FParams);

  inherited;
end;

procedure TLapeTree_Invoke.ClearCache;
begin
  FPicked := nil;
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
var
  Typ: TLapeType;
  Index: Integer;
begin
  Result := Self;
  if (ExpectType = nil) or isEmpty(FCallee) then
    Exit;

  FPicked := nil;
  Typ := FCallee.resType();
  if (Typ is TLapeType_OverloadedMethod) then
  begin
    Index := PickOverload(TLapeType_OverloadedMethod(Typ), ExpectType);
    if (Index >= 0) then
      FPicked := TLapeType_OverloadedMethod(Typ).Methods[Index];
  end;
end;

function TLapeTree_Invoke.resType: TLapeType;
var
  Typ: TLapeType;
  Picked: TLapeGlobalVar;
begin
  if (FResType = nil) and (not isEmpty(FCallee)) then
  begin
    Picked := PickedMethod();
    if (Picked <> nil) then
      Typ := Picked.VarType
    else
      Typ := FCallee.resType();

    if (Typ is TLapeType_Method) then
      FResType := TLapeType_Method(Typ).Res;
  end;

  Result := inherited;
end;

function TLapeTree_Invoke.pushToStack(AVar: TResVar; APos: TDocPos; var Offset: Integer; Addr: Boolean = True): TResVar;
var
  NullVar: TResVar;
begin
  if (AVar.VarPos.MemPos = mpStack) then
    Exit(AVar);
  if (AVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) and ((AVar.VarType <> nil) or Addr) then
  begin
    Result := NullResVar;
    NullVar := NullResVar;

    Result.VarPos.MemPos := mpStack;
    if Addr then
    begin
      Result.VarType := FCompiler.getBaseType(ltPointer);
      FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, AVar, NullResVar, Offset, @APos);
    end
    else
    begin
      if AVar.HasType() and AVar.VarType.NeedInitialization then
        FCompiler.Emitter._InitStack(AVar.VarType.Size, Offset, @APos);
      Result.VarType := AVar.VarType;
      Result := AVar.VarType.Eval(op_Assign, NullVar, Result, AVar, [lefAssigningParam], Offset, @APos);
    end;
  end
  else
    LapeException(lpeImpossible, APos);
end;

procedure TLapeTree_Invoke.pushRefParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar; var Offset: Integer);
var
  Copy, NullVar: TResVar;
begin
  if (Arg.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    ArgExpr.CompileToTempVar(Offset, Arg);
  if (Arg.VarPos.MemPos = NullResVar.VarPos.MemPos) or ((Param.VarType <> Arg.VarType) and (not Arg.HasType())) then
    LapeException(lpeCannotInvoke);
  Assert(Arg.VarPos.MemPos <> mpStack);

  if (not (Param.ParType in Lape_ValParams)) and Arg.isConstant then
    LapeException(lpeVariableExpected);

  if (Param.VarType <> nil) and (not Param.VarType.Equals(Arg.VarType)) then
  begin
    if (not (Param.ParType in Lape_ValParams)) or (not Param.VarType.CompatibleWith(Arg.VarType)) then
      LapeExceptionFmt(
        lpeVariableOfTypeExpected, [Param.VarType.AsString, Arg.VarType.AsString], NullDocPos,
        Format(lphExpectedParameters, [GetMethodName(Method), MethodToString(Method)])
      );

    NullVar := NullResVar;
    Copy := _ResVar.New(FCompiler.getTempVar(Param.VarType));
    Arg := Param.VarType.Eval(op_Assign, NullVar, Copy, Arg, [lefAssigningParam], Offset, @_DocPos);
  end;

  pushToStack(Arg, _DocPos, Offset);
end;

procedure TLapeTree_Invoke.pushValueParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar; var Offset: Integer);
var
  Copy, NullVar: TResVar;
  Typ: TLapeType;
begin
  Assert(Param.ParType in Lape_ValParams);

  if (Arg.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    if (Param.VarType <> nil) and Param.VarType.NeedFinalization then
      ArgExpr.CompileToTempVar(Offset, Arg)
    else
    begin
      if (ArgExpr is TLapeTree_DestExprBase) then
      begin
        Typ := ArgExpr.resType();
        if (Typ <> nil) and (Typ.BaseType in LapeStackTypes) then
          TLapeTree_DestExprBase(ArgExpr).Dest := StackResVar;
      end;
      Arg := ArgExpr.Compile(Offset);
    end;
  if (Arg.VarPos.MemPos = NullResVar.VarPos.MemPos) or ((Param.VarType <> Arg.VarType) and (not Arg.HasType())) then
    LapeException(lpeCannotInvoke);

  if (Param.VarType = nil) or ((Arg.VarPos.MemPos = mpStack) and Param.VarType.Equals(Arg.VarType)) then
    Exit;

  if (not Param.VarType.CompatibleWith(Arg.VarType)) then
    LapeExceptionFmt(
      lpeVariableOfTypeExpected, [Param.VarType.AsString, Arg.VarType.AsString], NullDocPos,
      Format(lphExpectedParameters, [GetMethodName(Method), MethodToString(Method)])
    );

  NullVar := NullResVar;
  Copy := NullResVar;
  Copy.VarPos.MemPos := mpStack;
  Copy.VarType := Param.VarType;
  if Copy.VarType.NeedInitialization then
    FCompiler.Emitter._InitStack(Copy.VarType.Size, Offset, @_DocPos);
  Arg := Param.VarType.Eval(op_Assign, NullVar, Copy, Arg, [lefAssigningParam], Offset, @_DocPos);
  Assert((Arg.VarPos.MemPos = mpStack) and Arg.HasType());
end;

procedure TLapeTree_Invoke.pushSelf(Called: TResVar; var Offset: Integer);
var
  SelfPtr, NullVar, Copy: TResVar;
begin
  NullVar := NullResVar;

  if (Called.VarType is TLapeType_MethodOfObject) and (Called.VarPos.MemPos <> mpStack) then
  begin
    if (Called.VarType is TLapeType_MethodOfType) and (FCallee is TLapeTree_GlobalVar) then
      LapeExceptionFmt(lpeStaticMethodExpected, [GetMethodName(TLapeTree_GlobalVar(FCallee).GlobalVar.VarType)], Callee.DocPos);

    SelfPtr := Called;
    SelfPtr.VarType := FCompiler.getBaseType(ltPointer);
    SelfPtr.IncOffset(SizeOf(Pointer));

    pushToStack(SelfPtr, FCallee.DocPos, Offset, False);
  end;
end;

procedure TLapeTree_Invoke.pushParams(Method: TLapeType_Method; var Args: TResVarArray; Imported: Boolean; var Offset: Integer);
var
  i: Integer;
  ArgExpr: TLapeTree_ExprBase;
begin
  for i := 0 to Method.Params.Count - 1 do
  begin
    ArgExpr := FParams[i - Method.ImplicitParams];
    try
      if Imported or (Method.Params[i].ParType in Lape_RefParams) then
        pushRefParam(Method, Method.Params[i], ArgExpr, Args[i], Offset)
      else
        pushValueParam(Method, Method.Params[i], ArgExpr, Args[i], Offset);
    except
      on E: lpException do
        LapeExceptionReraise(E, [ArgExpr, Self]);
    end;
  end;
end;

function TLapeTree_Invoke.CallScript(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar; var Offset: Integer): TResVar;
var
  Method: TLapeType_Method;
begin
  Assert(Called.VarType is TLapeType_Method);
  Assert(Called.VarType.BaseType in [ltPointer, ltScriptMethod]);
  Assert(Length(Args) = TLapeType_Method(Called.VarType).Params.Count);
  Result := NullResVar;
  Method := TLapeType_Method(Called.VarType);

  pushParams(Method, Args, False, Offset);
  Dest := NullResVar;

  if (Method.Res <> nil) then
  begin
    Result := ResultVar;
    if (Result.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    begin
      Result := _ResVar.New(FCompiler.getTempVar(Method.Res));
      if (lcoAlwaysInitialize in FCompilerOptions) then
        FCompiler.VarToDefault(Result, Offset, @Self._DocPos);
    end;
    Result.isConstant := True;
    pushToStack(Result, Self._DocPos, Offset);
  end;

  FCompiler.Emitter._IncCall(Called, Method.ParamSize, Offset, @Self._DocPos);
end;

function TLapeTree_Invoke.CallImported(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar; var Offset: Integer): TResVar;
var
  Init: TResVar;
  ParamCount: Integer;
  Method: TLapeType_Method;
begin
  Assert(Called.VarType is TLapeType_Method);
  Assert(Called.VarType.BaseType in [ltPointer, ltImportedMethod]);
  Assert(Length(Args) = TLapeType_Method(Called.VarType).Params.Count);
  Result := NullResVar;
  Method := TLapeType_Method(Called.VarType);

  pushParams(Method, Args, True, Offset);

  ParamCount := Method.Params.Count;
  if (Called.VarType is TLapeType_MethodOfObject) then
    Inc(ParamCount);

  if (Method.Res = nil) then
  begin
    Dest := NullResVar;
    FCompiler.Emitter._InvokeImportedProc(Called, ParamCount * SizeOf(Pointer), Offset, @Self._DocPos);
    Exit;
  end;

  // the result goes in ResultVar (or make it)
  Result := ResultVar;
  if (Result.VarPos.MemPos = NullResVar.VarPos.MemPos) then
  begin
    Result.VarType := Method.Res;
    if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      FDest := VarResVar;
    FCompiler.getDestVar(FDest, Result, op_Unknown);

    if (lcoInitExternalResult in FCompilerOptions) then
    begin
      Init := Result;
      Init.isConstant := False;
      FCompiler.VarToDefault(Init, Offset, @Self._DocPos);
    end;
  end;

  FCompiler.Emitter._InvokeImportedFunc(Called, Result, ParamCount * SizeOf(Pointer), Offset, @Self._DocPos);
end;

function TLapeTree_Invoke.CallEither(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar; var Offset: Integer): TResVar;
var
  IsScript, ScriptRes: TResVar;
  ScriptArgs: TResVarArray;
  ToImported, ToEnd: Integer;
begin
  with TLapeTree_InternalMethod_IsScriptMethod.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(Called, Self));
    IsScript := Compile(Offset);
  finally
    Free();
  end;

  FCompiler.Emitter.FullEmit := False;
  ToImported := FCompiler.Emitter._JmpRIfNot(0, IsScript, Offset, @_DocPos);
  FCompiler.Emitter.FullEmit := True;

  ScriptArgs := Copy(Args);
  ScriptRes := CallScript(Called, ScriptArgs, ResultVar, Offset);
  ToEnd := FCompiler.Emitter._JmpR(0, Offset, @_DocPos);
  FCompiler.Emitter._JmpRIfNot(Offset - ToImported, IsScript, ToImported, @_DocPos);

  Dest := ScriptRes;
  Result := CallImported(Called, Args, ResultVar, Offset);
  FCompiler.Emitter._JmpR(Offset - ToEnd, ToEnd, @_DocPos);
  Dest := NullResVar;

  Assert(Result.VarPos.MemPos = ScriptRes.VarPos.MemPos);
end;

function TLapeTree_Invoke.CompileInto(ResultVar: TResVar; var Offset: Integer): TResVar;
var
  i, ParamCount, ArgIndex: Integer;
  Picked, IndexVar: TLapeGlobalVar;
  Overloaded: TLapeType_OverloadedMethod;
  Called, NullVar: TResVar;
  Method: TLapeType_Method;
  Args: TResVarArray;
begin
  Result := NullResVar;
  if (FCallee = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  Picked := PickedMethod();
  if (Picked = nil) then
    Called := FCallee.Compile(Offset)
  else
  begin
    Overloaded := TLapeType_OverloadedMethod(FCallee.resType());
    IndexVar := FCompiler.getConstant(Overloaded.Declarations.IndexOf(Picked));
    Assert(IndexVar.AsInteger >= 0);

    if (FCallee is TLapeTree_GlobalVar) and
       Overloaded.CanEvalConst(op_Index, TLapeTree_GlobalVar(FCallee).GlobalVar, IndexVar)
    then
      Called := _ResVar.New(FCompiler.addManagedDecl(Overloaded.EvalConst(op_Index, TLapeTree_GlobalVar(FCallee).GlobalVar, IndexVar, [])) as TLapeGlobalVar)
    else
    begin
      Called := FCallee.Compile(Offset);
      NullVar := NullResVar;
      Called := Overloaded.Eval(op_Index, NullVar, Called, _ResVar.New(IndexVar), [], Offset, @FCallee._DocPos);
    end;
  end;

  CheckPickedMethod(Called.VarType);
  CheckHints(Called.VarType, @FCallee._DocPos);
  Method := TLapeType_Method(Called.VarType);

  ParamCount := Method.Params.Count - Method.ImplicitParams;
  if (FParams.Count > ParamCount) then
    LapeException(lpeTooMuchParameters, FParams[ParamCount].DocPos);

  if (lcoInitExternalResult in FCompilerOptions) then
    Dest := NullResVar;

  pushSelf(Called, Offset);

  // build param vars
  SetLength(Args, Method.Params.Count);
  for i := 0 to Method.Params.Count - 1 do
  begin
    ArgIndex := i - Method.ImplicitParams;
    if isEmpty(FParams[ArgIndex]) then
      if (Method.Params[i].Default <> nil) then
        Args[i] := _ResVar.New(Method.Params[i].Default).InScope(FCompiler.StackInfo, @Self._DocPos)
      else
        LapeExceptionFmt(lpeNoDefaultForParam, [ArgIndex + 1], [FParams[ArgIndex], Self])
    else
    begin
      FParams[ArgIndex] := FParams[ArgIndex].setExpectedType(Method.Params[i].VarType) as TLapeTree_ExprBase;
      Args[i] := NullResVar;
    end;
  end;

  case Method.BaseType of
    ltScriptMethod:   Result := CallScript(Called, Args, ResultVar, Offset);
    ltImportedMethod: Result := CallImported(Called, Args, ResultVar, Offset);
    else
      Result := CallEither(Called, Args, ResultVar, Offset);
  end;
end;

function TLapeTree_Invoke.Compile(var Offset: Integer): TResVar;
begin
  Result := CompileInto(NullResVar, Offset);
end;

function TLapeTree_Invoke.isInvoking: Boolean;
begin
  Result := True;
end;

constructor TLapeTree_InvokeConstruct.Create(ObjType: TLapeType; ASource: TLapeTree_Base);
var
  Construct: TLapeTree_Operator;
begin
  Construct := TLapeTree_Operator.Create(op_Dot, ASource);
  Construct.Left := TLapeTree_VarType.Create(ObjType, ASource);
  Construct.Right := TLapeTree_Field.Create(lpString('Construct'), ASource);
  FHasConstruct := (Construct.resType() <> nil);
  inherited Create(Construct, ASource);
  FObjectType := ObjType;
end;

procedure TLapeTree_InvokeConstruct.CheckPickedMethod(VarType: TLapeType);
begin
  if (VarType is TLapeType_Method) then
  begin
    if (VarType is TLapeType_MethodOfObject) then
      LapeExceptionFmt(lpeInvalidConstructMethod1, ['Not static method'], VarType.DocPos);
    if (not FObjectType.Equals(TLapeType_Method(VarType).Res)) then
      LapeExceptionFmt(lpeInvalidConstructMethod1, ['Result is not object type'], VarType.DocPos);
  end
  else if (VarType <> nil) and (not (VarType is TLapeType_OverloadedMethod)) then
    LapeException(lpeInvalidConstructMethod, VarType.DocPos);

  inherited;
end;

function TLapeTree_InvokeConstruct.CompileInto(ResultVar: TResVar; var Offset: Integer): TResVar;
begin
  Assert(ResultVar.VarPos.MemPos <> NullResVar.VarPos.MemPos);
  Assert(FObjectType is TLapeType_Object);
  Result := inherited;
end;

procedure TLapeTree_InvokeProperty.CheckPickedMethod(VarType: TLapeType);
var
  i: Integer;
  Reads, Writes: Boolean;
begin
  if (VarType is TLapeType_OverloadedMethod) and (TLapeType_OverloadedMethod(VarType).MethodDef = mdProperty) then
    with TLapeType_OverloadedMethod(VarType) do
    begin
      Writes := False;
      Reads := False;
      for i := 0 to MethodCount - 1 do
        if (TLapeType_Method(Methods[i].VarType).Res = nil) then
          Writes := True
        else
          Reads := True;

      if (FPropertyType = ptWrite) and (not Writes) then
        LapeExceptionFmt(lpePropertyReadOnly, [GetMethodName(VarType)], FCallee._DocPos);
      if (FPropertyType = ptRead) and (not Reads) then
        LapeExceptionFmt(lpePropertyWriteOnly, [GetMethodName(VarType)], FCallee._DocPos);
    end;

  inherited;
end;

function TLapeTree_InvokeProperty.PickOverload(Overloaded: TLapeType_OverloadedMethod; ExpectType: TLapeType): Integer;
begin
  Result := inherited;
  if (Result >= 0) and ((TLapeType_Method(Overloaded.Methods[Result].VarType).Res = nil) <> (FPropertyType = ptWrite)) then
    Result := -1;
end;

function TLapeTree_InvokeProperty.Compile(var Offset: Integer): TResVar;
var
  Value, Arg: TLapeTree_ExprBase;
  ReadCall: TLapeTree_InvokeProperty;
  NewValue: TLapeTree_Operator;
  ArgVar: TResVar;
  i: Integer;

  // in a variable not on the stack
  function CompileToVar(Node: TLapeTree_ExprBase): TResVar;
  begin
    Node.CompileToTempVar(Offset, Result);
    Assert(Result.VarPos.MemPos <> mpStack);
  end;

begin
  Assert(FCallee <> nil);

  // x.P[a] += v
  // becomes x.P[a] := x.P[a] + v
  // evaluating x and a only once
  if (FPropertyType = ptWrite) and (FAssignOp in CompoundOperators) then
  begin
    Value := FParams[FParams.Count - 1];
    FParams.Delete(FParams.Count - 1);

    if (FCallee is TLapeTree_Operator) and (TLapeTree_Operator(FCallee).OperatorType = op_Dot) and (not isEmpty(TLapeTree_Operator(FCallee).Left)) then
      with TLapeTree_Operator(FCallee) do
        Left := TLapeTree_ResVar.Create(CompileToVar(Left), Left);

    ReadCall := TLapeTree_InvokeProperty.Create(TLapeTree_ResVar.Create(CompileToVar(FCallee), FCallee), Self);
    for i := 0 to FParams.Count - 1 do
    begin
      ArgVar := CompileToVar(FParams[i]);
      ReadCall.addParam(TLapeTree_ResVar.Create(ArgVar, FParams[i]));
      Arg := FParams[i];
      FParams[i] := TLapeTree_ResVar.Create(ArgVar, Arg);
      FParams[i].Parent := Self;
      Arg.Free();
    end;

    NewValue := TLapeTree_Operator.Create(ResolveCompoundOp(FAssignOp, Value.resType()), Self);
    NewValue.Left := ReadCall;
    NewValue.Right := Value;
    addParam(NewValue);
  end;
  Result := inherited;
end;

constructor TLapeTree_InternalMethod.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create(TLapeTree_ExprBase(nil), ACompiler, ADocPos);
  FConstant := bFalse;
end;

procedure TLapeTree_InternalMethod.ClearCache;
begin
  {nothing}
end;

end.
