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
    FParams: TLapeExpressionList;

    procedure CheckHints(VarType: TLapeType); virtual;
    procedure CheckCalled(VarType: TLapeType); virtual;
    procedure NoMethodFits(const Methods: TLapeDeclArray); virtual;

    // What is called, and the Self to give it when it has one
    function EmitCallee(out SelfVar: TResVar): TResVar; virtual;

    procedure setCallee(Node: TLapeTree_ExprBase); virtual;

    procedure DeleteChild(Node: TLapeTree_Base); override;
    function getParamTypes: TLapeTypeArray; virtual;
    function getParamTypesStr: lpString; virtual;

    // AVar on the stack: its address (Addr), or a copy
    procedure pushToStack(AVar: TResVar); virtual;
    procedure pushRefParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar); virtual;
    procedure pushValueParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar); virtual;
    // The Self that a method of an object holds, on the stack
    procedure pushSelf(Called, SelfVar: TResVar); virtual;
    procedure pushParams(Method: TLapeType_Method; var Args: TResVarArray; Imported: Boolean); virtual;

    function CallScript(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar): TResVar; virtual;
    function CallImported(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar): TResVar; virtual;
    // Use IsScriptMethod to determine what above to use (for function pointers etc)
    function CallEither(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar): TResVar; virtual;

    function CompileInto(ResultVar: TResVar): TResVar; virtual;
  public
    constructor Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase); reintroduce; overload; virtual;
    constructor Create(Ident: TLapeTree_ExprBase; ASource: TLapeTree_Base); reintroduce; overload; virtual;
    constructor Create(Ident: lpString; ACompiler: TLapeCompilerBase); reintroduce;overload; virtual;
    constructor Create(Ident: lpString; ASource: TLapeTree_Base); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure ClearCache; override;
    function addParam(p: TLapeTree_ExprBase): Integer; virtual;
    // If the result type is known for overloads
    function setExpectedType(ExpectType: TLapeType): TLapeTree_Base; override;

    function resType: TLapeType; override;
    function isInvoking: Boolean; override;

    // callee: what method is being called
    function Compile: TResVar; override;
    property Callee: TLapeTree_ExprBase read FCallee;
    // what params to invoke with
    property Params: TLapeExpressionList read FParams;
  end;

  // object `new` constructor
  // A call of a value: a method variable, what a call returned, a method that is already known
  TLapeTree_InvokePtr = class(TLapeTree_Invoke)
  protected
    function EmitCallee(out SelfVar: TResVar): TResVar; override;
  public
    constructor Create(Ident: TLapeGlobalVar; ASource: TLapeTree_Base); reintroduce; overload; virtual;
  end;

  TLapeTree_InvokeConstruct = class(TLapeTree_Invoke)
  protected
    FObjectType: TLapeType;

    procedure CheckCalled(VarType: TLapeType); override;
    function CompileInto(ResultVar: TResVar): TResVar; override;
  public
    constructor Create(ObjType: TLapeType; ASource: TLapeTree_Base); reintroduce; overload; virtual;
  end;

  TLapeTree_InvokeProperty = class(TLapeTree_Invoke)
  protected
    FAssignOp: EOperator;

    procedure NoMethodFits(const Methods: TLapeDeclArray); override;
    function EmitCallee(out SelfVar: TResVar): TResVar; override;
  public
    function Compile: TResVar; override;
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
    constructor Create(ACompiler: TLapeCompilerBase); override;
    procedure ClearCache; override;
    property SpecialParam: ESpecialParam read FSpecialParam;
  end;

implementation

uses
  lpvartypes_object, lpmessages, lpeval, lpinternalmethods;

procedure TLapeTree_Invoke.CheckHints(VarType: TLapeType);
begin
  if not (lcoHints in FCompilerOptions) then
    Exit;

  if TLapeType_Method(VarType).HasHints() then
    with TLapeType_Method(VarType).Hints do
    begin
      if (ldhDeprecated in Types) then
        if (Message <> '') then
          FCompiler.Hint(lphDeprecatedMethodHint, [GetMethodName(VarType), Message], FCallee.DocPos)
        else
          FCompiler.Hint(lphDeprecatedMethod, [GetMethodName(VarType)], FCallee.DocPos);

      if (ldhExperimental  in Types) then FCompiler.Hint(lphExperimentalMethod, [GetMethodName(VarType)], FCallee.DocPos);
      if (ldhUnImplemented in Types) then FCompiler.Hint(lphUnImplementedMethod, [GetMethodName(VarType)], FCallee.DocPos);
    end;
end;

procedure TLapeTree_Invoke.CheckCalled(VarType: TLapeType);
begin
  if (not (VarType is TLapeType_Method)) then
    LapeException(lpeCannotInvoke, FCallee.DocPos);

  // disallow calling object construct methods normally: only `new` or the inherited of a Construct, which give the object
  if (TLapeType_Method(VarType).Res is TLapeType_Object) and (VarType.Name = 'Construct') and (not (Self is TLapeTree_InvokeConstruct)) then
    LapeException(lpeCannonInvokeConstruct, FCallee.DocPos);
end;

procedure TLapeTree_Invoke.NoMethodFits(const Methods: TLapeDeclArray);
var
  i: Integer;
  Msg, Available: String;
begin
  Available := '';
  for i := 0 to High(Methods) do
  begin
    if (i = 0) then
      Available := Format(lphAvailableMethods, [GetMethodName(TLapeGlobalVar(Methods[0]).VarType)])
    else
      Available := Available + LineEnding;
    Available := Available + '> ' + MethodToString(TLapeGlobalVar(Methods[i]).VarType);
  end;

  if IsProperty(Methods) then
    Msg := lpeNoProperty
  else
    Msg := lpeNoOverloadedMethod;

  if (Available <> '') then
    LapeExceptionFmt(Msg, [getParamTypesStr()], FCallee.DocPos, Available)
  else
    LapeExceptionFmt(Msg, [getParamTypesStr()], FCallee.DocPos);
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
begin
  Result := ExprTypes(FParams);
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

constructor TLapeTree_Invoke.Create(Ident: TLapeTree_ExprBase; ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);

  FParams := TLapeExpressionList.Create(nil, dupAccept, False);
  setCallee(Ident);
  FConstant := bFalse;
end;

constructor TLapeTree_Invoke.Create(Ident: TLapeTree_ExprBase; ASource: TLapeTree_Base);
begin
  Assert(ASource <> nil);
  Create(Ident, ASource.Compiler);
  FDocPos := ASource.DocPos;
  FCompilerOptions := ASource.CompilerOptions;
end;

constructor TLapeTree_Invoke.Create(Ident: lpString; ACompiler: TLapeCompilerBase);
begin
  Assert(ACompiler <> nil);
  Create(GlobalNode(Ident, ACompiler), ACompiler);
end;

constructor TLapeTree_Invoke.Create(Ident: lpString; ASource: TLapeTree_Base);
begin
  Assert((ASource <> nil) and (ASource.Compiler <> nil));
  Create(GlobalNode(Ident, ASource.Compiler, ASource), ASource);
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
  if (FCallee is TLapeTree_MethodByName) then
    FCallee.ClearCache();
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
  Result := Self;
  if (FCallee is TLapeTree_MethodByName) then
    FCallee.setExpectedType(ExpectType);
end;

function TLapeTree_Invoke.resType: TLapeType;
var
  Typ: TLapeType;
  Picked: TLapeGlobalVar;
begin
  if (FResType = nil) and (FCallee <> nil) then
  begin
    Typ := FCallee.resType();
    if (FCallee is TLapeTree_MethodByName) then
    begin
      Picked := TLapeTree_MethodByName(FCallee).Resolve(FParams);
      if (Picked <> nil) then
        Typ := Picked.VarType;
    end;

    if (Typ is TLapeType_Method) then
      FResType := TLapeType_Method(Typ).Res;
  end;

  Result := inherited;
end;

procedure TLapeTree_Invoke.pushToStack(AVar: TResVar);
var
  Stack: TResVar;
begin
  if (AVar.VarPos.MemPos = mpStack) then
    Exit;
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    LapeException(lpeImpossible, FCompiler.DocPos);

  Stack := NullResVar;
  Stack.VarPos.MemPos := mpStack;
  Stack.VarType := FCompiler.getBaseType(ltPointer);
  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Stack, AVar, NullResVar);
end;

procedure TLapeTree_Invoke.pushRefParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar);
var
  Copy, NullVar: TResVar;
begin
  if (Arg.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    Arg := ArgExpr.Compile();
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
    FCompiler.Emitter.DocPos := DocPos;
    Arg := Param.VarType.Eval(op_Assign, NullVar, Copy, Arg, []);
  end;

  pushToStack(Arg);
end;

procedure TLapeTree_Invoke.pushValueParam(Method: TLapeType_Method; const Param: TLapeParameter; ArgExpr: TLapeTree_ExprBase; var Arg: TResVar);
var
  Copy, NullVar: TResVar;
  Typ: TLapeType;
begin
  Assert(Param.ParType in Lape_ValParams);

  if (Arg.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    if (Param.VarType <> nil) and Param.VarType.NeedFinalization then
      Arg := ArgExpr.Compile()
    else
    begin
      if (ArgExpr is TLapeTree_DestExprBase) then
      begin
        Typ := ArgExpr.resType();
        if (Typ <> nil) and (Typ.BaseType in LapeStackTypes) then
          TLapeTree_DestExprBase(ArgExpr).Dest := StackResVar;
      end;
      Arg := ArgExpr.Compile();
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
  FCompiler.Emitter.DocPos := DocPos;
  if Copy.VarType.NeedInitialization then
    FCompiler.Emitter._InitStack(Copy.VarType.Size);
  FCompiler.Emitter.DocPos := DocPos;
  Arg := Param.VarType.Eval(op_Assign, NullVar, Copy, Arg, []);
  Assert((Arg.VarPos.MemPos = mpStack) and Arg.HasType());
end;

procedure TLapeTree_Invoke.pushSelf(Called, SelfVar: TResVar);
var
  SelfPtr, Stack, NullVar: TResVar;
begin
  if (SelfVar.VarPos.MemPos <> NullResVar.VarPos.MemPos) then
    pushToStack(SelfVar)
  else if (Called.VarType is TLapeType_MethodOfObject) and (Called.VarPos.MemPos <> mpStack) then
  begin
    if (Called.VarType is TLapeType_MethodOfType) and (Called.VarPos.MemPos = mpMem) and (Called.VarPos.GlobalVar is TLapeMethod) then
      LapeExceptionFmt(lpeStaticMethodExpected, [GetMethodName(Called.VarType)], Callee.DocPos);

    SelfPtr := Called;
    SelfPtr.VarType := FCompiler.getBaseType(ltPointer);
    SelfPtr.IncOffset(SizeOf(Pointer));

    Stack := NullResVar;
    Stack.VarPos.MemPos := mpStack;
    Stack.VarType := SelfPtr.VarType;
    NullVar := NullResVar;
    FCompiler.Emitter.DocPos := DocPos;
    SelfPtr.VarType.Eval(op_Assign, NullVar, Stack, SelfPtr, []);
  end;
end;

procedure TLapeTree_Invoke.pushParams(Method: TLapeType_Method; var Args: TResVarArray; Imported: Boolean);
var
  i: Integer;
  ArgExpr: TLapeTree_ExprBase;
begin
  for i := 0 to Method.Params.Count - 1 do
  begin
    ArgExpr := FParams[i - Method.ImplicitParams];
    try
      if Imported or (Method.Params[i].ParType in Lape_RefParams) then
        pushRefParam(Method, Method.Params[i], ArgExpr, Args[i])
      else
        pushValueParam(Method, Method.Params[i], ArgExpr, Args[i]);
    except
      on E: lpException do
        LapeExceptionReraise(E, [ArgExpr, Self]);
    end;
  end;
end;

function TLapeTree_Invoke.CallScript(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar): TResVar;
var
  Method: TLapeType_Method;
begin
  Assert(Called.VarType is TLapeType_Method);
  Assert(Called.VarType.BaseType in [ltPointer, ltScriptMethod]);
  Assert(Length(Args) = TLapeType_Method(Called.VarType).Params.Count);
  Result := NullResVar;
  Method := TLapeType_Method(Called.VarType);

  pushParams(Method, Args, False);
  Dest := NullResVar;

  if (Method.Res <> nil) then
  begin
    Result := ResultVar;
    if (Result.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    begin
      Result := _ResVar.New(FCompiler.getTempVar(Method.Res));
      FCompiler.Emitter.DocPos := DocPos;
      if (lcoAlwaysInitialize in FCompilerOptions) then
        FCompiler.VarToDefault(Result);
    end;
    Result.isConstant := True;
    pushToStack(Result);
  end;

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._IncCall(Called, Method.ParamSize);
end;

function TLapeTree_Invoke.CallImported(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar): TResVar;
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

  pushParams(Method, Args, True);

  ParamCount := Method.Params.Count;
  if (Called.VarType is TLapeType_MethodOfObject) then
    Inc(ParamCount);

  if (Method.Res = nil) then
  begin
    Dest := NullResVar;
    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._InvokeImportedProc(Called, ParamCount * SizeOf(Pointer));
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
      FCompiler.Emitter.DocPos := DocPos;
      FCompiler.VarToDefault(Init);
    end;
  end;

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._InvokeImportedFunc(Called, Result, ParamCount * SizeOf(Pointer));
end;

function TLapeTree_Invoke.CallEither(Called: TResVar; var Args: TResVarArray; ResultVar: TResVar): TResVar;
var
  IsScript, ScriptRes: TResVar;
  ScriptArgs: TResVarArray;
  ToImported, ToEnd: Integer;
begin
  with TLapeTree_InternalMethod_IsScriptMethod.Create(Self) do
  try
    addParam(TLapeTree_Var.Create(Called, Self));
    IsScript := Compile();
  finally
    Free();
  end;

  FCompiler.Emitter.DocPos := DocPos;
  ToImported := FCompiler.Emitter._JmpRIfNot(0, IsScript);

  ScriptArgs := Copy(Args);
  ScriptRes := CallScript(Called, ScriptArgs, ResultVar);
  ToEnd := FCompiler.Emitter._JmpR(0);
  FCompiler.Emitter.PatchJump(ToImported, FCompiler.Emitter.CodeLen);

  Dest := ScriptRes;
  Result := CallImported(Called, Args, ResultVar);
  FCompiler.Emitter.PatchJump(ToEnd, FCompiler.Emitter.CodeLen);
  Dest := NullResVar;

  Assert(Result.VarPos.MemPos = ScriptRes.VarPos.MemPos);
end;

function TLapeTree_Invoke.EmitCallee(out SelfVar: TResVar): TResVar;
var
  Picked: TLapeGlobalVar;
  Header: TLapeType_Method;
  LeftVar: TResVar;
begin
  if (not (FCallee is TLapeTree_MethodByName)) then
    LapeException(lpeCannotInvoke, FCallee.DocPos);

  Picked := TLapeTree_MethodByName(FCallee).Resolve(FParams);
  if (Picked = nil) then
    with TLapeTree_MethodByName(FCallee) do
      if (Methods() <> nil) or isArrayHelper() then
        NoMethodFits(Methods())
      else
        LapeExceptionFmt(lpeUnknownDeclaration, [Name], DocPos);

  Result := _ResVar.New(Picked);

  // (on the stack it can not be pointed at: the method and Self go together)
  Header := TLapeType_Method(Picked.VarType);
  if TLapeTree_MethodByName(FCallee).SelfFor(Header, LeftVar, SelfVar) and (SelfVar.VarPos.MemPos = mpStack) then
  begin
    FCompiler.Emitter.DocPos := DocPos;
    Result := Header.BindSelf(Picked, SelfVar);
    SelfVar := NullResVar;
  end;
end;

constructor TLapeTree_InvokePtr.Create(Ident: TLapeGlobalVar; ASource: TLapeTree_Base);
begin
  inherited Create(TLapeTree_Var.Create(Ident, ASource), ASource);
end;

function TLapeTree_InvokePtr.EmitCallee(out SelfVar: TResVar): TResVar;
begin
  SelfVar := NullResVar;
  Result := FCallee.Compile();
end;

function TLapeTree_Invoke.CompileInto(ResultVar: TResVar): TResVar;
var
  i, ParamCount, ArgIndex: Integer;
  Called, SelfVar: TResVar;
  Method: TLapeType_Method;
  Args: TResVarArray;
begin
  Result := NullResVar;
  if (FCallee = nil) then
    LapeException(lpeImpossible, DocPos);

  Called := EmitCallee(SelfVar);

  CheckCalled(Called.VarType);
  CheckHints(Called.VarType);
  Method := TLapeType_Method(Called.VarType);

  ParamCount := Method.Params.Count - Method.ImplicitParams;
  if (FParams.Count > ParamCount) then
    LapeException(lpeTooMuchParameters, FParams[ParamCount].DocPos);

  if (lcoInitExternalResult in FCompilerOptions) then
    Dest := NullResVar;

  pushSelf(Called, SelfVar);

  // build param vars
  SetLength(Args, Method.Params.Count);
  for i := 0 to Method.Params.Count - 1 do
  begin
    ArgIndex := i - Method.ImplicitParams;
    if (FParams[ArgIndex] = nil) then
      if (Method.Params[i].Default <> nil) then
        Args[i] := _ResVar.New(Method.Params[i].Default).InScope(FCompiler.StackInfo)
      else
        LapeExceptionFmt(lpeNoDefaultForParam, [ArgIndex + 1], [FParams[ArgIndex], Self])
    else
    begin
      FParams[ArgIndex] := FParams[ArgIndex].setExpectedType(Method.Params[i].VarType) as TLapeTree_ExprBase;
      Args[i] := NullResVar;
    end;
  end;

  case Method.BaseType of
    ltScriptMethod:   Result := CallScript(Called, Args, ResultVar);
    ltImportedMethod: Result := CallImported(Called, Args, ResultVar);
    else
      Result := CallEither(Called, Args, ResultVar);
  end;
end;

function TLapeTree_Invoke.Compile: TResVar;
begin
  Result := CompileInto(NullResVar);
end;

function TLapeTree_Invoke.isInvoking: Boolean;
begin
  Result := True;
end;

constructor TLapeTree_InvokeConstruct.Create(ObjType: TLapeType; ASource: TLapeTree_Base);
begin
  inherited Create(TLapeTree_MethodByName.Create(TLapeTree_VarType.Create(ObjType, ASource), lpString('Construct'), ASource), ASource);
  FObjectType := ObjType;
end;

procedure TLapeTree_InvokeConstruct.CheckCalled(VarType: TLapeType);
begin
  if (VarType is TLapeType_MethodOfObject) then
    LapeExceptionFmt(lpeInvalidConstructMethod1, ['Not static method'], VarType.DocPos);
  if (not FObjectType.Equals(TLapeType_Method(VarType).Res)) then
    LapeExceptionFmt(lpeInvalidConstructMethod1, ['Result is not object type'], VarType.DocPos);

  inherited;
end;

function TLapeTree_InvokeConstruct.CompileInto(ResultVar: TResVar): TResVar;
begin
  Assert(ResultVar.VarPos.MemPos <> NullResVar.VarPos.MemPos);
  Assert(FObjectType is TLapeType_Object);
  Result := inherited;
end;

procedure TLapeTree_InvokeProperty.NoMethodFits(const Methods: TLapeDeclArray);
var
  i: Integer;
  Reads, Writes: Boolean;
begin
  Writes := False;
  Reads := False;
  for i := 0 to High(Methods) do
    if (TLapeType_Method(TLapeGlobalVar(Methods[i]).VarType).Res = nil) then
      Writes := True
    else
      Reads := True;

  if (FAssignOp in AssignOperators) and (not Writes) then
    LapeExceptionFmt(lpePropertyReadOnly, [GetMethodName(TLapeGlobalVar(Methods[0]).VarType)], FCallee.DocPos);
  if (not (FAssignOp in AssignOperators)) and (not Reads) then
    LapeExceptionFmt(lpePropertyWriteOnly, [GetMethodName(TLapeGlobalVar(Methods[0]).VarType)], FCallee.DocPos);

  inherited;
end;

// (a write has no result, a read has one)
function TLapeTree_InvokeProperty.EmitCallee(out SelfVar: TResVar): TResVar;
var
  Picked: TLapeGlobalVar;
begin
  Picked := TLapeTree_MethodByName(FCallee).Resolve(FParams);
  if (Picked <> nil) and ((TLapeType_Method(Picked.VarType).Res = nil) <> (FAssignOp in AssignOperators)) then
    NoMethodFits(TLapeTree_MethodByName(FCallee).Methods());

  Result := inherited;
end;

function TLapeTree_InvokeProperty.Compile: TResVar;
var
  Value, Arg: TLapeTree_ExprBase;
  ReadCall: TLapeTree_InvokeProperty;
  ReadCallee: TLapeTree_MethodByName;
  NewValue: TLapeTree_Operator;
  ArgVar: TResVar;
  i: Integer;

  // in a variable not on the stack
  function CompileToVar(Node: TLapeTree_ExprBase): TResVar;
  begin
    Result := Node.Compile();
    Assert(Result.VarPos.MemPos <> mpStack);
  end;

begin
  Assert(FCallee <> nil);

  // x.P[a] += v
  // becomes x.P[a] := x.P[a] + v
  // evaluating x and a only once
  if (FAssignOp in CompoundOperators) then
  begin
    Value := FParams[FParams.Count - 1];
    FParams.Delete(FParams.Count - 1);

    Assert(FCallee is TLapeTree_MethodByName);
    with TLapeTree_MethodByName(FCallee) do
      if (Left is TLapeTree_VarType) then
        ReadCallee := TLapeTree_MethodByName.Create(TLapeTree_VarType.Create(TLapeTree_VarType(Left).VarType, Left), Name, Left)
      else
      begin
        Left := TLapeTree_Var.Create(CompileToVar(Left), Left);
        ReadCallee := TLapeTree_MethodByName.Create(TLapeTree_Var.Create(TLapeTree_Var(Left).ResVar, Left), Name, Left);
      end;

    ReadCall := TLapeTree_InvokeProperty.Create(ReadCallee, Self);
    for i := 0 to FParams.Count - 1 do
    begin
      ArgVar := CompileToVar(FParams[i]);
      ReadCall.addParam(TLapeTree_Var.Create(ArgVar, FParams[i]));
      Arg := FParams[i];
      FParams[i] := TLapeTree_Var.Create(ArgVar, Arg);
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

constructor TLapeTree_InternalMethod.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(TLapeTree_ExprBase(nil), ACompiler);
end;

procedure TLapeTree_InternalMethod.ClearCache;
begin
  {nothing}
end;

end.
