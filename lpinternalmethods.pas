{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 
}
unit lpinternalmethods;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lptree, lpinvoke, lpvartypes, lpeval_extra;

type
  TLapeTree_InternalMethod_Write = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_WriteLn = class(TLapeTree_InternalMethod_Write)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_ToStr = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Assert = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_IsScriptMethod = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_GetExceptionMessage = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Break = class(TLapeTree_InternalMethod)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Continue = class(TLapeTree_InternalMethod)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Exit = class(TLapeTree_InternalMethod)
  public
    function addParam(p: TLapeTree_ExprBase): Integer; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Halt = class(TLapeTree_InternalMethod)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_New = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Dispose = class(TLapeTree_InternalMethod)
  public
    FunctionOnly: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Default = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Swap = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_SizeOf = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Ord = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Low = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_High = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Length = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_SetLength = class(TLapeTree_InternalMethod)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Copy = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Delete = class(TLapeTree_InternalMethod)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Insert = class(TLapeTree_InternalMethod)
  public
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Succ = class(TLapeTree_InternalMethod)
  public
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Pred = class(TLapeTree_InternalMethod_Succ)
  public
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Inc = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Dec = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Label = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Goto = class(TLapeTree_InternalMethod)
  protected
    function PlacesLabel(Node: TLapeTree_Base): Boolean; virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Raise = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Objectify = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_IndexOf = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_IndicesOf = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Contains = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Remove = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_Slice = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_GetCallerAddress = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_GetExceptionLocation = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_GetExceptionLocationStr = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
  end;

  TLapeTree_InternalMethod_GetScriptMethodName = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_GetCallerName = class(TLapeTree_InternalMethod_GetScriptMethodName)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
  end;

  TLapeTree_InternalMethod_DumpCallStack = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_PType = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase); override;
    function FoldConstants(DoFree: Boolean = True): TLapeTree_Base; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile: TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayEquals = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile: TResVar; override;
  end;

implementation

uses
  lptokenizer, lpvartypes_array, lpmessages, lpeval, lpvartypes_ord, lpinterpreter_types,
  lpvartypes_object;

// (of a set named as a type: its element)
function TypeOfParam(Param: TLapeTree_ExprBase): TLapeType;
begin
  Result := Param.resType();
  if (Param is TLapeTree_VarType) and (Result is TLapeType_Set) then
    Result := TLapeType_Set(Result).Element;
end;

// (where an empty one of a type is: an element is cleaned up by assigning it, see _ArraySetLength)
function EmptyOf(Compiler: TLapeCompilerBase; AType: TLapeType): TResVar;
begin
  Result := _ResVar.New(Compiler.addManagedDecl(Compiler.getBaseType(ltPointer).NewGlobalVarP()) as TLapeGlobalVar);
  PPointer(Result.VarPos.GlobalVar.Ptr)^ := AType.EmptyValue.Ptr;
end;

type
  __TLapeTree_Operator = class(TLapeTree_Operator);
  __TLapeTree_Var = class(TLapeTree_Var);

function TLapeTree_InternalMethod_IndexOf.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltInt32);

  Result := inherited;
end;

function TLapeTree_InternalMethod_IndexOf.Compile: TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not (FParams[1].resType is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);
  ArrayElementType := TLapeType_DynArray(FParams[1].resType()).PType;

  // ensure we can generate such a method
  if (not HasMagicMethod(Compiler, '_ArrayIndexOf', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_Equal], ArrayElementType);

  setCallee(GlobalNode('_ArrayIndexOf', FCompiler, Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_IndicesOf.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getGlobalType('TIntegerArray');

  Result := inherited;
end;

function TLapeTree_InternalMethod_IndicesOf.Compile: TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not (FParams[1].resType is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);
  ArrayElementType := TLapeType_DynArray(FParams[1].resType()).PType;

  // ensure we can generate such a method
  if (not HasMagicMethod(Compiler, '_ArrayIndicesOf', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_Equal], ArrayElementType);

  setCallee(GlobalNode('_ArrayIndicesOf', FCompiler, Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_Contains.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltBoolean);

  Result := inherited;
end;

function TLapeTree_InternalMethod_Contains.Compile: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (resType() = nil) then
    LapeException(lpeImpossible, DocPos);

  with TLapeTree_Operator.Create(op_cmp_GreaterThan, Self) do
  try
    Left := TLapeTree_InternalMethod_IndexOf.Create(Self);
    while (Self.Params.Count > 0) do
      TLapeTree_InternalMethod_IndexOf(Left).addParam(Self.Params[0]);

    Right := TLapeTree_Var.Create(FCompiler.getIntConst(-1, ltNativeInt), Self);

    Result := Compile();
  finally
    Free();
  end;
end;

function TLapeTree_InternalMethod_Remove.resType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count > 0) and (FParams[0].resType <> nil) then
    FResType := TypeOfParam(FParams[0]);

  Result := inherited;
end;

function TLapeTree_InternalMethod_Remove.Compile: TResVar;
var
  ItemVar, ArrayVar, IndexVar, Found: TResVar;
  JumpOver: TCodeLabel;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);
  ItemVar := FParams[0].Compile();
  if (not ItemVar.HasType()) then
    LapeException(lpeExpressionExpected, DocPos);
  ArrayVar := FParams[1].Compile();
  if (not ArrayVar.HasType()) then
    LapeException(lpeExpressionExpected, DocPos);

  if (ArrayVar.VarType.BaseType <> ltDynArray) then
    LapeException(lpeExpectedDynamicArray, DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(resType()));
  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.VarToDefault(Result);

  with TLapeTree_InternalMethod_IndexOf.Create(Self) do
  try
    addParam(TLapeTree_Var.Create(ItemVar, Self));
    addParam(TLapeTree_Var.Create(ArrayVar, Self));

    IndexVar := Compile();
  finally
    Free();
  end;

  FCompiler.Emitter.DocPos := DocPos;
  Found := NullResVar;
  Found := IndexVar.VarType.Eval(op_cmp_GreaterThan, Found, IndexVar, _ResVar.New(FCompiler.getIntConst(-1, ltNativeInt)), [lefConstRangeCheck]);

  JumpOver := FCompiler.Emitter.NewLabel();
  FCompiler.Emitter.JumpTo(JumpOver, FCompiler.Emitter._JmpRIfNot(0, Found));

  with TLapeTree_Operator.Create(op_Assign, Self) do
  try
    Left := TLapeTree_Var.Create(Result, Self);
    Right := TLapeTree_Operator.Create(op_Index, Self);
    TLapeTree_Operator(Right).Left := TLapeTree_Var.Create(ArrayVar, Self);
    TLapeTree_Operator(Right).Right := TLapeTree_Var.Create(IndexVar, Self);

    Compile();
  finally
    Free();
  end;

  with TLapeTree_InternalMethod_Delete.Create(Self) do
  try
    addParam(TLapeTree_Var.Create(ArrayVar, Self));
    addParam(TLapeTree_Var.Create(IndexVar, Self));
    addParam(TLapeTree_Var.Create(FCompiler.getIntConst(1, ltNativeInt), Self));

    Compile();
  finally
    Free();
  end;

  FCompiler.Emitter.PlaceLabel(JumpOver);

  Result.isConstant := True;
end;

function TLapeTree_InternalMethod_Slice.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count > 0) then
  begin
    ParamType := TypeOfParam(FParams[0]);

    if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes-LapeStringTypes) then
    begin
      FResType := ParamType;
      if (ParamType is TLapeType_StaticArray) then
        with TLapeType_StaticArray(ParamType) do
          FResType := PType.ArrayOf;
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Slice.Compile: TResVar;
var
  ArrayVar: TResVar;
  ArrayType: TLapeType;
  Len: TLapeTree_InternalMethod_Length;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (resType() = nil) then
    LapeException(lpeExpectedArray, DocPos);
  ArrayVar := FParams[0].Compile();
  if (not ArrayVar.HasType()) then
    LapeException(lpeExpectedArray, DocPos);

  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;

  Result := _ResVar.New(FCompiler.getTempVar(resType()));
  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.VarToDefault(Result);

  Len := TLapeTree_InternalMethod_Length.Create(Self);
  Len.addParam(TLapeTree_Var.Create(ArrayVar, Self));

  ArrayVar.VarType := FCompiler.getBaseType(ltPointer);
  Result.VarType := FCompiler.getBaseType(ltPointer);

  with TLapeTree_Invoke.Create('_ArraySlice', Self) do
  try
    addParam(TLapeTree_Var.Create(ArrayVar, Self));
    addParam(TLapeTree_Var.Create(FCompiler.getIntConst(ArrayType.Size, ltNativeInt), Self));
    addParam(Len.FoldConstants() as TLapeTree_ExprBase);
    while (Self.Params.Count > 1) do
      addParam(Self.Params[1]);

    addParam(TLapeTree_Var.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
    addParam(TLapeTree_Var.Create(Result, Self));

    Compile();

    Result.VarType := Self.resType();
    Result.isConstant := True;
  finally
    Free();
  end;

end;

constructor TLapeTree_InternalMethod_Write.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FSpecialParam := spForce;

  setCallee(GlobalNode('_Write', ACompiler, Self));
end;

function TLapeTree_InternalMethod_Write.Compile: TResVar;
var
  i: Integer;
  TempParams: TLapeExpressionList;
begin
  Assert(FCallee <> nil);
  if (FParams.Count < 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  TempParams := FParams;
  FParams := TLapeExpressionList.Create(nil, dupAccept, False);
  FParams.Add(TempParams[0]);
  try
    for i := 0 to TempParams.Count - 1 do
    begin
      FParams[0] := TempParams[i];

      if (TempParams[i] <> nil) and (TempParams[i].resType() <> nil) and (not (TempParams[i].resType().BaseType in LapeStringTypes)) then
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

function TLapeTree_InternalMethod_WriteLn.Compile: TResVar;
begin
  if (FParams.Count > 0) then
    Result := inherited
  else
    Result := NullResvar;

  with TLapeTree_Invoke.Create('_WriteLn', Self) do
  try
    Compile();
  finally
    Free();
  end;
end;

constructor TLapeTree_InternalMethod_ToStr.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltString);
end;

function TLapeTree_InternalMethod_ToStr.Compile: TResVar;
var
  i: Integer;
  Res, Par: TLapeTree_ExprBase;
begin
  Result := NullResVar;
  Res := nil;
  if (FParams.Count < 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  for i := 0 to FParams.Count - 1 do
    if (FParams[i] <> nil) and (FParams[i].resType() <> nil) and (FParams[i].resType().BaseType <> ltString) then
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
        __TLapeTree_Operator(Par).FLeft := FParams[i];
      __TLapeTree_Operator(Par).FRight := FParams[i + 1];
      Res := Par;
    end;

    if (Res is TLapeTree_DestExprBase) then
      TLapeTree_DestExprBase(Res).Dest := FDest
    else
      Dest := NullResVar;

    Result := Res.Compile();
    if (Res is TLapeTree_DestExprBase) then
      FDest := TLapeTree_DestExprBase(Res).Dest;
  finally
    if (FParams.Count >= 2) and (Res <> nil) then
      Res.Free();
  end;
end;

constructor TLapeTree_InternalMethod_Assert.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create('_Assert', ACompiler);
  FSpecialParam := spForce;
end;

function TLapeTree_InternalMethod_Assert.Compile: TResVar;
begin
  if (not (lcoAssertions in FCompilerOptions)) then
    Exit(NullResVar);
  Result := inherited;
end;

constructor TLapeTree_InternalMethod_IsScriptMethod.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltBoolean);
end;

function TLapeTree_InternalMethod_IsScriptMethod.Compile: TResVar;
var
  tmpVar, DestVar, Param: TResVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  tmpVar := NullResVar;
  Param := FParams[0].Compile();
  if (not Param.HasType()) or (not (Param.VarType is TLapeType_Method)) then
    LapeExceptionFmt(lpeExpected, ['Method'], DocPos);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;

  Param.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType := Param.VarType;

  FCompiler.Emitter.DocPos := DocPos;
  Param.VarType.Eval(op_Assign, tmpVar, DestVar, Param, []);

  FCompiler.Emitter._IsScriptMethod();
  Result.VarPos.MemPos := mpStack;
  Result.VarType := resType();

  if (FDest.VarPos.MemPos in [mpNone, mpVar]) and ((not FDest.HasType()) or FDest.VarType.Equals(Result.VarType)) then
  begin
    if (not FDest.HasType()) then
    begin
      Dest := _ResVar.New(Compiler.getTempVar(Result.VarType));
      Dest.isConstant := True;
    end;

    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._PopStackToVar(Result.VarType.Size, FDest.VarPos.StackVar.Offset);
    Result := FDest;
  end
  else
    Dest := NullResVar;
end;

constructor TLapeTree_InternalMethod_GetExceptionMessage.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltString);
end;

function TLapeTree_InternalMethod_GetExceptionMessage.Compile: TResVar;

  function InExceptBlock: Boolean;
  var
    Node: TLapeTree_Base;
  begin
    Node := Self;

    while (Node.Parent <> nil) do
    begin
      if (Node.Parent is TLapeTree_Try) then
        with Node.Parent as TLapeTree_Try do
          if (Node = ExceptBody) then
            Exit(True);

      Node := Node.Parent;
    end;

    Result := False;
  end;

var
  TempVar, DestVar: TResVar;
begin
  Dest := NullResVar;
  Result := NullResVar;
  TempVar := NullResVar;

  if (not InExceptBlock()) then
    LapeException(lpeOutsideExceptBlock, DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(ltString));
  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.VarToDefault(Result);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType.Eval(op_Assign, TempVar, DestVar, Result.VarType.Eval(op_Addr, TempVar, Result, NullResVar, []), []);

  FCompiler.Emitter._GetExceptionMessage();

  Result.isConstant := True;
end;

function TLapeTree_InternalMethod_Break.Compile: TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanBreak;
  BreakDepth, BreakCount: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (not (FParams.Count in [0, 1])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) then
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

  Node := FParent;
  FoundNode := nil;

  BreakCount := 0;
  JumpSafe := False;

  while (Node <> nil) and (BreakCount < BreakDepth) do
  begin
    if (Node.QueryInterface(ILapeTree_CanBreak, FoundNode) = 0) and FoundNode.canBreak() then
      Inc(BreakCount)
    else
      FoundNode := nil;
    if (Node is TLapeTree_Try) then
      JumpSafe := True;
    Node := Node.Parent;
  end;

  FCompiler.Emitter.DocPos := DocPos;
  if (FParams.Count = 1) and (BreakCount < BreakDepth) then
    LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
  else if (FoundNode = nil) then
    LapeException(lpeCannotBreak, DocPos)
  else
    FoundNode.addBreakStatement(JumpSafe);
end;

function TLapeTree_InternalMethod_Continue.Compile: TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanContinue;
  ContinueDepth, ContinueCount: Integer;
  JumpSafe: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (not (FParams.Count in [0, 1])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count < 1) then
    ContinueDepth := 1
  else
    with FParams[0].Evaluate() do
    begin
      ContinueDepth := AsInteger;
      if (not isConstant) then
        LapeException(lpeConstantExpected, FParams[0].DocPos)
      else if (ContinueDepth < 1) then
        LapeException(lpeOutOfTypeRange, FParams[0].DocPos);
    end;

  Node := FParent;
  FoundNode := nil;

  ContinueCount := 0;
  JumpSafe := False;

  while (Node <> nil) and (ContinueCount < ContinueDepth) do
  begin
    if (Node.QueryInterface(ILapeTree_CanContinue, FoundNode) = 0) and FoundNode.canContinue() then
      Inc(ContinueCount)
    else
      FoundNode := nil;
    if (Node is TLapeTree_Try) then
      JumpSafe := True;
    Node := Node.Parent;
  end;

  FCompiler.Emitter.DocPos := DocPos;
  if (FParams.Count = 1) and (ContinueCount < ContinueDepth) then
    LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
  else if (FoundNode = nil) then
    LapeException(lpeCannotContinue, DocPos)
  else
      FoundNode.addContinueStatement(JumpSafe);
end;

function TLapeTree_InternalMethod_Exit.addParam(p: TLapeTree_ExprBase): Integer;
begin
  Result := inherited;

  // (Exit(x) sets Result: noted while the body is read, see TLapeCompiler.checkDeclHints)
  if (FCompiler.StackInfo.ResultVar <> nil) then
    FCompiler.StackInfo.ResultVar.IsUsed := True;
end;

function TLapeTree_InternalMethod_Exit.Compile: TResVar;
var
  Node: TLapeTree_Base;
  FoundNode: ILapeTree_CanExit;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 0) then
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if (FParams[0] = nil) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos);

  if (FParams.Count = 1) then
    with TLapeTree_Operator.Create(op_Assign, FParams[0]) do
    try
      if (FCompiler.StackInfo.ResultVar = nil) then
        LapeExceptionFmt(lpeWrongNumberParams, [0], DocPos);
      Left := TLapeTree_Var.Create(_ResVar.New(FCompiler.StackInfo.ResultVar), FParams[0]);
      Right := FParams[0].setExpectedType(Left.resType()) as TLapeTree_ExprBase;

      Compile();
    finally
      Free();
    end;

  Node := FParent;
  FoundNode := nil;

  FCompiler.Emitter.DocPos := DocPos;
  while (Node <> nil) and (FoundNode = nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanExit, FoundNode) = 0) then
      if FoundNode.canExit() then
        FoundNode.addExitStatement()
      else
        FoundNode := nil;
    Node := Node.Parent;
  end;

  if (FoundNode = nil) then
    FCompiler.Emitter._JmpSafe(EndJump);
end;

function TLapeTree_InternalMethod_Halt.Compile: TResVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 0) then
    LapeException(lpeTooMuchParameters, DocPos);

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._JmpSafe(EndJump);
end;

constructor TLapeTree_InternalMethod_New.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FSpecialParam := spTypeThenParams;
end;

function TLapeTree_InternalMethod_New.resType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count >= 1) and (FParams[0] is TLapeTree_VarType) then
    FResType := TypeOfParam(FParams[0]);

  Result := inherited resType;
end;

function TLapeTree_InternalMethod_New.Compile: TResVar;

  procedure DoObject(ObjType: TLapeType_Object);

    procedure Resize(Len: Integer);
    begin
      with TLapeTree_InternalMethod_SetLength.Create(Self) do
      try
        addParam(TLapeTree_Var.Create(Result, Self));
        addParam(TLapeTree_Var.Create(FCompiler.getIntConst(Len, ltNativeInt), Self));
        Compile();
      finally
        Free();
      end;
    end;

  begin
    Assert(resType() = ObjType);
    if (FParams.Count = 0) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    // A new record (all 0) for a temporary to refer to: what the temporary had before is let go of first
    Result := _ResVar.New(FCompiler.getTempVar(ObjType));
    Resize(0);
    Resize(ObjType.TotalFieldSize);

    // which its Construct fills in, given it as its Result
    with TLapeTree_InvokeConstruct.Create(ObjType, Self) do
    try
      if IsMethodNode(Callee) then
      begin
        Parent := Self;
        while (Self.FParams.Count > 1) do
          addParam(Self.FParams[1]);

        Result := CompileInto(Result);
      end;
    finally
      Free();
    end;

    Result.isConstant := True;
  end;

var
  Param: TResVar;
  VarType: TLapeType;
  IsPointer: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (resType() is TLapeType_Object) then
  begin
    DoObject(resType() as TLapeType_Object);
    Exit;
  end;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile();
  VarType := Param.VarType;
  IsPointer := (VarType <> nil) and (VarType.BaseType = ltPointer);

  if IsPointer then
    if (VarType is TLapeType_Pointer) then
      if TLapeType_Pointer(VarType).PConst then
        LapeException(lpeVariableExpected, [FParams[0], Self])
      else
        VarType := TLapeType_Pointer(VarType).PType
    else
      LapeException(lpeImpossible, DocPos);

  if (VarType = nil) or Param.isConstant then
    LapeException(lpeVariableExpected, [FParams[0], Self]);

  with TLapeTree_Operator.Create(op_Assign, Self) do
  try
    Left := TLapeTree_Var.Create(Param, Self.FParams[0]);

    if isPointer then
    begin
      Right := TLapeTree_Invoke.Create('AllocMem', Self);
      TLapeTree_Invoke(Right).addParam(TLapeTree_Var.Create(FCompiler.getIntConst(VarType.Size, ltNativeInt), Self.FParams[0]));
    end
    else
      Right := TLapeTree_Var.Create(VarType.EmptyValue, Self.FParams[0]);

    Compile();
  finally
    Free();
  end;

end;

constructor TLapeTree_InternalMethod_Dispose.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FSpecialParam := spForce;
  FunctionOnly := False;
end;

function TLapeTree_InternalMethod_Dispose.Compile: TResVar;
var
  tmpVar: TResVar;
  Param: TResVar;
  VarType: TLapeType;
  IsPointer: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile();
  VarType := Param.VarType;
  IsPointer := (not FunctionOnly) and (VarType <> nil) and (VarType.BaseType = ltPointer);

  if IsPointer then
    if (VarType is TLapeType_Pointer) then
      if TLapeType_Pointer(VarType).PConst then
        LapeException(lpeVariableExpected, [FParams[0], Self])
      else
        VarType := TLapeType_Pointer(VarType).PType
    else
      LapeException(lpeImpossible, DocPos);

  if ((VarType = nil) and (not IsPointer)) or Param.isConstant then
    LapeException(lpeVariableExpected, [FParams[0], Self]);

  // (cleaned up: it is given what an empty one has)
  FCompiler.Emitter.DocPos := DocPos;
  if (VarType <> nil) and VarType.NeedFinalization then
    if IsPointer then
    begin
      tmpVar := NullResVar;
      FCompiler.Emitter.DocPos := DocPos;
      FCompiler.FinalizeVar(Param.VarType.Eval(op_Deref, tmpVar, Param, NullResVar, [lefConstRangeCheck]));
    end
    else
      FCompiler.FinalizeVar(Param);

  FCompiler.Emitter.DocPos := DocPos;
  if (not FunctionOnly) then
    if isPointer then
      with TLapeTree_Invoke.Create('FreeMem', Self) do
      try
        addParam(TLapeTree_Var.Create(Param, Self.FParams[0]));
        Compile();
      finally
        Free();
      end
    else if (not VarType.NeedFinalization) then
      FCompiler.VarToDefault(Param);

end;

procedure TLapeTree_InternalMethod_Default.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_Default.isConstant: Boolean;
begin
  if (FConstant = bUnknown) then
    if (FParams.Count = 1) and FParams[0].isConstant() then
      FConstant := bTrue
    else
      FConstant := bFalse;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Default.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) then
    begin
      FResType := FParams[0].resType();
          end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Default.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) and isConstant() then
  begin
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (ParamType = nil) or (not (FParams[0] is TLapeTree_VarType)) then
      LapeException(lpeTypeExpected, DocPos);

    FRes := TLapeGlobalVar(FCompiler.addManagedDecl(ParamType.NewGlobalVarP()));
  end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Default.Compile: TResVar;
begin
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams[0] is TLapeTree_VarType) then
    LapeException(lpeCannotEvalRunTime, DocPos);

  Result := FParams[0].Compile();
  if (not Result.HasType()) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if Result.isConstant then
    LapeException(lpeVariableExpected, DocPos);

  if (Result.VarType is TLapeType_Object) then
  begin
    with TLapeTree_InternalMethod_SetLength.Create(Self) do
    try
      addParam(TLapeTree_Var.Create(Result, Self));
      addParam(TLapeTree_Var.Create(FCompiler.getIntConst(0, ltNativeInt), Self));
      Compile();
    finally
      Free();
    end;

    with TLapeTree_InternalMethod_SetLength.Create(Self) do
    try
      addParam(TLapeTree_Var.Create(Result, Self));
      addParam(TLapeTree_Var.Create(FCompiler.getIntConst(TLapeType_Object(Result.VarType).TotalFieldSize, ltNativeInt), Self));
      Compile();
    finally
      Free();
    end;

    Exit;
  end;

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.VarToDefault(Result);
end;

constructor TLapeTree_InternalMethod_Swap.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create('_Swap', ACompiler);
end;

function TLapeTree_InternalMethod_Swap.Compile: TResVar;
var
  Type1, Type2: TLapeType;
begin
  Result := NullResVar;
  if (FParams.Count <> 2) then
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

  addParam(TLapeTree_Var.Create(FCompiler.getIntConst(Type1.Size, ltNativeInt), Self));
  try
    inherited;
  finally
    FParams.Delete(2).Free();
  end;
end;

constructor TLapeTree_InternalMethod_SizeOf.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FConstant := bTrue;
  FResType := ACompiler.getBaseType(ltSizeInt);
end;

function TLapeTree_InternalMethod_SizeOf.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
  begin
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := FParams[0].resType();
    if (ParamType = nil) then
      LapeException(lpeExpressionExpected, DocPos);

    FRes := FCompiler.getIntConst(ParamType.Size, ltSizeInt);
  end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_SizeOf.Compile: TResVar;
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
    if (FParams.Count = 1) and FParams[0].isConstant() then
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
    if (FParams.Count = 1) then
    begin
      ParamType := TypeOfParam(FParams[0]);
      if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
        FResType := FCompiler.getBaseType(DetermineIntType(ParamType.Size, ParamType.BaseType in LapeSignedOrdinalTypes));
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.Evaluate: TLapeGlobalVar;
var
  Cast: TLapeTree_Cast;
begin
  if (FRes = nil) then
  begin
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    Cast := TLapeTree_Cast.Create(Self);
    try
      Cast.CastTo := resType();
      Cast.Param := FParams[0];

      FRes := Cast.Evaluate();
    finally
      Cast.Free();
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Ord.Compile: TResVar;
var
  Cast: TLapeTree_Cast;
  Param: TResVar;
begin
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  Param := FParams[0].Compile();

  Cast := TLapeTree_Cast.Create(Self);
  try
    Cast.Dest := FDest;
    Cast.CastTo := resType();
    Cast.Param := TLapeTree_Var.Create(Param, Self);

    Result := Cast.Compile();
    Self.FDest := Cast.Dest;
  finally
    Cast.Free();
  end;
end;

constructor TLapeTree_InternalMethod_Low.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FConstant := bTrue;
end;

function TLapeTree_InternalMethod_Low.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) then
    begin
      ParamType := TypeOfParam(FParams[0]);
      if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
        FResType := ParamType
      else if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes) then
        FResType := FCompiler.getBaseType(ltSizeInt);
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Low.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isConstant() then
    begin
      ParamType := TypeOfParam(FParams[0]);

      if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes)) then
        LapeExceptionFmt(lpeExpected, ['Ordinal or array'], DocPos);

      FRes := ParamType.VarLo();
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Low.Compile: TResVar;
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
    if (FParams.Count = 1) then
    begin
      ParamType := TypeOfParam(FParams[0]);
      if (ParamType <> nil) and (not (FParams[0] is TLapeTree_VarType)) and
        (ParamType.BaseType in LapeStringTypes+[ltDynArray]) then
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
    if (FParams.Count = 1) then
    begin
      ParamType := TypeOfParam(FParams[0]);
      if (ParamType <> nil) and (ParamType.BaseType in LapeOrdinalTypes) then
        FResType := ParamType
      else if (ParamType <> nil) and (ParamType.BaseType = ltShortString) then
        FResType := FCompiler.getBaseType(ltUInt8)
      else if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes) then
        FResType := FCompiler.getBaseType(ltSizeInt);
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_High.Evaluate: TLapeGlobalVar;
var
  ParamType: TLapeType;
begin
  if (FRes = nil) then
    if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isConstant() then
    begin
      ParamType := TypeOfParam(FParams[0]);

      if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes)) then
        LapeExceptionFmt(lpeExpected, ['Ordinal or array'], DocPos);

      FRes := ParamType.VarHi();
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_High.Compile: TResVar;
var
  Param: TResVar;
begin
  if isConstant() or (resType() = nil) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile();
  if (Param.VarType.BaseType = ltShortString) then
  begin
    Dest := NullResVar;
    Result := Param;
    Result.VarType := FCompiler.getBaseType(ltUInt8);
  end else
  begin
    Result := NullResVar;
    Result.VarType := FCompiler.getBaseType(ltSizeInt);
    if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      FDest := VarResVar;
    FCompiler.getDestVar(FDest, Result, op_Unknown);

    FCompiler.Emitter.DocPos := DocPos;
    if (Param.VarType.BaseType in LapeStringTypes) then
      FCompiler.Emitter._Eval(getEvalProc_StringHigh(), Result, Param, Param)
    else
      FCompiler.Emitter._Eval(getEvalProc_DynArrayHigh(), Result, Param, Param);
  end;
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
    if (FParams.Count = 1) then
    begin
      ParamType := TypeOfParam(FParams[0]);
      if (ParamType <> nil) and (ParamType.BaseType in [ltStaticArray, ltSmallEnum, ltLargeEnum]) then
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
    FResType := FCompiler.getBaseType(ltSizeInt);
    if (FParams.Count = 1) then
    begin
      ParamType := TypeOfParam(FParams[0]);
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
    else if (FParams.Count <> 1) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    ParamType := TypeOfParam(FParams[0]);
    if (ParamType = nil) or (not (ParamType.BaseType in [ltStaticArray, ltSmallEnum, ltLargeEnum])) then
      LapeExceptionFmt(lpeExpected, ['Array or enum'], DocPos);

    case ParamType.BaseType of
      ltStaticArray:
        with TLapeType_StaticArray(ParamType) do
          FRes := FCompiler.getIntConst(Range.Hi - Range.Lo + 1, ltSizeInt);

      ltSmallEnum, ltLargeEnum:
        FRes := FCompiler.getIntConst(TLapeType_Enum(ParamType).ElementCount, ltSizeInt);
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Length.Compile: TResVar;
var
  Param: TResVar;
begin
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile();
  if (not Param.HasType()) then
    LapeException(lpeExpressionExpected, DocPos);

  case Param.VarType.BaseType of
    ltSmallSet, ltLargeSet:
      begin
        Dest := NullResVar;
        with TLapeTree_Invoke.Create('BitCount', Self) do
        try
          addParam(TLapeTree_Var.Create(Param, Self));
          addParam(TLapeTree_Var.Create(FCompiler.getIntConst(Param.VarType.Size, ltNativeInt), Self));

          Result := Compile();
        finally
          Free();
        end;
      end;

    ltShortString:
      begin
        Dest := NullResVar;
        Result := Param;
        Result.VarType := FCompiler.getBaseType(ltUInt8);
      end;

     ltDynArray, ltAnsiString, ltWideString, ltUnicodeString:
      begin
        Result := NullResVar;
        Result.VarType := FCompiler.getBaseType(ltSizeInt);
        if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
          FDest := VarResVar;
        FCompiler.getDestVar(FDest, Result, op_Unknown);

        FCompiler.Emitter.DocPos := DocPos;
        if (Param.VarType.BaseType in LapeStringTypes) then
          FCompiler.Emitter._Eval(getEvalProc_StringLength(), Result, Param, Param)
        else
          FCompiler.Emitter._Eval(getEvalProc_DynArrayLength(), Result, Param, Param);
      end;

     else
       LapeExceptionFmt(lpeExpected, ['Array or enum'], DocPos);
  end;
end;

function TLapeTree_InternalMethod_SetLength.Compile: TResVar;
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

  if (FParams.Count < 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);

  Param := FParams[0].Compile();
  if (not Param.HasType()) or
     (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray]))
  then
    LapeException(lpeExpectedArray, DocPos);

  tmpType := Param.VarType;
  ArrayType := TLapeType_DynArray(Param.VarType).PType;

  Len := FParams[1].Compile();
  if (not Len.HasType()) or (ArrayType = nil) or
     (not Len.HasType()) or (not (Len.VarType.BaseType in LapeIntegerTypes))
	then
    LapeExceptionFmt(lpeExpected, ['Integer'], DocPos);

  if (Param.VarType is TLapeType_Object) then
  begin
    Param.VarType := FCompiler.getBaseType(ltPointer);
    with TLapeTree_Invoke.Create('_ObjectSetLength', Self) do
    try
      addParam(TLapeTree_Var.Create(Param, Self.FParams[0]));
      addParam(TLapeTree_Var.Create(Len, Self.FParams[1]));
      addParam(TLapeTree_Var.Create(GetMagicMethodOrNil(FCompiler, '_DisposeObject', [tmpType]), Self));

      Result := Compile();
    finally
      Free();
    end;
    Exit;
  end;

  case Param.VarType.BaseType of
    ltShortString:   _ArraySetLength := FCompiler['_SStr_SetLen'];
    ltAnsiString:    _ArraySetLength := FCompiler['_AStr_SetLen'];
    ltWideString:    _ArraySetLength := FCompiler['_WStr_SetLen'];
    ltUnicodeString: _ArraySetLength := FCompiler['_UStr_SetLen'];
    else
    begin
      _ArraySetLength := FCompiler['_ArraySetLength'];
      Param.VarType := FCompiler.getBaseType(ltPointer);
    end;
  end;

  try
    with TLapeTree_InvokePtr.Create(_ArraySetLength, Self) do
    try
      addParam(TLapeTree_Var.Create(Param, Self.FParams[0]));
      addParam(TLapeTree_Var.Create(Len, Self.FParams[1]));
      if (Param.VarType.BaseType = ltShortString) then
        addParam(TLapeTree_Var.Create(Param.VarType.VarHi(), Self.FParams[0]))
      else if (not (Param.VarType.BaseType in LapeStringTypes)) then
      begin
        addParam(TLapeTree_Var.Create(FCompiler.getIntConst(ArrayType.Size, ltNativeInt), Self.FParams[0]));
        addParam(TLapeTree_Var.Create(EmptyOf(FCompiler, ArrayType), Self));
        addParam(TLapeTree_Var.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
      end;
      Result := Compile();
    finally
      Free();
    end;

    if (FParams.Count > 2) then
    begin
      Param.VarType := tmpType;
      Counter := FCompiler.getTempVar(ltSizeInt);
      FCompiler.Emitter.DocPos := DocPos;
      tmpVar := Counter.VarType.Eval(op_Assign, tmpVar, _ResVar.New(Counter), _ResVar.New(FCompiler.getIntConst(0)), []);

      with TLapeTree_For.Create(Self) do
      try
        Counter := TLapeTree_Var.Create(tmpVar, Self);
        Limit := TLapeTree_Operator.Create(op_Minus, Self);
        with TLapeTree_Operator(Limit) do
        begin
          Left := TLapeTree_Var.Create(Len, FParams[1]);
          Right := TLapeTree_Var.Create(FCompiler.getIntConst(1, ltNativeInt), Self);
        end;
        Body := TSetLength(Self.ClassType).Create(Self);
        with TLapeTree_InternalMethod_SetLength(Body) do
          for i := 0 to Self.FParams.Count - 1 do
            if (i = 0) then
            begin
              addParam(TLapeTree_Operator.Create(op_Index, Self.FParams[0]));
              with TLapeTree_Operator(Params[0]) do
              begin
                Left := TLapeTree_Var.Create(Param, Self.FParams[0]);
                Right := TLapeTree_Var.Create(tmpVar, Self.FParams[0]);
              end;
            end
            else if (i <> 1) then
              Params.Add(Self.FParams[i]);
        Compile();
      finally
        Free();
      end;
    end;
  finally
  end;
end;

function TLapeTree_InternalMethod_Copy.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count > 0) then
  begin
    ParamType := TypeOfParam(FParams[0]);

    if (ParamType <> nil) then
    begin
      if (ParamType.BaseType in LapeArrayTypes) then
        FResType := ParamType;

      if (ParamType is TLapeType_StaticArray) then
        if (ParamType.BaseType in LapeStringTypes) then
          FResType := FCompiler.getBaseType(ltAnsiString)
        else with TLapeType_StaticArray(ParamType) do
          FResType := PType.ArrayOf;
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Copy.Compile: TResVar;
var
  ParRes, StartRes: TResVar;
  Param, Start, TmpExpr: TLapeTree_ExprBase;
  Lo: TLapeTree_InternalMethod_Low;
  ArrayType: TLapeType;
  _ArrayCopy: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count < 1) or (FParams.Count > 3) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ParRes := FParams[0].Compile();
  if (not ParRes.HasType()) or
     (not (ParRes.VarType.BaseType in LapeArrayTypes))
  then
    LapeException(lpeExpectedArray, DocPos);

  // (its address is taken, to read it through: also of a constant)
  ParRes.isConstant := False;
  ArrayType := TLapeType_DynArray(ParRes.VarType).PType;
  Param := TLapeTree_Var.Create(ParRes, FParams[0]);
  Start := FParams[1];

  if (Start = nil) then
    StartRes := NullResVar
  else
  begin
    StartRes := Start.Compile();
    if (not StartRes.HasType()) or (not (StartRes.VarType.BaseType in LapeOrdinalTypes)) then
      LapeExceptionFmt(lpeExpected, ['Ordinal'], DocPos);
    StartRes.VarType := FCompiler.getBaseType(DetermineIntType(StartRes.VarType.Size, StartRes.VarType.BaseType in LapeSignedOrdinalTypes));
    Start := TLapeTree_Var.Create(StartRes, Self.FParams[1]);
  end;

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
        if (Start <> nil) then
        begin
          TLapeType_StaticArray(ParRes.VarType).RangeCheck(ParRes, StartRes, [lefRangeCheck]);

          Lo := TLapeTree_InternalMethod_Low.Create(Self);
          Lo.AddParam(TLapeTree_Var.Create(ParRes, Self.FParams[0]));

          TmpExpr := TLapeTree_Operator.Create(op_Minus, Self);
          with TLapeTree_Operator(TmpExpr) do
          begin
            Left := Start;
            Right := Lo.FoldConstants() as TLapeTree_ExprBase;
          end;
          Start := TmpExpr;
        end;

        TmpExpr := TLapeTree_Operator.Create(op_Addr, Self);
        with TLapeTree_Operator(TmpExpr) do
          Left := Param;

        Param := TmpExpr;
      end else
        __TLapeTree_Var(Param).FResVar.VarType := FCompiler.getBaseType(ltPointer);
    end;
  end;

  Result := _ResVar.New(FCompiler.getTempVar(resType()));
  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.VarToDefault(Result);

  with TLapeTree_InvokePtr.Create(_ArrayCopy, Self) do
  try
    addParam(Param);
    addParam(Start);
    addParam(Self.FParams[2]);

    if (not (ParRes.VarType.BaseType in LapeStringTypes)) then
    begin
      TmpExpr := TLapeTree_InternalMethod_Length.Create(Self);
      TLapeTree_InternalMethod_Length(TmpExpr).addParam(TLapeTree_Var.Create(ParRes, Self.FParams[0]));
      addParam(TLapeTree_ExprBase(TmpExpr.FoldConstants()));

      addParam(TLapeTree_Var.Create(FCompiler.getIntConst(ArrayType.Size, ltNativeInt), Self.FParams[0]));
      addParam(TLapeTree_Var.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));

      Result.VarType := FCompiler.getBaseType(ltPointer);
    end;

    addParam(TLapeTree_Var.Create(Result, Self));
    Compile();

    Result.VarType := Self.resType();
    Result.isConstant := True;
  finally
    Self.addParam(Params[2]);
    Free();
  end;
end;

function TLapeTree_InternalMethod_Delete.Compile: TResVar;
var
  Param: TResVar;
  ArrayType: TLapeType;
  _ArrayDelete: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count < 2) or (FParams.Count > 3) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);

  Param := FParams[0].Compile();
  if (not Param.HasType()) or
     (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray]))
  then
    LapeException(lpeExpectedArray, DocPos);

  ArrayType := TLapeType_DynArray(Param.VarType).PType;
  case Param.VarType.BaseType of
    ltShortString:   _ArrayDelete := FCompiler['_SStr_Delete'];
    ltAnsiString:    _ArrayDelete := FCompiler['_AStr_Delete'];
    ltWideString:    _ArrayDelete := FCompiler['_WStr_Delete'];
    ltUnicodeString: _ArrayDelete := FCompiler['_UStr_Delete'];
    else
    begin
      _ArrayDelete := FCompiler['_ArrayDelete'];
      Param.VarType := FCompiler.getBaseType(ltPointer);
    end;
  end;

  try
    with TLapeTree_InvokePtr.Create(_ArrayDelete, Self) do
    try
      addParam(TLapeTree_Var.Create(Param, Self.FParams[0]));
      addParam(Self.FParams[1]);
      addParam(Self.FParams[1]);

      if (not (Param.VarType.BaseType in LapeStringTypes)) then
      begin
        addParam(TLapeTree_Var.Create(FCompiler.getIntConst(ArrayType.Size, ltNativeInt), Self.FParams[0]));
        addParam(TLapeTree_Var.Create(EmptyOf(FCompiler, ArrayType), Self));
        addParam(TLapeTree_Var.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
      end;

      Result := Compile();
    finally
      Self.addParam(Params[1]);
      Self.addParam(Params[1]);
      Free();
    end;
  finally
  end;
end;

function TLapeTree_InternalMethod_Insert.Compile: TResVar;
var
  SrcRes, DstRes, TempVar, tmpVar: TResVar;
  Src, TmpExpr: TLapeTree_ExprBase;
  ArrayType: TLapeType;
  _ArrayInsert: TLapeGlobalVar;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TmpExpr := nil;

  if (FParams.Count < 2) or (FParams.Count > 4) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  DstRes := FParams[1].Compile();
  if (not DstRes.HasType()) or
     (not (DstRes.VarType.BaseType in LapeArrayTypes - [ltStaticArray, ltShortString]))
  then
    LapeException(lpeExpectedArray, DocPos);

  ArrayType := TLapeType_DynArray(DstRes.VarType).PType;
  FParams[0] := TLapeTree_ExprBase(FParams[0].setExpectedType(DstRes.VarType).setExpectedType(ArrayType));
  SrcRes := FParams[0].Compile();
  if (not SrcRes.HasType()) or (ArrayType = nil) then
    LapeException(lpeExpressionExpected, DocPos);

  // (its address is taken, to read it through: also of a constant)
  SrcRes.isConstant := False;
  Src := TLapeTree_Var.Create(SrcRes, FParams[0]);

  case DstRes.VarType.BaseType of
    ltAnsiString:    _ArrayInsert := FCompiler['_AStr_Insert'];
    ltWideString:    _ArrayInsert := FCompiler['_WStr_Insert'];
    ltUnicodeString: _ArrayInsert := FCompiler['_UStr_Insert'];
    else
    begin
      _ArrayInsert := FCompiler['_ArrayInsert'];

      if DstRes.VarType.Equals(SrcRes.VarType) then
        __TLapeTree_Var(Src).FResVar.VarType := FCompiler.getBaseType(ltPointer)
      else if ArrayType.CompatibleWith(SrcRes.VarType) or (
        (SrcRes.VarType is TLapeType_StaticArray) and
        ArrayType.Equals(TLapeType_StaticArray(SrcRes.VarType).PType))
      then
      begin
        if (not (SrcRes.VarType is TLapeType_StaticArray)) and (not ArrayType.Equals(SrcRes.VarType)) then
        begin
          TempVar := _ResVar.New(FCompiler.getTempVar(ArrayType));
          FCompiler.Emitter.DocPos := DocPos;
          tmpVar := NullResVar;
          TempVar := TempVar.VarType.Eval(op_Assign, tmpVar, TempVar, SrcRes, [lefConstRangeCheck]);

          __TLapeTree_Var(Src).FResVar := TempVar;
        end;

        TmpExpr := TLapeTree_Operator.Create(op_Addr, Src);
        TLapeTree_Operator(TmpExpr).Left := Src;
        Src := TmpExpr;

        if ArrayType.CompatibleWith(SrcRes.VarType) then
          TmpExpr := TLapeTree_Var.Create(FCompiler.getIntConst(1, ltNativeInt), FParams[0])
        else
          TmpExpr := nil;
      end else
        LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], DocPos);

      DstRes.VarType := FCompiler.getBaseType(ltPointer);
    end;
  end;

  with TLapeTree_InvokePtr.Create(_ArrayInsert, Self) do
  try
    addParam(Src);
    addParam(TLapeTree_Var.Create(DstRes, Self.FParams[1]));
    addParam(Self.FParams[2]);
    addParam(Self.FParams[2]);

    if (not (DstRes.VarType.BaseType in LapeStringTypes)) then
    begin
      if (TmpExpr = nil) then
      begin
        TmpExpr := TLapeTree_InternalMethod_Length.Create(Self);
        TLapeTree_InternalMethod_Length(TmpExpr).addParam(TLapeTree_Var.Create(SrcRes, Self.FParams[0]));
      end;
      addParam(TLapeTree_ExprBase(TmpExpr.FoldConstants()));

      addParam(TLapeTree_Var.Create(FCompiler.getIntConst(ArrayType.Size, ltNativeInt), Self.FParams[0]));
      addParam(TLapeTree_Var.Create(EmptyOf(FCompiler, ArrayType), Self));
      addParam(TLapeTree_Var.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
    end;

    Result := Compile();
  finally
    Self.addParam(Params[2]);
    Self.addParam(Params[2]);
    Free();
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
    if  (FParams.Count in [1, 2]) and FParams[0].isConstant() and
       ((FParams.Count = 1) or       ((FParams[1] <> nil) and FParams[1].isConstant())) then
      FConstant := bTrue
    else
      FConstant := bFalse;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Succ.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count in [1, 2]) then
    begin
      FResType := TypeOfParam(FParams[0]);
          end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Succ.Evaluate: TLapeGlobalVar;
var
  VarParam, CountParam: TLapeGlobalVar;
  ResultType: TLapeType;
begin
  if (FRes = nil) then
  begin
    CountParam := nil;

    if (not (FParams.Count in [1, 2])) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
    if (FParams[0] = nil) then
      LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos);

    if (FParams.Count = 2) then
    begin
      if (FParams[1] = nil) then
        LapeExceptionFmt(lpeNoDefaultForParam, [2], DocPos);

      CountParam := Params[1].Evaluate();
    end else
      CountParam := FCompiler.getIntConst(1);

    VarParam := FParams[0].Evaluate();
    if (VarParam = nil) or (not VarParam.HasType()) or (not (VarParam.VarType.BaseType in LapeOrdinalTypes + [ltPointer])) then
      LapeExceptionFmt(lpeExpected, ['Ordinal or pointer'], DocPos);

    if (VarParam.VarType.BaseType = ltPointer) then
      with TLapeTree_Operator.Create(op_Index, Self) do
      try
        Left := TLapeTree_Var.Create(VarParam, FParams[0]);
        Right := TLapeTree_Var.Create(CountParam, Self);
        FRes := Evaluate();
      finally
        Free();
      end
    else
    begin
      ResultType := VarParam.VarType;
      VarParam := FCompiler.addManagedDecl(FCompiler.getBaseType(DetermineIntType(VarParam.VarType.Size, VarParam.VarType.BaseType in LapeSignedOrdinalTypes)).NewGlobalVarP(VarParam.Ptr)) as TLapeGlobalVar;

      with TLapeTree_Cast.Create(Self) do
      try
        CastTo := ResultType;
        Param := TLapeTree_Operator.Create(op_Plus, Self);
        with TLapeTree_Operator(Param) do
        begin
          Left := TLapeTree_Var.Create(VarParam, Self);
          Right := TLapeTree_Var.Create(CountParam, Self);
        end;
        Self.FRes := Evaluate();
      finally
        Free();
      end;
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Succ.Compile: TResVar;
var
  VarParam, CountParam: TResVar;
  ResultType: TLapeType;
begin
  CountParam := NullResVar;

  if (not (FParams.Count in [1, 2])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (FParams[0] = nil) then
    LapeExceptionFmt(lpeNoDefaultForParam, [1], DocPos);

  if (FParams.Count = 2) then
  begin
    if (FParams[1] = nil) then
      LapeExceptionFmt(lpeNoDefaultForParam, [2], DocPos);

    CountParam := FParams[1].Compile();
  end else
    CountParam := _ResVar.New(FCompiler.getIntConst(1));

  try
    VarParam := FParams[0].Compile();
    if (not VarParam.HasType()) or (not (VarParam.VarType.BaseType in LapeOrdinalTypes + [ltPointer])) then
      LapeExceptionFmt(lpeExpected, ['Ordinal or pointer'], DocPos);

    FCompiler.Emitter.DocPos := DocPos;
    if (VarParam.VarType.BaseType = ltPointer) then
      Result := VarParam.VarType.Eval(op_Index, FDest, VarParam, CountParam, [lefConstRangeCheck])
    else
    begin
      ResultType := VarParam.VarType;
      VarParam.VarType := FCompiler.getBaseType(DetermineIntType(VarParam.VarType.Size, VarParam.VarType.BaseType in LapeSignedOrdinalTypes));

      with TLapeTree_Cast.Create(Self) do
      try
        Dest := Self.FDest;
        CastTo := ResultType;
        Param := TLapeTree_Operator.Create(op_Plus, Self);
        with TLapeTree_Operator(Param) do
        begin
          Left := TLapeTree_Var.Create(VarParam, self);
          Right := TLapeTree_Var.Create(CountParam, Self);
        end;
        Result := Compile();
        Self.FDest := Dest;
      finally
        Free();
      end;
    end;
  finally
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
      addParam(TLapeTree_Var.Create(FCompiler.getIntConst(-1), Self))
    else if (FParams[1] <> nil) and (FParams.Count = 2) then
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

function TLapeTree_InternalMethod_Pred.Compile: TResVar;
var
  Negation: TLapeTree_Operator;
begin
  Negation := nil;

  if (FParams.Count < 2) then
    addParam(TLapeTree_Var.Create(FCompiler.getIntConst(-1), Self))
  else if (FParams[1] <> nil) and (FParams.Count = 2) then
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
    if (FParams.Count in [1, 2]) then
    begin
      FResType := TypeOfParam(FParams[0]);
          end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Inc.Compile: TResVar;
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
    Result := OldVarParam.Compile();
    Succ := TLapeTree_Operator.Create(op_Assign, Self);
    Succ.Left := TLapeTree_Var.Create(Result, OldVarParam);
    Succ.Right := TLapeTree_InternalMethod_Succ.Create(Self);
    TLapeTree_InternalMethod_Succ(Succ.Right).addParam(TLapeTree_Var.Create(Result, OldVarParam));
    if (OldCountParam <> nil) then
      TLapeTree_InternalMethod_Succ(Succ.Right).addParam(OldCountParam);
    Result := Succ.Compile();
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
    if (FParams.Count in [1, 2]) then
    begin
      FResType := TypeOfParam(FParams[0]);
          end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Dec.Compile: TResVar;
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
    Result := OldVarParam.Compile();
    Pred := TLapeTree_Operator.Create(op_Assign, Self);
    Pred.Left := TLapeTree_Var.Create(Result, OldVarParam);
    Pred.Right := TLapeTree_InternalMethod_Pred.Create(Self);
    TLapeTree_InternalMethod_Pred(Pred.Right).addParam(TLapeTree_Var.Create(Result, OldVarParam));
    if (OldCountParam <> nil) then
      TLapeTree_InternalMethod_Pred(Pred.Right).addParam(OldCountParam);
    Result := Pred.Compile();
  finally
    addParam(OldVarParam);
    if (OldCountParam <> nil) then
      addParam(OldCountParam);
    if (Pred <> nil) then
      Pred.Free();
  end;
end;

constructor TLapeTree_InternalMethod_Label.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FSpecialParam := spForce;
end;

function TLapeTree_InternalMethod_Label.Compile: TResVar;
var
  ResVar, tmpVar: TResVar;
  Lbl: TLapeGlobalVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ResVar := FParams[0].Compile();

  if (not ResVar.HasType()) or (ResVar.VarType.BaseType <> ltPointer) or (not (ResVar.VarType is TLapeType_Pointer)) then
    LapeException(lpeInvalidLabel, [FParams[0]]);

  if (ResVar.VarType is TLapeType_Label) then
  begin
    if (ResVar.VarPos.MemPos <> mpMem) or ResVar.VarPos.GlobalVar.Known or (PPointer(ResVar.VarPos.GlobalVar.Ptr)^ <> nil) then
      LapeException(lpeInvalidLabel, [FParams[0]]);

    PCodeOffset(ResVar.VarPos.GlobalVar.Ptr)^ := FCompiler.Emitter.CodeLen;
    ResVar.VarPos.GlobalVar.isConstant := True;
  end
  else
  begin
    Lbl := FCompiler.getLabel(FCompiler.Emitter.CodeLen);
    FCompiler.Emitter.DocPos := DocPos;
    tmpVar := NullResVar;
    ResVar.VarType.Eval(op_Assign, tmpVar, ResVar, _ResVar.New(Lbl), [lefConstRangeCheck]);
    PCodePos(Lbl.Ptr)^ := FCompiler.Emitter.CodeLen;
  end;
end;

constructor TLapeTree_InternalMethod_Goto.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FSpecialParam := spForce;
end;

// Whether Node places the label this goes to, or holds where it does in statements that can be jumped into
function TLapeTree_InternalMethod_Goto.PlacesLabel(Node: TLapeTree_Base): Boolean;
begin
  if (Node is TLapeTree_InternalMethod_Label) then
    with TLapeTree_InternalMethod_Label(Node) do
      Result := (Params.Count = 1) and ((Params[0] is TLapeTree_Var) and (TLapeTree_Var(Params[0]).GlobalVar <> nil)) and
                (TLapeTree_Var(Params[0]).GlobalVar = TLapeTree_Var(Self.FParams[0]).GlobalVar)
  else
    Result := ((Node is TLapeTree_StatementList) or (Node is TLapeTree_Case) or
               ((Node is TLapeTree_If) and (not (Node is TLapeTree_For)))) and Node.Walk(@Self.PlacesLabel);
end;

function TLapeTree_InternalMethod_Goto.Compile: TResVar;
var
  tmpVar, ResVar, Param: TResVar;
  Node: TLapeTree_Base;
begin
  Result := NullResVar;
  tmpVar := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile();
  if (not Param.HasType()) or (not (Param.VarType.BaseType in [ltPointer] + LapeProcTypes)) then
    LapeException(lpeInvalidLabel, [FParams[0]]);

  if ((FParams[0] is TLapeTree_Var) and (TLapeTree_Var(FParams[0]).GlobalVar <> nil)) and (TLapeTree_Var(FParams[0]).GlobalVar.VarType is TLapeType_Label) then
  begin
    Node := Self;
    repeat
      Node := Node.Parent;
      if (Node = nil) or (Node is TLapeTree_Method) then
        LapeExceptionFmt(lpeCannotJumpToLabel, [TLapeTree_Var(FParams[0]).GlobalVar.Name], DocPos);
    until PlacesLabel(Node);
  end;

  ResVar := FCompiler.getTempStackVar(FCompiler.getGlobalType('ConstPointer'));
  FCompiler.Emitter.DocPos := DocPos;
  ResVar := ResVar.VarType.Eval(op_Assign, tmpVar, ResVar, Param, []);
  FCompiler.Emitter._JmpVar();

end;

constructor TLapeTree_InternalMethod_Raise.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FSpecialParam := spForce;
end;

function TLapeTree_InternalMethod_Raise.Compile: TResVar;

  function InExceptBlock: Boolean;
  var
    Node: TLapeTree_Base;
  begin
    Node := Self;

    while (Node.Parent <> nil) do
    begin
      if (Node.Parent is TLapeTree_Try) then
        with Node.Parent as TLapeTree_Try do
          if (Node = ExceptBody) then
            Exit(True);

      Node := Node.Parent;
    end;

    Result := False;
  end;

var
  Invoke: TLapeTree_Invoke;
begin
  Dest := NullResVar;
  Result := NullResVar;

  if (FParams.Count = 0) then
  begin
    if (not InExceptBlock()) then
      LapeException(lpeOutsideExceptBlock, DocPos);

    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._ReRaiseException();
  end else
  begin
    if (FParams.Count > 1) then
      LapeException(lpeTooMuchParameters, DocPos);

    Invoke := TLapeTree_Invoke.Create('RaiseException', Self);
    Invoke.Parent := Self;
    try
      Invoke.addParam(FParams[0]);

      Result := Invoke.Compile();
    finally
      Invoke.Free();
    end;
  end;
end;

constructor TLapeTree_InternalMethod_Objectify.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);

  FConstant := bFalse;
end;

function TLapeTree_InternalMethod_Objectify.resType: TLapeType;
var
  VarType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count = 1) then
  begin
    VarType := FParams[0].resType();
    if (VarType <> nil) and (VarType.ClassType = TLapeType_Method) then
      FResType := FCompiler.addManagedType(TLapeType_MethodOfObject.Create(VarType as TLapeType_Method));
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Objectify.Compile: TResVar;
var
  Method: TLapeGlobalVar;
  Param, AsRecord, tmpVar: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (resType() = nil) then
    LapeException(lpeExpectedNormalMethod, DocPos);

  Method := FCompiler.GenerateMethod('_Objectify', nil, getTypeArray([resType()]));

  // (Self is the method itself, not where it is kept: what is made can be kept after that is gone)
  Param := FParams[0].Compile();
  Param.VarType := FCompiler.getBaseType(ltPointer);

  Result := _ResVar.New(FCompiler.getTempVar(Method.VarType));
  FCompiler.Emitter.DocPos := DocPos;
  tmpVar := NullResVar;
  Result.VarType.Eval(op_Assign, tmpVar, Result, _ResVar.New(Method), [lefConstRangeCheck]);

  AsRecord := Result;
  AsRecord.VarType := FCompiler.getGlobalType('TMethod');
  tmpVar := NullResVar;
  AsRecord := AsRecord.VarType.EvalMember(tmpVar, AsRecord, 'Self', [lefConstRangeCheck, lefAssigning]);
  tmpVar := NullResVar;
  AsRecord.VarType.Eval(op_Assign, tmpVar, AsRecord, Param, [lefConstRangeCheck]);

  Result.isConstant := True;
end;

constructor TLapeTree_InternalMethod_GetCallerAddress.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltPointer);
end;

function TLapeTree_InternalMethod_GetCallerAddress.Compile: TResVar;
begin
  Result := NullResVar;

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._GetCallerAddress();
  Result.VarPos.MemPos := mpStack;
  Result.VarType := resType();

  if (FDest.VarPos.MemPos in [mpNone, mpVar]) and ((not FDest.HasType()) or FDest.VarType.Equals(Result.VarType)) then
  begin
    if (not FDest.HasType()) then
    begin
      Dest := _ResVar.New(Compiler.getTempVar(Result.VarType));
      Dest.isConstant := True;
    end;

    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._PopStackToVar(Result.VarType.Size, FDest.VarPos.StackVar.Offset);
    Result := FDest;
  end
  else
    Dest := NullResVar;
end;

constructor TLapeTree_InternalMethod_GetCallerName.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;

  addParam(TLapeTree_InternalMethod_GetCallerAddress.Create(Self));
end;

constructor TLapeTree_InternalMethod_GetExceptionLocation.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltPointer);
end;

function TLapeTree_InternalMethod_GetExceptionLocation.Compile: TResVar;
begin
  Result := NullResVar;

  FCompiler.Emitter.DocPos := DocPos;
  FCompiler.Emitter._GetExceptionLocation();
  Result.VarPos.MemPos := mpStack;
  Result.VarType := resType();

  if (FDest.VarPos.MemPos in [mpNone, mpVar]) and ((not FDest.HasType()) or FDest.VarType.Equals(Result.VarType)) then
  begin
    if (not FDest.HasType()) then
    begin
      Dest := _ResVar.New(Compiler.getTempVar(Result.VarType));
      Dest.isConstant := True;
    end;

    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._PopStackToVar(Result.VarType.Size, FDest.VarPos.StackVar.Offset);
    Result := FDest;
  end
  else
    Dest := NullResVar;
end;

constructor TLapeTree_InternalMethod_GetExceptionLocationStr.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create('_LocationToStr', ACompiler);

  FResType := ACompiler.getBaseType(ltString);

  addParam(TLapeTree_InternalMethod_GetExceptionLocation.Create(Self));
end;

constructor TLapeTree_InternalMethod_GetScriptMethodName.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltShortString);
end;

function TLapeTree_InternalMethod_GetScriptMethodName.Compile: TResVar;
var
  tmpVar, DestVar, Param: TResVar;
begin
  Result := NullResVar;
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  tmpVar := NullResVar;
  Param := FParams[0].Compile();
  if (not Param.HasType()) or (not (Param.VarType.BaseType in [ltScriptMethod, ltPointer])) then
    LapeExceptionFmt(lpeExpected, ['Method'], DocPos);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;

  Param.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType := Param.VarType;

  FCompiler.Emitter.DocPos := DocPos;
  Param.VarType.Eval(op_Assign, tmpVar, DestVar, Param, []);

  FCompiler.Emitter._GetScriptMethodName();
  Result.VarPos.MemPos := mpStack;
  Result.VarType := resType();

  if (FDest.VarPos.MemPos in [mpNone, mpVar]) and ((not FDest.HasType()) or FDest.VarType.Equals(Result.VarType)) then
  begin
    if (not FDest.HasType()) then
    begin
      Dest := _ResVar.New(Compiler.getTempVar(Result.VarType));
      Dest.isConstant := True;
    end;

    FCompiler.Emitter.DocPos := DocPos;
    FCompiler.Emitter._PopStackToVar(Result.VarType.Size, FDest.VarPos.StackVar.Offset);
    Result := FDest;
  end
  else
    Dest := NullResVar;
end;

constructor TLapeTree_InternalMethod_DumpCallStack.Create(ACompiler: TLapeCompilerBase);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltString);
end;

function TLapeTree_InternalMethod_DumpCallStack.Compile: TResVar;
var
  TempVar, DestVar, ParamVar, OffsetVar: TResVar;
begin
  Dest := NullResVar;
  Result := NullResVar;
  TempVar := NullResVar;

  if (Params.Count > 1) then
    LapeException(lpeTooMuchParameters, DocPos);

  if (FParams[0] = nil) then
    OffsetVar := _ResVar.New(FCompiler.getIntConst(0, ltInt32))
  else
  begin
    ParamVar := FParams[0].Compile();
    if (not ParamVar.HasType()) or (not (ParamVar.VarType.BaseType in LapeIntegerTypes)) then
      LapeExceptionFmt(lpeExpected, ['Integer'], DocPos);

    OffsetVar := _ResVar.New(FCompiler.getTempVar(ltInt32));
    FCompiler.Emitter.DocPos := DocPos;
    OffsetVar := OffsetVar.VarType.Eval(op_Assign, TempVar, OffsetVar, ParamVar, []);

  end;

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := OffsetVar.VarType;

  FCompiler.Emitter.DocPos := DocPos;
  OffsetVar.VarType.Eval(op_Assign, TempVar, DestVar, OffsetVar, []);

  Result := _ResVar.New(FCompiler.getTempVar(ltString));
  FCompiler.VarToDefault(Result);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType.Eval(op_Assign, TempVar, DestVar, Result.VarType.Eval(op_Addr, TempVar, Result, NullResVar, []), []);

  FCompiler.Emitter._DumpCallStack();

  Result.isConstant := True;
end;

constructor TLapeTree_InternalMethod_PType.Create(ACompiler: TLapeCompilerBase);
begin
  inherited Create(ACompiler);
  FConstant := bTrue;
end;

function TLapeTree_InternalMethod_PType.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) and (FParams.Count = 1) then
  begin
    Typ := FParams[0].resType();
    if (Typ is TLapeType_Pointer) then
      FResType := TLapeType_Pointer(Typ).PType
    else if (Typ is TLapeType_Set) then
      FResType := TLapeType_Set(Typ).Element;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_PType.Compile: TResVar;
begin
  Result := NullResVar;
  LapeException(lpeImpossible);
end;

function TLapeTree_InternalMethod_PType.Evaluate: TLapeGlobalVar;
begin
  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (resType() = nil) then
    LapeException(lpeExpectedPointerType, DocPos);

  Result := nil;
end;

// (it is a type: what is left is the name of that)
function TLapeTree_InternalMethod_PType.FoldConstants(DoFree: Boolean = True): TLapeTree_Base;
var
  Replacement: TLapeTree_ExprBase;
begin
  if (FParams.Count = 1) and (resType() <> nil) then
  begin
    Replacement := TLapeTree_VarType.Create(resType(), Self);
    Replacement.Parent := FParent;

    FParent := nil;
    if DoFree then
      Free();
    Result := Replacement;
  end
  else
    Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayEquals.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltBoolean);

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayEquals.Compile: TResVar;
var
  Left, Right: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);
  if (not (FParams[0].resType() is TLapeType_DynArray)) or (not (FParams[1].resType() is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  Left := TLapeType_DynArray(FParams[0].resType()).PType;
  Right := TLapeType_DynArray(FParams[1].resType()).PType;
  if (not Left.CompatibleWith(Right)) then
    LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op_cmp_Equal), Left.AsString, Right.AsString], DocPos);

  setCallee(GlobalNode('_ArrayEquals', FCompiler, Self));
  Result := inherited;
end;

end.

