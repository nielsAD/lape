{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
unit lpinternalmethods;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lptree, lpvartypes;

type
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
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_IsScriptMethod = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_GetExceptionMessage = class(TLapeTree_InternalMethod)
  public
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

  TLapeTree_InternalMethod_Raise = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Objectify = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_IsEnumGap = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_IndexOf = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;

    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_IndicesOf = class(TLapeTree_InternalMethod_IndexOf)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;

    function resType: TLapeType; override;
  end;

  TLapeTree_InternalMethod_FallThrough = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Contains = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Remove = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_RemoveAll = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Slice = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_GetCallerLocation = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_GetCallerLocationStr = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
  end;

  TLapeTree_InternalMethod_GetExceptionLocation = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_GetExceptionLocationStr = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
  end;

  TLapeTree_InternalMethod_GetScriptMethodName = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_DumpCallStack = class(TLapeTree_InternalMethod)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

implementation

uses
  lpparser, lpvartypes_array, lpmessages, lpeval, lpvartypes_ord, lpinterpreter_types;

type
  __TLapeTree_Operator = class(TLapeTree_Operator);
  __TLapeTree_ResVar = class(TLapeTree_ResVar);

function TLapeTree_InternalMethod_FallThrough.Compile(var Offset: Integer): TResVar;

  function getBranch(out InTry: Boolean): TLapeTree_CaseBranch;
  var
    Node: TLapeTree_Base;
  begin
    InTry := False;

    Node := Parent;
    while (Node <> nil) do
    begin
      if (Node is TLapeTree_Try) then
        InTry := True;

      if (Node is TLapeTree_CaseBranch) then
      begin
        Result := TLapeTree_CaseBranch(Node);
        Exit;
      end;

      Node := Node.Parent;
    end;

    Result := nil;
  end;

var
  Branch: TLapeTree_CaseBranch;
  InTry: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;

  Branch := getBranch(InTry);
  if (Branch = nil) then
    LapeException(lpeCannotFallthrough, DocPos);

  FCompiler.Emitter.FullEmit := False;
  try
    if InTry then
      Branch.addFallThroughStatement(True, FCompiler.Emitter._JmpSafeR(0, Offset, @_DocPos), @_DocPos)
    else
      Branch.addFallThroughStatement(False, FCompiler.Emitter._JmpR(0, Offset, @_DocPos), @_DocPos);
  finally
    FCompiler.Emitter.FullEmit := True;
  end;
end;

function TLapeTree_InternalMethod_IsEnumGap.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltEvalBool);

  Result := inherited resType();
end;

function TLapeTree_InternalMethod_IsEnumGap.Compile(var Offset: Integer): TResVar;
var
  Method: TLapeGlobalVar;
  Typ: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Typ := FParams[0].resType();
  if (not (Typ is TLapeType_Enum)) then
    LapeException(lpeExpectedEnum, DocPos);

  Method := TLapeType_OverloadedMethod(FCompiler['_IsEnumGap'].VarType).getMethod(getTypeArray([Typ]), FCompiler.getBaseType(ltEvalBool));
  with TLapeTree_Invoke.Create(Method, Self) do
  try
    addParam(Self.FParams[0]);

    Result := Compile(Offset);
  finally
    Free();
  end;
end;

function TLapeTree_InternalMethod_IndexOf.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltInt32);

  Result := inherited;
end;

function TLapeTree_InternalMethod_IndexOf.Compile(var Offset: Integer): TResVar;
var
  ArrayPointerVar, ItemVar, ArrayVar, EqualsVar, TempVar: TResVar;
  ArrayType: TLapeType;
  ArrayLen: TLapeTree_InternalMethod_Length;
  ItemPointer: TLapeTree_Operator;
  ItemWasConstant, ArrayWasConstant: Boolean;
  i: Integer;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TempVar := NullResVar;

  if (Params.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);
  if (not FParams[0].CompileToTempVar(Offset, ItemVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);
  if (not FParams[1].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (not (ArrayVar.VarType is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  ItemWasConstant := ItemVar.isConstant;
  if ItemWasConstant then
    ItemVar.isConstant := False;

  ArrayWasConstant := ArrayVar.isConstant;
  if ArrayWasConstant then
    ArrayVar.isConstant := False;

  try
    // Check if user defined `_IndexOf` exists. Useful for providing a native method
    if InvokeMagicMethod(Self, '_IndexOf', Result, Offset) then
      Exit;

    ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;
    if (not ItemVar.VarType.CompatibleWith(ArrayType)) then
      LapeExceptionFmt(lpeIncompatibleOperator2, [LapeOperatorToString(op_cmp_Equal), ItemVar.VarType.AsString, ArrayType.AsString], DocPos);

    EqualsVar := GetMagicMethodOrNil(FCompiler, '_Equals', [ItemVar.VarType, ArrayType], FCompiler.getBaseType(ltEvalBool));

    if (ArrayVar.VarType.BaseType = ltDynArray) then
    begin
      ArrayPointerVar := ArrayVar;
      ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);
    end else
      ArrayPointerVar := ArrayVar.VarType.Eval(op_Addr, TempVar, ArrayVar, NullResVar, [], Offset, @_DocPos); // ltStaticArray

    ArrayLen := TLapeTree_InternalMethod_Length.Create(Self);
    ArrayLen.addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    ItemPointer := TLapeTree_Operator.Create(op_Addr, Self);
    ItemPointer.Left := TLapeTree_ResVar.Create(ItemVar.IncLock(), Self);

    for i := FParams.Count - 1 downto 0 do
      if (FParams[i] <> nil) and (FParams[i].Parent = Self) then
        FParams[i].Free();

    addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
    addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
    addParam(TLapeTree_GlobalVar.Create(ArrayVar.VarType.VarLo(), Self));
    addParam(ArrayLen.FoldConstants() as TLapeTree_ExprBase);
    addParam(ItemPointer.FoldConstants() as TLapeTree_ExprBase);
    addParam(TLapeTree_ResVar.Create(EqualsVar.IncLock(), Self));

    Result := inherited;
  finally
    ItemVar.isConstant := ItemWasConstant;
    ArrayVar.isConstant := ArrayWasConstant;
  end;
end;

constructor TLapeTree_InternalMethod_IndexOf.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
var
  _IndexOf: TLapeGlobalVar;
begin
  inherited;

  _IndexOf := ACompiler['_IndexOf'];
  if (_IndexOf <> nil) and (_IndexOf.VarType is TLapeType_OverloadedMethod) then
    _IndexOf := _IndexOf.VarType.ManagedDeclarations[0] as TLapeGlobalVar;
  Assert(_IndexOf <> nil);

  setExpr(TLapeTree_GlobalVar.Create(_IndexOf, Self));
end;

constructor TLapeTree_InternalMethod_IndicesOf.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
var
  _IndicesOf: TLapeGlobalVar;
begin
  inherited;

  _IndicesOf := ACompiler['_IndicesOf'];
  if (_IndicesOf <> nil) and (_IndicesOf.VarType is TLapeType_OverloadedMethod) then
    _IndicesOf := _IndicesOf.VarType.ManagedDeclarations[0] as TLapeGlobalVar;
  Assert(_IndicesOf <> nil);

  setExpr(TLapeTree_GlobalVar.Create(_IndicesOf, Self));
end;

function TLapeTree_InternalMethod_IndicesOf.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getIntegerArray();

  Result := inherited;
end;

function TLapeTree_InternalMethod_Contains.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltEvalBool);

  Result := inherited;
end;

function TLapeTree_InternalMethod_Contains.Compile(var Offset: Integer): TResVar;
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

    Right := TLapeTree_Integer.Create(-1, Self);

    Result := Compile(Offset);
  finally
    Free();
  end;
end;

function TLapeTree_InternalMethod_Remove.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltEvalBool);

  Result := inherited;
end;

function TLapeTree_InternalMethod_Remove.Compile(var Offset: Integer): TResVar;
var
  ItemVar, ArrayVar, ArrayPointerVar: TResVar;
  IndexOf: TLapeTree_InternalMethod_IndexOf;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);
  if (not FParams[0].CompileToTempVar(Offset, ItemVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);
  if (not FParams[1].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (ArrayVar.VarType.BaseType <> ltDynArray) then
    LapeException(lpeExpectedDynamicArray, DocPos);

  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;
  ArrayPointerVar := ArrayVar;
  ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);

  IndexOf := TLapeTree_InternalMethod_IndexOf.Create(Self);
  IndexOf.addParam(TLapeTree_ResVar.Create(ItemVar.IncLock(), Self));
  IndexOf.addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

  with TLapeTree_Invoke.Create('_ArrayDeleteIndex', Self) do
  try
    addParam(IndexOf);
    addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
    addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Dispose', [ArrayType]), Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));

    Result := Compile(Offset);
  finally
    Free();
  end;
end;

function TLapeTree_InternalMethod_RemoveAll.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltSizeInt);

  Result := inherited;
end;

function TLapeTree_InternalMethod_RemoveAll.Compile(var Offset: Integer): TResVar;
var
  ItemVar, ArrayVar, ArrayPointerVar: TResVar;
  IndicesOf: TLapeTree_InternalMethod_IndicesOf;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);
  if (not FParams[0].CompileToTempVar(Offset, ItemVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);
  if (not FParams[1].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (ArrayVar.VarType.BaseType <> ltDynArray) then
    LapeException(lpeExpectedDynamicArray, DocPos);

  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;
  ArrayPointerVar := ArrayVar;
  ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);

  IndicesOf := TLapeTree_InternalMethod_IndicesOf.Create(Self);
  IndicesOf.addParam(TLapeTree_ResVar.Create(ItemVar.IncLock(), Self));
  IndicesOf.addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

  with TLapeTree_Invoke.Create('_ArrayDeleteIndices', Self) do
  try
    addParam(IndicesOf);
    addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
    addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Dispose', [ArrayType]), Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));

    Result := Compile(Offset);
  finally
    Free();
  end;
end;

function TLapeTree_InternalMethod_Slice.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count > 0) and (not isEmpty(FParams[0])) then
  begin
    ParamType := FParams[0].resType();

    if (ParamType <> nil) and (ParamType.BaseType in LapeArrayTypes-LapeStringTypes) then
    begin
      FResType := ParamType;
      if (ParamType is TLapeType_StaticArray) then
        with TLapeType_StaticArray(ParamType) do
          FResType := FCompiler.addManagedType(TLapeType_DynArray.Create(PType, FCompiler));
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Slice.Compile(var Offset: Integer): TResVar;
var
  ArrayVar: TResVar;
  ArrayType: TLapeType;
  Len: TLapeTree_InternalMethod_Length;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (resType() = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeExpectedArray, DocPos);

  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;

  Result := _ResVar.New(FCompiler.getTempVar(resType()));
  FCompiler.VarToDefault(Result, Offset, @Self._DocPos);

  Len := TLapeTree_InternalMethod_Length.Create(Self);
  Len.addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

  ArrayVar.VarType := FCompiler.getBaseType(ltPointer);
  Result.VarType := FCompiler.getBaseType(ltPointer);

  with TLapeTree_Invoke.Create('_ArraySlice', Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));
    addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
    addParam(Len.FoldConstants() as TLapeTree_ExprBase);
    while (Self.Params.Count > 1) do
      addParam(Self.Params[1]);

    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType, ArrayType]), Self));
    addParam(TLapeTree_ResVar.Create(Result.IncLock(), Self));

    Compile(Offset).Spill(1);

    Result.VarType := Self.resType();
    Result.isConstant := True;
  finally
    Free();
  end;
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
        __TLapeTree_Operator(Par).FLeft := FParams[i];
      __TLapeTree_Operator(Par).FRight := FParams[i + 1];
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
    Exit(NullResVar);
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

constructor TLapeTree_InternalMethod_GetExceptionMessage.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltString);
end;

function TLapeTree_InternalMethod_GetExceptionMessage.Compile(var Offset: Integer): TResVar;

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
  FCompiler.VarToDefault(Result, Offset);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType.Eval(op_Assign, TempVar, DestVar, Result.VarType.Eval(op_Addr, TempVar, Result, NullResVar, [], Offset), [], Offset, @_DocPos);

  FCompiler.Emitter._GetExceptionMessage(Offset, @_DocPos);

  Result.isConstant := True;
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

  if (FParams.Count = 1) and (BreakCount < BreakDepth) then
    LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
  else if (FoundNode = nil) then
    LapeException(lpeCannotBreak, DocPos)
  else
    FoundNode.addBreakStatement(JumpSafe, Offset, @_DocPos);
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

  if (FParams.Count = 1) and (ContinueCount < ContinueDepth) then
    LapeException(lpeOutOfTypeRange, FParams[0].DocPos)
  else if (FoundNode = nil) then
    LapeException(lpeCannotContinue, DocPos)
  else
      FoundNode.addContinueStatement(JumpSafe, Offset, @_DocPos);
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
  ResultVar: TLapeVar absolute ResultDecl;
begin
  Result := NullResVar;
  Dest := NullResVar;

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
      ResultDecl.Used := duTrue;

      Left := TLapeTree_ResVar.Create(_ResVar.New(ResultVar), FParams[0]);
      Right := FParams[0].setExpectedType(ResultVar.VarType) as TLapeTree_ExprBase;

      Compile(Offset);
    finally
      Free();
    end;

  Node := FParent;
  FoundNode := nil;

  while (Node <> nil) and (FoundNode = nil) do
  begin
    if (Node.QueryInterface(ILapeTree_CanExit, FoundNode) = 0) then
      if FoundNode.canExit() then
        FoundNode.addExitStatement(True, Offset, @_DocPos)
      else
        FoundNode := nil;
    Node := Node.Parent;
  end;

  if (FoundNode = nil) then
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
      if TLapeType_Pointer(VarType).PConst then
        LapeException(lpeVariableExpected, [FParams[0], Self])
      else
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
      if TLapeType_Pointer(VarType).PConst then
        LapeException(lpeVariableExpected, [FParams[0], Self])
      else
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

  with TLapeTree_Operator.Create(op_Assign, Self) do
  try
    Left := TLapeTree_ResVar.Create(Result.IncLock(), Self.FParams[0]);
    Right := TLapeTree_GlobalVar.Create(Result.VarType.NewGlobalVarP(), Self);
    Compile(Offset);
  finally
    Free();
  end;
end;

constructor TLapeTree_InternalMethod_Swap.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create('_Swap', ACompiler, ADocPos);
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
  FResType := ACompiler.getBaseType(ltSizeInt);
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

    FRes := FCompiler.getConstant(ParamType.Size, ltSizeInt);
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
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isConstant() then
    begin
      ParamType := FParams[0].resType();

      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes)) then
        LapeException(lpeInvalidEvaluation, DocPos);

      FRes := ParamType.VarLo();
    end;

  Result := inherited;
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
      if (ParamType <> nil) and (not (ParamType is TLapeType_Type)) and
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
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
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
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
    else if isConstant() then
    begin
      ParamType := FParams[0].resType();

      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;
      if (ParamType = nil) or (not (ParamType.BaseType in LapeOrdinalTypes + LapeArrayTypes)) then
        LapeException(lpeInvalidEvaluation, DocPos);

      FRes := ParamType.VarHi();
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_High.Compile(var Offset: Integer): TResVar;
var
  Param: TResVar;
begin
  if isConstant() or (resType() = nil) then
    LapeException(lpeCannotEvalRunTime, DocPos)
  else if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  Param := FParams[0].Compile(Offset);
  if (Param.VarType.BaseType = ltShortString) then
  begin
    Dest := NullResVar;
    Result := Param;
    Result.VarType := FCompiler.getBaseType(ltUInt8);
  end else
  begin
    Result := NullResVar;
    Result.VarPos.MemPos := mpStack;
    Result.VarType := Compiler.getBaseType(ltPointer);

    FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), Result, Param, NullResVar, Offset, @Self._DocPos);

    Result := NullResVar;
    Result.VarType := FCompiler.getBaseType(ltSizeInt);
    if (FDest.VarPos.MemPos = NullResVar.VarPos.MemPos) then
      FDest := VarResVar;
    FCompiler.getDestVar(FDest, Result, op_Unknown);
    case Param.VarType.BaseType of
      ltAnsiString:    FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!astr_getlen']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      ltWideString:    FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!wstr_getlen']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      ltUnicodeString: FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!ustr_getlen']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
      else FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!high']), Result, SizeOf(Pointer), Offset, @Self._DocPos);
    end;
  end;
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
    if (ParamType = nil) or (not (ParamType.BaseType in [ltStaticArray, ltSmallEnum, ltLargeEnum])) then
      LapeException(lpeInvalidEvaluation, DocPos);

    case ParamType.BaseType of
      ltStaticArray:
        with TLapeType_StaticArray(ParamType) do
          FRes := FCompiler.getConstant(Range.Hi - Range.Lo + 1, ltSizeInt);

      ltSmallEnum, ltLargeEnum:
        with TLapeType_Enum(ParamType) do
          FRes := FCompiler.getConstant(MemberMap.Count - GapCount, ltSizeInt);
    end;
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
    Result.VarType := FCompiler.getBaseType(ltSizeInt);
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
     (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray]))
  then
    LapeException(lpeInvalidEvaluation, DocPos);

  tmpType := Param.VarType;
  ArrayType := TLapeType_DynArray(Param.VarType).PType;

  if (not FParams[1].CompileToTempVar(Offset, Len)) or (ArrayType = nil) or
     (not Len.HasType()) or (not (Len.VarType.BaseType in LapeIntegerTypes))
	then
    LapeException(lpeInvalidEvaluation, DocPos);

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
    with TLapeTree_Invoke.Create(_ArraySetLength, Self) do
    try
      addParam(TLapeTree_ResVar.Create(Param.IncLock(), Self.FParams[0]));
      addParam(TLapeTree_ResVar.Create(Len.IncLock(), Self.FParams[1]));
      if (Param.VarType.BaseType = ltShortString) then
        addParam(TLapeTree_GlobalVar.Create(Param.VarType.VarHi(), Self.FParams[0]))
      else if (not (Param.VarType.BaseType in LapeStringTypes)) then
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
      Counter := FCompiler.getTempVar(ltSizeInt);
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
  Lo: TLapeTree_InternalMethod_Low;
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

  wasConstant := not ParRes.Writeable;
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
          if not isEmpty(Start) then
          begin
            TLapeType_StaticArray(ParRes.VarType).RangeCheck(ParRes, StartRes, [lefRangeCheck], Offset);

            Lo := TLapeTree_InternalMethod_Low.Create(Self);
            Lo.AddParam(TLapeTree_ResVar.Create(ParRes.IncLock(), Self.FParams[0]));

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
          __TLapeTree_ResVar(Param).FResVar.VarType := FCompiler.getBaseType(ltPointer);
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
     (not (Param.VarType.BaseType in LapeArrayTypes - [ltStaticArray]))
  then
    LapeException(lpeInvalidEvaluation, DocPos);

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
  SrcRes, DstRes, TempVar: TResVar;
  Src, TmpExpr: TLapeTree_ExprBase;
  wasConstant: Boolean;
  ArrayType: TLapeType;
  _ArrayInsert: TLapeGlobalVar;
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
      ltAnsiString:    _ArrayInsert := FCompiler['_AStr_Insert'];
      ltWideString:    _ArrayInsert := FCompiler['_WStr_Insert'];
      ltUnicodeString: _ArrayInsert := FCompiler['_UStr_Insert'];
      else
      begin
        _ArrayInsert := FCompiler['_ArrayInsert'];

        if DstRes.VarType.Equals(SrcRes.VarType) then
          __TLapeTree_ResVar(Src).FResVar.VarType := FCompiler.getBaseType(ltPointer)
        else if ArrayType.CompatibleWith(SrcRes.VarType) or (
          (SrcRes.VarType is TLapeType_StaticArray) and
          ArrayType.Equals(TLapeType_StaticArray(SrcRes.VarType).PType))
        then
        begin
          if (not (SrcRes.VarType is TLapeType_StaticArray)) and (not ArrayType.Equals(SrcRes.VarType)) then
          begin
            TempVar := _ResVar.New(FCompiler.getTempVar(ArrayType));
            with TLapeTree_Operator.Create(op_Assign, Self) do
            try
              Left := TLapeTree_ResVar.Create(TempVar.IncLock(), Self);
              Right := TLapeTree_ResVar.Create(SrcRes.IncLock(), Self);

              TempVar := Compile(Offset);
            finally
              Free();
            end;

            __TLapeTree_ResVar(Src).FResVar := TempVar;
          end;

          TmpExpr := TLapeTree_Operator.Create(op_Addr, Src);
          TLapeTree_Operator(TmpExpr).Left := Src;
          Src := TmpExpr;

          if ArrayType.CompatibleWith(SrcRes.VarType) then
            TmpExpr := TLapeTree_Integer.Create(1, FParams[0])
          else
            TmpExpr := nil;
        end else
          LapeExceptionFmt(lpeNoOverloadedMethod, [getParamTypesStr()], DocPos);

        DstRes.VarType := FCompiler.getBaseType(ltPointer);
      end;
    end;

    with TLapeTree_Invoke.Create(_ArrayInsert, Self) do
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
  FForceParam := True;
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

  ResVar := FCompiler.getTempStackVar(FCompiler.getGlobalType('ConstPointer'));
  ResVar := ResVar.VarType.Eval(op_Assign, tmpVar, ResVar, Param, [], Offset, @_DocPos);
  FCompiler.Emitter._JmpVar(Offset, @_DocPos);

  Param.Spill(1);
  ResVar.Spill(1);
end;

constructor TLapeTree_InternalMethod_Raise.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FForceParam := True;
end;

function TLapeTree_InternalMethod_Raise.Compile(var Offset: Integer): TResVar;

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
  Result := NullResVar;

  if (FParams.Count = 0) then
  begin
    if (not InExceptBlock()) then
      LapeException(lpeOutsideExceptBlock, _DocPos);

    FCompiler.Emitter._ReRaiseException(Offset, @_DocPos);
  end else
  begin
    if (FParams.Count > 2) then
      LapeException(lpeTooMuchParameters, _DocPos);

    Invoke := TLapeTree_Invoke.Create('RaiseException', Self);
    Invoke.Parent := Self;
    try
      if (FParams.Count = 1) and (FParams[0].resType() <> nil) and (FParams[0].resType.BaseType = ltPointer) then
        Invoke.addParam(TLapeTree_InternalMethod_GetExceptionMessage.Create(Self));
      while (FParams.Count > 0) do
        Invoke.addParam(FParams[0]);

      Result := Invoke.Compile(Offset);
    finally
      Invoke.Free();
    end;
  end;
end;

constructor TLapeTree_InternalMethod_Objectify.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FConstant := bFalse;
end;

function TLapeTree_InternalMethod_Objectify.resType: TLapeType;
var
  VarType: TLapeType;
begin
  if (FResType = nil) then
  begin
    if (FParams.Count <> 1) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

    VarType := FParams[0].resType();
    if (VarType = nil) then
      LapeException(lpeTypeExpected, DocPos);
    if (VarType.ClassType <> TLapeType_Method) then
      LapeException(lpeExpectedNormalMethod, DocPos);

    FResType := FCompiler.addManagedType(TLapeType_MethodOfObject.Create(VarType as TLapeType_Method)) as TLapeType_MethodOfObject;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Objectify.Compile(var Offset: Integer): TResVar;
var
  Method: TLapeGlobalVar;
  Param: TResVar;
  _Method, _Callback: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  with FCompiler['_Objectify'].VarType as TLapeType_OverloadedMethod do
    Method := OnFunctionNotFound(TLapeType_OverloadedMethod(GetSelf()), TLapeType_Method(resType()));

  Result := _ResVar.New(Method);

  Param := FParams[0].Compile(Offset);

  if (Param.VarPos.MemPos = mpMem) then
  begin
    if (Param.VarPos.GlobalVar.Ptr = nil) then
      LapeException(lpeImpossible, DocPos);

    PMethod(Method.Ptr)^.Data := Param.VarPos.GlobalVar.Ptr;
  end else
  begin
    _Callback := _ResVar.New(FCompiler.getTempVar(ltPointer)).IncLock();
    _Callback.isConstant := False;

    with TLapeTree_Operator.Create(op_Assign, Self) do
    try
      Left := TLapeTree_ResVar.Create(_Callback.IncLock(), Self);
      Right := TLapeTree_ResVar.Create(Param.IncLock(), Self);

      Compile(Offset).Spill(1);
    finally
      Free();
    end;

    Result.isConstant := False;

    with TLapeTree_Operator.Create(op_Deref, Self) do
    try
      Left := TLapeTree_Operator.Create(op_Addr, Self);
      with Left as TLapeTree_Operator do
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);

      _Method := Compile(Offset);
    finally
      Free();
    end;

    with TLapeTree_Operator.Create(op_Assign, Self) do
    try
      Left := TLapeTree_Operator.Create(op_Dot, Self);
      with Left as TLapeTree_Operator do
      begin
        Left := TLapeTree_ResVar.Create(_Method.IncLock(), Self);
        Right := TLapeTree_String.Create('Self', Self);
      end;

      Right := TLapeTree_Operator.Create(op_Addr, Self);
      with Right as TLapeTree_Operator do
        Left := TLapeTree_ResVar.Create(_Callback.IncLock(), Self);

      Compile(Offset).Spill(1);
    finally
      Free();
    end;

    Result.isConstant := True;
  end;
end;

constructor TLapeTree_InternalMethod_GetCallerLocation.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltPointer);
end;

function TLapeTree_InternalMethod_GetCallerLocation.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;

  FCompiler.Emitter._GetCallerLocation(Offset, @_DocPos);
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

constructor TLapeTree_InternalMethod_GetCallerLocationStr.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create('_LocationToStr', ACompiler, ADocPos);

  FResType := ACompiler.getBaseType(ltString);

  addParam(TLapeTree_InternalMethod_GetCallerLocation.Create(Self));
end;

constructor TLapeTree_InternalMethod_GetExceptionLocation.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltPointer);
end;

function TLapeTree_InternalMethod_GetExceptionLocation.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;

  FCompiler.Emitter._GetExceptionLocation(Offset, @_DocPos);
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

constructor TLapeTree_InternalMethod_GetExceptionLocationStr.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited Create('_LocationToStr', ACompiler, ADocPos);

  FResType := ACompiler.getBaseType(ltString);

  addParam(TLapeTree_InternalMethod_GetExceptionLocation.Create(Self));
end;

constructor TLapeTree_InternalMethod_GetScriptMethodName.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltShortString);
end;

function TLapeTree_InternalMethod_GetScriptMethodName.Compile(var Offset: Integer): TResVar;
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

  FCompiler.Emitter._GetScriptMethodName(Offset, @_DocPos);
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

constructor TLapeTree_InternalMethod_DumpCallStack.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;
  FResType := ACompiler.getBaseType(ltString);
end;

function TLapeTree_InternalMethod_DumpCallStack.Compile(var Offset: Integer): TResVar;
var
  TempVar, DestVar, ParamVar, OffsetVar: TResVar;
begin
  Dest := NullResVar;
  Result := NullResVar;
  TempVar := NullResVar;

  if (Params.Count > 1) then
    LapeException(lpeTooMuchParameters, DocPos);

  if isEmpty(FParams[0]) then
    OffsetVar := _ResVar.New(FCompiler.getConstant(0, ltInt32))
  else
  begin
    if (not FParams[0].CompileToTempVar(Offset, ParamVar)) or (not (ParamVar.VarType.BaseType in LapeIntegerTypes)) then
      LapeExceptionFmt(lpeExpected, ['Integer'], DocPos);

    OffsetVar := _ResVar.New(FCompiler.getTempVar(ltInt32));
    OffsetVar := OffsetVar.VarType.Eval(op_Assign, TempVar, OffsetVar, ParamVar, [], Offset);

    ParamVar.Spill(1);
  end;

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := OffsetVar.VarType;

  OffsetVar.VarType.Eval(op_Assign, TempVar, DestVar, OffsetVar, [], Offset, @_DocPos);
  OffsetVar.Spill(1);

  Result := _ResVar.New(FCompiler.getTempVar(ltString));
  FCompiler.VarToDefault(Result, Offset);

  DestVar := NullResVar;
  DestVar.VarPos.MemPos := mpStack;
  DestVar.VarType := FCompiler.getBaseType(ltPointer);
  DestVar.VarType.Eval(op_Assign, TempVar, DestVar, Result.VarType.Eval(op_Addr, TempVar, Result, NullResVar, [], Offset), [], Offset, @_DocPos);

  FCompiler.Emitter._DumpCallStack(Offset, @_DocPos);

  Result.isConstant := True;
end;

end.

