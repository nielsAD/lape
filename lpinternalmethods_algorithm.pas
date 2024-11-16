{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
unit lpinternalmethods_algorithm;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lptree, lpvartypes;

type
  TLapeTree_InternalMethod_Sort = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Sorted = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Reverse = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Reversed = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Unique = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayMedian = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayMode = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayMin = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayMax = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArraySum = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayMean = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayVariance = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayStdev = class(TLapeTree_InternalMethod)
  public
    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArrayDifference = class(TLapeTree_InternalMethod)
  protected
    FMethod: lpString;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;

    function resType: TLapeType; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_ArraySymDifference = class(TLapeTree_InternalMethod_ArrayDifference)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
  end;

  TLapeTree_InternalMethod_ArrayIntersection = class(TLapeTree_InternalMethod_ArrayDifference)
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
  end;

implementation

uses
  lpparser, lpvartypes_array, lpmessages, lpinternalmethods;

// sort(arr)
// sort(arr, comparefunc)
// sort(arr, weights, lowtohigh)
function TLapeTree_InternalMethod_Sort.Compile(var Offset: Integer): TResVar;
var
  ArrayVar: TResVar;
  ArrayElementType: TLapeType;

  procedure DoNormal;
  begin
    // ensure we can generate such a method
    if (not HasMagicMethod(Compiler, '_ArraySort', getParamTypes(), resType())) then
      RequireOperators(FCompiler, [op_cmp_GreaterThan], ArrayElementType, DocPos);

    with TLapeTree_Invoke.Create('_ArraySort', Self) do
    try
      addParam(TLapeTree_ResVar.Create(ArrayVar, Self));

      Compile(Offset);
    finally
      Free();
    end;
  end;

  procedure DoCompareFunc;
  var
    CompareVar: TResVar;
  begin
    CompareVar := FParams[1].Compile(Offset);
    if (not (CompareVar.VarType is TLapeType_Method)) then
      LapeException(lpeInvalidCompareMethod, DocPos);

    with TLapeType_Method(CompareVar.VarType) do
    begin
      if not (
          (Params.Count = 2) and
          (Res.IsOrdinal()) and
          (ArrayElementType.Equals(Params[0].VarType)) and
          (ArrayElementType.Equals(Params[1].VarType))
      ) then
        LapeException(lpeInvalidCompareMethod, DocPos);
    end;

    with TLapeTree_Invoke.Create('_ArraySort', Self) do
    try
      addParam(TLapeTree_ResVar.Create(ArrayVar, Self));
      addParam(TLapeTree_ResVar.Create(CompareVar, Self));

      Compile(Offset);
    finally
      Free();
    end;
  end;

  procedure DoWeighted;
  var
    ArrayPtr: TResVar;
    Len, Weights: TLapeTree_Invoke;
  begin
    if (ArrayVar.VarType.BaseType = ltDynArray) then
    begin
      ArrayPtr := ArrayVar;
      ArrayPtr.VarType := FCompiler.getBaseType(ltPointer);
    end else
      ArrayPtr := ArrayVar.VarType.Eval(op_Addr, Result, ArrayVar, NullResVar, [], Offset, @_DocPos); // ltStaticArray

    Len := TLapeTree_InternalMethod_Length.Create(Self);
    Len.addParam(TLapeTree_ResVar.Create(ArrayVar, Self));
    Weights := TLapeTree_InternalMethod_Copy.Create(Self);
    Weights.addParam(Params[1]);

    // _ArraySortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int32; SortUp: EvalBool)
    with TLapeTree_Invoke.Create('_ArraySortWeighted', Self) do
    try
      addParam(TLapeTree_ResVar.Create(ArrayPtr, Self));
      addParam(TLapeTree_Integer.Create(ArrayElementType.Size, Self));
      addParam(Len.FoldConstants() as TLapeTree_ExprBase);
      addParam(Weights.FoldConstants() as TLapeTree_ExprBase);
      addParam(Self.Params[1]);

      Compile(Offset);
    finally
      Free();
    end;
  end;

begin
  Result := NullResVar;
  Dest := NullResVar;
  if (not (FParams.Count in [1, 2, 3])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayVar := FParams[0].Compile(Offset);
  if (not (ArrayVar.VarType is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);
  ArrayElementType := TLapeType_DynArray(ArrayVar.VarType).PType;

  case FParams.Count of
    1: DoNormal();
    2: DoCompareFunc();
    3: DoWeighted();
  end;
end;

function TLapeTree_InternalMethod_Sorted.resType: TLapeType;
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

function TLapeTree_InternalMethod_Sorted.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (resType() = nil) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  with TLapeTree_InternalMethod_Copy.Create(Self) do
  try
    addParam(Self.FParams[0]);

    Result := Compile(Offset);
    Result.isConstant := False;
  finally
    Free();
  end;

  with TLapeTree_InternalMethod_Sort.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(Result, Self));
    while (Self.FParams.Count > 0) do
      addParam(Self.FParams[0]);

    Compile(Offset).Spill(1);
  finally
    Free();
  end;

  Result.isConstant := True;
  Assert((Result.VarPos.MemPos <> mpVar) or (Result.Lock > 0));
end;

function TLapeTree_InternalMethod_Reverse.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, ArrayPointer: TResVar;
  ArrayHigh: TLapeTree_InternalMethod_High;
  ArrayType: TLapeType;
  ArrayWasConstant: Boolean;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (Params.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not FParams[0].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);

  ArrayWasConstant := ArrayVar.isConstant;
  if ArrayWasConstant then
    ArrayVar.isConstant := False;

  try
    case ArrayVar.VarType.BaseType of
      ltStaticArray:
        begin
          with TLapeTree_Operator.Create(op_Addr, Self) do
          try
            Left := TLapeTree_Operator.Create(op_Index, Self);

            TLapeTree_Operator(Left).Left := TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self);
            TLapeTree_Operator(Left).Right := TLapeTree_GlobalVar.Create(ArrayVar.VarType.VarLo(), Self);

            ArrayPointer := Compile(Offset);
          finally
            ArrayVar.DecLock(1);

            Free();
          end;
        end;

      ltDynArray:
        begin
          ArrayPointer := ArrayVar;
          ArrayPointer.VarType := FCompiler.getBaseType(ltPointer);
        end;
      else
        LapeException(lpeExpectedArray, DocPos);
    end;

    ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;
    with TLapeTree_Invoke.Create('_ArrayReverse', Self) do
    try
      addParam(TLapeTree_ResVar.Create(ArrayPointer.IncLock(), Self));
      addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));

      case ArrayVar.VarType.BaseType of
        ltDynArray:
          begin
            ArrayHigh := TLapeTree_InternalMethod_High.Create(Self);
            ArrayHigh.addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

            addParam(ArrayHigh);
          end;

        ltStaticArray:
          addParam(TLapeTree_Integer.Create(TLapeType_StaticArray(ArrayVar.VarType).Range.Hi - TLapeType_StaticArray(ArrayVar.VarType).Range.Lo, Self));
      end;

      Compile(Offset).Spill(1);
    finally
      ArrayPointer.DecLock(1);

      Free();
    end;
  finally
    ArrayVar.isConstant := ArrayWasConstant;
  end;
end;

function TLapeTree_InternalMethod_Reversed.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count = 1) and (not isEmpty(FParams[0])) then
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

function TLapeTree_InternalMethod_Reversed.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;
  if (resType() = nil) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  with TLapeTree_InternalMethod_Copy.Create(Self) do
  try
    addParam(Self.FParams[0]);

    Result := Compile(Offset);
  finally
    Free();
  end;

  with TLapeTree_InternalMethod_Reverse.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(Result.IncLock(), Self));

    Compile(Offset).Spill(1);
  finally
    Free();
  end;

  Assert((Result.VarPos.MemPos <> mpVar) or (Result.Lock > 0));
end;

function TLapeTree_InternalMethod_Unique.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) and (FParams.Count = 1) and (not isEmpty(FParams[0])) then
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

function TLapeTree_InternalMethod_Unique.Compile(var Offset: Integer): TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

   if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  ArrayElementType := TLapeType_DynArray(resType()).PType;

  // ensure we can generate such a method
  if (not HasMagicMethod(Compiler, '_ArrayUnique', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_Equal], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayUnique'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMedian.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();
      if (Typ is TLapeType_DynArray) then
      begin
        if (TLapeType_DynArray(Typ).PType.BaseType in LapeIntegerTypes + LapeRealTypes) then
          FResType := FCompiler.getBaseType(ltDouble)
        else
          FResType := TLapeType_DynArray(Typ).PType;
      end;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMedian.Compile(var Offset: Integer): TResVar;
var
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayType := resType();
  if (ArrayType = nil) then
    LapeException(lpeExpectedArray, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayMedian'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMode.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();
      if (Typ is TLapeType_DynArray) then
        FResType := TLapeType_DynArray(Typ).PType;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMode.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, SortedArrayVar, LengthVar: TResVar;
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not (FParams[0].resType() is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  ArrayElementType := resType();
  if (ArrayElementType = nil) then
    LapeException(lpeExpectedArray, DocPos);

  // ensure we can generate such a method
  if (not HasMagicMethod(Compiler, '_ArrayMode', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_Equal], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayMode'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMin.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();
      if (Typ is TLapeType_DynArray) then
        FResType := TLapeType_DynArray(Typ).PType;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMin.Compile(var Offset: Integer): TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  ArrayElementType := resType();
  if (ArrayElementType = nil) then
    LapeException(lpeExpectedArray, DocPos);

  // ensure we can generate such a method
  if (not HasMagicMethod(Compiler, '_ArrayMin', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_LessThan], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayMin'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMax.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();
      if (Typ is TLapeType_DynArray) then
        FResType := TLapeType_DynArray(Typ).PType;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMax.Compile(var Offset: Integer): TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  ArrayElementType := resType();
  if (ArrayElementType = nil) then
    LapeException(lpeExpectedArray, DocPos);

  // ensure we can generate such a method
  if (not HasMagicMethod(Compiler, '_ArrayMax', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_GreaterThan], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayMax'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArraySum.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();

      if (Typ is TLapeType_DynArray) then
      begin
        if (TLapeType_DynArray(Typ).PType.BaseType in LapeRealTypes) then
          FResType := FCompiler.getBaseType(ltDouble)
        else
        if (TLapeType_DynArray(Typ).PType.BaseType in LapeIntegerTypes) then
          FResType := FCompiler.getBaseType(ltInt64)
        else
          FResType := TLapeType_DynArray(Typ).PType;
       end;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArraySum.Compile(var Offset: Integer): TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (resType() = nil) then
    LapeException(lpeExpectedArray, DocPos);

  // ensure we can generate such a method, best to error here
  ArrayElementType := TLapeType_DynArray(FParams[0].resType()).PType;
  if (not HasMagicMethod(Compiler, '_ArraySum', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_Plus], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArraySum'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMean.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();

      if (Typ is TLapeType_DynArray) and TLapeType_DynArray(Typ).HasType then
      begin
        if (TLapeType_DynArray(Typ).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
          FResType := FCompiler.getBaseType(ltDouble)
        else
          FResType := TLapeType_DynArray(Typ).PType;
      end;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayMean.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, CounterVar: TResVar;
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (resType() = nil) then
    LapeException(lpeExpectedArray, DocPos);

  // ensure we can generate such a method, best to error here
  ArrayElementType := TLapeType_DynArray(FParams[0].resType()).PType;
  if (not HasMagicMethod(Compiler, '_ArrayMean', getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_Plus], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayMean'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayVariance.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();

      if (Typ is TLapeType_DynArray) and TLapeType_DynArray(Typ).HasType then
      begin
        if (TLapeType_DynArray(Typ).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
          FResType := FCompiler.getBaseType(ltDouble)
        else
          FResType := TLapeType_DynArray(Typ).PType;
      end;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayVariance.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not (FParams[0].resType() is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayVariance'], Self));
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayStdev.resType: TLapeType;
begin
  if (FResType = nil) then
    FResType := FCompiler.getBaseType(ltDouble);
  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayStdev.Compile(var Offset: Integer): TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not (FParams[0].resType() is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayStdev'], Self));
  Result := inherited;
end;

constructor TLapeTree_InternalMethod_ArrayDifference.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FMethod := '_ArrayDifference';
end;

function TLapeTree_InternalMethod_ArrayDifference.resType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 2) and
       (FParams[0].resType() is TLapeType_DynArray) and (FParams[1].resType() is TLapeType_DynArray) and
       (FParams[0].resType().Equals(FParams[1].resType())) then
      FResType := FParams[0].resType();

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayDifference.Compile(var Offset: Integer): TResVar;
var
  ArrayElementType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 2) or isEmpty(FParams[0]) or isEmpty(FParams[1]) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos);
  if (resType() = nil) then
    LapeException(lpeInvalidEvaluation, DocPos);

  // ensure we can generate such a method, best to error here
  ArrayElementType := TLapeType_DynArray(FParams[0].resType()).PType;
  if (not HasMagicMethod(Compiler, FMethod, getParamTypes(), resType())) then
    RequireOperators(FCompiler, [op_cmp_Equal], ArrayElementType, DocPos);

  setExpr(TLapeTree_GlobalVar.Create(FCompiler[FMethod], Self));
  Result := inherited;
end;

constructor TLapeTree_InternalMethod_ArraySymDifference.Create( ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FMethod := '_ArraySymDifference';
end;

constructor TLapeTree_InternalMethod_ArrayIntersection.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, ADocPos);

  FMethod := '_ArrayIntersection';
end;

end.

