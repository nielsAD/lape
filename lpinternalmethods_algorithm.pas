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
  var
    Lo, Hi: TLapeTree_Invoke;
  begin
    // Check if user defined `_Sort` exists. Useful for providing a native method
    if InvokeMagicMethod(Self, '_Sort', Result, Offset) then
      Exit;

    RequireOperators(FCompiler, [op_cmp_GreaterThan], ArrayElementType, DocPos);

    Lo := TLapeTree_InternalMethod_Low.Create(Self);
    Lo.addParam(TLapeTree_ResVar.Create(ArrayVar, Self));
    Hi := TLapeTree_InternalMethod_High.Create(Self);
    Hi.addParam(TLapeTree_ResVar.Create(ArrayVar, Self));

    with TLapeTree_Invoke.Create('_Sort', Self) do
    try
      addParam(TLapeTree_ResVar.Create(ArrayVar, Self));
      addParam(Lo.FoldConstants() as TLapeTree_ExprBase);
      addParam(Hi.FoldConstants() as TLapeTree_ExprBase);

      Compile(Offset);
    finally
      Free();
    end;
  end;

  procedure DoCompareFunc;
  var
    CompareVar: TResVar;
    Lo, Hi: TLapeTree_Invoke;
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

    Lo := TLapeTree_InternalMethod_Low.Create(Self);
    Lo.addParam(TLapeTree_ResVar.Create(ArrayVar, Self));
    Hi := TLapeTree_InternalMethod_High.Create(Self);
    Hi.addParam(TLapeTree_ResVar.Create(ArrayVar, Self));

    with TLapeTree_Invoke.Create('_Sort', Self) do
    try
      addParam(TLapeTree_ResVar.Create(ArrayVar, Self));
      addParam(Lo.FoldConstants() as TLapeTree_ExprBase);
      addParam(Hi.FoldConstants() as TLapeTree_ExprBase);
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

    // _SortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int32; SortUp: EvalBool)
    with TLapeTree_Invoke.Create('_SortWeighted', Self) do
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
    // Check if user defined `_Reverse` exists. Useful for providing a native method
    if InvokeMagicMethod(Self, '_Reverse', Result, Offset) then
      Exit;

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
    with TLapeTree_Invoke.Create('_Reverse', Self) do
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

  // Check if user defined `_Unique` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_Unique', Result, Offset) then
    Exit;
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
  LengthVar, ArrayVar, ArraySortedVar, MiddleIndexVar: TResVar;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayType := resType();
  if (ArrayType = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  RequireOperators(FCompiler, [op_cmp_Equal, op_cmp_LessThan, op_cmp_GreaterThan, op_Plus, op_Minus], ArrayType, DocPos);
  RequireOperators(FCompiler, [op_Divide], ArrayType, FCompiler.getBaseType(ltDouble), DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(resType));
  ArraySortedVar := _ResVar.New(FCompiler.getTempVar(ArrayVar.VarType));
  MiddleIndexVar := _ResVar.New(FCompiler.getTempVar(ltDouble));

  FCompiler.VarToDefault(Result, Offset);

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // if Length(ArrayVar) > 0 then
  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_StatementList.Create(Self);
    with TLapeTree_StatementList(Body) do
    begin
      // SortedArrayVar := Sorted(ArrayVar)
      addStatement(TLapeTree_Operator.Create(op_Assign, Self));
      with TLapeTree_Operator(Statements[0]) do
      begin
        Left := TLapeTree_ResVar.Create(ArraySortedVar.IncLock(), Self);
        Right := TLapeTree_InternalMethod_Sorted.Create(Self);
        with TLapeTree_InternalMethod_Sorted(Right) do
          addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));
      end;

      // MiddleIndexVar := (LengthVar - 1) / 2
      addStatement(TLapeTree_Operator.Create(op_Assign, Self));
      with TLapeTree_Operator(Statements[1]) do
      begin
        Left := TLapeTree_ResVar.Create(MiddleIndexVar.IncLock(), Self);
        Right := TLapeTree_Operator.Create(op_Divide, Self);
        with TLapeTree_Operator(Right) do
        begin
          Left := TLapeTree_Operator.Create(op_Minus, Self);
          with TLapeTree_Operator(Left) do
          begin
            Left := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
            Right := TLapeTree_Integer.Create(1, Self);
          end;
          Right := TLapeTree_Integer.Create(2, Self);
        end;
      end;

      // Result := (SortedArrayVar[Trunc(MiddleIndexVar)] + SortedArrayVar[Ceil(High(MiddleIndexVar)]) / 2
      addStatement(TLapeTree_Operator.Create(op_Assign, Self));
      with TLapeTree_Operator(Statements[2]) do
      begin
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        Right := TLapeTree_Operator.Create(op_Divide, Self);

        TLapeTree_Operator(Right).Left := TLapeTree_Operator.Create(op_Plus, Self);
        with TLapeTree_Operator(TLapeTree_Operator(Right).Left) do
        begin
          Left := TLapeTree_Operator.Create(op_Index, Self);
          with TLapeTree_Operator(Left) do
          begin
            Left := TLapeTree_ResVar.Create(ArraySortedVar.IncLock(), Self);
            Right := TLapeTree_Invoke.Create('Trunc', Self);
            with TLapeTree_Invoke(Right) do
              TLapeTree_Invoke(Right).addParam(TLapeTree_ResVar.Create(MiddleIndexVar.IncLock(), Self));
          end;

          Right := TLapeTree_Operator.Create(op_Index, Self);
          with TLapeTree_Operator(Right) do
          begin
            Left := TLapeTree_ResVar.Create(ArraySortedVar.IncLock(), Self);
            Right := TLapeTree_Invoke.Create('Ceil', Self);
            with TLapeTree_Invoke(Right) do
              TLapeTree_Invoke(Right).addParam(TLapeTree_ResVar.Create(MiddleIndexVar.IncLock(), Self));
          end;
        end;

        TLapeTree_Operator(Right).Right := TLapeTree_Integer.Create(2, Self);
      end;
    end;

    Compile(Offset);
  finally
    ArraySortedVar.Spill(2);
    LengthVar.Spill(1);

    Free();
  end;

  Assert((Result.VarPos.MemPos <> mpVar) or (Result.Lock > 0));
  Result.isConstant := True;
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
  NewParam: TLapeTree_InternalMethod_Sorted;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not (FParams[0].resType() is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  // Check if user defined `_ArrayMode` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArrayMode', Result, Offset) then
    Exit;

  ArrayElementType := resType();
  if (ArrayElementType = nil) then
    LapeException(lpeExpectedArray, DocPos);

  RequireOperators(FCompiler, [op_cmp_Equal], ArrayElementType, DocPos);

  NewParam := TLapeTree_InternalMethod_Sorted.Create(Self);
  NewParam.addParam(FParams[0]);
  setExpr(TLapeTree_GlobalVar.Create(FCompiler['_ArrayMode'], Self));
  addParam(NewParam);
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

  // Check if user defined `_ArrayMin` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArrayMin', Result, Offset) then
    Exit;

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

  // Check if user defined `_ArrayMax` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArrayMax', Result, Offset) then
    Exit;

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
  ArrayVar, CounterVar: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (resType() = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  // Check if user defined `_ArraySum` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArraySum', Result, Offset) then
    Exit;

  RequireOperators(FCompiler, [op_Plus], TLapeType_DynArray(ArrayVar.VarType).PType, DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(resType()));
  CounterVar := _ResVar.New(FCompiler.getTempVar(TLapeType_DynArray(ArrayVar.VarType).PType));

  FCompiler.VarToDefault(Result, Offset);

  with TLapeTree_For.Create(Self) do
  try
    LoopType := loopOver;

    Counter := TLapeTree_ResVar.Create(CounterVar.IncLock(), Self);
    Limit := TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self);
    Body := TLapeTree_Operator.Create(op_Assign, Self);
    with TLapeTree_Operator(Body) do
    begin
      Left := TLapeTree_ResVar.Create(Result.IncLock(2), Self);
      Right := TLapeTree_Operator.Create(op_Plus, Self);
      with TLapeTree_Operator(Right) do
      begin
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        Right := TLapeTree_ResVar.Create(CounterVar.IncLock(), Self);
      end;
    end;

    Compile(Offset);
  finally
    CounterVar.Spill(1);

    Free();
  end;

  Assert((Result.VarPos.MemPos <> mpVar) or (Result.Lock > 0));
  Result.isConstant := True;
end;

function TLapeTree_InternalMethod_ArrayMean.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();

      if (Typ is TLapeType_DynArray) then
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
  ArrayVar, LengthVar: TResVar;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (resType() = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  RequireOperators(FCompiler, [op_Plus], TLapeType_DynArray(ArrayVar.VarType).PType, DocPos);
  RequireOperators(FCompiler, [op_Divide], TLapeType_DynArray(ArrayVar.VarType).PType, FCompiler.getBaseType(ltDouble), DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(resType()));

  FCompiler.VarToDefault(Result, Offset);

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_Operator.Create(op_Assign, Self);
    with TLapeTree_Operator(Body) do
    begin
      Left := TLapeTree_ResVar.Create(Result.IncLock(2), Self);

      Right := TLapeTree_Operator.Create(op_Divide, Self);
      with TLapeTree_Operator(Right) do
      begin
        Left := TLapeTree_InternalMethod_ArraySum.Create(Self);
        with TLapeTree_InternalMethod_ArraySum(Left) do
          addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));
        Right := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
      end;
    end;

    Compile(Offset);
  finally
    LengthVar.Spill(1);

    Free();
  end;

  Assert((Result.VarPos.MemPos <> mpVar) or (Result.Lock > 0));
  Result.isConstant := True;
end;

function TLapeTree_InternalMethod_ArrayVariance.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();

      if (Typ is TLapeType_DynArray) then
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
var
  LengthVar, ArrayVar, CounterVar, SquareVar, MeanVar: TResVar;
  ArrayType: TLapeType;
  Minus: TLapeTree_Operator;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayType := resType();
  if (ArrayType = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  RequireOperators(FCompiler, [op_cmp_LessThan, op_cmp_GreaterThan, op_Minus], ArrayType, DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(ArrayType));
  CounterVar := _ResVar.New(FCompiler.getTempVar(ArrayType));
  SquareVar := _ResVar.New(FCompiler.getTempVar(ArrayType));
  MeanVar := _ResVar.New(FCompiler.getTempVar(ArrayType)).IncLock();

  FCompiler.VarToDefault(Result, Offset);
  FCompiler.VarToDefault(SquareVar, Offset);

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_StatementList.Create(Self);
    with TLapeTree_StatementList(Body) do
    begin
      addStatement(TLapeTree_Operator.Create(op_Assign, Self));
      with TLapeTree_Operator(Statements[0]) do
      begin
        Left := TLapeTree_ResVar.Create(MeanVar.IncLock(), Self);
        Right := TLapeTree_InternalMethod_ArrayMean.Create(Self);
        with TLapeTree_InternalMethod_ArrayMean(Right) do
          addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));
      end;

      addStatement(TLapeTree_For.Create(Self));
      with TLapeTree_For(Statements[1]) do
      begin
        LoopType := loopOver;

        Counter := TLapeTree_ResVar.Create(CounterVar.IncLock(), Self);
        Limit := TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self);
        Body := TLapeTree_Operator.Create(op_AssignPlus, Self);
        with TLapeTree_Operator(Body) do
        begin
          Left := TLapeTree_ResVar.Create(SquareVar.IncLock(), Self);

          Minus := TLapeTree_Operator.Create(op_Minus, Self);
          with TLapeTree_Operator(Minus) do
          begin
            Left := TLapeTree_ResVar.Create(CounterVar.IncLock(), Self);
            Right := TLapeTree_ResVar.Create(MeanVar.IncLock(), Self);
          end;

          Right := TLapeTree_Invoke.Create('Sqr', Self);
          with TLapeTree_Invoke(Right) do
            addParam(Minus);
        end;
      end;

      addStatement(TLapeTree_Operator.Create(op_Assign, Self));
      with TLapeTree_Operator(Statements[2]) do
      begin
        Left := TLapeTree_ResVar.Create(Result.IncLock(2), Self);
        Right := TLapeTree_Operator.Create(op_Divide, Self);
        with TLapeTree_Operator(Right) do
        begin
          Left := TLapeTree_ResVar.Create(SquareVar.IncLock(), Self);
          Right := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
        end;
      end;
    end;

    Compile(Offset);
  finally
    LengthVar.Spill(1);
    CounterVar.Spill(1);
    MeanVar.Spill(1);

    Free();
  end;

  Assert((Result.VarPos.MemPos <> mpVar) or (Result.Lock > 0));
  Result.isConstant := True;
end;

function TLapeTree_InternalMethod_ArrayStdev.resType: TLapeType;
var
  Typ: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      Typ := FParams[0].resType();

      if (Typ is TLapeType_DynArray) then
      begin
        if (TLapeType_DynArray(Typ).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
          FResType := FCompiler.getBaseType(ltDouble)
        else
          FResType := TLapeType_DynArray(Typ).PType;
      end;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_ArrayStdev.Compile(var Offset: Integer): TResVar;
var
  Variance: TLapeTree_InternalMethod_ArrayVariance;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (resType = nil) then
    LapeException(lpeExpectedArray, DocPos);

  // Check if user defined `_ArrayStdev` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArrayStdev', Result, Offset) then
    Exit;

  with TLapeTree_Invoke.Create('Sqrt', Self) do
  try
    Variance := TLapeTree_InternalMethod_ArrayVariance.Create(Self);
    Variance.addParam(Self.Params[0]);

    addParam(Variance);

    Result := Compile(Offset);
  finally
    Free();
  end;
end;

end.

