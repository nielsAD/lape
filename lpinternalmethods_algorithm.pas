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
  TLapeTree_InternalMethod_SortWeighted = class(TLapeTree_InternalMethod)
  public
    function Compile(var Offset: Integer): TResVar; override;
  end;

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

const
  _LapeSort: lpString =
    'procedure _Sort(p: Pointer; ElSize, Len: SizeInt; Compare: _LapeCompareFunc); overload;' + LineEnding +
    'const'                                                                                   + LineEnding +
    '  ShellSortGaps = [835387, 392925, 184011, 85764, 39744, 18298, 8359,'                   + LineEnding +
    '                   3785, 1695, 701, 301, 132, 57, 23, 10, 4, 1];'                        + LineEnding +
    'var'                                                                                     + LineEnding +
    '  Gap, Hi, i, j: SizeInt;'                                                               + LineEnding +
    '  Item, Src: Pointer;'                                                                   + LineEnding +
    'begin'                                                                                   + LineEnding +
    '  Item := GetMem(ElSize);'                                                               + LineEnding +
    '  Hi := Len - 1;'                                                                        + LineEnding +
    ''                                                                                        + LineEnding +
    '  for Gap in ShellSortGaps do'                                                           + LineEnding +
    '    for i := Gap to Hi do'                                                               + LineEnding +
    '    begin'                                                                               + LineEnding +
    '      Move(p[i * ElSize]^, Item^, ElSize);'                                              + LineEnding +
    ''                                                                                        + LineEnding +
    '      j := i;'                                                                           + LineEnding +
    '      while (j >= Gap) do'                                                               + LineEnding +
    '      begin'                                                                             + LineEnding +
    '        Src := p[(j - Gap) * ElSize];'                                                   + LineEnding +
    '        if (Compare(Src^, Item^) <= 0) then'                                             + LineEnding +
    '          Break;'                                                                        + LineEnding +
    ''                                                                                        + LineEnding +
    '        Move(Src^, p[j * ElSize]^, ElSize);'                                             + LineEnding +
    '        Move(Item^, Src^, ElSize);'                                                      + LineEnding +
    '        j := j - Gap;'                                                                   + LineEnding +
    '      end;'                                                                              + LineEnding +
    '    end;'                                                                                + LineEnding +
    ''                                                                                        + LineEnding +
    '  FreeMem(Item);'                                                                        + LineEnding +
    'end;';

  _LapeArrayUnique: lpString =
    'procedure _Unique(var p: Pointer; ElSize: SizeInt; Equals: _LapeEqualsFunc;'            + LineEnding +
    '                  Dispose: private procedure(p: Pointer);'                              + LineEnding +
    '                  Copy: private procedure(Src: ConstPointer; Dst: Pointer)); overload;' + LineEnding +
    'type'                                                                                   + LineEnding +
    '  PSizeInt = ^SizeInt;'                                                                 + LineEnding +
    'var'                                                                                    + LineEnding +
    '  i, j, Len: SizeInt;'                                                                  + LineEnding +
    '  Src: Pointer;'                                                                        + LineEnding +
    'begin'                                                                                  + LineEnding +
    '  if (p = nil) then'                                                                    + LineEnding +
    '    Exit;'                                                                              + LineEnding +
    ''                                                                                       + LineEnding +
    '  Len := PSizeInt(p)[-1]^' {$IFDEF FPC}+'+1'{$ENDIF}+';'                                + LineEnding +
    '  while (i < Len) do'                                                                   + LineEnding +
    '  begin'                                                                                + LineEnding +
    '    Src := p[i * ElSize];'                                                              + LineEnding +
    ''                                                                                       + LineEnding +
    '    j := i + 1;'                                                                        + LineEnding +
    '    while (j < Len) do'                                                                 + LineEnding +
    '    begin'                                                                              + LineEnding +
    '      if Equals(p[i * ElSize]^, p[j * ElSize]^) then'                                   + LineEnding +
    '      begin'                                                                            + LineEnding +
    '        if (Pointer(Copy) = nil) then'                                                  + LineEnding +
    '          Move(Src^, p[j * ElSize]^, ElSize)'                                           + LineEnding +
    '        else'                                                                           + LineEnding +
    '          Copy(Src^, p[j * ElSize]^);'                                                  + LineEnding +
    ''                                                                                       + LineEnding +
    '        Dec(Len);'                                                                      + LineEnding +
    '        Dec(j);'                                                                        + LineEnding +
    '      end;'                                                                             + LineEnding +
    ''                                                                                       + LineEnding +
    '      Inc(j);'                                                                          + LineEnding +
    '    end;'                                                                               + LineEnding +
    ''                                                                                       + LineEnding +
    '    Inc(i);'                                                                            + LineEnding +
    '  end;'                                                                                 + LineEnding +
    ''                                                                                       + LineEnding +
    '  _ArraySetLength(p, Len, ElSize, Dispose, nil);'                                       + LineEnding +
    'end;';

  _LapeArrayMode: lpString =
    'function _ArrayMode(p: Pointer; ElSize, Len: SizeInt;'            + LineEnding +
    '                    Equals: _LapeEqualsFunc): SizeInt; overload;' + LineEnding +
    'var'                                                              + LineEnding +
    '  i, Hits, Best, Index: SizeInt;'                                 + LineEnding +
    '  Cur: Pointer;'                                                  + LineEnding +
    'begin'                                                            + LineEnding +
    '  Result := 0;'                                                   + LineEnding +
    '  Index := 0;'                                                    + LineEnding +
    '  Hits := 1;'                                                     + LineEnding +
    ''                                                                 + LineEnding +
    '  Cur := p;'                                                      + LineEnding +
    '  Inc(p, ElSize);'                                                + LineEnding +
    ''                                                                 + LineEnding +
    '  for i := 1 to Len - 1 do'                                       + LineEnding +
    '  begin'                                                          + LineEnding +
    '    if not Equals(p^, Cur^) then'                                 + LineEnding +
    '    begin'                                                        + LineEnding +
    '      if (Hits > Best) then'                                      + LineEnding +
    '      begin'                                                      + LineEnding +
    '        Best := Hits;'                                            + LineEnding +
    '        Result := Index;'                                         + LineEnding +
    '      end;'                                                       + LineEnding +
    ''                                                                 + LineEnding +
    '      Index := i;'                                                + LineEnding +
    '      Hits := 0;'                                                 + LineEnding +
    '      Cur := p;'                                                  + LineEnding +
    '    end;'                                                         + LineEnding +
    ''                                                                 + LineEnding +
    '    Inc(Hits);'                                                   + LineEnding +
    '    Inc(p, ElSize);'                                              + LineEnding +
    '  end;'                                                           + LineEnding +
    ''                                                                 + LineEnding +
    '  if (Hits > Best) then'                                          + LineEnding +
    '    Result := Index;'                                             + LineEnding +
    'end;';

  _LapeArrayMinMax: lpString =
    'function _ArrayMinMax(p: Pointer; ElSize, Start, Len: SizeInt;' + LineEnding +
    '                      Func: _LapeEqualsFunc): SizeInt;'         + LineEnding +
    'var'                                                            + LineEnding +
    '  i: SizeInt;'                                                  + LineEnding +
    '  Cur: Pointer;'                                                + LineEnding +
    'begin'                                                          + LineEnding +
    '  Result := 0;'                                                 + LineEnding +
    ''                                                               + LineEnding +
    '  Cur := p;'                                                    + LineEnding +
    '  Inc(p, ElSize);'                                              + LineEnding +
    ''                                                               + LineEnding +
    '  for i := 1 to Len - 1 do'                                     + LineEnding +
    '  begin'                                                        + LineEnding +
    '    if Func(p^, Cur^) then'                                     + LineEnding +
    '    begin'                                                      + LineEnding +
    '      Cur := p;'                                                + LineEnding +
    '      Result := i;'                                             + LineEnding +
    '    end;'                                                       + LineEnding +
    ''                                                               + LineEnding +
    '    Inc(p, ElSize);'                                            + LineEnding +
    '  end;'                                                         + LineEnding +
    ''                                                               + LineEnding +
    '  Inc(Result, Start);'                                          + LineEnding +
    'end;';

implementation

uses
  lpparser, lpvartypes_array, lpmessages, lpinternalmethods;

function TLapeTree_InternalMethod_SortWeighted.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, ArrayPointerVar, LengthVar, TempVar, WeightsVar: TResVar;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TempVar := NullResVar;

  if (not FParams[0].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);
  if (not (ArrayVar.VarType is TLapeType_DynArray)) then
    LapeException(lpeExpectedArray, DocPos);

  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;

  if (ArrayVar.VarType.BaseType = ltDynArray) then
  begin
    ArrayPointerVar := ArrayVar;
    ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);
  end else
    ArrayPointerVar := ArrayVar.VarType.Eval(op_Addr, TempVar, ArrayVar, NullResVar, [], Offset, @_DocPos); // ltStaticArray

  with TLapeTree_InternalMethod_Copy.Create(Self) do
  try
    addParam(Self.Params[1]);

    WeightsVar := Compile(Offset);
  finally
    Free();
  end;

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // if Length(ArrayVar) > 0 then
  //   _SortWeighted(A: Pointer; ElSize, Len: SizeInt; Weights: array of Int32; SortUp: EvalBool)
  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_Invoke.Create('_SortWeighted', Self);
    with TLapeTree_Invoke(Body) do
    begin
      addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
      addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
      addParam(TLapeTree_ResVar.Create(LengthVar.IncLock(), Self));
      addParam(TLapeTree_ResVar.Create(WeightsVar.IncLock(), Self));
      addParam(Self.Params[1]);
    end;

    Compile(Offset).Spill(1);
  finally
    ArrayVar.Spill(1);
    ArrayPointerVar.Spill(1);
    LengthVar.Spill(1);
    WeightsVar.Spill(1);

    Free();
  end;
end;

function TLapeTree_InternalMethod_Sort.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, ArrayPointerVar, CompareVar, LengthVar, TempVar: TResVar;
  ArrayType, ResultType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TempVar := NullResVar;

  // Check if user defined `_Sort` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_Sort', Result, Offset) then
    Exit;

  if (not (FParams.Count in [1..3])) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  if (FParams.Count = 3) then
    with TLapeTree_InternalMethod_SortWeighted.Create(Self) do
    try
      while Self.Params.Count > 0 do
        addParam(Self.Params[0]);

      Result := Compile(Offset);
      Exit;
    finally
      Free();
    end;

  if (not FParams[0].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);

  if (ArrayVar.VarType.BaseType = ltDynArray) then
  begin
    ArrayPointerVar := ArrayVar;
    ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);
  end else
    ArrayPointerVar := ArrayVar.VarType.Eval(op_Addr, TempVar, ArrayVar, NullResVar, [], Offset, @_DocPos); // ltStaticArray

  ResultType := FCompiler.getBaseType(ltInt32);
  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;

  if (FParams.Count > 1) then
  begin
    if (not FParams[1].CompileToTempVar(Offset, CompareVar)) then
      LapeException(lpeInvalidEvaluation, DocPos);
    if (not (CompareVar.VarType is TLapeType_Method)) then
      LapeException(lpeInvalidCompareMethod, DocPos);

    with TLapeType_Method(CompareVar.VarType) do
    begin
      if not (
          (Params.Count = 2) and
          (ResultType.Equals(Res)) and
          (ArrayType.Equals(Params[0].VarType)) and
          (ArrayType.Equals(Params[1].VarType)) and
          (Params[0].ParType = lptConstRef) and
          (Params[1].ParType = lptConstRef)
      ) then
        LapeException(lpeInvalidCompareMethod, DocPos);
    end;

    CompareVar.VarType := FCompiler.getGlobalType('_LapeCompareFunc');
  end else
  begin
    RequireOperators(FCompiler, [op_cmp_LessThan, op_cmp_GreaterThan], ArrayType, DocPos);

    CompareVar := GetMagicMethodOrNil(FCompiler, '_Compare', [ArrayType, ArrayType], ResultType);
  end;

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // if Length(ArrayVar) > 0 then
  //   _Sort(p: Pointer; ElSize, Len: SizeInt; Compare: function(constref A, B): Int32);
  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_Invoke.Create('_Sort', Self);
    with TLapeTree_Invoke(Body) do
    begin
      addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
      addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
      addParam(TLapeTree_ResVar.Create(LengthVar.IncLock(), Self));
      addParam(TLapeTree_ResVar.Create(CompareVar.IncLock(), Self));
    end;

    Compile(Offset);
  finally
    ArrayVar.Spill(1);
    ArrayPointerVar.Spill(1);
    LengthVar.Spill(1);
    CompareVar.Spill(1);

    Free();
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
    addParam(TLapeTree_ResVar.Create(Result.IncLock(), Self));
    while (Self.FParams.Count > 0) do
      addParam(Self.FParams[0]);

    Compile(Offset).Spill(1);
  finally
    Free();
  end;

  Result.isConstant := True;
end;

function TLapeTree_InternalMethod_Reverse.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, ArrayPointer: TResVar;
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
        ltStaticArray:
          with TLapeType_StaticArray(ArrayVar.VarType) do
            addParam(TLapeTree_Integer.Create(Range.Hi - Range.Lo, Self));

        ltDynArray:
          addParam(TLapeTree_Integer.Create(-1, Self));
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
  ArrayVar: TResVar;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (resType() = nil) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);
  if (not FParams[0].CompileToTempVar(Offset, ArrayVar)) then
    LapeException(lpeInvalidEvaluation, DocPos);

  // Check if user defined `_Unique` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_Unique', Result, Offset) then
    Exit;

  ArrayType := TLapeType_DynArray(ArrayVar.VarType).PType;
  with TLapeTree_InternalMethod_Copy.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    Result := Compile(Offset);
    Result.isConstant := False;
  finally
    Free();
  end;

  ArrayVar := Result;
  ArrayVar.VarType := FCompiler.getBaseType(ltPointer);

  with TLapeTree_Invoke.Create('_Unique', Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));
    addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Equals', [ArrayType, ArrayType], FCompiler.getBaseType(ltEvalBool)), Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Dispose', [ArrayType]), Self));
    addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Assign', [ArrayType]), Self));

    Compile(Offset).Spill(1);
  finally
    ArrayVar.DecLock();

    Free();
  end;

  Result.isConstant := True;
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
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayType := resType();
  if (ArrayType = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  RequireOperators(FCompiler, [op_cmp_Equal, op_cmp_LessThan, op_cmp_GreaterThan], ArrayType, DocPos);

  Result := _ResVar.New(FCompiler.getTempVar(ArrayType));
  SortedArrayVar := _ResVar.New(FCompiler.getTempVar(ArrayVar.VarType));

  FCompiler.VarToDefault(Result, Offset);

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // // if Length(ArrayVar) > 0 then
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
        Left := TLapeTree_ResVar.Create(SortedArrayVar.IncLock(), Self);
        Right := TLapeTree_InternalMethod_Sorted.Create(Self);
        with TLapeTree_InternalMethod_Sorted(Right) do
          addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));
      end;

      // Result := SortedArrayVar[_ArrayMode(...)]
      addStatement(TLapeTree_Operator.Create(op_Assign, Self));
      with TLapeTree_Operator(Statements[1]) do
      begin
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        Right := TLapeTree_Operator.Create(op_Index, Self);
        with TLapeTree_Operator(Right) do
        begin
          Left := TLapeTree_ResVar.Create(SortedArrayVar.IncLock(), Self);
          Right := TLapeTree_Invoke.Create('_ArrayMode', Self);

          with TLapeTree_Invoke(Right) do
          begin
            SortedArrayVar.VarType := FCompiler.getBaseType(ltPointer);

            addParam(TLapeTree_ResVar.Create(SortedArrayVar.IncLock(), Self));
            addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
            addParam(TLapeTree_ResVar.Create(LengthVar.IncLock(), Self));
            addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_Equals', [ArrayType, ArrayType], FCompiler.getBaseType(ltEvalBool)), Self));
          end;
        end;
      end;
    end;

    Compile(Offset);
  finally
    LengthVar.Spill(1);
    SortedArrayVar.Spill(1);

    Free();
  end;

  Result.isConstant := True;
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
  ArrayVar, ArrayPointerVar, LengthVar, TempVar: TResVar;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TempVar := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayType := resType();
  if (ArrayType = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  // Check if user defined `_ArrayMin` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArrayMin', Result, Offset) then
    Exit;

  RequireOperators(FCompiler, [op_cmp_LessThan], ArrayType, DocPos);

  if (ArrayVar.VarType.BaseType = ltDynArray) then
  begin
    ArrayPointerVar := ArrayVar;
    ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);
  end else
    ArrayPointerVar := ArrayVar.VarType.Eval(op_Addr, TempVar, ArrayVar, NullResVar, [], Offset, @_DocPos); // ltStaticArray

  Result := _ResVar.New(FCompiler.getTempVar(ArrayType));

  FCompiler.VarToDefault(Result, Offset);

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // if Length(ArrayVar) > 0 then
  //   Result := ArrayVar[_ArrayMinMax(...)]
  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_Operator.Create(op_Assign, Self);
    with TLapeTree_Operator(Body) do
    begin
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Right := TLapeTree_Operator.Create(op_Index, Self);
      with TLapeTree_Operator(Right) do
      begin
        Left := TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self);
        Right := TLapeTree_Invoke.Create('_ArrayMinMax', Self);

        with TLapeTree_Invoke(Right) do
        begin
          addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
          addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
          addParam(TLapeTree_GlobalVar.Create(ArrayVar.VarType.VarLo(), Self));
          addParam(TLapeTree_ResVar.Create(LengthVar.IncLock(), Self));
          addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_LessThan', [ArrayType, ArrayType], FCompiler.getBaseType(ltEvalBool)), Self));
        end;
      end;
    end;

    Compile(Offset);
  finally
    if (ArrayVar.VarType.BaseType = ltStaticArray)then
      ArrayPointerVar.Spill(1);
    LengthVar.Spill(1);

    Free();
  end;

  Result.isConstant := True;
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
  ArrayVar, ArrayPointerVar, LengthVar, TempVar: TResVar;
  ArrayType: TLapeType;
begin
  Result := NullResVar;
  Dest := NullResVar;
  TempVar := NullResVar;

  if (FParams.Count <> 1) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos);

  ArrayType := resType();
  if (ArrayType = nil) or (not FParams[0].CompileToTempVar(Offset, ArrayVar, 1)) then
    LapeException(lpeExpectedArray, DocPos);

  // Check if user defined `_ArrayMax` exists. Useful for providing a native method
  if InvokeMagicMethod(Self, '_ArrayMax', Result, Offset) then
    Exit;

  RequireOperators(FCompiler, [op_cmp_GreaterThan], ArrayType, DocPos);

  if (ArrayVar.VarType.BaseType = ltDynArray) then
  begin
    ArrayPointerVar := ArrayVar;
    ArrayPointerVar.VarType := FCompiler.getBaseType(ltPointer);
  end else
    ArrayPointerVar := ArrayVar.VarType.Eval(op_Addr, TempVar, ArrayVar, NullResVar, [], Offset, @_DocPos); // ltStaticArray

  Result := _ResVar.New(FCompiler.getTempVar(ArrayType));

  FCompiler.VarToDefault(Result, Offset);

  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // if Length(ArrayVar) > 0 then
  //   Result := ArrayVar[_ArrayMinMax(...)]
  with TLapeTree_If.Create(Self) do
  try
    Condition := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
    Body := TLapeTree_Operator.Create(op_Assign, Self);
    with TLapeTree_Operator(Body) do
    begin
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
      Right := TLapeTree_Operator.Create(op_Index, Self);
      with TLapeTree_Operator(Right) do
      begin
        Left := TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self);
        Right := TLapeTree_Invoke.Create('_ArrayMinMax', Self);

        with TLapeTree_Invoke(Right) do
        begin
          addParam(TLapeTree_ResVar.Create(ArrayPointerVar.IncLock(), Self));
          addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
          addParam(TLapeTree_GlobalVar.Create(ArrayVar.VarType.VarLo(), Self));
          addParam(TLapeTree_ResVar.Create(LengthVar.IncLock(), Self));
          addParam(TLapeTree_ResVar.Create(GetMagicMethodOrNil(FCompiler, '_GreaterThan', [ArrayType, ArrayType], FCompiler.getBaseType(ltEvalBool)), Self));
        end;
      end;
    end;

    Compile(Offset);
  finally
    if (ArrayVar.VarType.BaseType = ltStaticArray)then
      ArrayPointerVar.Spill(1);
    LengthVar.Spill(1);

    Free();
  end;

  Result.isConstant := True;
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
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
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
      Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);

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
  MeanVar := _ResVar.New(FCompiler.getTempVar(ArrayType));

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
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
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

    Free();
  end;

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

