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

const
  _LapeArrayMode: lpString =
    'function _ArrayMode(p: Pointer; ElSize, Len: SizeInt;'         + LineEnding +
    '                    Equals: _TEqualsFunc): SizeInt; overload;' + LineEnding +
    'var'                                                           + LineEnding +
    '  i, Hits, Best, Index: SizeInt;'                              + LineEnding +
    '  Cur: Pointer;'                                               + LineEnding +
    'begin'                                                         + LineEnding +
    '  Result := 0;'                                                + LineEnding +
    '  Index := 0;'                                                 + LineEnding +
    '  Hits := 1;'                                                  + LineEnding +
    ''                                                              + LineEnding +
    '  Cur := p;'                                                   + LineEnding +
    '  Inc(p, ElSize);'                                             + LineEnding +
    ''                                                              + LineEnding +
    '  for i := 1 to Len - 1 do'                                    + LineEnding +
    '  begin'                                                       + LineEnding +
    '    if not Equals(p^, Cur^) then'                              + LineEnding +
    '    begin'                                                     + LineEnding +
    '      if (Hits > Best) then'                                   + LineEnding +
    '      begin'                                                   + LineEnding +
    '        Best := Hits;'                                         + LineEnding +
    '        Result := Index;'                                      + LineEnding +
    '      end;'                                                    + LineEnding +
    ''                                                              + LineEnding +
    '      Index := i;'                                             + LineEnding +
    '      Hits := 0;'                                              + LineEnding +
    '      Cur := p;'                                               + LineEnding +
    '    end;'                                                      + LineEnding +
    ''                                                              + LineEnding +
    '    Inc(Hits);'                                                + LineEnding +
    '    Inc(p, ElSize);'                                           + LineEnding +
    '  end;'                                                        + LineEnding +
    ''                                                              + LineEnding +
    '  if (Hits > Best) then'                                       + LineEnding +
    '    Result := Index;'                                          + LineEnding +
    'end;';

implementation

uses
  lpparser, lpvartypes_array, lpmessages;

function TLapeTree_InternalMethod_ArrayMedian.resType: TLapeType;
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

function TLapeTree_InternalMethod_ArrayMedian.Compile(var Offset: Integer): TResVar;
var
  ArrayVar, LengthVar: TResVar;
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

  // Len := Length(Arr);
  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  Result := _ResVar.New(FCompiler.getTempVar(ArrayType));

  // if Len > 0 then
  //   Result := Sorted(Arr)[Len div 2]
  // else
  //   Result := 0;
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
        Left := TLapeTree_InternalMethod_Sorted.Create(Self);
        with TLapeTree_InternalMethod_Sorted(Left) do
          addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

        Right := TLapeTree_Operator.Create(op_DIV, Self);
        with TLapeTree_Operator(Right) do
        begin
          Left := TLapeTree_ResVar.Create(LengthVar.IncLock(), Self);
          Right := TLapeTree_Integer.Create(2, Self);
        end;
      end;
    end;

    if (not (lcoAlwaysInitialize in FCompilerOptions)) then
    begin
      ElseBody := TLapeTree_Operator.Create(op_Assign, Self);
      with TLapeTree_Operator(ElseBody) do
      begin
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        Right := TLapeTree_Integer.Create(0, Self);
      end;
    end;

    Compile(Offset);
  finally
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
  ArrayVar, ArraySortedVar, LengthVar, EqualsVar: TResVar;
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

  // Len := Length(Arr)
  with TLapeTree_InternalMethod_Length.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    LengthVar := FoldConstants(False).Compile(Offset);
  finally
    Free();
  end;

  // Arr := Sorted(Arr)
  with TLapeTree_InternalMethod_Sorted.Create(Self) do
  try
    addParam(TLapeTree_ResVar.Create(ArrayVar.IncLock(), Self));

    ArraySortedVar := Compile(Offset);
  finally
    Free();
  end;

  Result := _ResVar.New(FCompiler.getTempVar(ArrayType));
  EqualsVar := GetMagicMethodOrNil(FCompiler, '_Equals', [ArrayType, ArrayType], FCompiler.getBaseType(ltEvalBool));

  // if Len > 0 then
  //   Result := Arr[_ArrayMode(...)]
  // else
  //   Result := 0;
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
        Left := TLapeTree_ResVar.Create(ArraySortedVar.IncLock(), Self);
        Right := TLapeTree_Invoke.Create('_ArrayMode', Self);

        with TLapeTree_Invoke(Right) do
        begin
          ArraySortedVar.VarType := FCompiler.getBaseType(ltPointer);

          addParam(TLapeTree_ResVar.Create(ArraySortedVar.IncLock(), Self));
          addParam(TLapeTree_Integer.Create(ArrayType.Size, Self));
          addParam(TLapeTree_ResVar.Create(LengthVar.IncLock(), Self));
          addParam(TLapeTree_ResVar.Create(EqualsVar.IncLock(), Self));
        end;
      end;
    end;

    if (not (lcoAlwaysInitialize in FCompilerOptions)) then
    begin
      ElseBody := TLapeTree_Operator.Create(op_Assign, Self);
      with TLapeTree_Operator(ElseBody) do
      begin
        Left := TLapeTree_ResVar.Create(Result.IncLock(), Self);
        Right := TLapeTree_Integer.Create(0, Self);
      end;
    end;

    Compile(Offset);
  finally
    ArraySortedVar.Spill(1);
    LengthVar.Spill(1);

    Free();
  end;

  Result.isConstant := True;
end;

end.

