{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
unit lpvartypes_helper;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes;

type
  TLapeType_HelperClass = class of TLapeType_Helper;
  TLapeType_Helper = class(TLapeType_OverloadedMethod)
  protected
    function FunctionNotFound(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    procedure CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType = nil);
    procedure CreateFunctions(VarType: TLapeType); virtual; abstract;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; override;
  end;

  TLapeType_ArrayHelper_SetLength = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Length = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Contains = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Remove = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Low = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_High = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Copy = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Delete = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Insert = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_First = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Last = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Pop = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Swap = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_RandomValue = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Reverse = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Reversed = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Unique = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_IndexOf = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_IndicesOf = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Sort = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Sorted = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Clear = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Append = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Extend = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Median = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Mode = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Min = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Max = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Sum = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Mean = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Variance = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

  TLapeType_ArrayHelper_Stddev = class(TLapeType_Helper)
  protected
    procedure CreateFunctions(VarType: TLapeType); override;
  end;

implementation

uses
  lpcompiler, lpmessages,
  lpvartypes_array;

function TLapeType_Helper.FunctionNotFound(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  OnFunctionNotFound := nil;

  if (AObjectType <> nil) then
    CreateFunctions(AObjectType);
  if (FManagedDecls.Count = 0) then
    LapeException(lpeImpossible);

  Result := getMethod(AParams, AResult);
end;

procedure TLapeType_Helper.CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType);
var
  Header: TLapeType_Method;
  Method: TLapeGlobalVar;
  Param: TLapeParameter;
  i: Integer;
begin
  Assert(FCompiler is TLapeCompiler);

  with FCompiler as TLapeCompiler do
  begin
    if (not hasDeclaration(VarType, False, False)) then // var a: array of x
      addGlobalDecl(VarType);

    Header := addManagedType(TLapeType_MethodOfType.Create(FCompiler, VarType, nil, ResultType)) as TLapeType_Method;
    for i := 0 to High(ParamTypes) do
    begin
      Param.Default := nil;
      Param.ParType := lptConst;
      Param.VarType := ParamTypes[i];

      Header.addParam(Param);
    end;

    Method := addGlobalFunc(Header, '!Helper', Body).Method;
    Method.Name := Name;

    FManagedDecls.addDeclaration(Method);
  end;
end;

constructor TLapeType_Helper.Create(ACompiler: TLapeCompilerBase; AName: lpString; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, AName, ADocPos);

  FOfObject := bTrue;
  FHiddenSelf := bFalse;

  OnFunctionNotFound := @FunctionNotFound;
end;

procedure TLapeType_ArrayHelper_SetLength.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                             + LineEnding +
    '  System.SetLength(Self, Param0);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)]
  );
end;

procedure TLapeType_ArrayHelper_Length.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                            + LineEnding +
    '  Result := System.Length(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

procedure TLapeType_ArrayHelper_Contains.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                      + LineEnding +
    '  Result := System.Contains(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltEvalBool)
  );
end;

procedure TLapeType_ArrayHelper_Remove.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                    + LineEnding +
    '  Result := System.Remove(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltEvalBool)
  );
end;

procedure TLapeType_ArrayHelper_Low.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                         + LineEnding +
    '  Result := System.Low(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

procedure TLapeType_ArrayHelper_High.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                          + LineEnding +
    '  Result := System.High(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

procedure TLapeType_ArrayHelper_Copy.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                          + LineEnding +
    '  Result := System.Copy(Self, Param0, Param1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    VarType
  );

  CreateFunction(
    'begin'                                  + LineEnding +
    '  Result := System.Copy(Self, Param0);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)],
    VarType
  );

  CreateFunction(
    'begin'                          + LineEnding +
    '  Result := System.Copy(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    VarType
  );
end;

procedure TLapeType_ArrayHelper_Delete.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                  + LineEnding +
    '  System.Delete(Self, Param0, Param1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)]
  );

  CreateFunction(
    'begin'                          + LineEnding +
    '  System.Delete(Self, Param0);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)]
  );
end;

procedure TLapeType_ArrayHelper_Insert.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                  + LineEnding +
    '  System.Insert(Param0, Self, Param1);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType, FCompiler.getBaseType(ltSizeInt)]
  );
end;

procedure TLapeType_ArrayHelper_First.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                               + LineEnding +
    '  Result := Self[System.Low(Self)];' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Last.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                + LineEnding +
    '  Result := Self[System.High(Self)];' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Pop.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'var Index: Integer := System.Length(Self);' + LineEnding +
    'begin'                                      + LineEnding +
    '  Result := Self[Index];'                   + LineEnding +
    '  System.SetLength(Self, Index - 1);'       + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );

  CreateFunction(
    'begin'                             + LineEnding +
    '  Result := Self[Param0];'         + LineEnding +
    '  System.Delete(Self, Param0, 1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Swap.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                      + LineEnding +
    '  System.Swap(Self[Param0], Self[Param1]);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)]
  );
end;

procedure TLapeType_ArrayHelper_RandomValue.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                                                 + LineEnding +
    '  Result := Self[System.Random(System.Low(Self), System.High(Self))];' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Reverse.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                   + LineEnding +
    '  System.Reverse(Self);' + LineEnding +
    'end;',
    VarType,
    []
  );
end;

procedure TLapeType_ArrayHelper_Reversed.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.Reversed(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    VarType
  );
end;

procedure TLapeType_ArrayHelper_Unique.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                            + LineEnding +
    '  Result := System.Unique(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    VarType
  );
end;

procedure TLapeType_ArrayHelper_IndexOf.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                     + LineEnding +
    '  Result := System.IndexOf(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltInt32)
  );
end;

procedure TLapeType_ArrayHelper_IndicesOf.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                                       + LineEnding +
    '  Result := System.IndicesOf(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getIntegerArray()
  );
end;

procedure TLapeType_ArrayHelper_Sort.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                + LineEnding +
    '  System.Sort(Self);' + LineEnding +
    'end;',
    VarType,
    []
  );

  CreateFunction(
    'begin'                         + LineEnding +
    '  System.Sort(Self, @Param0);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))]
    );

  CreateFunction(
    'begin'                                + LineEnding +
    '  System.Sort(Self, Param0, Param1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getIntegerArray(), FCompiler.getBaseType(ltEvalBool)]
  );

  CreateFunction(
    'begin'                                + LineEnding +
    '  System.Sort(Self, Param0, Param1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getFloatArray(), FCompiler.getBaseType(ltEvalBool)]
  );
end;

procedure TLapeType_ArrayHelper_Sorted.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                            + LineEnding +
    '  Result := System.Sorted(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    VarType
  );

  CreateFunction(
    'begin'                                     + LineEnding +
    '  Result := System.Sorted(Self, @Param0);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))],
    VarType
  );

  CreateFunction(
    'begin'                                            + LineEnding +
    '  Result := System.Sorted(Self, Param0, Param1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getIntegerArray(), FCompiler.getBaseType(ltEvalBool)],
    VarType
  );

  CreateFunction(
    'begin'                                            + LineEnding +
    '  Result := System.Sorted(Self, Param0, Param1);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getFloatArray(), FCompiler.getBaseType(ltEvalBool)],
    VarType
  );
end;

procedure TLapeType_ArrayHelper_Clear.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'         + LineEnding +
    '  Self := [];' + LineEnding +
    'end;',
    VarType,
    []
  );
end;

procedure TLapeType_ArrayHelper_Append.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                    + LineEnding +
    '  Self := Self + Param0;' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType]
  );
end;

procedure TLapeType_ArrayHelper_Extend.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                    + LineEnding +
    '  Self := Self + Param0;' + LineEnding +
    'end;',
    VarType,
    [VarType]
  );
end;

procedure TLapeType_ArrayHelper_Median.CreateFunctions(VarType: TLapeType);
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeIntegerTypes + LapeRealTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  CreateFunction(
    'begin'                                 + LineEnding +
    '  Result := System.ArrayMedian(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

procedure TLapeType_ArrayHelper_Mode.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                               + LineEnding +
    '  Result := System.ArrayMode(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Min.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.ArrayMin(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Max.CreateFunctions(VarType: TLapeType);
begin
  CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.ArrayMax(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

procedure TLapeType_ArrayHelper_Sum.CreateFunctions(VarType: TLapeType);
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltInt64)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.ArraySum(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

procedure TLapeType_ArrayHelper_Mean.CreateFunctions(VarType: TLapeType);
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  CreateFunction(
    'begin'                               + LineEnding +
    '  Result := System.ArrayMean(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

procedure TLapeType_ArrayHelper_Variance.CreateFunctions(VarType: TLapeType);
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  CreateFunction(
    'begin'                                   + LineEnding +
    '  Result := System.ArrayVariance(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

procedure TLapeType_ArrayHelper_Stddev.CreateFunctions(VarType: TLapeType);
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  CreateFunction(
    'begin'                                + LineEnding +
    '  Result := System.ArrayStdev(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

end.

