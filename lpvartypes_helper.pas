{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  A helper type is a type method that is generated when required.
  Side effect is these can currently not be overriden (or overloaded with the same name).
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
    procedure addMethod(AMethod: TLapeGlobalVar; DoOverride: Boolean = False); override;
    function overrideMethod(AMethod: TLapeGlobalVar): TLapeGlobalVar; override;

    function getMethodIndex(AType: TLapeType_Method): Integer; override;
    function getMethodIndex(AParams: TLapeTypeArray; AResult: TLapeType=nil; AObjectType: TLapeType=nil): Integer; override;

    function getFunc(ObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; virtual; abstract;
  public
    function CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType = nil): TLapeGlobalVar;
  end;

  TLapeType_HelperProperty = class(TLapeType_Helper)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString=''; ADocPos: PDocPos=nil); override;
  end;

  // Low,High,Length,Pop,First,Last are performed inline in DynArray.Eval to reduce overhead and wont be used like the others
  TLapeType_ArrayHelper_Low = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_High = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Length = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_First = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Last = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Pop = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Empty = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Clear = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_SetLength = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Contains = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Copy = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_CopyRange = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Remove = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Delete = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_DeleteRange = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Insert = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Swap = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Random = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Reverse = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Reversed = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Unique = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_IndexOf = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_IndicesOf = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Sort = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Sorted = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Median = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Mode = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Min = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Max = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Sum = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Mean = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Variance = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Stdev = class(TLapeType_HelperProperty)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Slice = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Difference = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_SymDifference = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Intersection = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Equals = class(TLapeType_Helper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  procedure LapeCreateArrayHelpers(Compiler: TLapeCompilerBase);
  procedure LapeCreateStaticArrayHelpers(Compiler: TLapeCompilerBase);

implementation

uses
  lpcompiler, lpparser, lptree,
  lpvartypes_array, lpmessages;

procedure TLapeType_Helper.addMethod(AMethod: TLapeGlobalVar; DoOverride: Boolean);
begin
  LapeException(lpeImpossible);
end;

function TLapeType_Helper.overrideMethod(AMethod: TLapeGlobalVar): TLapeGlobalVar;
begin
  LapeException(lpeImpossible);
end;

function TLapeType_Helper.getMethodIndex(AType: TLapeType_Method): Integer;
var
  ParamTypes: TLapeTypeArray;
  i: Integer;
begin
  if (AType is TLapeType_MethodOfType) then
  begin
    SetLength(ParamTypes, AType.Params.Count);
    for i := 0 to AType.Params.Count - 1 do
      ParamTypes[i] := AType.Params[i].VarType;
    Result := getMethodIndex(ParamTypes, AType.Res, TLapeType_MethodOfType(AType).ObjectType);
  end else
    Result := -1;
end;

function TLapeType_Helper.getMethodIndex(AParams: TLapeTypeArray; AResult: TLapeType; AObjectType: TLapeType): Integer;

  function EqualMethod(Method: TLapeType_MethodOfType): Boolean;

    function EqualTypes(Left, Right: TLapeType): Boolean;
    begin
      Result := (Left = Right) or ((Left <> nil) and Left.Equals(Right, False));
    end;

  var
    i: Integer;
  begin
    if (not EqualTypes(AObjectType, Method.ObjectType)) or (Length(AParams) <> Method.Params.Count) then
      Exit(False);
    for i := 0 to High(AParams) do
      if (not EqualTypes(AParams[i], Method.Params[i].VarType)) then
        Exit(False);

    Result := True;
  end;

var
  i: Integer;
begin
  for i := 0 to FManagedDecls.Count - 1 do
  begin
    Assert(TLapeGlobalVar(FManagedDecls[i]).VarType is TLapeType_MethodOfType);
    if EqualMethod(TLapeType_MethodOfType(TLapeGlobalVar(FManagedDecls[i]).VarType)) then
      Exit(i);
  end;
  Result := FManagedDecls.IndexOf(getFunc(AObjectType, AParams, AResult));
end;

function TLapeType_Helper.CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Param: TLapeParameter;
  i: Integer;
  Method: TLapeTree_Method;
  p: TDocPos;
begin
  Assert(FCompiler is TLapeCompiler);

  with FCompiler as TLapeCompiler do
  begin
    if (not hasDeclaration(VarType, False, False)) then // for `var a: array of x` otherwise "parent declaration out of scope"
      addLocalDecl(VarType);

    Header := addManagedType(TLapeType_MethodOfType.Create(FCompiler, VarType, nil, ResultType)) as TLapeType_Method;
    for i := 0 to High(ParamTypes) do
    begin
      Param.Default := nil;
      Param.ParType := lptConst;
      Param.VarType := ParamTypes[i];

      Header.addParam(Param);
    end;

    p := DocPos;
    Method := addGlobalFunc(Header, '!Helper',
      '{$X+}'                             + LineEnding +
      'begin'                             + LineEnding +
      '  try'                             + LineEnding +
      '    ' + Body                       + LineEnding +
      '  except'                          + LineEnding +
      '    raise at GetCallerLocation();' + LineEnding +
      '  end;'                            + LineEnding +
      'end;',
      @p);

    Result := Method.Method;
    Result.VarType.Name := FName;

    FManagedDecls.addDeclaration(Result);
  end;
end;

constructor TLapeType_HelperProperty.Create(ACompiler: TLapeCompilerBase; AName: lpString; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, AName, ADocPos);

  MethodDef := mdProperty;
end;

function TLapeType_ArrayHelper_SetLength.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'System.SetLength(Self, Param0);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeType_ArrayHelper_Low.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);

  //Result := CreateFunction(
  //  'Result := System.Low(Self);',
  //  VarType,
  //  [],
  //  FCompiler.getBaseType(ltSizeInt)
  //);
end;

function TLapeType_ArrayHelper_High.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);

  //Result := CreateFunction(
  //  'Result := System.High(Self);',
  //  VarType,
  //  [],
  //  FCompiler.getBaseType(ltSizeInt)
  //);
end;

function TLapeType_ArrayHelper_Length.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);

  //Result := CreateFunction(
  //  'Result := System.Length(Self);',
  //  VarType,
  //  [],
  //  FCompiler.getBaseType(ltSizeInt)
  //);
end;

function TLapeType_ArrayHelper_First.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);

  //Result := CreateFunction(
  //  'Result := Self[System.Low(Self)];',
  //  VarType,
  //  [],
  //  TLapeType_DynArray(VarType).PType
  //);
end;

function TLapeType_ArrayHelper_Last.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);

  //Result := CreateFunction(
  //  'Result := Self[System.High(Self)];',
  //  VarType,
  //  [],
  //  TLapeType_DynArray(VarType).PType
  //);
end;

function TLapeType_ArrayHelper_Pop.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);

  //if VarType.BaseType in LapeStringTypes then
  //begin
  //  Result := CreateFunction(
  //    'var Index: Integer := System.Length(Self);' + LineEnding +
  //    'Result := Self[Index];'                   + LineEnding +
  //    'System.SetLength(Self, Index - 1);',
  //    VarType,
  //    [],
  //    TLapeType_DynArray(VarType).PType
  //  );
  //end else
  //begin
  //  Result := CreateFunction(
  //    'var Index: Integer := System.High(Self);' + LineEnding +
  //    'Result := Self[Index];'                 + LineEnding +
  //    'System.SetLength(Self, Index);',
  //    VarType,
  //    [],
  //    TLapeType_DynArray(VarType).PType
  //  );
  //end;
end;

function TLapeType_ArrayHelper_Empty.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotOverload);
end;

function TLapeType_ArrayHelper_Clear.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Self := [];',
    VarType,
    []
  );
end;

function TLapeType_ArrayHelper_Contains.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.Contains(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltEvalBool)
  );
end;

function TLapeType_ArrayHelper_Copy.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  Result := nil;

  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Copy(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_CopyRange.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Copy(Self, Param0, (Param1 - Param0) + 1);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    ResType
  );
end;

function TLapeType_ArrayHelper_Remove.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.Remove(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Delete.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := Self[Param0];'          + LineEnding +
    'System.Delete(Self, Param0, 1);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_DeleteRange.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := Self[Param0];'                            + LineEnding +
    'System.Delete(Self, Param0, (Param1 - Param0) + 1);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Insert.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;
  if Length(AParams) = 0 then
    Exit;

  Result := CreateFunction(
    'System.Insert(Param0, Self, Param1);',
    VarType,
    [AParams[0], FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeType_ArrayHelper_Swap.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'System.Swap(Self[Param0], Self[Param1]);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeType_ArrayHelper_Random.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := Self[System.Random(System.Low(Self), System.High(Self))];',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Reverse.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'System.Reverse(Self);',
    VarType,
    []
  );
end;

function TLapeType_ArrayHelper_Reversed.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Reversed(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Unique.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Unique(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_IndexOf.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.IndexOf(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltInt32)
  );
end;

function TLapeType_ArrayHelper_IndicesOf.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.IndicesOf(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getGlobalType('TIntegerArray')
  );
end;

function TLapeType_ArrayHelper_Sort.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  WeightType: TLapeType;
begin
  Result := nil;

  case Length(AParams) of
    0:
      begin
        Result := CreateFunction(
          'System.Sort(Self);',
          VarType,
          []
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'System.Sort(Self, @Param0);',
          VarType,
          [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))]
        );
      end;

   2:
     if (AParams[0] is TLapeType_DynArray) and TLapeType_DynArray(AParams[0]).HasType then
     begin
       WeightType := TLapeType_DynArray(AParams[0]).PType;

       Result := CreateFunction(
         'System.Sort(Self, Param0, Param1);',
         VarType,
         [FCompiler.addManagedType(TLapeType_DynArray.Create(WeightType, FCompiler)), FCompiler.getBaseType(ltEvalBool)]
       );
     end;
  end;
end;

function TLapeType_ArrayHelper_Sorted.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType, WeightType: TLapeType;
begin
  Result := nil;

  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  case Length(AParams) of
    0:
      begin
        Result := CreateFunction(
          'Result := System.Sorted(Self);',
          VarType,
          [],
          ResType
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'Result := System.Sorted(Self, @Param0);',
          VarType,
          [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))],
          ResType
        );
      end;

    2:
      if (AParams[0] is TLapeType_DynArray) and TLapeType_DynArray(AParams[0]).HasType then
      begin
        WeightType := TLapeType_DynArray(AParams[0]).PType;

        Result := CreateFunction(
            'Result := System.Sorted(Self, Param0, Param1);',
            VarType,
            [FCompiler.addManagedType(TLapeType_DynArray.Create(WeightType, FCompiler)), FCompiler.getBaseType(ltEvalBool)],
            ResType
          );
      end;
  end;
end;

function TLapeType_ArrayHelper_Median.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeIntegerTypes + LapeRealTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'Result := System.ArrayMedian(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Mode.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMode(Self);',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Min.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMin(Self);',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Max.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMax(Self);',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Sum.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
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

  Result := CreateFunction(
    'Result := System.ArraySum(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Mean.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'Result := System.ArrayMean(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Variance.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'Result := System.ArrayVariance(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Stdev.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'Result := System.ArrayStdev(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Slice.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Slice(Self, Param0, Param1, Param2);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    ResType
  );
end;

function TLapeType_ArrayHelper_Difference.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayDifference(Self, Param0);',
    VarType,
    [VarType],
    VarType
  );
end;

function TLapeType_ArrayHelper_SymDifference.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArraySymDifference(Self, Param0);',
    VarType,
    [VarType],
    VarType
  );
end;

function TLapeType_ArrayHelper_Intersection.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayIntersection(Self, Param0);',
    VarType,
    [VarType],
    VarType
  );
end;

function TLapeType_ArrayHelper_Equals.getFunc(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayEquals(Self, Param0);',
    VarType,
    [VarType],
    FCompiler.getBaseType(ltBoolean)
  );
end;

procedure LapeCreateArrayHelpers(Compiler: TLapeCompilerBase);
var
  Typ: TLapeType;

  procedure Add(Helper: TLapeType_HelperClass; Name: lpString);
  begin
    Typ.ManagedDeclarations.addDeclaration(
      TLapeType_Helper(Compiler.addManagedType(Helper.Create(Compiler, Name))).NewGlobalVar(Name)
    );
  end;

begin
  Typ := TLapeType.Create(ltUnknown, Compiler);
  Typ.Name := '!arrayhelpers';

  Add(TLapeType_ArrayHelper_Low, 'Low');
  Add(TLapeType_ArrayHelper_High, 'High');
  Add(TLapeType_ArrayHelper_Length, 'Length');
  Add(TLapeType_ArrayHelper_First, 'First');
  Add(TLapeType_ArrayHelper_Last, 'Last');
  Add(TLapeType_ArrayHelper_Contains, 'Contains');
  Add(TLapeType_ArrayHelper_Swap, 'Swap');
  Add(TLapeType_ArrayHelper_Unique, 'Unique');
  Add(TLapeType_ArrayHelper_IndexOf, 'IndexOf');
  Add(TLapeType_ArrayHelper_IndicesOf, 'IndicesOf');
  Add(TLapeType_ArrayHelper_Sorted, 'Sorted');
  Add(TLapeType_ArrayHelper_Copy, 'Copy');
  Add(TLapeType_ArrayHelper_CopyRange, 'CopyRange');
  Add(TLapeType_ArrayHelper_Random, 'Random');
  Add(TLapeType_ArrayHelper_Reversed, 'Reversed');
  Add(TLapeType_ArrayHelper_Slice, 'Slice');
  Add(TLapeType_ArrayHelper_Difference, 'Difference');
  Add(TLapeType_ArrayHelper_SymDifference, 'SymDifference');
  Add(TLapeType_ArrayHelper_Intersection, 'Intersection');
  Add(TLapeType_ArrayHelper_Equals, 'Equals');
  Add(TLapeType_ArrayHelper_Empty, 'Empty');
  Add(TLapeType_ArrayHelper_Clear, 'Clear');

  Add(TLapeType_ArrayHelper_Median, 'Median');
  Add(TLapeType_ArrayHelper_Mode, 'Mode');
  Add(TLapeType_ArrayHelper_Min, 'Min');
  Add(TLapeType_ArrayHelper_Max, 'Max');
  Add(TLapeType_ArrayHelper_Sum, 'Sum');
  Add(TLapeType_ArrayHelper_Mean, 'Mean');
  Add(TLapeType_ArrayHelper_Variance, 'Variance');
  Add(TLapeType_ArrayHelper_Stdev, 'Stdev');

  Add(TLapeType_ArrayHelper_SetLength, 'SetLength');
  Add(TLapeType_ArrayHelper_Pop, 'Pop');
  Add(TLapeType_ArrayHelper_Remove, 'Remove');
  Add(TLapeType_ArrayHelper_Delete, 'Delete');
  Add(TLapeType_ArrayHelper_DeleteRange, 'DeleteRange');
  Add(TLapeType_ArrayHelper_Insert, 'Insert');
  Add(TLapeType_ArrayHelper_Sort, 'Sort');
  Add(TLapeType_ArrayHelper_Reverse, 'Reverse');

  Compiler.addGlobalDecl(Typ);
end;

procedure LapeCreateStaticArrayHelpers(Compiler: TLapeCompilerBase);
var
  Typ: TLapeType;

  procedure Add(Helper: TLapeType_HelperClass; Name: lpString);
  begin
    Typ.ManagedDeclarations.addDeclaration(
      TLapeType_Helper(Compiler.addManagedType(Helper.Create(Compiler, Name))).NewGlobalVar(Name)
    );
  end;

begin
  Typ := TLapeType.Create(ltUnknown, Compiler);
  Typ.Name := '!arrayhelpersstatic';

  Add(TLapeType_ArrayHelper_Low, 'Low');
  Add(TLapeType_ArrayHelper_High, 'High');
  Add(TLapeType_ArrayHelper_Length, 'Length');
  Add(TLapeType_ArrayHelper_First, 'First');
  Add(TLapeType_ArrayHelper_Last, 'Last');
  Add(TLapeType_ArrayHelper_Contains, 'Contains');
  Add(TLapeType_ArrayHelper_Swap, 'Swap');
  Add(TLapeType_ArrayHelper_Unique, 'Unique');
  Add(TLapeType_ArrayHelper_IndexOf, 'IndexOf');
  Add(TLapeType_ArrayHelper_IndicesOf, 'IndicesOf');
  Add(TLapeType_ArrayHelper_Sorted, 'Sorted');
  Add(TLapeType_ArrayHelper_Copy, 'Copy');
  Add(TLapeType_ArrayHelper_CopyRange, 'CopyRange');
  Add(TLapeType_ArrayHelper_Random, 'Random');
  Add(TLapeType_ArrayHelper_Reversed, 'Reversed');
  Add(TLapeType_ArrayHelper_Slice, 'Slice');
  Add(TLapeType_ArrayHelper_Difference, 'Difference');
  Add(TLapeType_ArrayHelper_SymDifference, 'SymDifference');
  Add(TLapeType_ArrayHelper_Intersection, 'Intersection');
  Add(TLapeType_ArrayHelper_Equals, 'Equals');

  Add(TLapeType_ArrayHelper_Median, 'Median');
  Add(TLapeType_ArrayHelper_Mode, 'Mode');
  Add(TLapeType_ArrayHelper_Min, 'Min');
  Add(TLapeType_ArrayHelper_Max, 'Max');
  Add(TLapeType_ArrayHelper_Sum, 'Sum');
  Add(TLapeType_ArrayHelper_Mean, 'Mean');
  Add(TLapeType_ArrayHelper_Variance, 'Variance');
  Add(TLapeType_ArrayHelper_Stdev, 'Stdev');

  Compiler.addGlobalDecl(Typ);
end;

end.
