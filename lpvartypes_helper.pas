{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  A helper type is a type method that is generated when required.
  These can currently not be overriden (or overloaded with the same name).
}
unit lpvartypes_helper;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes;

type
  TLapeArrayHelperClass = class of TLapeArrayHelper;

  // What an array can do (arr.Sort), made for the array and what it is called with when that is asked for. Declared on
  // the type that the kinds of array that can do it come from.
  TLapeArrayHelper = class(TLapeDeclaration)
  protected
    FCompiler: TLapeCompilerBase;
    FMade: TLapeDeclarationList;
    function getFunc(ObjectType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; virtual;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString); reintroduce; virtual;
    destructor Destroy; override;

    function Make(AObjectType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; virtual;
    function CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType = nil): TLapeGlobalVar;
  end;

  TLapeArrayHelper_Inline = class(TLapeArrayHelper);

  // Low,High,Length,Pop,First,Last are performed inline in DynArray.Eval to reduce function overhead and wont be used like the others
  TLapeArrayHelper_Low = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_High = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_Length = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_First = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_Last = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_Pop = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_IsEmpty = class(TLapeArrayHelper_Inline);

  TLapeArrayHelper_SetLength = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Clear = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Contains = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Copy = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_CopyRange = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Remove = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Delete = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_DeleteRange = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Insert = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Swap = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Random = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Reverse = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Reversed = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Unique = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_IndexOf = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_IndicesOf = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Sort = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Sorted = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Median = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Mode = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Min = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Max = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Sum = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Mean = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Variance = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Stdev = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Slice = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Difference = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_SymDifference = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Intersection = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  TLapeArrayHelper_Equals = class(TLapeArrayHelper)
  protected
    function getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar; override;
  end;

  // What arrays of AObjectType have as AName (nil: nothing)
  function FindArrayHelper(AObjectType: TLapeType; AName: lpString): TLapeArrayHelper;
  procedure LapeCreateArrayHelpers(Compiler: TLapeCompilerBase);

implementation

uses
  lpcompiler, lptree, lpvartypes_array, lpmessages;

type
  // Never created: a compiler is cast to it, see TLapeMethodParser
  TLapeArrayHelpers = class(TLapeCompilerBase)
  public
    function Generate(AName: lpString; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
  end;

function TLapeArrayHelpers.Generate(AName: lpString; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Helper: TLapeArrayHelper;
begin
  Result := nil;
  Helper := FindArrayHelper(AObjectType, AName);
  if (Helper <> nil) then
    Result := Helper.Make(AObjectType, AParams);
end;

function FindArrayHelper(AObjectType: TLapeType; AName: lpString): TLapeArrayHelper;
var
  Decl: TLapeDeclaration;
begin
  Result := nil;
  while (AObjectType <> nil) do
  begin
    while (AObjectType.AliasType <> nil) do
      AObjectType := AObjectType.AliasType;
    if AObjectType.Declarations.Get(AName, TLapeArrayHelper, Decl) then
      Exit(TLapeArrayHelper(Decl));
    AObjectType := AObjectType.ParentType;
  end;
end;

constructor TLapeArrayHelper.Create(ACompiler: TLapeCompilerBase; AName: lpString);
begin
  inherited Create(AName);

  FCompiler := ACompiler;
  FMade := TLapeDeclarationList.Create();
end;

destructor TLapeArrayHelper.Destroy;
begin
  FreeAndNil(FMade);
  inherited;
end;

function TLapeArrayHelper.Make(AObjectType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;

  function EqualTypes(Left, Right: TLapeType): Boolean;
  begin
    Result := (Left = Right) or ((Left <> nil) and Left.Equals(Right, False));
  end;

  function EqualMethod(Method: TLapeType_MethodOfType): Boolean;
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
  for i := 0 to FMade.Count - 1 do
    if EqualMethod(TLapeGlobalVar(FMade[i]).VarType as TLapeType_MethodOfType) then
      Exit(TLapeGlobalVar(FMade[i]));

  Result := getFunc(AObjectType, AParams);
end;

// (what is done inline makes no method)
function TLapeArrayHelper.getFunc(ObjectType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := nil;
end;

function TLapeArrayHelper.CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  Param: TLapeParameter;
  i: Integer;
  Method: TLapeTree_Method;
begin
  Assert(FCompiler is TLapeCompiler);

  with FCompiler as TLapeCompiler do
  begin
    if (not hasDeclaration(VarType)) and (VarType.Name = '') then // for `var a: array of x` otherwise "parent declaration out of scope"
      addGlobalType(VarType);

    Header := addManagedType(TLapeType_MethodOfType.Create(FCompiler, VarType, ResultType, FName)) as TLapeType_Method;
    for i := 0 to High(ParamTypes) do
    begin
      Param.Default := nil;
      Param.ParType := lptConst;
      Param.VarType := ParamTypes[i];

      Header.addParam(Param);
    end;

    // (what goes wrong in it is said to be at its caller, see TLapeCodeRunner.RaisedAt)
    Method := addGlobalFunc(Header,
      '{$X+}'       + LineEnding +
      'begin'       + LineEnding +
      '  ' + Body   + LineEnding +
      'end;');

    Result := Method.Method;
    FMade.addDeclaration(Result);
  end;
end;

function TLapeArrayHelper_SetLength.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'System.SetLength(Self, Param0);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeArrayHelper_Clear.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Self := [];',
    VarType,
    []
  );
end;

function TLapeArrayHelper_Contains.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.Contains(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltBoolean)
  );
end;

function TLapeArrayHelper_Copy.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  Result := nil;

  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := TLapeType_DynArray(VarType).PType.ArrayOf
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Copy(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeArrayHelper_CopyRange.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := TLapeType_DynArray(VarType).PType.ArrayOf
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Copy(Self, Param0, (Param1 - Param0) + 1);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    ResType
  );
end;

function TLapeArrayHelper_Remove.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.Remove(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_Delete.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := Self[Param0];'          + LineEnding +
    'System.Delete(Self, Param0, 1);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_DeleteRange.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := Self[Param0];'                            + LineEnding +
    'System.Delete(Self, Param0, (Param1 - Param0) + 1);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_Insert.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
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

function TLapeArrayHelper_Swap.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'System.Swap(Self[Param0], Self[Param1]);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeArrayHelper_Random.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := Self[System.Random(System.Low(Self), System.High(Self))];',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_Reverse.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'System.Reverse(Self);',
    VarType,
    []
  );
end;

function TLapeArrayHelper_Reversed.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := TLapeType_DynArray(VarType).PType.ArrayOf
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Reversed(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeArrayHelper_Unique.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.Unique(Self);',
    VarType,
    [],
    VarType
  );
end;

function TLapeArrayHelper_IndexOf.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.IndexOf(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltInt32)
  );
end;

function TLapeArrayHelper_IndicesOf.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.IndicesOf(Param0, Self);',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getGlobalType('TIntegerArray')
  );
end;

function TLapeArrayHelper_Sort.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
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
         [WeightType.ArrayOf, FCompiler.getBaseType(ltBoolean)]
       );
     end;
  end;
end;

function TLapeArrayHelper_Sorted.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  WeightType: TLapeType;
begin
  Result := nil;

  case Length(AParams) of
    0:
      begin
        Result := CreateFunction(
          'Result := System.Sorted(Self);',
          VarType,
          [],
          VarType
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'Result := System.Sorted(Self, @Param0);',
          VarType,
          [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))],
          VarType
        );
      end;

    2:
      if (AParams[0] is TLapeType_DynArray) and TLapeType_DynArray(AParams[0]).HasType then
      begin
        WeightType := TLapeType_DynArray(AParams[0]).PType;

        Result := CreateFunction(
            'Result := System.Sorted(Self, Param0, Param1);',
            VarType,
            [WeightType.ArrayOf, FCompiler.getBaseType(ltBoolean)],
            VarType
          );
      end;
  end;
end;

function TLapeArrayHelper_Median.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMedian(Self);',
    VarType,
    [],
    FCompiler.getBaseType(ltDouble)
  );
end;

function TLapeArrayHelper_Mode.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMode(Self);',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_Min.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMin(Self);',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_Max.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMax(Self);',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeArrayHelper_Sum.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := FCompiler.getBaseType(ltInt64);

  Result := CreateFunction(
    'Result := System.ArraySum(Self);',
    VarType,
    [],
    ResType
  );
end;

function TLapeArrayHelper_Mean.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayMean(Self);',
    VarType,
    [],
    FCompiler.getBaseType(ltDouble)
  );
end;

function TLapeArrayHelper_Variance.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayVariance(Self);',
    VarType,
    [],
    FCompiler.getBaseType(ltDouble)
  );
end;

function TLapeArrayHelper_Stdev.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayStdev(Self);',
    VarType,
    [],
    FCompiler.getBaseType(ltDouble)
  );
end;

function TLapeArrayHelper_Slice.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := TLapeType_DynArray(VarType).PType.ArrayOf
  else
    ResType := VarType;

  Result := CreateFunction(
    'Result := System.Slice(Self, Param0, Param1, Param2);',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    ResType
  );
end;

function TLapeArrayHelper_Difference.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayDifference(Self, Param0);',
    VarType,
    [VarType],
    VarType
  );
end;

function TLapeArrayHelper_SymDifference.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArraySymDifference(Self, Param0);',
    VarType,
    [VarType],
    VarType
  );
end;

function TLapeArrayHelper_Intersection.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayIntersection(Self, Param0);',
    VarType,
    [VarType],
    VarType
  );
end;

function TLapeArrayHelper_Equals.getFunc(VarType: TLapeType; AParams: TLapeTypeArray): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'Result := System.ArrayEquals(Self, Param0);',
    VarType,
    [VarType],
    FCompiler.getBaseType(ltBoolean)
  );
end;

procedure LapeCreateArrayHelpers(Compiler: TLapeCompilerBase);

  procedure Build(Helper: TLapeArrayHelperClass; Name: lpString; Types: array of TLapeType);
  var
    Typ: TLapeType;
  begin
    for Typ in Types do
      Typ.Declarations.addDeclaration(Helper.Create(Compiler, Name));
  end;

var
  dynSingle: TLapeType;
  dynMulti: TLapeType;
  dynOrdinal: TLapeType;
  staticArr: TLapeType;
begin
  dynSingle  := TLapeType.Create(ltUnknown, Compiler, '!arrayhelpers_dynsingledim');
  dynMulti   := TLapeType.Create(ltUnknown, Compiler, '!arrayhelpers_dynmultidim');
  dynOrdinal := TLapeType.Create(ltUnknown, Compiler, '!arrayhelpers_dynordinal');
  staticArr  := TLapeType.Create(ltUnknown, Compiler, '!arrayhelpers_static');
  Compiler.addGlobalType(dynSingle);
  Compiler.addGlobalType(dynMulti);
  Compiler.addGlobalType(dynOrdinal);
  Compiler.addGlobalType(staticArr);

  Build(TLapeArrayHelper_SetLength, 'SetLength', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Low, 'Low', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_High, 'High', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Length, 'Length', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_First, 'First', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Last, 'Last', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Swap, 'Swap', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Copy, 'Copy', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_CopyRange, 'CopyRange', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Random, 'Random', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Slice, 'Slice', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_IsEmpty, 'IsEmpty', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Clear, 'Clear', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Pop, 'Pop', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Delete, 'Delete', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_DeleteRange, 'DeleteRange', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Remove, 'Remove', [dynSingle, dynOrdinal]);
  Build(TLapeArrayHelper_Insert, 'Insert', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Equals, 'Equals', [dynSingle, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Unique, 'Unique', [dynSingle, dynOrdinal]);
  Build(TLapeArrayHelper_Contains, 'Contains', [dynSingle, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_IndexOf, 'IndexOf', [dynSingle, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_IndicesOf, 'IndicesOf', [dynSingle, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Sort, 'Sort', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Sorted, 'Sorted', [dynSingle, dynMulti, dynOrdinal]);
  Build(TLapeArrayHelper_Reverse, 'Reverse', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Reversed, 'Reversed', [dynSingle, dynMulti, dynOrdinal, staticArr]);
  Build(TLapeArrayHelper_Difference, 'Difference', [dynSingle, dynOrdinal]);
  Build(TLapeArrayHelper_SymDifference, 'SymDifference', [dynSingle, dynOrdinal]);
  Build(TLapeArrayHelper_Intersection, 'Intersection', [dynSingle, dynOrdinal]);
  Build(TLapeArrayHelper_Median, 'Median', [dynOrdinal]);
  Build(TLapeArrayHelper_Mode, 'Mode', [dynOrdinal]);
  Build(TLapeArrayHelper_Min, 'Min', [dynOrdinal]);
  Build(TLapeArrayHelper_Max, 'Max', [dynOrdinal]);
  Build(TLapeArrayHelper_Sum, 'Sum', [dynOrdinal]);
  Build(TLapeArrayHelper_Mean, 'Mean', [dynOrdinal]);
  Build(TLapeArrayHelper_Variance, 'Variance', [dynOrdinal]);
  Build(TLapeArrayHelper_Stdev, 'Stdev', [dynOrdinal]);

  Compiler.addMethodGenerate({$IFDEF FPC}@{$ENDIF}TLapeArrayHelpers(Compiler).Generate);
end;

end.
