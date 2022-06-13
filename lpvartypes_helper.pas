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
    FHelperName: lpString;

    function FunctionNotFound(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method; AObjectType: TLapeType; AParams: TLapeTypeArray = nil; AResult: TLapeType = nil): TLapeGlobalVar; virtual;

    function CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType = nil): TLapeGlobalVar;
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; virtual; abstract;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; override;
  end;

  TLapeType_ArrayHelper_SetLength = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Length = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Contains = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Remove = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_RemoveAll = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Low = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_High = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Copy = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Delete = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Insert = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_First = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Last = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Pop = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Swap = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_RandomValue = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Reverse = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Reversed = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Unique = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_IndexOf = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_IndicesOf = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Sort = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Sorted = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Clear = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Append = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Extend = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Median = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Mode = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Min = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Max = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Sum = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Mean = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Variance = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Stddev = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

  TLapeType_ArrayHelper_Slice = class(TLapeType_Helper)
  protected
    function GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar; override;
  end;

implementation

uses
  lpcompiler, lpparser,
  lpvartypes_array;

function TLapeType_Helper.FunctionNotFound(Sender: TLapeType_OverloadedMethod; AType: TLapeType_Method; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  i: Integer;
  Temp: TLapeGetOverloadedMethod;
begin
  Result := nil;

  Assert(FCompiler is TLapeCompiler);
  if (TLapeCompiler(FCompiler).Tokenizer.LastTok = tk_kw_Overload) then // Don't generate on overload. Will still fail nicely later on if invalid overload.
    Exit;

  if (AObjectType <> nil) then
  begin
    if (AType <> nil) then
    begin
      if (AType.Params.Count > 0) then
      begin
        SetLength(AParams,  AType.Params.Count);
        for i := 0 to AType.Params.Count - 1 do
          AParams[i] := AType.Params[i].VarType;
      end;
      AResult := AType.Res;
    end;

    GetFunction(AObjectType, AParams, AResult);

    // Return nil if a function was generated but doesn't match params
    Temp := OnFunctionNotFound;
    try
      OnFunctionNotFound := nil;

      Result := getMethod(AParams, AResult, AObjectType);
    finally
      OnFunctionNotFound := Temp;
    end;
  end;
end;

function TLapeType_Helper.CreateFunction(Body: String; VarType: TLapeType; ParamTypes: array of TLapeType; ResultType: TLapeType): TLapeGlobalVar;

  function EqualMethod(Method: TLapeType_Method): Boolean;

    function EqualTypes(Left, Right: TLapeType): Boolean;
    begin
      Result := (Left = Right) or ((Left <> nil) and Left.Equals(Right, False));
    end;

  var
    i: Integer;
  begin
    if (Length(ParamTypes) <> Method.Params.Count) or (not EqualTypes(ResultType, Method.Res)) then
      Exit(False);
    for i := 0 to High(ParamTypes) do
      if (not EqualTypes(ParamTypes[i], Method.Params[i].VarType)) then
        Exit(False);

    Result := True;
  end;

var
  Header: TLapeType_Method;
  Param: TLapeParameter;
  i: Integer;
  Pos: TDocPos;
begin
  Assert(FCompiler is TLapeCompiler);

  for i := 0 to FManagedDecls.Count - 1 do
    if EqualMethod(TLapeType_Method(TLapeGlobalVar(FManagedDecls[i]).VarType)) then
      Exit(TLapeGlobalVar(FManagedDecls[i]));

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

    Pos := DocPos;
    Result := addGlobalFunc(Header, '!Helper', Body, @Pos).Method;
    Result.VarType.Name := FHelperName;

    addMethod(Result);
  end;
end;

constructor TLapeType_Helper.Create(ACompiler: TLapeCompilerBase; AName: lpString; ADocPos: PDocPos);
begin
  inherited Create(ACompiler, '', ADocPos);

  FHelperName := AName;

  OnFunctionNotFound := @FunctionNotFound;
end;

function TLapeType_ArrayHelper_SetLength.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                             + LineEnding +
    '  System.SetLength(Self, Param0);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeType_ArrayHelper_Length.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                            + LineEnding +
    '  Result := System.Length(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

function TLapeType_ArrayHelper_Contains.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                      + LineEnding +
    '  Result := System.Contains(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltEvalBool)
  );
end;

function TLapeType_ArrayHelper_Remove.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                    + LineEnding +
    '  Result := System.Remove(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltEvalBool)
  );
end;

function TLapeType_ArrayHelper_RemoveAll.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                       + LineEnding +
    '  Result := System.RemoveAll(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

function TLapeType_ArrayHelper_Low.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                         + LineEnding +
    '  Result := System.Low(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

function TLapeType_ArrayHelper_High.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                          + LineEnding +
    '  Result := System.High(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    FCompiler.getBaseType(ltSizeInt)
  );
end;

function TLapeType_ArrayHelper_Copy.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
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
          'begin'                          + LineEnding +
          '  Result := System.Copy(Self);' + LineEnding +
          'end;',
          VarType,
          [],
          ResType
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'begin'                                  + LineEnding +
          '  Result := System.Copy(Self, Param0);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.getBaseType(ltSizeInt)],
          ResType
        );
      end;

    2:
      begin
        Result := CreateFunction(
          'begin'                                          + LineEnding +
          '  Result := System.Copy(Self, Param0, Param1);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
          ResType
        );
      end;
  end;
end;

function TLapeType_ArrayHelper_Delete.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;

  case Length(AParams) of
    1:
      begin
        Result := CreateFunction(
          'begin'                          + LineEnding +
          '  System.Delete(Self, Param0);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.getBaseType(ltSizeInt)]
        );
      end;

    2:
      begin
        Result := CreateFunction(
          'begin'                                  + LineEnding +
          '  System.Delete(Self, Param0, Param1);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)]
        );
     end;
  end;
end;

function TLapeType_ArrayHelper_Insert.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                  + LineEnding +
    '  System.Insert(Param0, Self, Param1);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType, FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeType_ArrayHelper_First.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                               + LineEnding +
    '  Result := Self[System.Low(Self)];' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Last.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                + LineEnding +
    '  Result := Self[System.High(Self)];' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Pop.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := nil;

  case Length(AParams) of
    0:
      if VarType.BaseType in LapeStringTypes then
      begin
        Result := CreateFunction(
          'var Index: Integer := System.Length(Self);' + LineEnding +
          'begin'                                      + LineEnding +
          '  Result := Self[Index];'                   + LineEnding +
          '  System.SetLength(Self, Index - 1);'       + LineEnding +
          'end;',
          VarType,
          [],
          TLapeType_DynArray(VarType).PType
        );
      end else
      begin
        Result := CreateFunction(
          'var Index: Integer := System.High(Self);' + LineEnding +
          'begin'                                    + LineEnding +
          '  Result := Self[Index];'                 + LineEnding +
          '  System.SetLength(Self, Index);'         + LineEnding +
          'end;',
          VarType,
          [],
          TLapeType_DynArray(VarType).PType
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'begin'                             + LineEnding +
          '  Result := Self[Param0];'         + LineEnding +
          '  System.Delete(Self, Param0, 1);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.getBaseType(ltSizeInt)],
          TLapeType_DynArray(VarType).PType
        );
      end;
  end;
end;

function TLapeType_ArrayHelper_Swap.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                      + LineEnding +
    '  System.Swap(Self[Param0], Self[Param1]);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)]
  );
end;

function TLapeType_ArrayHelper_RandomValue.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                                                 + LineEnding +
    '  Result := Self[System.Random(System.Low(Self), System.High(Self))];' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Reverse.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                   + LineEnding +
    '  System.Reverse(Self);' + LineEnding +
    'end;',
    VarType,
    []
  );
end;

function TLapeType_ArrayHelper_Reversed.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.Reversed(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Unique.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'begin'                            + LineEnding +
    '  Result := System.Unique(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_IndexOf.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                     + LineEnding +
    '  Result := System.IndexOf(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getBaseType(ltInt32)
  );
end;

function TLapeType_ArrayHelper_IndicesOf.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                                       + LineEnding +
    '  Result := System.IndicesOf(Param0, Self);' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType],
    FCompiler.getIntegerArray()
  );
end;

function TLapeType_ArrayHelper_Sort.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  WeightType: TLapeType;
begin
  Result := nil;

  case Length(AParams) of
    0:
      begin
        Result := CreateFunction(
          'begin'                + LineEnding +
          '  System.Sort(Self);' + LineEnding +
          'end;',
          VarType,
          []
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'begin'                         + LineEnding +
          '  System.Sort(Self, @Param0);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))]
        );
      end;

   2:
     if (AParams[0] is TLapeType_DynArray) and TLapeType_DynArray(AParams[0]).HasType then
     begin
       WeightType := TLapeType_DynArray(AParams[0]).PType;

       if (WeightType.BaseType in LapeIntegerTypes) then
       begin
         Result := CreateFunction(
           'begin'                                + LineEnding +
           '  System.Sort(Self, Param0, Param1);' + LineEnding +
           'end;',
           VarType,
           [FCompiler.getIntegerArray(), FCompiler.getBaseType(ltEvalBool)]
         );
       end;

       if (WeightType.BaseType in LapeRealTypes) then
       begin
         Result := CreateFunction(
           'begin'                                + LineEnding +
           '  System.Sort(Self, Param0, Param1);' + LineEnding +
           'end;',
           VarType,
           [FCompiler.getFloatArray(), FCompiler.getBaseType(ltEvalBool)]
         );
       end;
     end;
  end;
end;

function TLapeType_ArrayHelper_Sorted.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
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
          'begin'                            + LineEnding +
          '  Result := System.Sorted(Self);' + LineEnding +
          'end;',
          VarType,
          [],
          ResType
        );
      end;

    1:
      begin
        Result := CreateFunction(
          'begin'                                     + LineEnding +
          '  Result := System.Sorted(Self, @Param0);' + LineEnding +
          'end;',
          VarType,
          [FCompiler.addManagedType(TLapeType_Method.Create(FCompiler, [TLapeType_DynArray(VarType).PType, TLapeType_DynArray(VarType).PType], [lptConstRef, lptConstRef], [nil, nil], FCompiler.getBaseType(ltInt32)))],
          ResType
        );
      end;

    2:
      if (AParams[0] is TLapeType_DynArray) and TLapeType_DynArray(AParams[0]).HasType then
      begin
        WeightType := TLapeType_DynArray(AParams[0]).PType;

        if (WeightType.BaseType in LapeIntegerTypes) then
        begin
          Result := CreateFunction(
            'begin'                                           + LineEnding +
            '  Result := System.Sorted(Self, Param0, Param1);' + LineEnding +
            'end;',
            VarType,
            [FCompiler.getIntegerArray(), FCompiler.getBaseType(ltEvalBool)],
            ResType
          );
        end;

        if (WeightType.BaseType in LapeRealTypes) then
        begin
          Result := CreateFunction(
            'begin'                                           + LineEnding +
            '  Result := System.Sorted(Self, Param0, Param1);' + LineEnding +
            'end;',
            VarType,
            [FCompiler.getFloatArray(), FCompiler.getBaseType(ltEvalBool)],
            ResType
          );
        end;
      end;
  end;
end;

function TLapeType_ArrayHelper_Clear.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'         + LineEnding +
    '  Self := [];' + LineEnding +
    'end;',
    VarType,
    []
  );
end;

function TLapeType_ArrayHelper_Append.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                    + LineEnding +
    '  Self := Self + Param0;' + LineEnding +
    'end;',
    VarType,
    [TLapeType_DynArray(VarType).PType]
  );
end;

function TLapeType_ArrayHelper_Extend.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                    + LineEnding +
    '  Self := Self + Param0;' + LineEnding +
    'end;',
    VarType,
    [VarType]
  );
end;

function TLapeType_ArrayHelper_Median.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeIntegerTypes + LapeRealTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'begin'                                 + LineEnding +
    '  Result := System.ArrayMedian(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Mode.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                               + LineEnding +
    '  Result := System.ArrayMode(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Min.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.ArrayMin(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Max.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
begin
  Result := CreateFunction(
    'begin'                              + LineEnding +
    '  Result := System.ArrayMax(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    TLapeType_DynArray(VarType).PType
  );
end;

function TLapeType_ArrayHelper_Sum.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
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
    'begin'                              + LineEnding +
    '  Result := System.ArraySum(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Mean.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'begin'                               + LineEnding +
    '  Result := System.ArrayMean(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Variance.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'begin'                                   + LineEnding +
    '  Result := System.ArrayVariance(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Stddev.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := FCompiler.getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(VarType).PType;

  Result := CreateFunction(
    'begin'                                + LineEnding +
    '  Result := System.ArrayStdev(Self);' + LineEnding +
    'end;',
    VarType,
    [],
    ResType
  );
end;

function TLapeType_ArrayHelper_Slice.GetFunction(VarType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  ResType: TLapeType;
begin
  if (TLapeType_DynArray(VarType).BaseType = ltStaticArray) then
    ResType := FCompiler.addManagedType(TLapeType_DynArray.Create(TLapeType_DynArray(VarType).PType, FCompiler))
  else
    ResType := VarType;

  Result := CreateFunction(
    'begin'                                                   + LineEnding +
    '  Result := System.Slice(Self, Param0, Param1, Param2);' + LineEnding +
    'end;',
    VarType,
    [FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt), FCompiler.getBaseType(ltSizeInt)],
    ResType
  );
end;

end.

