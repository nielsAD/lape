{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0)

  The global methods that are made when they are asked for: ToString of a type, what assigns and clears one, Objectify,
  and what arrays do (_ArraySort, _ArrayMin, ...): Generate asks for them by their name.
  What arrays have as arr.Name is in lpvartypes_helper.
}
unit lpgeneratedmethods;

{$I lape.inc}

interface

uses
  lptypes, lpvartypes, lpcompiler;

procedure LapeAddGeneratedMethods(Compiler: TLapeCompiler);

implementation

uses
  SysUtils,
  lptree, lpinvoke, lpinternalmethods, lpinterpreter_types, lpeval, lpvartypes_ord, lpvartypes_record, lpvartypes_array, lpvartypes_object;

type
  // Never created: a compiler is cast to it, see TLapeMethodParser
  TLapeGeneratedMethods = class(TLapeCompiler)
  protected
    procedure addMade(Method: TLapeGlobalVar);
    function ValueHeader(AName: lpString; AParams: TLapeTypeArray; AResult: TLapeType): TLapeType_Method;
    function MakeMethod(Header: TLapeType_Method; Body: lpString): TLapeGlobalVar;

    function GetToStringMethod(AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
    function GetObjectifyMethod(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetDisposeObjectMethod(AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
    function GetCopyMethod(AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
    function GetMethod_ArraySort(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayRange(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayMinMax(AName, Op: lpString; AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayUnique(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayIndexOf(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayIndicesOf(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayMode(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArraySum(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayMean(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayMedian(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayVariance(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayStdev(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayDifference(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArraySymDifference(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayIntersection(AParams: TLapeTypeArray): TLapeGlobalVar;
    function GetMethod_ArrayEquals(AParams: TLapeTypeArray): TLapeGlobalVar;
  public
    function Generate(AName: lpString; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
    procedure addGeneratedMethods;
  end;

procedure TLapeGeneratedMethods.addMade(Method: TLapeGlobalVar);
begin
  Method.isConstant := True;
  Method.Known := False;
  GlobalScope.addDeclaration(Method);
end;

// All it takes by value
function TLapeGeneratedMethods.ValueHeader(AName: lpString; AParams: TLapeTypeArray; AResult: TLapeType): TLapeType_Method;
var
  ParTypes: array of ELapeParameterType;
  Defaults: array of TLapeGlobalVar;
  i: Integer;
begin
  SetLength(ParTypes, Length(AParams));
  SetLength(Defaults, Length(AParams));
  for i := 0 to High(AParams) do
  begin
    ParTypes[i] := lptNormal;
    Defaults[i] := nil;
  end;

  Result := addManagedType(TLapeType_Method.Create(Self, AParams, ParTypes, Defaults, AResult, AName)) as TLapeType_Method;
end;

function TLapeGeneratedMethods.MakeMethod(Header: TLapeType_Method; Body: lpString): TLapeGlobalVar;
begin
  Result := addGlobalFunc(Header, Body).Method;
end;

function TLapeGeneratedMethods.GetToStringMethod(AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Body: lpString;
  Header: TLapeType_Method;
  Methods: TLapeDeclArray;
  BaseType: TLapeType;
begin
  Result := nil;
  if (Length(AParams) <> 1) or ((AResult <> nil) and (AResult.BaseType <> ltString)) then
    Exit;

  AResult := getBaseType(ltString);
  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptConstRef], [TLapeGlobalVar(nil)], AResult, 'ToString')) as TLapeType_Method;

  // (there before its body: a type that has itself in it asks for this one again)
  Result := Header.NewGlobalVar(EndJump);
  addMade(Result);
  Body := AParams[0].VarToStringBody();
  FreeAndNil(Result);

  if (Body <> '') then
    Exit(MakeMethod(Header, LapeCodeFlags + Body + LineEnding));

  // No way of its own: nothing is made, it is written by the ToString of what it is a range of, of the nearest type
  // that has one, or of its base type

  Methods := GlobalScope.findMethods('ToString');
  if (AParams[0] is TLapeType_SubRange) and (TLapeType_SubRange(AParams[0]).VarType <> nil) then
    Exit(PickMethod('ToString', Methods, nil, getTypeArray([TLapeType_SubRange(AParams[0]).VarType]), AResult));

  Result := FitMethod(Methods, False, AParams, AResult);
  BaseType := getBaseType(AParams[0].BaseType);
  if (Result = nil) and (BaseType <> AParams[0]) then
    Result := PickMethod('ToString', Methods, nil, getTypeArray([BaseType]), AResult);
end;

function TLapeGeneratedMethods.GetObjectifyMethod(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Invoke: TLapeTree_Invoke;
  Assignment: TLapeTree_Operator;
  Callback: TResVar;
  i: Int32;
  AType, Header: TLapeType_MethodOfObject;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (AParams[0] = nil) or (AParams[0].ClassType <> TLapeType_MethodOfObject) then
    Exit;

  Method := nil;
  AType := TLapeType_MethodOfObject(AParams[0]);
  // (shared by every caller, so at none of them)
  FEmitter.DocPos := TDocPos.CreateInternal('Objectify');
  IncStackInfo([]);

  try
    Header := AType.CreateCopy() as TLapeType_MethodOfObject;
    Header.Name := '_Objectify';
    Result := addManagedDecl(TLapeType_MethodOfObject(addManagedType(Header)).NewGlobalVar(EndJump)) as TLapeGlobalVar;

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Method);

    // (Self is the method that it calls, see TLapeTree_InternalMethod_Objectify.Compile)
    Callback := _ResVar.New(FStackInfo.addSelfVar(lptNormal, getGlobalType('ConstPointer')));
    Callback.VarType := addManagedType(TLapeType_Method.Create(AType));

    Invoke := TLapeTree_InvokePtr.Create(TLapeTree_Var.Create(Callback, Self), Self);
    for i := 0 to AType.Params.Count - 1 do
      Invoke.addParam(TLapeTree_Var.Create(_ResVar.New(FStackInfo.addVar(AType.Params[i].ParType, AType.Params[i].VarType)), Self));

    if (AType.Res <> nil) then
    begin
      Assignment := TLapeTree_Operator.Create(op_Assign, Self);
      Assignment.Left := TLapeTree_Var.Create(_ResVar.New(FStackInfo.addVar(lptOut, AType.Res)), Self);
      Assignment.Right := Invoke;

      Method.Statements.addStatement(Assignment);
    end else
      Method.Statements.addStatement(Invoke);

    FMethods.addStatement(Method);
  finally
    DecStackInfo([lsfFunction], Method = nil);
  end;
end;

function TLapeGeneratedMethods.GetDisposeObjectMethod(AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Header: TLapeType_Method;
  ParamVar: TResVar;
  Member: TLapeTree_ExprBase;
  Dispose: TLapeTree_InternalMethod_Dispose;
  i: Integer;
begin
  Result := nil;
  Method := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_Object)) or (AResult <> nil) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptConstRef], [TLapeGlobalVar(nil)], nil, '_DisposeObject')) as TLapeType_Method;

  // (shared by every caller, so at none of them)
  FEmitter.DocPos := TDocPos.CreateInternal('DisposeObject');
  IncStackInfo([]);
  try
    Result := Header.NewGlobalVar(EndJump);
    addMade(Result);

    ParamVar := _ResVar.New(FStackInfo.addVar(lptConstRef, AParams[0], '!AVar'));
    ParamVar.isConstant := False;

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Self);

    if (ParamVar.VarType.getDeclaration('Destroy') <> nil) then
    begin
      Member := TLapeTree_MethodByName.Create(TLapeTree_Var.Create(ParamVar, Method.Statements), 'Destroy', Method.Statements);
      Method.Statements.addStatement(TLapeTree_Invoke.Create(Member, Self));
    end;

    with TLapeType_Object(AParams[0]) do
      for i := 0 to FieldMap.Count - 1 do
      begin
        if not FieldMap.ItemsI[i].FieldType.NeedFinalization then
          Continue;

        Member := TLapeTree_Member.Create(ParamVar, FieldMap.Key[i], Method.Statements);

        Dispose := TLapeTree_InternalMethod_Dispose.Create(Method.Statements);
        Dispose.FunctionOnly := True;
        Dispose.addParam(Member);

        Method.Statements.addStatement(Dispose);
      end;

    FMethods.addStatement(Method);
  finally
    DecStackInfo([lsfFunction], Method = nil);
  end;
end;

function TLapeGeneratedMethods.GetCopyMethod(AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  Method: TLapeTree_Method;
  Assignment: TLapeTree_Operator;
  Header: TLapeType_Method;
begin
  Result := nil;
  Method := nil;
  if (Length(AParams) <> 2) or (AResult <> nil) or (not AParams[0].CompatibleWith(AParams[1])) then
    Exit;
  if (not AParams[0].NeedFinalization) and (not AParams[1].NeedInitialization) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptConstRef, lptOut], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], AResult, '_Assign')) as TLapeType_Method;

  // (shared by every caller, so at none of them)
  FEmitter.DocPos := TDocPos.CreateInternal('Assign');
  IncStackInfo([]);
  try
    Result := Header.NewGlobalVar(EndJump);
    addMade(Result);

    Assignment := TLapeTree_Operator.Create(op_Assign, Self);
    Assignment.Right := TLapeTree_Var.Create(_ResVar.New(FStackInfo.addVar(lptConstRef, AParams[0], '!Src')), Self);
    Assignment.Left  := TLapeTree_Var.Create(_ResVar.New(FStackInfo.addVar(lptOut, AParams[1], '!Dst')), Self);

    Method := TLapeTree_Method.Create(Result, FStackInfo, Self);
    Method.Statements := TLapeTree_StatementList.Create(Self);
    Method.Statements.addStatement(Assignment);
    FMethods.addStatement(Method);
  finally
    DecStackInfo([lsfFunction], Method = nil);
  end;
end;

function TLapeGeneratedMethods.GetMethod_ArraySort(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (not (Length(AParams) in [1, 2])) then
    Exit;

  if (Length(AParams) = 1) then
    Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0]], [lptVar], [TLapeGlobalVar(nil)], nil, '_ArraySort')) as TLapeType_Method
  else
    Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1]], [lptVar, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], nil, '_ArraySort')) as TLapeType_Method;

  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                       ' + LineEnding +
    'type                                                     ' + LineEnding +
    '  TType = PType(Param0);                                 ' + LineEnding +
    'const                                                    ' + LineEnding +
    '  Gaps: TIntegerArray = [                                ' + LineEnding +
    '    835387, 392925, 184011, 85764, 39744, 18298, 8359,   ' + LineEnding +
    '    3785, 1695, 701, 301, 132, 57, 23, 10, 4, 1          ' + LineEnding +
    '  ];                                                     ' + LineEnding +
    'var                                                      ' + LineEnding +
    '  i, j, Gap, GapLo, Len, Lo, Hi: Int32;                  ' + LineEnding +
    '  Temp: TType;                                           ' + LineEnding +
    'begin                                                    ' + LineEnding +
    '  Len := Length(Param0);                                 ' + LineEnding +
    '  Lo := Low(Param0);                                     ' + LineEnding +
    '  Hi := High(Param0);                                    ' + LineEnding +
    '                                                         ' + LineEnding +
    '  for Gap in Gaps do                                     ' + LineEnding +
    '  begin                                                  ' + LineEnding +
    '    // skip gaps larger than the subsection size         ' + LineEnding +
    '    if (Gap > Len) then                                  ' + LineEnding +
    '      Continue;                                          ' + LineEnding +
    '                                                         ' + LineEnding +
    '    GapLo := Lo + Gap;                                   ' + LineEnding +
    '    for i := GapLo to Hi do                              ' + LineEnding +
    '    begin                                                ' + LineEnding +
    '      Temp := Param0[i];                                 ' + LineEnding +
    '      j := i;                                            ' + LineEnding +
    '      while (j >= GapLo) and                             ' + LineEnding +
    '        {$IFPARAM Param1}                                ' + LineEnding +
    '        (Param1(Param0[j - Gap], Temp) > 0)              ' + LineEnding +
    '        {$ELSE}                                          ' + LineEnding +
    '        (Param0[j - Gap] > Temp)                         ' + LineEnding +
    '        {$ENDIF} do                                      ' + LineEnding +
    '      begin                                              ' + LineEnding +
    '        Param0[j] := Param0[j - Gap];                    ' + LineEnding +
    '        j := j - Gap;                                    ' + LineEnding +
    '      end;                                               ' + LineEnding +
    '      Param0[j] := Temp;                                 ' + LineEnding +
    '    end;                                                 ' + LineEnding +
    '  end;                                                   ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayRange(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 3) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := addManagedType(TLapeType_Method.Create(Self, [AParams[0], AParams[1], AParams[2]], [lptNormal, lptOut, lptOut], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], getBaseType(ltInt32), '_ArrayRange')) as TLapeType_Method;
  Result := MakeMethod(Header,
    'begin                                    ' + LineEnding +
    '  Result := Length(Param0);              ' + LineEnding +
    '  if (Result > 0) then                   ' + LineEnding +
    '  begin                                  ' + LineEnding +
    '    Param1 := @Param0[Low(Param0)];      ' + LineEnding +
    '    Param2 := @Param0[High(Param0)];     ' + LineEnding +
    '  end;                                   ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayMinMax(AName, Op: lpString; AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader(AName, AParams, TLapeType_DynArray(AParams[0]).PType);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                              ' + LineEnding +
    'type                                            ' + LineEnding +
    '  TType = PType(Param0);                        ' + LineEnding +
    'var                                             ' + LineEnding +
    '  Ptr, Upper: ^TType;                           ' + LineEnding +
    'begin                                           ' + LineEnding +
    '  case _ArrayRange(Param0, Ptr, Upper) of       ' + LineEnding +
    '    0: Result := Default(TType);                ' + LineEnding +
    '    1: Result := Param0[Low(Param0)];                     ' + LineEnding +
    '    else                                        ' + LineEnding +
    '    begin                                       ' + LineEnding +
    '      Result := Param0[Low(Param0)];                      ' + LineEnding +
    '      while (PtrUInt(Ptr) <= PtrUInt(Upper)) do ' + LineEnding +
    '      begin                                     ' + LineEnding +
    '        if (Ptr^ ' + Op + ' Result) then                 ' + LineEnding +
    '          Result := Ptr^;                       ' + LineEnding +
    '        Inc(Ptr);                               ' + LineEnding +
    '      end;                                      ' + LineEnding +
    '    end;                                        ' + LineEnding +
    '  end;                                          ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayUnique(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  ResType: TLapeType;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  // Copy() creates dynamic arrays from static arrays
  if (AParams[0] is TLapeType_StaticArray) then
    ResType := TLapeType_DynArray(AParams[0]).PType.ArrayOf
  else
    ResType := AParams[0];

  Header := ValueHeader('_ArrayUnique', AParams, ResType);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                 ' + LineEnding +
    'type                                               ' + LineEnding +
    '  TType = PType(Param0);                           ' + LineEnding +
    'var                                                ' + LineEnding +
    '  Ptr, Upper: ^TType;                              ' + LineEnding +
    '  I, NewLen: Int32;                                ' + LineEnding +
    'begin                                              ' + LineEnding +
    '  Result := Copy(Param0);                          ' + LineEnding +
    '  if (_ArrayRange(Result, Ptr, Upper) <= 1) then   ' + LineEnding +
    '    Exit;                                          ' + LineEnding +
    '                                                   ' + LineEnding +
    '  while (PtrUInt(Ptr) <= PtrUInt(Upper)) do        ' + LineEnding +
    '  begin                                            ' + LineEnding +
    '    I := 0;                                        ' + LineEnding +
    '    while (I < NewLen) and (Ptr^ <> Result[I]) do  ' + LineEnding +
    '      Inc(I);                                      ' + LineEnding +
    '                                                   ' + LineEnding +
    '    if (I = NewLen) then                           ' + LineEnding +
    '    begin                                          ' + LineEnding +
    '      Result[NewLen] := Ptr^;                      ' + LineEnding +
    '      Inc(NewLen);                                 ' + LineEnding +
    '    end;                                           ' + LineEnding +
    '                                                   ' + LineEnding +
    '    Inc(Ptr);                                      ' + LineEnding +
    '  end;                                             ' + LineEnding +
    '                                                   ' + LineEnding +
    '  SetLength(Result, NewLen);                       ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayIndexOf(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 2) or (not (AParams[1] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader('_ArrayIndexOf', AParams, getBaseType(ltInt32));
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                        ' + LineEnding +
    'var                                       ' + LineEnding +
    '  i: Int32;                               ' + LineEnding +
    'begin                                     ' + LineEnding +
    '  for i := Low(Param1) to High(Param1) do ' + LineEnding +
    '    if (Param1[i] = Param0) then          ' + LineEnding +
    '      Exit(i);                            ' + LineEnding +
    '  Result := -1;                           ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayIndicesOf(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 2) or (not (AParams[1] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader('_ArrayIndicesOf', AParams, getGlobalType('TIntegerArray'));
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                        ' + LineEnding +
    'var                                       ' + LineEnding +
    '  i, Count, Size: Int32;                  ' + LineEnding +
    'begin                                     ' + LineEnding +
    '  Result := [];                           ' + LineEnding +
    '  Count := Size := 0;                     ' + LineEnding +
    '  for i := Low(Param1) to High(Param1) do ' + LineEnding +
    '    if (Param1[i] = Param0) then          ' + LineEnding +
    '    begin                                 ' + LineEnding +
    '      if (Count = Size) then              ' + LineEnding +
    '      begin                               ' + LineEnding +
    '        Size := 4 + (Size * 2);           ' + LineEnding +
    '        SetLength(Result, Size);          ' + LineEnding +
    '      end;                                ' + LineEnding +
    '                                          ' + LineEnding +
    '      Result[Count] := i;                 ' + LineEnding +
    '      Inc(Count);                         ' + LineEnding +
    '    end;                                  ' + LineEnding +
    '  SetLength(Result, Count);               ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayMode(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader('_ArrayMode', AParams, TLapeType_DynArray(AParams[0]).PType);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                      ' + LineEnding +
    'type                                                    ' + LineEnding +
    '  TType = PType(Param0);                                ' + LineEnding +
    'var                                                     ' + LineEnding +
    '  Arr: array of TType;                                  ' + LineEnding +
    '  Best, Current: record Value: TType; Hits: Int32; end; ' + LineEnding +
    '  i: Int32;                                             ' + LineEnding +
    'begin                                                   ' + LineEnding +
    '  if (Length(Param0) = 0) then                          ' + LineEnding +
    '    Exit(Default(Result));                              ' + LineEnding +
    '                                                        ' + LineEnding +
    '  Arr := Sorted(Param0);                                ' + LineEnding +
    '  Best := [];                                           ' + LineEnding +
    '  Current := [Arr[0], 1];                               ' + LineEnding +
    '                                                        ' + LineEnding +
    '  for i := 1 to High(Arr) do                            ' + LineEnding +
    '  begin                                                 ' + LineEnding +
    '    if (Arr[i] <> Current.Value) then                   ' + LineEnding +
    '    begin                                               ' + LineEnding +
    '      if (Current.Hits > Best.Hits) then                ' + LineEnding +
    '        Best := Current;                                ' + LineEnding +
    '      Current := [Arr[i], 1];                           ' + LineEnding +
    '    end else                                            ' + LineEnding +
    '      Current.Hits := Current.Hits + 1;                 ' + LineEnding +
    '  end;                                                  ' + LineEnding +
    '                                                        ' + LineEnding +
    '  if (Current.Hits > Best.Hits) then                    ' + LineEnding +
    '     Best := Current;                                   ' + LineEnding +
    '  Result := Best.Value;                                 ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArraySum(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  ResType: TLapeType;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeRealTypes) then
    ResType := getBaseType(ltDouble)
  else
  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeIntegerTypes) then
    ResType := getBaseType(ltInt64)
  else
    ResType := TLapeType_DynArray(AParams[0]).PType;

  Header := ValueHeader('_ArraySum', AParams, ResType);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                 ' + LineEnding +
    'type                                               ' + LineEnding +
    '  TType = PType(Param0);                           ' + LineEnding +
    'var                                                ' + LineEnding +
    '  Ptr, Upper: ^TType;                              ' + LineEnding +
    'begin                                              ' + LineEnding +
    '  Result := Default(Result);                       ' + LineEnding +
    '  if (_ArrayRange(Param0, Ptr, Upper) > 0) then    ' + LineEnding +
    '    while (PtrUInt(Ptr) <= PtrUInt(Upper)) do      ' + LineEnding +
    '    begin                                          ' + LineEnding +
    '      Result := Result + Ptr^;                     ' + LineEnding +
    '      Inc(Ptr);                                    ' + LineEnding +
    '    end;                                           ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayMean(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  ResType: TLapeType;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(AParams[0]).PType;

  Header := ValueHeader('_ArrayMean', AParams, ResType);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                ' + LineEnding +
    'begin                                             ' + LineEnding +
    '  if (Length(Param0) > 0) then                    ' + LineEnding +
    '    Result := ArraySum(Param0) / Length(Param0)   ' + LineEnding +
    '  else                                            ' + LineEnding +
    '    Result := Default(Result);                    ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayMedian(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
  ResType: TLapeType;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  if (TLapeType_DynArray(AParams[0]).PType.BaseType in LapeRealTypes+LapeIntegerTypes) then
    ResType := getBaseType(ltDouble)
  else
    ResType := TLapeType_DynArray(AParams[0]).PType;

  Header := ValueHeader('_ArrayMedian', AParams, ResType);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                     ' + LineEnding +
    'type                                                   ' + LineEnding +
    '  TType = PType(Param0);                               ' + LineEnding +
    'var                                                    ' + LineEnding +
    '  Arr: array of TType;                                 ' + LineEnding +
    '  Mid: Double;                                         ' + LineEnding +
    'begin                                                  ' + LineEnding +
    '  if (Length(Param0) < 2) then                         ' + LineEnding +
    '    raise "ArrayMedian requires at least two values";  ' + LineEnding +
    '  Arr := Sorted(Param0);                               ' + LineEnding +
    '  Mid := (Length(Arr) - 1) / 2;                        ' + LineEnding +
    '  Result := (Arr[Trunc(Mid)] + Arr[Ceil(Mid)]) / 2;    ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayVariance(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader('_ArrayVariance', AParams, getBaseType(ltDouble));
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                             ' + LineEnding +
    'type                                           ' + LineEnding +
    '  TType = PType(Param0);                       ' + LineEnding +
    'var                                            ' + LineEnding +
    '  Mean, Square: Double;                        ' + LineEnding +
    '  i: Int32;                                    ' + LineEnding +
    'begin                                          ' + LineEnding +
    '  if Length(Param0) = 0 then                   ' + LineEnding +
    '    Exit(Default(Result));                     ' + LineEnding +
    '                                               ' + LineEnding +
    '  Mean := ArrayMean(Param0);                   ' + LineEnding +
    '  for i := Low(Param0) to High(Param0) do      ' + LineEnding +
    '    Square := Square + Sqr(Param0[i] - Mean);  ' + LineEnding +
    '  Result := Square / Length(Param0);           ' + LineEnding +
    'end;                                           '
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayStdev(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 1) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader('_ArrayStdev', AParams, getBaseType(ltDouble));
  Result := MakeMethod(Header,
    'begin                                     ' + LineEnding +
    '  Result := Sqrt(ArrayVariance(Param0));  ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayDifference(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 2) then
    Exit;

  Header := ValueHeader('_ArrayDifference', AParams, AParams[0]);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                    ' + LineEnding +
    'var                                   ' + LineEnding +
    '  Val: PType(Param0);                 ' + LineEnding +
    '  Count: Int32 = 0;                   ' + LineEnding +
    'begin                                 ' + LineEnding +
    '  SetLength(Result, Length(Param0));  ' + LineEnding +
    '  for Val in Param0 do                ' + LineEnding +
    '    if IndexOf(Val, Param1) = -1 then ' + LineEnding +
    '    begin                             ' + LineEnding +
    '      Result[Count] := Val;           ' + LineEnding +
    '      Count := Count + 1;             ' + LineEnding +
    '    end;                              ' + LineEnding +
    '  SetLength(Result, Count);           ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArraySymDifference(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 2) then
    Exit;

  Header := ValueHeader('_ArraySymDifference', AParams, AParams[0]);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                    ' + LineEnding +
    'var                                                   ' + LineEnding +
    '  Val: PType(Param0);                                 ' + LineEnding +
    '  Count: Int32 = 0;                                   ' + LineEnding +
    'begin                                                 ' + LineEnding +
    '  SetLength(Result, Length(Param0) + Length(Param1)); ' + LineEnding +
    '  for Val in Param0 do                                ' + LineEnding +
    '    if IndexOf(val, Param1) = -1 then                 ' + LineEnding +
    '    begin                                             ' + LineEnding +
    '      Result[Count] := Val;                           ' + LineEnding +
    '      Count := Count + 1;                             ' + LineEnding +
    '    end;                                              ' + LineEnding +
    '  for Val in Param1 do                                ' + LineEnding +
    '    if IndexOf(val, Param0) = -1 then                 ' + LineEnding +
    '    begin                                             ' + LineEnding +
    '      Result[Count] := Val;                           ' + LineEnding +
    '      Count := Count + 1;                             ' + LineEnding +
    '    end;                                              ' + LineEnding +
    '  SetLength(Result, Count);                           ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayIntersection(AParams: TLapeTypeArray): TLapeGlobalVar;
var
  Header: TLapeType_Method;
begin
  Result := nil;
  if (Length(AParams) <> 2) then
    Exit;

  Header := ValueHeader('_ArrayIntersection', AParams, AParams[0]);
  Result := MakeMethod(Header,
    '{$RANGECHECKS OFF}                                        ' + LineEnding +
    'var                                                       ' + LineEnding +
    '  Val: PType(Param0);                                     ' + LineEnding +
    '  Count: Int32 = 0;                                       ' + LineEnding +
    'begin                                                     ' + LineEnding +
    '  SetLength(Result, Min(Length(Param0), Length(Param1))); ' + LineEnding +
    '  for Val in Param0 do                                    ' + LineEnding +
    '    if IndexOf(Val, Param1) > -1 then                     ' + LineEnding +
    '    begin                                                 ' + LineEnding +
    '      Result[Count] := Val;                               ' + LineEnding +
    '      Count := Count + 1;                                 ' + LineEnding +
    '    end;                                                  ' + LineEnding +
    '  SetLength(Result, Count);                               ' + LineEnding +
    'end;'
  );
end;

function TLapeGeneratedMethods.GetMethod_ArrayEquals(AParams: TLapeTypeArray): TLapeGlobalVar;

  function IsMultiDimensional: Boolean;
  begin
    Result := TLapeType_DynArray(AParams[0]).PType is TLapeType_DynArray;
  end;

  function CanCompareMem: Boolean;
  var
    ElementType: TLapeType;
  begin
    Result := False;
    if (AParams[0].ClassType = TLapeType_DynArray) then
    begin
      ElementType := TLapeType_DynArray(AParams[0]).PType;
      if (ElementType <> nil) and (not ElementType.NeedInitialization) then
        if (ElementType.BaseType in LapeStackTypes + [ltPointer]) then
          Result := True
        else if (ElementType is TLapeType_Record) and (TLapeType_Record(ElementType).Padding = 0) then
          Result := True;
    end;
  end;

var
  Header: TLapeType_Method;
  Body: lpString;
begin
  Result := nil;
  if (Length(AParams) <> 2) or (not (AParams[0] is TLapeType_DynArray)) then
    Exit;

  Header := ValueHeader('_ArrayEquals', AParams, getBaseType(ltBoolean));

  if IsMultiDimensional() then
    Body := '{$RANGECHECKS OFF}'                                   + LineEnding +
            'var i: SizeInt;'                                      + LineEnding +
            'begin'                                                + LineEnding +
            '  Result := False;'                                   + LineEnding +
            '  if (Length(Param0) = Length(Param1)) then'          + LineEnding +
            '  begin'                                              + LineEnding +
            '    for i := 0 to High(Param0) do'                    + LineEnding +
            '      if not ArrayEquals(Param0[i], Param1[i]) then'  + LineEnding +
            '        Exit;'                                        + LineEnding +
            '    Result := True;'                                  + LineEnding +
            '  end;'                                               + LineEnding +
            'end;'
  else if CanCompareMem() then
    Body := '{$RANGECHECKS OFF}'                                                                          + LineEnding +
            'var Len1 := Length(Param0);'                                                                 + LineEnding +
            'var Len2 := Length(Param1);'                                                                 + LineEnding +
            'begin'                                                                                       + LineEnding +
            '  if (Len1 = Len2) then'                                                                     + LineEnding +
            '    Result := (Len1 = 0) or CompareMem(Param0[0], Param1[0], Len1 * SizeOf(PType(Param0)))'  + LineEnding +
            '  else'                                                                                      + LineEnding +
            '    Result := False;'                                                                        + LineEnding +
            'end;'
  else
    Body := '{$RANGECHECKS OFF}'                                 + LineEnding +
            'type'                                               + LineEnding +
            '  TType = PType(Param0);'                           + LineEnding +
            '  TOtherType = PType(Param1);'                      + LineEnding +
            'var'                                                + LineEnding +
            '  Ptr, Upper: ^TType;'                              + LineEnding +
            '  OtherPtr: ^TOtherType;'                           + LineEnding +
            'begin'                                              + LineEnding +
            '  Result := False;'                                 + LineEnding +
            ''                                                   + LineEnding +
            '  if (Length(Param0) = Length(Param1)) then'        + LineEnding +
            '  begin'                                            + LineEnding +
            '    if (_ArrayRange(Param0, Ptr, Upper) > 0) then'  + LineEnding +
            '    begin'                                          + LineEnding +
            '      OtherPtr := @Param1[Low(Param1)];'            + LineEnding +
            '      while (PtrUInt(Ptr) <= PtrUInt(Upper)) do'    + LineEnding +
            '      begin'                                        + LineEnding +
            '        if (Ptr^ <> OtherPtr^) then'                + LineEnding +
            '          Exit;'                                    + LineEnding +
            '        Inc(Ptr);'                                  + LineEnding +
            '        Inc(OtherPtr);'                             + LineEnding +
            '      end;'                                         + LineEnding +
            '    end;'                                           + LineEnding +
            ''                                                   + LineEnding +
            '    Result := True;'                                + LineEnding +
            '  end;'                                             + LineEnding +
            'end;';

  Result := MakeMethod(Header, Body);
end;

function TLapeGeneratedMethods.Generate(AName: lpString; AObjectType: TLapeType; AParams: TLapeTypeArray; AResult: TLapeType): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := nil;
  if (AObjectType <> nil) then
    Exit;

  // (a name of several methods has no type to make one for)
  for i := 0 to High(AParams) do
    if (AParams[i] = nil) then
      Exit;

  if LapeSameString(AName, 'ToString') then
    Result := GetToStringMethod(AParams, AResult)
  else if LapeSameString(AName, '_DisposeObject') then
    Result := GetDisposeObjectMethod(AParams, AResult)
  else if LapeSameString(AName, '_Assign') then
    Result := GetCopyMethod(AParams, AResult)
  else if LapeSameString(AName, '_Objectify') then
    Result := GetObjectifyMethod(AParams)
  else if LapeSameString(AName, '_ArraySort') then
    Result := GetMethod_ArraySort(AParams)
  else if LapeSameString(AName, '_ArrayRange') then
    Result := GetMethod_ArrayRange(AParams)
  else if LapeSameString(AName, '_ArrayMin') then
    Result := GetMethod_ArrayMinMax('_ArrayMin', '<', AParams)
  else if LapeSameString(AName, '_ArrayMax') then
    Result := GetMethod_ArrayMinMax('_ArrayMax', '>', AParams)
  else if LapeSameString(AName, '_ArrayUnique') then
    Result := GetMethod_ArrayUnique(AParams)
  else if LapeSameString(AName, '_ArrayIndexOf') then
    Result := GetMethod_ArrayIndexOf(AParams)
  else if LapeSameString(AName, '_ArrayIndicesOf') then
    Result := GetMethod_ArrayIndicesOf(AParams)
  else if LapeSameString(AName, '_ArrayMode') then
    Result := GetMethod_ArrayMode(AParams)
  else if LapeSameString(AName, '_ArraySum') then
    Result := GetMethod_ArraySum(AParams)
  else if LapeSameString(AName, '_ArrayMean') then
    Result := GetMethod_ArrayMean(AParams)
  else if LapeSameString(AName, '_ArrayMedian') then
    Result := GetMethod_ArrayMedian(AParams)
  else if LapeSameString(AName, '_ArrayVariance') then
    Result := GetMethod_ArrayVariance(AParams)
  else if LapeSameString(AName, '_ArrayStdev') then
    Result := GetMethod_ArrayStdev(AParams)
  else if LapeSameString(AName, '_ArrayDifference') then
    Result := GetMethod_ArrayDifference(AParams)
  else if LapeSameString(AName, '_ArraySymDifference') then
    Result := GetMethod_ArraySymDifference(AParams)
  else if LapeSameString(AName, '_ArrayIntersection') then
    Result := GetMethod_ArrayIntersection(AParams)
  else if LapeSameString(AName, '_ArrayEquals') then
    Result := GetMethod_ArrayEquals(AParams);
end;

procedure TLapeGeneratedMethods.addGeneratedMethods;
var
  BaseType: ELapeBaseType;
begin
  for BaseType := Low(ELapeBaseType) to High(ELapeBaseType) do
    if ({$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType] <> nil) then
      addMade(
        TLapeType_Method(addManagedType(
          TLapeType_Method.Create(
            Self,
            [getBaseType(BaseType)],
            [lptConstRef],
            [TLapeGlobalVar(nil)],
            getBaseType(ltString),
            'ToString'
          )
        )).NewGlobalVar(
          {$IFNDEF FPC}@{$ENDIF}LapeToStrArr[BaseType]
        )
      );

  addMethodGenerate({$IFDEF FPC}@{$ENDIF}Generate);
end;

procedure LapeAddGeneratedMethods(Compiler: TLapeCompiler);
begin
  TLapeGeneratedMethods(Compiler).addGeneratedMethods();
end;

end.
