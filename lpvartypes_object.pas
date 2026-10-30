{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
unit lpvartypes_object;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpvartypes_array;

type
  TObjectField = record
    Offset: SizeInt;
    FieldType: TLapeType;
  end;
  TObjectFieldMap = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TObjectField>;

  TLapeType_Object = class(TLapeType_DynArray)
  protected
    FFieldMap: TObjectFieldMap;
    FTotalFieldSize: Integer;

    function getAsString: lpString; override;
    function HasMembers: Boolean; override;
  public
    FreeFieldMap: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TObjectFieldMap = nil; AName: lpString = ''); reintroduce;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    procedure ClearCache; override;
    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;

    procedure addField(FieldType: TLapeType; AName: lpString); virtual;
    procedure addConstField(FieldType: TLapeType; FieldValue: TLapeGlobalVar; AName: lpString); virtual;

    function EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalMemberConst(Left: TLapeGlobalVar; AName: lpString; Flags: ELapeEvalFlags = []): TLapeGlobalVar; override;
    function EvalMember(var Dest: TResVar; Left: TResVar; AName: lpString; Flags: ELapeEvalFlags; var Offset: Integer): TResVar; override;

    procedure addArrayHelpers; override;

    property FieldMap: TObjectFieldMap read FFieldMap;
    property TotalFieldSize: Integer read FTotalFieldSize;
  end;

  function HasObject(Typ: TLapeType): Boolean;

implementation

uses
  lpmessages, lpvartypes_record;

function HasObject(Typ: TLapeType): Boolean;

  function Check(Typ: TLapeType): Boolean;
  var
    i: Integer;
  begin
    if (Typ is TLapeType_Object) then
      Exit(True);

    if (Typ is TLapeType_Record) then
      for i := 0 to TLapeType_Record(Typ).FieldMap.Count - 1 do
        if Check(TLapeType_Record(Typ).FieldMap.ItemsI[i].FieldType) then
          Exit(True);

    if (Typ is TLapeType_DynArray) then
      if Check(TLapeType_DynArray(Typ).PType) then
        Exit(True);
    Result := False;
  end;

begin
  Result := Check(Typ);
end;

function TLapeType_Object.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := 'object ';
    for i := 0 to FFieldMap.Count - 1 do
      FAsString := FAsString + '[' + lpString(IntToStr(FFieldMap.ItemsI[i].Offset)) + ']' + FFieldMap.Key[i] + ': ' + FFieldMap.ItemsI[i].FieldType.AsString + '; ';
    FAsString := FAsString + 'end';
  end;
  Result := inherited;
end;

function TLapeType_Object.HasMembers: Boolean;
begin
  Result := (FFieldMap.Count > 0) or inherited;
end;

constructor TLapeType_Object.Create(ACompiler: TLapeCompilerBase; AFieldMap: TObjectFieldMap; AName: lpString);
const
  InvalidRec: TObjectField = (Offset: -1; FieldType: nil);
begin
  inherited Create(ACompiler.getBaseType(ltUInt8), ACompiler, AName);

  FreeFieldMap := (AFieldMap = nil);
  if (AFieldMap = nil) then
    AFieldMap := TObjectFieldMap.Create(InvalidRec);
  FFieldMap := AFieldMap;
end;

destructor TLapeType_Object.Destroy;
begin
  if FreeFieldMap then
    FreeAndNil(FFieldMap);

  inherited Destroy();
end;

procedure TLapeType_Object.ClearCache;
begin
  FAsString := '';
end;

function TLapeType_Object.VarToStringBody(ToStr: TLapeType_OverloadedMethod): lpString;
var
  i: Integer;
begin
  Result :=
    'begin' +
    '  if (Param0 = nil) then' +
    '    Exit("nil");' +
    '  Result := '#39'{'#39;

  for i := 0 to FFieldMap.Count - 1 do
    with FFieldMap.ItemsI[i] do
    begin
      if (i > 0) then
        Result := Result + ' + ' + #39', '#39;
      if (ToStr <> nil) and (ToStr.getMethod(getTypeArray([FieldType])) <> nil) then
        Result := Result + ' + '#39 + FFieldMap.Key[i] + ' = '#39' + System.ToString(Param0.' + FFieldMap.Key[i] + ')';
    end;

  Result := Result + ' + '#39'}'#39'; end;';
end;

function TLapeType_Object.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Object;
var
  i: Integer;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, nil, '');
  Result.DocPos := FDocPos;
  for i := 0 to FFieldMap.Count - 1 do
    TLapeType_Object(Result).FFieldMap.Add(FFieldMap.Key[i], FFieldMap.ItemsI[i]);

  with TLapeType_Object(Result) do
  begin
    FTotalFieldSize := Self.FTotalFieldSize;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

procedure TLapeType_Object.addField(FieldType: TLapeType; AName: lpString);
var
  Field: TObjectField;
begin
  Field.Offset := FTotalFieldSize;
  Field.FieldType := FieldType;

  FFieldMap[AName] := Field;

  Inc(FTotalFieldSize, FieldType.Size);
end;

procedure TLapeType_Object.addConstField(FieldType: TLapeType; FieldValue: TLapeGlobalVar; AName: lpString);
var
  Field: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);

  Field := TLapeGlobalVar.Create(FieldType);
  Field.Name := AName;
  if (FieldValue <> nil) then
    FieldType.EvalConst(op_Assign, Field, FieldValue, [lefAssigning]);
  Field.isConstant := True;

  addDeclaration(Field);
end;

function TLapeType_Object.EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if FFieldMap.ExistsKey(AName) then
    Result := FFieldMap[AName].FieldType
  else
    Result := inherited;
end;

function TLapeType_Object.EvalMemberConst(Left: TLapeGlobalVar; AName: lpString; Flags: ELapeEvalFlags = []): TLapeGlobalVar;
begin
  if FFieldMap.ExistsKey(AName) then
    Result := nil
  else
    Result := inherited;
end;

function TLapeType_Object.EvalMember(var Dest: TResVar; Left: TResVar; AName: lpString; Flags: ELapeEvalFlags; var Offset: Integer): TResVar;
var
  Field: TObjectField;
begin
  if FFieldMap.ExistsKey(AName) then
  begin
    Assert(FCompiler <> nil);
    Assert(Left.VarType = Self);

    Result := NullResVar;
    Dest := NullResVar;
    Field := FFieldMap[AName];

    Left.VarType := FCompiler.getBaseType(ltPointer);

    Result := Left.VarType.Eval(op_Plus, Dest, Left, _ResVar.New(FCompiler.getIntConst(Field.Offset)), Flags, Offset);
    Result := Result.VarType.Eval(op_Deref, Dest, Result, NullResVar, [], Offset);
    Result.isConstant := Left.isConstant;
    Result.VarType := Field.FieldType;

    Left.VarType := Self;
  end else
    Result := inherited;
end;

procedure TLapeType_Object.addArrayHelpers;
begin
  { nothing }
end;


end.

