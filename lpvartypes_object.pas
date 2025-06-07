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
    Offset: Word;
    FieldType: TLapeType;
  end;
  TObjectFieldMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TObjectField>;

  TLapeType_Object = class(TLapeType_DynArray)
  protected
    FFieldMap: TObjectFieldMap;
    FTotalFieldSize: Integer;

    function getAsString: lpString; override;
  public
    FreeFieldMap: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TObjectFieldMap = nil; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;
    destructor Destroy; override;

    procedure ClearCache; override;
    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;

    procedure addField(FieldType: TLapeType; AName: lpString); virtual;
    procedure addConstField(FieldType: TLapeType; FieldValue: TLapeGlobalVar; AName: lpString); virtual;

    function HasChild(AName: lpString): Boolean; overload; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
    function EvalConst(Op: EOperator; ALeft, ARight: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;

    procedure addArrayHelpers; override;

    property FieldMap: TObjectFieldMap read FFieldMap;
    property TotalFieldSize: Integer read FTotalFieldSize;
  end;

  function hasObject(Typ: TLapeType): Boolean;

implementation

uses
  lpmessages, lpvartypes_record;

function hasObject(Typ: TLapeType): Boolean;

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

constructor TLapeType_Object.Create(ACompiler: TLapeCompilerBase; AFieldMap: TObjectFieldMap; AName: lpString; ADocPos: PDocPos);
const
  InvalidRec: TObjectField = (Offset: Word(-1); FieldType: nil);
begin
  inherited Create(ACompiler.getBaseType(ltUInt8), ACompiler, AName, ADocPos);

  FreeFieldMap := (AFieldMap = nil);
  if (AFieldMap = nil) then
    AFieldMap := TObjectFieldMap.Create(InvalidRec, dupError, False);
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

function TLapeType_Object.CreateCopy(DeepCopy: Boolean): TLapeType;
type
  TLapeClassType = class of TLapeType_Object;
begin
  if DeepCopy then
  begin
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, nil, Name);
    TLapeType_Object(Result).FFieldMap.ImportFromArrays(FFieldMap.ExportToArrays());
  end
  else
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, FFieldMap, Name);

  with TLapeType_Object(Result) do
  begin
    inheritManagedDecls(Self, not DeepCopy);
    TypeID := Self.TypeID;
    FTotalFieldSize := Self.FTotalFieldSize;
    CopyHints(Self);
  end;
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

  addSubDeclaration(Field);
end;

function TLapeType_Object.HasChild(AName: lpString): Boolean;
begin
  Result := FFieldMap.ExistsKey(AName) or HasSubDeclaration(AName, bTrue);
end;

function TLapeType_Object.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeType;
begin
  if (Op = op_Dot) and ValidFieldName(Right) and FFieldMap.ExistsKey(PlpString(Right.Ptr)^) then
    Result := FFieldMap[PlpString(Right.Ptr)^].FieldType
  else
    Result := inherited;
end;

function TLapeType_Object.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos): TResVar;
var
  Field: TObjectField;
begin
  if (Op = op_Dot) and ValidFieldName(Right) and FFieldMap.ExistsKey(PlpString(Right.VarPos.GlobalVar.Ptr)^) then
  begin
    Assert(FCompiler <> nil);
    Assert(Left.VarType = Self);

    Result := NullResVar;
    Dest := NullResVar;
    Field := FFieldMap[PlpString(Right.VarPos.GlobalVar.Ptr)^];

    Left.VarType := FCompiler.getBaseType(ltPointer);

    Result := Left.VarType.Eval(op_Plus, Dest, Left, _ResVar.New(FCompiler.getConstant(Field.Offset)), Flags, Offset, Pos);
    Result := Result.VarType.Eval(op_Deref, Dest, Result, NullResVar, [], Offset, Pos);
    Result.CopyFlags(Left);
    Result.VarType := Field.FieldType;

    Left.VarType := Self;
  end else
    Result := inherited;
end;

function TLapeType_Object.EvalConst(Op: EOperator; ALeft, ARight: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  if (Op = op_Dot) and (ALeft <> nil) and ValidFieldName(ARight) and FFieldMap.ExistsKey(PlpString(ARight.Ptr)^) then
  begin
    with FFieldMap[PlpString(ARight.Ptr)^] do
    begin
      Result := FieldType.NewGlobalVarP(Pointer(PtrUInt(ALeft.Ptr) + Offset));
      Result.CopyFlags(ALeft);
    end
  end else
    Result := inherited;
end;

procedure TLapeType_Object.addArrayHelpers;
begin
  { nothing }
end;


end.

