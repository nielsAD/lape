{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All (script)type and (script)variable classes, including corresponding evaluation functions (runtime/compile time).
}
unit lpvartypes_record;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes;

type
  TRecordField = record
    Offset: SizeInt;
    FieldType: TLapeType;
  end;
  TRecordFieldMap = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<TRecordField>;

  TLapeType_Record = class(TLapeType)
  protected
    FAlignment: UInt16;
    FFieldMap: TRecordFieldMap;
    function getAsString: lpString; override;
    function getPadding: SizeInt; override;
    function getSize: SizeInt; override;
  public
    FreeFieldMap: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    procedure ClearCache; override;
    procedure addField(FieldType: TLapeType; AName: lpString; AAlignment: UInt16 = 1); virtual;
    procedure addConstField(FieldType: TLapeType; FieldValue: TLapeGlobalVar; AName: lpString); virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;
    function NewGlobalVar(AName: lpString = ''): TLapeGlobalVar; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalMemberConst(Left: TLapeGlobalVar; AName: lpString; Flags: ELapeEvalFlags = []): TLapeGlobalVar; override;
    function EvalMember(var Dest: TResVar; Left: TResVar; AName: lpString; Flags: ELapeEvalFlags): TResVar; override;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;
    procedure FinalizeConst(AVar: TLapeGlobalVar); override;

    property FieldMap: TRecordFieldMap read FFieldMap;
    property Alignment: UInt16 read FAlignment;
  end;

  TLapeType_Union = class(TLapeType_Record)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''); override;
    procedure addField(FieldType: TLapeType; AName: lpString; AAlignment: UInt16 = 1); override;
  end;

implementation

uses
  Math,
  lpvartypes_array, lptokenizer, lpeval, lpmessages;

function TLapeType_Record.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := 'record ';
    for i := 0 to FFieldMap.Count - 1 do
      FAsString := FAsString + '[' + lpString(IntToStr(FFieldMap.ItemsI[i].Offset)) + ']' + FFieldMap.Key[i] + ': ' + FFieldMap.ItemsI[i].FieldType.AsString + '; ';
    FAsString := FAsString + 'end';
  end;
  Result := inherited;
end;

function TLapeType_Record.getSize: SizeInt;
begin
  if (FSize > 0) then
    Result := (inherited + (FAlignment - 1)) and not SizeInt(FAlignment - 1)
  else
    Result := 0;
end;

function TLapeType_Record.getPadding: SizeInt;
var
  i: Integer;
  Offset: SizeInt;
begin
  Result := 0;
  Offset := Size;

  for i := FFieldMap.Count - 1 downto 0 do
  begin
    Inc(Result, Offset - (FFieldMap.ItemsI[i].Offset + FFieldMap.ItemsI[i].FieldType.Size));
    Inc(Result, FFieldMap.ItemsI[i].FieldType.Padding);
    Offset := FFieldMap.ItemsI[i].Offset;
  end;
end;

constructor TLapeType_Record.Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = '');
const
  InvalidRec: TRecordField = (Offset: -1; FieldType: nil);
begin
  inherited Create(ltRecord, ACompiler, AName);

  FSize := 0;
  FreeFieldMap := (AFieldMap = nil);
  if (AFieldMap = nil) then
    AFieldMap := TRecordFieldMap.Create(InvalidRec);
  FFieldMap := AFieldMap;
  FAlignment := 1;
end;

destructor TLapeType_Record.Destroy;
begin
  if FreeFieldMap then
    FFieldMap.Free();
  inherited;
end;

procedure TLapeType_Record.ClearCache;
begin
  FAsString := '';
end;

procedure TLapeType_Record.addField(FieldType: TLapeType; AName: lpString; AAlignment: UInt16 = 1);

  //http://docwiki.embarcadero.com/RADStudio/en/Internal_Data_Formats#Record_Types
  function AlignmentMask(Typ: TLapeType): UInt16;
  begin
    if (Typ = nil) then
      Exit(AAlignment);

    if (Typ.BaseType in LapeOrdinalTypes + LapePointerTypes) then
      Result := Typ.Size
    else
    case Typ.BaseType of
      ltLargeSet:    Result := SizeOf(UInt8);
      {$IFNDEF Lape_NoExtended}
      ltExtended:    Result := SizeOf(Extended) and (SizeOf(Double) or (2*SizeOf(Double)));
      {$ENDIF}
      ltShortString,
      ltStaticArray: Result := AlignmentMask(TLapeType_StaticArray(Typ).PType);
      ltRecord:      Result := TLapeType_Record(Typ).Alignment;
      ltSingle,
      ltDouble,
      ltSmallSet:    Result := Typ.Size;
      else
        Result := AAlignment;
    end;

    if (Result < 1) then
      Result := 1;
  end;

var
  Field: TRecordField;
  FieldSize: SizeInt;
begin
  if (FSize < 0) or (FFieldMap.Count < 1) then
    FSize := 0;
  if (FInit = bUnknown) or (FFieldMap.Count < 1) then
    FInit := bFalse;
  if FFieldMap.ExistsKey(AName) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName]);

  FieldSize := FieldType.Size;
  if (AAlignment > 1) then
  begin
    AAlignment := Min(AAlignment, AlignmentMask(FieldType));
    FAlignment := Max(FAlignment, AAlignment);

    Dec(AAlignment);
    FSize := (FSize + AAlignment) and not SizeInt(AAlignment);
  end;

  Field.Offset := FSize;
  Field.FieldType := FieldType;

  FSize := FSize + FieldSize;
  if (FInit <> bTrue) and FieldType.NeedInitialization then
    FInit := bTrue;
  FFieldMap[AName] := Field;

  ClearCache();
end;

procedure TLapeType_Record.addConstField(FieldType: TLapeType; FieldValue: TLapeGlobalVar; AName: lpString);
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

function TLapeType_Record.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  i: Integer;
begin
  Result := 'begin Result := '#39'{'#39;
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

function TLapeType_Record.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to FFieldMap.Count - 1 do
  begin
    if (i > 0) then
      Result := Result + ', ';
    Result := Result + FFieldMap.Key[i] + ' = ' + FFieldMap.ItemsI[i].FieldType.VarToString(Pointer(PtrUInt(AVar) + FFieldMap.ItemsI[i].Offset));
  end;
  Result := Result + '}';
end;

function TLapeType_Record.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Record;
var
  i: Integer;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, nil, '');
  Result.DocPos := FDocPos;
  for i := 0 to FFieldMap.Count - 1 do
    TLapeType_Record(Result).FieldMap.Add(FFieldMap.Key[i], FFieldMap.ItemsI[i]);

  with TLapeType_Record(Result) do
  begin
    FInit := Self.FInit;
    FSize := Self.FSize;
    FAlignment := Self.FAlignment;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_Record.NewGlobalVar(AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
end;

function TLapeType_Record.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
var
  i: Integer;
begin
  if (Right <> nil) and (Right is TLapeType_Record) and  (TLapeType_Record(Right).FieldMap.Count = FFieldMap.Count) then
    if (op = op_Assign) then
    begin
      for i := 0 to FFieldMap.Count - 1 do
        if (not LapeSameString(FFieldMap.Key[i], TLapeType_Record(Right).FieldMap.Key[i])) or
           (not FFieldMap.ItemsI[i].FieldType.CompatibleWith(TLapeType_Record(Right).FieldMap.ItemsI[i].FieldType))
        then
        begin
          Result := inherited;
          Exit;
        end;
      Result := Self;
    end
    else if (Op in [op_cmp_Equal, op_cmp_NotEqual]) then
    begin
      for i := 0 to FFieldMap.Count - 1 do
        if (not LapeSameString(FFieldMap.Key[i], TLapeType_Record(Right).FieldMap.Key[i])) or
           (FFieldMap.ItemsI[i].FieldType.EvalRes(Op, TLapeType_Record(Right).FieldMap.ItemsI[i].FieldType, Flags) = nil)
        then
        begin
          Result := inherited;
          Exit;
        end;
      Result := FCompiler.getBaseType(ltEvalBool);
    end
    else
      Result := inherited
  else
    Result := inherited;
end;

function TLapeType_Record.EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if FFieldMap.ExistsKey(AName) then
    Result := FFieldMap[AName].FieldType
  else
    Result := inherited;
end;

function TLapeType_Record.EvalMemberConst(Left: TLapeGlobalVar; AName: lpString; Flags: ELapeEvalFlags = []): TLapeGlobalVar;
begin
  if (Left <> nil) and FFieldMap.ExistsKey(AName) then
    with FFieldMap[AName] do
    begin
      Result := FieldType.NewGlobalVarP(Pointer(PtrUInt(Left.Ptr) + Offset));
      Result.isConstant := Left.isConstant;
      Result.Known := Left.Known;
    end
  else
    Result := inherited;
end;

function TLapeType_Record.EvalMember(var Dest: TResVar; Left: TResVar; AName: lpString; Flags: ELapeEvalFlags): TResVar;
begin
  if FFieldMap.ExistsKey(AName) then
    with FFieldMap[AName] do
    begin
      Dest := NullResVar;
      Result := Left;
      Result.VarType := FieldType;
      Result.IncOffset(Offset);
      Result.isConstant := Left.isConstant;
    end
  else
    Result := inherited;
end;

function TLapeType_Record.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  i: Integer;
  LeftVar, RightVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType = Self));

  if (op = op_Assign) and (Right <> nil) and Right.HasType() and CompatibleWith(Right.VarType) then
  begin
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FFieldMap.Count - 1 do
    try
      LeftVar := EvalMemberConst(Left, FFieldMap.Key[i]);
      RightVar := Right.VarType.EvalMemberConst(Right, TLapeType_Record(Right.VarType).FieldMap.Key[i]);
      LeftVar.VarType.EvalConst(op, LeftVar, RightVar, []);
    finally
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;

    Result := Left;
  end
  else if (op in [op_cmp_Equal, op_cmp_NotEqual]) and (Right <> nil) and Right.HasType() and (EvalRes(op, Right, Flags) <> nil) then
  begin
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FFieldMap.Count - 1 do
    try
      LeftVar := EvalMemberConst(Left, FFieldMap.Key[i]);
      RightVar := Right.VarType.EvalMemberConst(Right, TLapeType_Record(Right.VarType).FieldMap.Key[i]);
      Result := LeftVar.VarType.EvalConst(op, LeftVar, RightVar, []);

      if (not Result.HasType()) or (not (Result.VarType.BaseType in LapeBoolTypes)) then
        LapeException(lpeImpossible);
      if (Result.AsInteger <> Ord(False)) xor (op = op_cmp_Equal) then
        Exit;
    finally
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_Record.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
var
  i: Integer;
  LoopOffset: array of Integer;
  FieldOffset: SizeInt;
  tmpVar, LeftVar, RightVar: TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType = Self);
  tmpVar := NullResVar;

  if (op = op_Assign) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (not NeedInitialization) and Equals(Right.VarType) and (Size > 0) and ((Left.VarPos.MemPos <> mpStack) or (DetermineIntType(Size, False) <> ltUnknown)) then
    begin
      Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
      begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op_Assign, Dest, Left, Right, []);
        Result.VarType := Self;
      end
      else
      begin
        RightVar := _ResVar.New(FCompiler.getIntConst(Size, ltSizeInt));
        tmpVar := Compiler.getTempStackVar(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, RightVar, NullResVar);
        FCompiler.Emitter._InvokeImportedProc(_ResVar.New(FCompiler['!move']), SizeOf(Pointer) * 3);
        Result := Left;
        Result.VarType := Self;
      end;
    end
    else
    begin
      for i := 0 to FFieldMap.Count - 1 do
      begin
        Dest := NullResVar;

        LeftVar := EvalMember(tmpVar, Left, FFieldMap.Key[i], [lefAssigning]);
        RightVar := Right.VarType.EvalMember(tmpVar, Right, TLapeType_Record(Right.VarType).FieldMap.Key[i], []);
        LeftVar.VarType.Eval(op_Assign, Dest, LeftVar, RightVar, []);

        if (LeftVar.VarPos.MemPos = mpStack) then
        begin
          if (i + 1 = FFieldMap.Count) then
            FieldOffset := Size
          else
            FieldOffset := FFieldMap.ItemsI[i + 1].Offset;
          FieldOffset := FieldOffset - (FFieldMap.ItemsI[i].Offset + FFieldMap.ItemsI[i].FieldType.Size);
          if (FieldOffset > 0) then
            FCompiler.Emitter._GrowStack(FieldOffset);
        end;

      end;
      Result := Left;
    end
  else if (op in [op_cmp_Equal, op_cmp_NotEqual]) and Right.HasType() and (EvalRes(Op, Right.VarType, Flags) <> nil) then
    if (not NeedInitialization) and Equals(Right.VarType) and (Size > 0) and (Padding = 0) then
    begin
      Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
      begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op, Dest, Left, Right, []);
      end
      else
      begin
        Assert(op in [op_cmp_Equal, op_cmp_NotEqual]);

        RightVar := _ResVar.New(FCompiler.getIntConst(Size, ltSizeInt));
        tmpVar := Compiler.getTempStackVar(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, RightVar, NullResVar);

        //Result := NullResVar;
        //Result.VarType := Compiler.getBaseType(ltEvalBool);
        //FCompiler.getDestVar(Dest, Result, op);

        Result := _ResVar.New(FCompiler.getTempVar(ltEvalBool));
        Dest := NullResVar;

        FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!cmp']), Result, SizeOf(Pointer) * 3);
        if (op = op_cmp_NotEqual) then
          FCompiler.Emitter._Eval(getEvalProc(op_NOT, ltEvalBool, ltUnknown), Result, Result, NullResVar);
      end;
    end
    else
    begin
      Result := _ResVar.New(FCompiler.getTempVar(ltEvalBool));
      Dest := NullResVar;

      SetLength(LoopOffset, FFieldMap.Count);
      for i := 0 to FFieldMap.Count - 1 do
      begin
        Dest := NullResVar;

        LeftVar := EvalMember(tmpVar, Left, FFieldMap.Key[i], []);
        RightVar := Right.VarType.EvalMember(tmpVar, Right, TLapeType_Record(Right.VarType).FieldMap.Key[i], []);

        tmpVar := Result;
        tmpVar := LeftVar.VarType.Eval(op_cmp_Equal, tmpVar, LeftVar, RightVar, []);
        if (tmpVar.VarPos.MemPos <> Result.VarPos.MemPos) or (tmpVar.VarPos.StackVar <> Result.VarPos.StackVar) then
          Result.VarType.Eval(op_Assign, Dest, Result, tmpVar, []);
        LoopOffset[i] := FCompiler.Emitter._JmpRIfNot(0, Result);

      end;

      for i := 0 to FFieldMap.Count - 1 do
        FCompiler.Emitter.PatchJump(LoopOffset[i], FCompiler.Emitter.CodeLen);

      if (op = op_cmp_NotEqual) then
        FCompiler.Emitter._Eval(getEvalProc(op_NOT, ltEvalBool, ltUnknown), Result, Result, NullResVar);
    end
  else
    Result := inherited;
end;

procedure TLapeType_Record.FinalizeConst(AVar: TLapeGlobalVar);
var
  i: Integer;
  Field: TLapeGlobalVar;
begin
  if (AVar = nil) or (not NeedFinalization) then
    Exit;

  for i := 0 to FFieldMap.Count - 1 do
  begin
    Field := FFieldMap.ItemsI[i].FieldType.NewGlobalVarP(Pointer(PtrUInt(AVar.Ptr) + FFieldMap.ItemsI[i].Offset));
    try
      Field.VarType.FinalizeConst(Field);
    finally
      Field.Free();
    end;
  end;
end;

function TLapeType_Union.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := 'union ';
    for i := 0 to FFieldMap.Count - 1 do
      FAsString := FAsString + FFieldMap.Key[i] + ': ' + FFieldMap.ItemsI[i].FieldType.AsString + '; ';
    FAsString := FAsString + 'end';
  end;
  Result := inherited;
end;

constructor TLapeType_Union.Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = '');
begin
  inherited;
  FBaseType := ltUnion;
  FInit := bFalse;
end;

procedure TLapeType_Union.addField(FieldType: TLapeType; AName: lpString; AAlignment: UInt16 = 1);
var
  Field: TRecordField;
  FieldSize: SizeInt;
begin
  if (FSize < 0) or (FFieldMap.Count < 1) then
    FSize := 0;
  if (FieldType = nil) or FieldType.NeedInitialization or FieldType.NeedFinalization then
    LapeException(lpeInvalidUnionType);

  Field.Offset := 0;
  Field.FieldType := FieldType;
  if FFieldMap.ExistsKey(AName) then
    LapeExceptionFmt(lpeDuplicateDeclaration, [AName]);

  FieldSize := FieldType.Size;
  if (FieldSize > FSize) then
    FSize := FieldSize;

  FFieldMap[AName] := Field;
  ClearCache();
end;

end.

