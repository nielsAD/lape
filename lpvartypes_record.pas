{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
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
    Offset: Word;
    FieldType: TLapeType;
  end;
  TRecordFieldMap = {$IFDEF FPC}specialize{$ENDIF} TLapeStringMap<TRecordField>;

  TLapeType_Record = class(TLapeType)
  protected
    FAlignment: UInt16;
    FFieldMap: TRecordFieldMap;
    function getAsString: lpString; override;
    function getPadding: SizeInt; override;
    function getSize: SizeInt; override;
    function getFinalizeOperator: Boolean; override;
  public
    FreeFieldMap: Boolean;

    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;
    destructor Destroy; override;

    procedure ClearCache; override;
    procedure addField(FieldType: TLapeType; AName: lpString; AAlignment: UInt16 = 1); virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;
    function NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;

    function HasChild(AName: lpString): Boolean; override;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType; override;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); override;

    property FieldMap: TRecordFieldMap read FFieldMap;
    property Alignment: UInt16 read FAlignment;
  end;

  TLapeType_Union = class(TLapeType_Record)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil); override;
    procedure addField(FieldType: TLapeType; AName: lpString; AAlignment: UInt16 = 1); override;
  end;

implementation

uses
  Math,
  lpvartypes_array, lpparser, lpeval, lpmessages, lptree;

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
  Result := (inherited + (FAlignment - 1)) and not (FAlignment - 1);
end;

function TLapeType_Record.getFinalizeOperator: Boolean;
var
  i: Int32;
begin
  if (FFinalizeOperator = bUnknown) then
  begin
    FFinalizeOperator := bFalse;

    for i := 0 to FFieldMap.Count - 1 do
      if FFieldMap.ItemsI[i].FieldType.NeedFinalizeOperator then
      begin
        FFinalizeOperator := bTrue;

        Break;
      end;
  end;

  Result := inherited;
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

constructor TLapeType_Record.Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  InvalidRec: TRecordField = (Offset: Word(-1); FieldType: nil);
begin
  inherited Create(ltRecord, ACompiler, AName, ADocPos);

  FSize := 0;
  FreeFieldMap := (AFieldMap = nil);
  if (AFieldMap = nil) then
    AFieldMap := TRecordFieldMap.Create(InvalidRec, dupError, False);
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

    if Typ.IsOrdinal() or (Typ.BaseType in LapePointerTypes) then
      Result := Typ.Size
    else
    case Typ.BaseType of
      ltLargeSet:    Result := SizeOf(UInt8);
      ltExtended:    Result := SizeOf(Extended) and (SizeOf(Double) or (2*SizeOf(Double)));
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
    FSize := (FSize + AAlignment) and not AAlignment;
  end;

  Field.Offset := FSize;
  Field.FieldType := FieldType;

  FSize := FSize + FieldSize;
  if (FInit <> bTrue) and FieldType.NeedInitialization then
    FInit := bTrue;
  FFieldMap[AName] := Field;

  ClearCache();
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

function TLapeType_Record.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_Record;
begin
  if DeepCopy then
  begin
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, nil, Name, @_DocPos);
    TLapeType_Record(Result).FieldMap.ImportFromArrays(FFieldMap.ExportToArrays());
  end
  else
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, FFieldMap, Name, @_DocPos);

  with TLapeType_Record(Result) do
  begin
    inheritManagedDecls(Self, not DeepCopy);
    TypeID := Self.TypeID;
    FInit := Self.FInit;
    FSize := Self.FSize;
    FAlignment := Self.FAlignment;
  end;
end;

function TLapeType_Record.NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
end;

function TLapeType_Record.HasChild(AName: lpString): Boolean;
begin
  Result := FFieldMap.ExistsKey(AName) or HasSubDeclaration(AName, bTrue);
end;

function TLapeType_Record.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
var
  i: Integer;
begin
  if (Right <> nil) and (Right is TLapeType_Record) and  (TLapeType_Record(Right).FieldMap.Count = FFieldMap.Count) then
    if (op = op_Assign) then
    begin
      for i := 0 to FFieldMap.Count - 1 do
        if (FFieldMap.Key[i] <> TLapeType_Record(Right).FieldMap.Key[i]) or
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
        if (FFieldMap.Key[i] <> TLapeType_Record(Right).FieldMap.Key[i]) or
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

function TLapeType_Record.EvalRes(Op: EOperator; Right: TLapeGlobalVar; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (Op = op_Dot) and ValidFieldName(Right) and FFieldMap.ExistsKey(PlpString(Right.Ptr)^) then
    Result := FFieldMap[PlpString(Right.Ptr)^].FieldType
  else
    Result := inherited;
end;

function TLapeType_Record.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  i: Integer;
  LeftVar, RightVar, LeftFieldName, RightFieldName: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType = Self));

  if (Op = op_Dot) and (Left <> nil) and ValidFieldName(Right) and FFieldMap.ExistsKey(PlpString(Right.Ptr)^) then
    with FFieldMap[PlpString(Right.Ptr)^] do
    begin
      Result := FieldType.NewGlobalVarP(Pointer(PtrUInt(Left.Ptr) + Offset));
      Result.CopyFlags(Left);
    end
  else if (op = op_Assign) and (Right <> nil) and Right.HasType() and CompatibleWith(Right.VarType) then
  begin
    LeftFieldName := nil;
    RightFieldName := nil;
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FFieldMap.Count - 1 do
    try
      LeftFieldName := FCompiler.getBaseType(ltString).NewGlobalVarStr(FFieldMap.Key[i]);
      RightFieldName := FCompiler.getBaseType(ltString).NewGlobalVarStr(TLapeType_Record(Right.VarType).FieldMap.Key[i]);

      LeftVar := EvalConst(op_Dot, Left, LeftFieldName, []);
      RightVar := Right.VarType.EvalConst(op_Dot, Right, RightFieldName, []);
      LeftVar.VarType.EvalConst(op, LeftVar, RightVar, []);
    finally
      if (LeftFieldName <> nil) then
        FreeAndNil(LeftFieldName);
      if (RightFieldName <> nil) then
        FreeAndNil(RightFieldName);
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;

    Result := Left;
  end
  else if (op in [op_cmp_Equal, op_cmp_NotEqual]) and (Right <> nil) and Right.HasType() and (EvalRes(op, Right, Flags) <> nil) then
  begin
    LeftFieldName := nil;
    RightFieldName := nil;
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FFieldMap.Count - 1 do
    try
      LeftFieldName := FCompiler.getBaseType(ltString).NewGlobalVarStr(FFieldMap.Key[i]);
      RightFieldName := FCompiler.getBaseType(ltString).NewGlobalVarStr(TLapeType_Record(Right.VarType).FieldMap.Key[i]);

      LeftVar := EvalConst(op_Dot, Left, LeftFieldName, []);
      RightVar := Right.VarType.EvalConst(op_Dot, Right, RightFieldName, []);
      Result := LeftVar.VarType.EvalConst(op, LeftVar, RightVar, []);

      if (not Result.HasType()) or (not (Result.VarType.BaseType in LapeBoolTypes)) then
        LapeException(lpeImpossible);
      if (Result.AsInteger <> Ord(False)) xor (op = op_cmp_Equal) then
        Exit;
    finally
      if (LeftFieldName <> nil) then
        FreeAndNil(LeftFieldName);
      if (RightFieldName <> nil) then
        FreeAndNil(RightFieldName);
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_Record.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  i: Integer;
  LoopOffset: array of Integer;
  FieldOffset: SizeInt;
  tmpVar, LeftVar, RightVar, LeftFieldName, RightFieldName: TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType = Self);
  tmpVar := NullResVar;

  if (Op = op_Dot) and ValidFieldName(Right) and FFieldMap.ExistsKey(PlpString(Right.VarPos.GlobalVar.Ptr)^) then
    with FFieldMap[PlpString(Right.VarPos.GlobalVar.Ptr)^] do
    begin
      Dest := NullResVar;
      Result := Left.IncLock();
      Result.VarType :=FieldType;
      case Left.VarPos.MemPos of
        mpMem: Result.VarPos.GlobalVar := TLapeGlobalVar(FCompiler.addManagedVar(Result.VarType.NewGlobalVarP(Pointer(PtrUInt(Left.VarPos.GlobalVar.Ptr) + Offset)), True));
        mpVar,
        mpStack: Result.IncOffset(Offset);
        else LapeException(lpeImpossible);
      end;
      Result.CopyFlags(Left);
    end
  else if (op = op_Assign) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (not NeedInitialization) and (not NeedFinalizeOperator) and Equals(Right.VarType) and (Size > 0) and ((Left.VarPos.MemPos <> mpStack) or (DetermineIntType(Size, False) <> ltUnknown)) then
    begin
      Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
      begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op_Assign, Dest, Left, Right, [], Offset, Pos);
        Result.VarType := Self;
      end
      else
      begin
        RightVar := _ResVar.New(FCompiler.getConstant(Size, ltSizeInt, False, True));
        tmpVar := Compiler.getTempStackVar(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, RightVar, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._InvokeImportedProc(_ResVar.New(FCompiler['!move']), SizeOf(Pointer) * 3, Offset, @Self._DocPos);
        Result := Left;
      end;
    end
    else
    begin;
      for i := 0 to FFieldMap.Count - 1 do
      begin
        LeftFieldName := _ResVar.New(FCompiler.getConstant(FFieldMap.Key[i]));
        RightFieldName := _ResVar.New(FCompiler.getConstant(TLapeType_Record(Right.VarType).FieldMap.Key[i]));
        Dest := NullResVar;

        LeftVar := Eval(op_Dot, tmpVar, Left, LeftFieldName, [lefAssigning], Offset, Pos);
        RightVar := Right.VarType.Eval(op_Dot, tmpVar, Right, RightFieldName, [], Offset, Pos);

        with TLapeTree_Operator.Create(op_Assign, FCompiler) do
        try
          Left := TLapeTree_ResVar.Create(LeftVar.IncLock(), FCompiler);
          Right := TLapeTree_ResVar.Create(RightVar.IncLock(), FCompiler);

          Compile(Offset).Spill(1);
        finally
          Free();
        end;

        if (LeftVar.VarPos.MemPos = mpStack) then
        begin
          if (i + 1 = FFieldMap.Count) then
            FieldOffset := Size
          else
            FieldOffset := FFieldMap.ItemsI[i + 1].Offset;
          FieldOffset := FieldOffset - (FFieldMap.ItemsI[i].Offset + FFieldMap.ItemsI[i].FieldType.Size);
          if (FieldOffset > 0) then
            FCompiler.Emitter._GrowStack(FieldOffset, Offset, Pos);
        end;

        LeftVar.Spill(2);
        RightVar.Spill(2);
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
        Result := Left.VarType.Eval(op, Dest, Left, Right, [], Offset, Pos);
      end
      else
      begin
        Assert(op in [op_cmp_Equal, op_cmp_NotEqual]);

        RightVar := _ResVar.New(FCompiler.getConstant(Size, ltSizeInt, False, True));
        tmpVar := Compiler.getTempStackVar(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar, Offset, @Self._DocPos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, RightVar, NullResVar, Offset, @Self._DocPos);

        Result := NullResVar;
        Result.VarType := Compiler.getBaseType(ltEvalBool);

        Compiler.getDestVar(Dest, Result, op);
        FCompiler.Emitter._InvokeImportedFunc(_ResVar.New(FCompiler['!cmp']), Result, SizeOf(Pointer) * 3, Offset, @Self._DocPos);
        if (op = op_cmp_NotEqual) then
          FCompiler.Emitter._Eval(getEvalProc(op_NOT, ltEvalBool, ltUnknown), Result, Result, NullResVar, Offset, @Self._DocPos);
      end;
    end
    else
    begin
      Result := _ResVar.New(FCompiler.getTempVar(ltEvalBool));
      Dest := NullResVar;

      SetLength(LoopOffset, FFieldMap.Count);
      for i := 0 to FFieldMap.Count - 1 do
      begin
        LeftFieldName := _ResVar.New(FCompiler.getConstant(FFieldMap.Key[i]));
        RightFieldName := _ResVar.New(FCompiler.getConstant(TLapeType_Record(Right.VarType).FieldMap.Key[i]));
        Dest := NullResVar;

        LeftVar := Eval(op_Dot, tmpVar, Left, LeftFieldName, [], Offset, Pos);
        RightVar := Right.VarType.Eval(op_Dot, tmpVar, Right, RightFieldName, [], Offset, Pos);

        tmpVar := Result;
        tmpVar := LeftVar.VarType.Eval(op_cmp_Equal, tmpVar, LeftVar, RightVar, [], Offset, Pos);
        if (tmpVar.VarPos.MemPos <> Result.VarPos.MemPos) or (tmpVar.VarPos.StackVar <> Result.VarPos.StackVar) then
          Result.VarType.Eval(op_Assign, Dest, Result, tmpVar, [], Offset, Pos);
        LoopOffset[i] := FCompiler.Emitter._JmpRIfNot(0, Result, Offset, Pos);

        LeftVar.Spill(1);
        RightVar.Spill(1);
      end;

      for i := 0 to FFieldMap.Count - 1 do
        FCompiler.Emitter._JmpRIfNot(Offset - LoopOffset[i], Result, LoopOffset[i], Pos);

      if (op = op_cmp_NotEqual) then
        FCompiler.Emitter._Eval(getEvalProc(op_NOT, ltEvalBool, ltUnknown), Result, Result, NullResVar, Offset, @Self._DocPos);
    end
  else
    Result := inherited;
end;

procedure TLapeType_Record.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);
var
  i: Integer;
  FieldVar: TResVar;
begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) {or (not NeedFinalization)} then
    Exit;

  for i := 0 to FFieldMap.Count - 1 do
  try
    FieldVar := AVar;
    FieldVar.VarType := FFieldMap.ItemsI[i].FieldType;
    case FieldVar.VarPos.MemPos of
      mpMem:
        if UseCompiler and (FCompiler <> nil) then
          FieldVar.VarPos.GlobalVar := TLapeGlobalVar(FCompiler.addManagedVar(FieldVar.VarType.NewGlobalVarP(Pointer(PtrUInt(FieldVar.VarPos.GlobalVar.Ptr) + FFieldMap.ItemsI[i].Offset)), True))
        else
          FieldVar.VarPos.GlobalVar := FieldVar.VarType.NewGlobalVarP(Pointer(PtrUInt(FieldVar.VarPos.GlobalVar.Ptr) + FFieldMap.ItemsI[i].Offset));
      mpVar: FieldVar.IncOffset(FFieldMap.ItemsI[i].Offset);
      else LapeException(lpeImpossible);
    end;

    if UseCompiler and (FCompiler <> nil) then
      FCompiler.FinalizeVar(FieldVar, Offset, Pos)
    else if NeedFinalization then
      FieldVar.VarType.Finalize(FieldVar, Offset, UseCompiler, Pos);
  finally
    if ((not UseCompiler) or (FCompiler = nil)) and (FieldVar.VarPos.MemPos = mpMem) and (FieldVar.VarPos.GlobalVar <> nil) then
      FreeAndNil(FieldVar.VarPos.GlobalVar);
    FieldVar.Spill(1);
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

constructor TLapeType_Union.Create(ACompiler: TLapeCompilerBase; AFieldMap: TRecordFieldMap; AName: lpString = ''; ADocPos: PDocPos = nil);
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

