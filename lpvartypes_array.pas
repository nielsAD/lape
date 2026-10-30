{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Array types.
}
unit lpvartypes_array;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lptree, lpinvoke, lpvartypes, lpvartypes_helper;

type
  TLapeType_DynArray = class(TLapeType_Pointer)
  protected
    function getAsString: lpString; override;

    function canInlineHelper(AName: lpString; out Typ: TLapeType): Boolean;
  public
    constructor Create(ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    procedure VarSetLength(var AVar: Pointer; ALen: SizeInt); overload; virtual;
    procedure VarSetLength(AVar, ALen: TResVar); overload; virtual;
    procedure VarSetLength(AVar: TLapeGlobalVar; ALen: SizeInt); overload; virtual;

    procedure RangeCheck(AVar, AIndex: TLapeGlobalVar; Flags: ELapeEvalFlags); overload; virtual;
    procedure RangeCheck(var AVar, AIndex: TResVar; Flags: ELapeEvalFlags); overload; virtual;

    function EvalRes(Op: EOperator; ARight: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalMember(var Dest: TResVar; ALeft: TResVar; AName: lpString; Flags: ELapeEvalFlags): TResVar; override;
    function EvalConst(Op: EOperator; ALeft, ARight: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; ALeft, ARight: TResVar; Flags: ELapeEvalFlags): TResVar; override;

    procedure addArrayHelpers; virtual;
  end;

  TLapeType_StaticArray = class(TLapeType_DynArray)
  protected
    FRange: TLapeRange;

    function getPadding: SizeInt; override;
    function getSize: SizeInt; override;
    function getAsString: lpString; override;
  public
    constructor Create(ARange: TLapeRange; ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;
    function NewGlobalVar(AName: lpString = ''): TLapeGlobalVar; reintroduce; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;
    procedure FinalizeConst(AVar: TLapeGlobalVar); override;

    property Range: TLapeRange read FRange;

    procedure addArrayHelpers; override;
  end;

  TLapeType_String = class(TLapeType_DynArray)
  public
    function VarToString(AVar: Pointer): lpString; override;
    procedure VarUnique(var AVar: Pointer); overload; virtual;
    procedure VarUnique(AVar: TResVar); overload; virtual;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    function NewGlobalVarStr(Str: AnsiString; AName: lpString = ''): TLapeGlobalVar; override;
    function NewGlobalVar(Str: AnsiString; AName: lpString = ''): TLapeGlobalVar; reintroduce; overload; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar; override;
    function NewGlobalVar(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar; reintroduce; overload; virtual;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;

    procedure addArrayHelpers; override;
  end;

  TLapeType_AnsiString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual; end;
  TLapeType_WideString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual; end;
  TLapeType_UnicodeString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''); reintroduce; virtual; end;

  TLapeType_ShortString = class(TLapeType_StaticArray)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ASize: UInt8 = High(UInt8); AName: lpString = ''); reintroduce; virtual;

    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar; override;
    function NewGlobalVar(Str: ShortString; AName: lpString = ''): TLapeGlobalVar; reintroduce; overload; virtual;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar; override;
  end;

implementation

uses
  lptokenizer, lpeval, lpmessages, lpinternalmethods, lpeval_extra;

function TLapeType_DynArray.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltDynArray) then
    if HasType() then
      FAsString := 'array of ' + FPType.AsString
    else
      FAsString := 'array';
  Result := inherited;
end;

function TLapeType_DynArray.canInlineHelper(AName: lpString; out Typ: TLapeType): Boolean;
var
  Decl: TLapeDeclaration;
begin
  Decl := getDeclaration(AName);
  if (Decl is TLapeGlobalVar) then
  begin
    Typ := TLapeGlobalVar(Decl).VarType;

    Result := (Typ is TLapeType_ArrayHelper_First) or
              (Typ is TLapeType_ArrayHelper_Last) or
              (Typ is TLapeType_ArrayHelper_Pop) or
              (Typ is TLapeType_ArrayHelper_High) or
              (Typ is TLapeType_ArrayHelper_Low) or
              (Typ is TLapeType_ArrayHelper_Length) or
              (Typ is TLapeType_ArrayHelper_IsEmpty);
  end else
    Result := False;
end;

constructor TLapeType_DynArray.Create(ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  inherited Create(ACompiler, ArrayType, False, AName);

  FBaseType := ltDynArray;

  addArrayHelpers();
end;

function TLapeType_DynArray.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_DynArray;
begin
  Result := TLapeClassType(Self.ClassType).Create(FPType, FCompiler, '');
  Result.DocPos := FDocPos;
  with TLapeType_DynArray(Result) do
  begin
    FBaseType := Self.BaseType;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_DynArray.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (FCompiler = nil) or (not HasType()) then
    Exit;

  Index := ToStr.getMethodIndex(getTypeArray([PType]), FCompiler.getBaseType(ltString));
  if (Index < 0) then
    Exit;

  Result := lpString(
    '  function _ElementToString(p: System.ConstPointer): System.string; static;'           + LineEnding +
    '  begin'                                                                               + LineEnding +
    '    Result := System.ToString['+IntToStr(Index)+'](p^);'                               + LineEnding +
    '  end;'                                                                                + LineEnding +
    'type'                                                                                  + LineEnding +
    '  TArrayToString = private function(constref Arr; AToString: System.ConstPointer; Len, Size: System.SizeInt): System.string;' + LineEnding +
    'var'                                                                                   + LineEnding +
    '  Len: System.SizeInt;'                                                                + LineEnding +
    'begin'                                                                                 + LineEnding +
    '  Len := System.Length(Param0);'                                                       + LineEnding +
    '  if (Len <= 0) then'                                                                  + LineEnding +
    '    Result := '#39'[]'#39''                                                            + LineEnding +
    '  else'                                                                                + LineEnding +
    '    Result := TArrayToString(System._ArrayToString)('                                  + LineEnding +
    '      Param0['+IntToStr(VarLo().AsInteger)+'],'                                        + LineEnding +
    '      _ElementToString, Len, System.SizeOf(Param0[0]));'                               + LineEnding +
    'end;');
end;

function TLapeType_DynArray.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
  p: Pointer;
  Builder: TLapeStringList;
begin
  Result := '[';
  if (AVar <> nil) and HasType() then
  try
    p := PPointer(AVar)^;
    Builder := TLapeStringList.Create('', dupAccept);
    for i := 0 to Length(PCodeArray(AVar)^) - 1 do
      Builder.Add(FPType.VarToString(Pointer(PtrUInt(p) + PtrUInt(FPType.Size * i))));
    Result := Result + Builder.Implode(', ');
  finally
    Builder.Free();
  end;
  Result := Result + ']';
end;

function TLapeType_DynArray.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(0, ltSizeInt);
end;

function TLapeType_DynArray.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (AVar = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(High(PCodeArray(AVar)^), ltSizeInt);
end;

procedure TLapeType_DynArray.VarSetLength(var AVar: Pointer; ALen: SizeInt);
var
  i, OldLen, NewSize: SizeInt;
  NewP: Pointer;
  DoFree: Boolean;
  tmpLeft, tmpRight: TLapeGlobalVar;
begin
  if (not (BaseType in LapeArrayTypes - [ltStaticArray])) then
    LapeException(lpeInvalidEvaluation);
  if (not HasType()) then
    Exit;

  if (BaseType in LapeStringTypes) then
  begin
    case BaseType of
      ltAnsiString:    SetLength(AnsiString(AVar), ALen);
      ltWideString:    SetLength(WideString(AVar), ALen);
      ltUnicodeString: SetLength(UnicodeString(AVar), ALen);
      ltShortString:
        begin
          tmpRight := VarHi();
          if (ALen < 0) or (tmpRight = nil) or (ALen > tmpRight.AsInteger) then
            LapeException(lpeOutOfTypeRange)
          else
            SetLength(PShortString(AVar)^, ALen);
        end
      else LapeException(lpeImpossible);
    end;
    Exit;
  end;

  NewSize := ALen * PType.Size;
  DoFree := NewSize <= 0;
  Inc(NewSize, SizeOf(PtrInt) + SizeOf(SizeInt));

  if (AVar = nil) then
  begin
    if DoFree then
      Exit;
    AVar := AllocMem(NewSize);

    PtrInt(AVar^) := 1;
    Inc(PtrUInt(AVar), SizeOf(PtrInt));
    SizeInt(AVar^) := ALen {$IFDEF FPC}-1{$ENDIF};
    Inc(PtrUInt(AVar), SizeOf(SizeInt));
    Exit;
  end;

  Dec(PtrUInt(AVar), SizeOf(SizeInt));
  OldLen := SizeInt(AVar^) {$IFDEF FPC}+1{$ENDIF};
  Dec(PtrUInt(AVar), SizeOf(PtrInt));

  if (PtrInt(AVar^) <= 1) then
  begin
    if (ALen = OldLen) then
    begin
      Inc(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
      Exit;
    end;

    if (ALen < OldLen) and PType.NeedFinalization then
    begin
      Inc(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
      i := ALen;
      while (i < OldLen) do
      begin
        tmpLeft := PType.NewGlobalVarP(Pointer(PtrInt(AVar) + (i * PType.Size)));
        try
          PType.FinalizeConst(tmpLeft);
        finally
          tmpLeft.Free();
        end;
        Inc(i);
      end;
      Dec(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
    end;

    if DoFree then
    begin
      FreeMem(AVar);
      AVar := nil;
      Exit;
    end;
    ReallocMem(AVar, NewSize);
    PtrInt(AVar^) := 1;
    Inc(PtrUInt(AVar), SizeOf(PtrInt));
    SizeInt(AVar^) := ALen {$IFDEF FPC}-1{$ENDIF};
    Inc(PtrUInt(AVar), SizeOf(SizeInt));

    if (ALen > OldLen) then
      FillChar(Pointer(PtrInt(AVar) + (OldLen * PType.Size))^, (ALen - OldLen) * PType.Size, 0);
  end
  else
  begin
    Dec(PtrInt(AVar^));
    NewP := nil;
    VarSetLength(NewP, ALen);

    i := OldLen;
    if (ALen < OldLen) then
      i := ALen;
    Inc(PtrUInt(AVar), SizeOf(PtrInt) + SizeOf(SizeInt));

    Dec(i);
    while (i >= 0) do
    begin
      tmpLeft := PType.NewGlobalVarP(Pointer(PtrInt(NewP) + (i * PType.Size)));
      tmpRight := PType.NewGlobalVarP(Pointer(PtrInt(AVar) + (i * PType.Size)));
      try
        PType.EvalConst(op_Assign, tmpLeft, tmpRight, []);
      finally
        tmpLeft.Free();
        tmpRight.Free();
      end;
      Dec(i);
    end;

    AVar := NewP;
  end;
end;

procedure TLapeType_DynArray.VarSetLength(AVar, ALen: TResVar);
begin
  Assert(FCompiler <> nil);
  //FCompiler.EmitCode('System.SetLength(AVar, ALen);', ['AVar', 'ALen'], [], [AVar, ALen], Offset, FCompiler.DocPos);

  with TLapeTree_InternalMethod_SetLength.Create(FCompiler) do
  try
    addParam(TLapeTree_Var.Create(AVar, FCompiler));
    addParam(TLapeTree_Var.Create(ALen, FCompiler));
    Emit();
  finally
    Free();
  end;
end;

procedure TLapeType_DynArray.VarSetLength(AVar: TLapeGlobalVar; ALen: SizeInt);
var
  Arr: Pointer;
begin
  if AVar.isNull() then
    Arr := nil
  else
    Arr := PPointer(AVar.Ptr)^;

  VarSetLength(Arr, ALen);
  PPointer(AVar.Ptr)^ := Arr;
end;

procedure TLapeType_DynArray.RangeCheck(AVar, AIndex: TLapeGlobalVar; Flags: ELapeEvalFlags);
var
  Idx: SizeInt;
  Lo, Hi: TLapeGlobalVar;
begin
  if (AVar = nil) or (not AIndex.HasType()) then
    LapeException(lpeInvalidEvaluation)
  else if (not (AIndex.VarType.BaseType in LapeOrdinalTypes)) then
    LapeExceptionFmt(lpeInvalidIndex, [AIndex.VarType.AsString])
  else if (not (lefConstRangeCheck in Flags)) then
    Exit;

  Idx := AIndex.AsInteger;
  if (FBaseType in LapeStringTypes) then
    Inc(Idx);

  Lo := VarLo(AVar.Ptr);
  Hi := VarHi(AVar.Ptr);
  if ((Lo <> nil) and (Idx < Lo.AsInteger)) or ((Hi <> nil) and (Idx > Hi.AsInteger)) then
    if (Lo <> nil) and (Hi <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [idx, Lo.AsInteger, Hi.AsInteger])
    else if (Lo <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRangeLow,  [idx, Lo.AsInteger])
    else if (Hi <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRangeHigh, [idx, Hi.AsInteger]);
end;

procedure TLapeType_DynArray.RangeCheck(var AVar, AIndex: TResVar; Flags: ELapeEvalFlags);
var
  Idx: SizeInt;
  Lo, Hi: TLapeGlobalVar;
  Check, Len: TLapeTree_Invoke;
begin
  if (not AIndex.HasType()) then
    LapeException(lpeInvalidEvaluation)
  else if (not (AIndex.VarType.BaseType in LapeOrdinalTypes)) then
    LapeExceptionFmt(lpeInvalidIndex, [AIndex.VarType.AsString])
  else if (([lefRangeCheck, lefConstRangeCheck]) * Flags = []) then
    Exit;

  if AVar.isConstant and (AVar.VarPos.MemPos = mpMem) then
  begin
    Lo := VarLo(AVar.VarPos.GlobalVar.Ptr);
    Hi := VarHi(AVar.VarPos.GlobalVar.Ptr);
  end
  else
  begin
    Lo := VarLo();
    Hi := VarHi();
  end;

  if (lefConstRangeCheck in Flags) and
     AVar.HasType() and (AVar.VarType.BaseType in LapeArrayTypes) and
     AIndex.isConstant and (AIndex.VarPos.MemPos = mpMem)
  then
  begin
    Idx := AIndex.VarPos.GlobalVar.AsInteger;

    if ((Lo <> nil) and (Idx < Lo.AsInteger)) or ((Hi <> nil) and (Idx > Hi.AsInteger)) then
    begin
      if (Lo <> nil) and (Hi <> nil) then
        LapeExceptionFmt(lpeIndexOutOfRange, [idx, Lo.AsInteger, Hi.AsInteger])
      else if (Lo <> nil) then
        LapeExceptionFmt(lpeIndexOutOfRangeLow,  [idx, Lo.AsInteger])
      else if (Hi <> nil) then
        LapeExceptionFmt(lpeIndexOutOfRangeHigh, [idx, Hi.AsInteger]);
    end
    else if (Lo <> nil) and (Hi <> nil) then
      Exit;
  end;

  if (not (lefRangeCheck in Flags)) then
    Exit;

  if (BaseType = ltDynArray) then
  begin
    AIndex.VarType := FCompiler.getBaseType(DetermineIntType(AIndex.VarType.Size, AIndex.VarType.BaseType in LapeSignedOrdinalTypes));
    if (AIndex.VarPos.MemPos = mpStack) then
    begin
      AIndex := _ResVar.New(FCompiler.getTempVar(AIndex.VarType));
      FCompiler.Emitter._PopStackToVar(AIndex.VarType.Size, AIndex.VarPos.StackVar.Offset);
    end;

    FCompiler.Emitter._Eval(getEvalProc_DynArrayRangeCheck(DetermineIntType(AIndex.VarType.Size, AIndex.VarType.BaseType in LapeSignedOrdinalTypes)), AVar, AVar, AIndex);
  end else
  begin
    AIndex.VarType := FCompiler.getBaseType(DetermineIntType(AIndex.VarType.Size, AIndex.VarType.BaseType in LapeSignedOrdinalTypes));
    if (AIndex.VarPos.MemPos = mpStack) then
    begin
      AIndex := _ResVar.New(FCompiler.getTempVar(AIndex.VarType));
      FCompiler.Emitter._PopStackToVar(AIndex.VarType.Size, AIndex.VarPos.StackVar.Offset);
    end;

    Check := TLapeTree_Invoke.Create('_RangeCheck', FCompiler);
    with Check do
    try
      addParam(TLapeTree_Var.Create(AIndex, Check));

      Len := TLapeTree_InternalMethod_Low.Create(FCompiler);
      Len.addParam(TLapeTree_Var.Create(AVar, Check));
      addParam(TLapeTree_ExprBase(Len.FoldConstants()));

      Len := TLapeTree_InternalMethod_High.Create(FCompiler);
      Len.addParam(TLapeTree_Var.Create(AVar, Check));
      addParam(TLapeTree_ExprBase(Len.FoldConstants()));

      Emit();
    finally
      Free();
    end;
  end;
end;

function TLapeType_DynArray.EvalMemberRes(AName: lpString; Flags: ELapeEvalFlags = []): TLapeType;
var
  Typ: TLapeType;
begin
  if CanInlineHelper(AName, Typ) then
    if (Typ is TLapeType_ArrayHelper_First) or (Typ is TLapeType_ArrayHelper_Last) or (Typ is TLapeType_ArrayHelper_Pop) then
      Result := FPType
    else if (Typ is TLapeType_ArrayHelper_Low) or (Typ is TLapeType_ArrayHelper_High) or (Typ is TLapeType_ArrayHelper_Length) then
      Result := FCompiler.getBaseType(ltSizeInt)
    else if (Typ is TLapeType_ArrayHelper_IsEmpty) then
      Result := FCompiler.getBaseType(ltEvalBool)
    else
      Result := nil
  else
    Result := inherited;
end;

function TLapeType_DynArray.EvalRes(Op: EOperator; ARight: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (op = op_Index) then
    Result := FPType
  else if (op = op_Assign) and (BaseType = ltDynArray) and HasType() and (ARight <> nil) and (ARight is ClassType) and FPType.Equals(TLapeType_DynArray(ARight).FPType) then
    Result := Self
  else if (op = op_Assign) and (BaseType = ltDynArray) and HasType() and (ARight is TLapeType_NilPointer) then
    Result := Self
  else if (op = op_Plus) and (BaseType = ltDynArray) and HasType() and FPType.CompatibleWith(ARight) then
    Result := Self
  else if (op = op_Plus) and (BaseType = ltDynArray) and HasType() and (ARight <> nil) and (ARight is ClassType) and FPType.Equals(TLapeType_DynArray(ARight).FPType) then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_DynArray.EvalConst(Op: EOperator; ALeft, ARight: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  tmpType: ELapeBaseType;
  IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((ALeft = nil) or (ALeft.VarType is TLapeType_Pointer));

  if (op = op_Index) then
  begin
    //Cache AsString and NeedInitialization before changing FBaseType
    GetAsString();
    getInitialization();
    RangeCheck(ALeft, ARight, Flags);

    tmpType := FBaseType;
    FBaseType := ltPointer;

    IndexVar := nil;
    try
      IndexVar := inherited EvalConst(Op, ALeft, ARight, Flags);
      Result := //Result := Pointer[Index]^
        EvalConst(
          op_Deref,
          IndexVar,
          nil,
          []
        );
      Result.isConstant := ALeft.isConstant;
      Result.Known := ALeft.Known;
    finally
      if (IndexVar <> nil) then
        IndexVar.Free();
      FBaseType := tmpType;
      Assert((tmpType <> ltDynArray) or NeedInitialization);
    end;
  end
  else if (op = op_Assign) and (BaseType = ltDynArray) and (ALeft <> nil) and (ARight <> nil) and CompatibleWith(ARight.VarType) then
    if (ARight.BaseType = ltDynArray) then
    begin
      if (PPointer(ALeft.Ptr)^ = PPointer(ARight.Ptr)^) then
        Exit(ALeft);

      VarSetLength(PPointer(ALeft.Ptr)^, 0);
      Result := inherited;
    end
    else if (ARight.VarType is TLapeType_StaticArray) then
    begin
      VarSetLength(PPointer(ALeft.Ptr)^, TLapeType_StaticArray(ARight.VarType).Range.Hi - TLapeType_StaticArray(ARight.VarType).Range.Lo + 1);
      Result := ALeft;

      IndexVar := EvalConst(op_Index, ALeft, FCompiler.getIntConst(0), [lefAssigning]);
      try
        IndexVar.VarType := ARight.VarType;
        IndexVar.VarType.EvalConst(op_Assign, IndexVar, ARight, []);
      finally
        IndexVar.Free();
      end;
    end
    else
      LapeException(lpeImpossible)
  else if (op = op_Plus) and (BaseType = ltDynArray) and (ALeft <> nil) and (ARight <> nil) and HasType() and FPType.CompatibleWith(ARight.VarType) then
  begin
    Result := EvalConst(op_Assign, NewGlobalVarP(), ALeft, []);
    IndexVar := FCompiler.getIntConst(Length(PCodeArray(Result.Ptr)^));
    VarSetLength(PPointer(Result.Ptr)^, IndexVar.AsInteger + 1);

    IndexVar := EvalConst(op_Index, Result, IndexVar, [lefAssigning]);
    try
      PType.EvalConst(op_Assign, IndexVar, ARight, []);
    finally
      IndexVar.Free();
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_DynArray.Eval(Op: EOperator; var Dest: TResVar; ALeft, ARight: TResVar; Flags: ELapeEvalFlags): TResVar;
var
  tmpType: ELapeBaseType;
  tmpVar: TLapeStackVar;
  IndexVar, tmpResVar: TResVar;
  wasConstant: Boolean;
  Node: TLapeTree_Base;
begin
  Assert(FCompiler <> nil);
  Assert(ALeft.VarType is TLapeType_Pointer);

  IndexVar := NullResVar;
  tmpResVar := NullResVar;
  tmpVar := nil;

  if (op = op_Index) then
  begin
    //Cache AsString and NeedInitialization before changing FBaseType
    GetAsString();
    getInitialization();
    RangeCheck(ALeft, ARight, Flags);

    tmpType := FBaseType;
    FBaseType := ltPointer;

    try
      if (not ALeft.VarPos.isPointer) then
      begin
        if (Dest.VarPos.MemPos = mpStack) then
          Dest := NullResVar;
        Result := inherited Eval(Op, Dest, ALeft, ARight, Flags);
        Result.VarPos.isPointer := True;
        Result.VarType := FPType;
      end
      else
      begin
        IndexVar := inherited Eval(Op, IndexVar, ALeft, ARight, Flags);
        Result := //Result := Pointer[Index]^
          Eval(op_Deref, Dest, IndexVar, NullResVar, []);
      end;
    finally
      FBaseType := tmpType;
      Assert((tmpType <> ltDynArray) or NeedInitialization);
      Result.isConstant := ALeft.isConstant;
    end;
  end
  else if (op = op_Assign) and (BaseType = ltDynArray) and CompatibleWith(ARight.VarType) then
  begin
    Dest := NullResVar;
    Result := ALeft;

    if (ALeft.VarPos.MemPos = mpStack) then
    begin
      tmpVar := FCompiler.StackInfo.addVar(TLapeStackVar.Create(Self, nil));
      tmpVar.isConstant := False;
      ALeft := _ResVar.New(tmpVar);
    end;

    if (ARight.VarType.BaseType = ltDynArray) then
    begin
      {FCompiler.EmitCode(
        'if (System.Pointer(Left) <> System.Pointer(nil)) and'              +
        '   (System.Pointer(Left) <> System.Pointer(Right)) then'           +
        'begin'                                                             +
        '  System.SetLength(Left, 0);'                                      +
        '  System.Pointer(Left) := System.Pointer(Right);'                  +
        '  if (System.Pointer(Left) <> nil) then'                           +
        '    System.Inc(System.PtrInt(System.Pointer(Left)[-'               +
               IntToStr(SizeOf(SizeInt)+SizeOf(PtrInt))                     +
             ']^));'                                                        +
        'end;'
      , ['Left', 'Right'], [], [ALeft, ARight], Offset, FCompiler.DocPos)}

      with TLapeTree_If.Create(FCompiler) do
      try
        Condition := TLapeTree_Operator.Create(op_AND, Self.FCompiler);

        TLapeTree_Operator(Condition).Left := TLapeTree_Operator.Create(op_cmp_NotEqual, Self.FCompiler);
        with TLapeTree_Operator(TLapeTree_Operator(Condition).Left) do
        begin
          Left := TLapeTree_Var.Create(ALeft, Condition);
          Right := TLapeTree_Var.Create(FCompiler.getBaseType(ltPointer).EmptyValue, Condition);
        end;

        TLapeTree_Operator(Condition).Right := TLapeTree_Operator.Create(op_cmp_NotEqual, Self.FCompiler);
        with TLapeTree_Operator(TLapeTree_Operator(Condition).Right) do
        begin
          Left := TLapeTree_Var.Create(ALeft, Condition);
          Right := TLapeTree_Var.Create(ARight, Condition);
        end;

        Body := TLapeTree_InternalMethod_SetLength.Create(Condition);
        with TLapeTree_InternalMethod_SetLength(Body) do
        begin
          addParam(TLapeTree_Var.Create(ALeft, Body));
          addParam(TLapeTree_Var.Create(FCompiler.getIntConst(0, ltNativeInt), Body));
        end;

        Emit();
      finally
        Free();
      end;

      inherited;
    end
    else if (ARight.VarType is TLapeType_StaticArray) then
    begin
      {FCompiler.EmitCode(
        'System.SetLength(Left, '+IntToStr(
          TLapeType_StaticArray(ARight.VarType).Range.Hi -
          TLapeType_StaticArray(ARight.VarType).Range.Lo + 1)               +
        ');'                                                                +
        'PType(@Left[0])^ := Right;',
        ['PType', 'Left', 'Right'], [FCompiler.getTypeVar(FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, ARight.VarType, False)))],
        [ALeft, ARight], Offset, FCompiler.DocPos)}

      with TLapeType_StaticArray(ARight.VarType).Range do
        VarSetLength(ALeft, _ResVar.New(FCompiler.getIntConst(Hi - Lo + 1)));

      with TLapeTree_Operator.Create(op_Index, FCompiler) do
      try
        Left := TLapeTree_Var.Create(ALeft, FCompiler);
        Right := TLapeTree_Var.Create(FCompiler.getIntConst(0, ltNativeInt), Left);

        CompilerOptions := CompilerOptions - [lcoRangeCheck];
        IndexVar := Emit();
      finally
        Free();
      end;

      IndexVar.VarType := ARight.VarType;
      with TLapeTree_Operator.Create(op_Assign, FCompiler) do
      try
        Left := TLapeTree_Var.Create(IndexVar, FCompiler);
        Right := TLapeTree_Var.Create(ARight, Left);
        Emit();
      finally
        Free();
      end;
    end
    else if (ARight.VarType is TLapeType_NilPointer) then
    begin
      with TLapeTree_InternalMethod_SetLength.Create(FCompiler) do
      try
        addParam(TLapeTree_Var.Create(ALeft, FCompiler));
        addParam(TLapeTree_Var.Create(FCompiler.getIntConst(0), FCompiler));

        Emit();
      finally
        Free();
      end;
    end
    else
      LapeException(lpeImpossible);

    if (tmpVar <> nil) then
      FCompiler.Emitter._PopVarToStack(Size, tmpVar.Offset);
  end
  else if (op = op_Plus) and (BaseType = ltDynArray) and HasType() and FPType.CompatibleWith(ARight.VarType) then
  begin
    Result := NullResVar;
    Result.VarType := Self;
    FCompiler.getDestVar(Dest, Result, op);

    if (Result.VarPos.MemPos = mpStack) then
    begin
      tmpVar := FCompiler.getTempVar(Self);
      Result := _ResVar.New(tmpVar);
    end;

    wasConstant := Result.isConstant;
    Result.isConstant := False;
    Result := Eval(op_Assign, tmpResVar, Result, ALeft, []);

    {FCompiler.EmitCode(
      'System.SetLength(Result, System.Length(Result) + 1);' +
      'Result[System.High(Result)] := Right;'
    , ['Result', 'Right'], [], [Result, ARight], Offset, FCompiler.DocPos);}

    IndexVar := _ResVar.New(FCompiler.getTempVar(ltSizeInt));
    with TLapeTree_Operator.Create(op_Plus, FCompiler) do
    try
      Left := TLapeTree_Operator.Create(op_Assign, FCompiler);
      Right := TLapeTree_Var.Create(FCompiler.getIntConst(1, ltNativeInt), Left);
      with TLapeTree_Operator(Left) do
      begin
        Left := TLapeTree_Var.Create(IndexVar, FCompiler);
        Right := TLapeTree_InternalMethod_Length.Create(Left);
        TLapeTree_InternalMethod_Length(Right).addParam(TLapeTree_Var.Create(Result, Left));
      end;

      Dest := VarResVar;
      tmpResVar := Emit();
    finally
      Free();
    end;

    VarSetLength(Result, tmpResVar);
    with TLapeTree_Operator.Create(op_Assign, FCompiler) do
    try
      Left := TLapeTree_Operator.Create(op_Index, FCompiler);
      with TLapeTree_Operator(Left) do
      begin
        CompilerOptions := CompilerOptions - [lcoRangeCheck];
        Left := TLapeTree_Var.Create(Result, FCompiler);
        Right := TLapeTree_Var.Create(IndexVar, Left);
      end;
      Right := TLapeTree_Var.Create(ARight, Left);
      Emit();
    finally
      Free();
    end;

    if wasConstant then
      Result.isConstant := True;
  end
  else if (op = op_Plus) and (BaseType = ltDynArray) and CompatibleWith(ARight.VarType) then
  begin
    //Result := NullResVar;
    //Result.VarType := Self;
    //FCompiler.getDestVar(Dest, Result, op);
    //
    //if (Result.VarPos.MemPos = mpStack) then
    //begin
    //  tmpVar := FCompiler.getTempVar(Self);
    //  Result := _ResVar.New(tmpVar);
    //end;

    Result :=  _ResVar.New(FCompiler.getTempVar(Self));
    Dest := NullResVar;


    Result := Eval(op_Assign, tmpResVar, Result, ALeft, []);
    with TLapeTree_InternalMethod_Insert.Create(FCompiler) do
    try
      addParam(TLapeTree_Var.Create(ARight, FCompiler));
      addParam(TLapeTree_Var.Create(Result, FCompiler));
      Node := TLapeTree_InternalMethod_Length.Create(FCompiler);
      TLapeTree_InternalMethod_Length(Node).addParam(
        TLapeTree_Var.Create(Result, FCompiler)
      );
      addParam(Node as TLapeTree_ExprBase);

      Emit();
    finally
      Free();
    end;
  end else
    Result := inherited;
end;

function TLapeType_DynArray.EvalMember(var Dest: TResVar; ALeft: TResVar; AName: lpString; Flags: ELapeEvalFlags): TResVar;
var
  IndexVar, tmpResVar: TResVar;
  Node: TLapeTree_Base;
  Typ: TLapeType;
begin
  IndexVar := NullResVar;
  tmpResVar := NullResVar;

  if CanInlineHelper(AName, Typ) then
  begin
    Dest := NullResVar;

    if (Typ is TLapeType_ArrayHelper_First) or
       (Typ is TLapeType_ArrayHelper_Last) or
       (Typ is TLapeType_ArrayHelper_Pop) then
    begin
      if (Typ is TLapeType_ArrayHelper_First) then
        IndexVar := _ResVar.New(VarLo())
      else
        with TLapeTree_InternalMethod_High.Create(FCompiler) do
        try
          addParam(TLapeTree_Var.Create(ALeft, FCompiler));
          IndexVar := Emit();
        finally
          Free();
        end;

      Result := _ResVar.New(FCompiler.getTempVar(FPType));
      Result := FPType.Eval(op_Assign, tmpResVar, Result, Eval(op_Index, tmpResVar, ALeft, IndexVar, [lefRangeCheck]), []);
      Result.isConstant := True;

      if (Typ is TLapeType_ArrayHelper_Pop) then
        with TLapeTree_InternalMethod_SetLength.Create(FCompiler) do
        try
          addParam(TLapeTree_Var.Create(ALeft, FCompiler));

          // Length=High for strings because 1 based indexing...
          if (ALeft.VarType.BaseType in LapeStringTypes) then
          begin
            addParam(TLapeTree_InternalMethod_Pred.Create(FCompiler));
            TLapeTree_InternalMethod_Pred(Params[1]).addParam(TLapeTree_Var.Create(IndexVar, FCompiler));
          end else
            addParam(TLapeTree_Var.Create(IndexVar, FCompiler));

          Emit();
        finally
          Free();
        end;
    end
    else if (Typ is TLapeType_ArrayHelper_Low) or (Typ is TLapeType_ArrayHelper_High) or (Typ is TLapeType_ArrayHelper_Length) then
    begin
      if (Typ is TLapeType_ArrayHelper_Low) then
        Node := TLapeTree_InternalMethod_Low.Create(FCompiler)
      else if (Typ is TLapeType_ArrayHelper_High) then
        Node := TLapeTree_InternalMethod_High.Create(FCompiler)
      else
        Node := TLapeTree_InternalMethod_Length.Create(FCompiler);
      try
        TLapeTree_InternalMethod(Node).addParam(TLapeTree_Var.Create(ALeft, FCompiler));
        Node := Node.FoldConstants();
        Result := Node.Emit();
      finally
        Node.Free();
      end;
    end
    else if (Typ is TLapeType_ArrayHelper_IsEmpty) then
    begin
      Result := _ResVar.New(FCompiler.getTempVar(ltEvalBool));
      Result.isConstant := True;
      inherited Eval(op_cmp_Equal, Result, ALeft, _ResVar.New(FCompiler.getBaseType(ltPointer).EmptyValue), []);
    end
    else
      LapeException(lpeImpossible, FCompiler.DocPos);

    Assert(Result.isConstant);
  end else
    Result := inherited;
end;

procedure TLapeType_DynArray.addArrayHelpers;
var
  Typ: TLapeType;
begin
  if (lcoArrayHelpers in FCompiler.Options) and HasType() then
  begin
    Assert(PType <> nil);

    if (PType.BaseType in LapeIntegerTypes + LapeRealTypes) then
      Typ := FCompiler.getGlobalType('!arrayhelpers_dynordinal')
    else if (PType.BaseType = ltDynArray) then
      Typ := FCompiler.getGlobalType('!arrayhelpers_dynmultidim')
    else
      Typ := FCompiler.getGlobalType('!arrayhelpers_dynsingledim');

    if (Typ <> nil) then
      FParentType := TLapeType(Typ);
  end;
end;

function TLapeType_StaticArray.getPadding: SizeInt;
begin
  if HasType() then
    Result := (FRange.Hi - FRange.Lo + 1) * FPType.Padding
  else
    Result := inherited;
end;

function TLapeType_StaticArray.getSize: SizeInt;
begin
  if (FSize = 0) then
  begin
    if (not HasType()) then
      FSize := -1;
    FSize := (FRange.Hi - FRange.Lo + 1) * FPType.Size;
  end;
  Result := inherited;
end;

function TLapeType_StaticArray.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltStaticArray) then
  begin
    FAsString := 'array [' + lpString(IntToStr(FRange.Lo)) + '..' + lpString(IntToStr(FRange.Hi)) + ']';
    if HasType() then
      FAsString := FAsString + ' of ' + FPType.AsString;
  end;
  Result := inherited;
end;

constructor TLapeType_StaticArray.Create(ARange: TLapeRange; ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  inherited Create(ArrayType, ACompiler, AName);
  FBaseType := ltStaticArray;
  if (ArrayType <> nil) and ArrayType.NeedInitialization then
    FInit := bTrue
  else
    FInit := bFalse;

  FRange := ARange;
end;

function TLapeType_StaticArray.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
  Builder: TLapeStringList;
begin
  Result := '[';
  if (AVar <> nil) and HasType() then
    try
      Builder := TLapeStringList.Create('', dupAccept);
      for i := 0 to FRange.Hi - FRange.Lo do
        Builder.Add(FPType.VarToString(Pointer(PtrInt(AVar) + (FPType.Size * i))));
      Result := Result + Builder.Implode(', ');
    finally
      Builder.Free();
    end;
  Result := Result + ']';
end;

function TLapeType_StaticArray.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(FRange.Lo, ltSizeInt);
end;

function TLapeType_StaticArray.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(FRange.Hi, ltSizeInt);
end;

function TLapeType_StaticArray.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_StaticArray;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FPType, FCompiler, '');
  Result.DocPos := FDocPos;
  with TLapeType_StaticArray(Result) do
  begin
    FBaseType := Self.BaseType;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_StaticArray.NewGlobalVar(AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
end;

function TLapeType_StaticArray.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (op = op_Assign) and (BaseType = ltStaticArray) and HasType() and (Right <> nil) and (Right is ClassType) and
     ((TLapeType_StaticArray(Right).Range.Hi - TLapeType_StaticArray(Right).Range.Lo) = (Range.Hi - Range.Lo)) and
     FPType.CompatibleWith(TLapeType_StaticArray(Right).FPType)
  then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_StaticArray.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  i: Integer;
  LeftVar, RightVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Left <> nil) and (Right <> nil) then
  begin
    Assert(HasType());
    RangeCheck(Left, Right, Flags);

    Result := FPType.NewGlobalVarP(Pointer(PtrInt(Left.Ptr) + (FPType.Size * (Right.AsInteger - FRange.Lo))));
    Result.isConstant := Left.isConstant;
    Result.Known := Left.Known;
  end
  else if (op = op_Assign) and (BaseType = ltStaticArray) and (Left <> nil) and (Right <> nil) and CompatibleWith(Right.VarType) then
  begin
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FRange.Hi - FRange.Lo do
    try
      LeftVar := FPType.NewGlobalVarP(Pointer(PtrInt(Left.Ptr) + (FPType.Size * i)));
      RightVar := FPType.NewGlobalVarP(Pointer(PtrInt(Right.Ptr) + (FPType.Size * i)));
      FPType.EvalConst(op_Assign, LeftVar, RightVar, []);
    finally
      if (LeftVar <> nil) then
        FreeAndNil(LeftVar);
      if (RightVar <> nil) then
        FreeAndNil(RightVar);
    end;
    Result := Left;
  end
  else
    Result := inherited;
end;

function TLapeType_StaticArray.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
var
  wasConstant: Boolean;
  tmpVar, LeftVar, CounterVar: TResVar;
  IndexLow, IndexHigh: TLapeVar;
  IndexOperator: TLapeTree_Operator;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  tmpVar := NullResVar;
  LeftVar := NullResVar;

  if (op = op_Index) then
  try
    RangeCheck(Left, Right, Flags);

    if (not Left.VarPos.isPointer) or (Left.VarPos.Offset > 0) then
    begin
      // its address to read it through
      LeftVar := Left;
      LeftVar.isConstant := False;
      LeftVar := Eval(op_Addr, tmpVar, LeftVar, NullResVar, []);
    end
    else
    begin
      LeftVar := Left;
      LeftVar.VarPos.isPointer := False;
      LeftVar.VarType := FCompiler.addManagedType(TLapeType_Pointer.Create(FCompiler, PType, PConst));
    end;

    if (FRange.Lo = 0) then
      Result := inherited Eval(Op, Dest, LeftVar, Right, Flags - [lefRangeCheck])
    else
    begin
      if Right.isConstant and (Right.VarPos.MemPos = mpMem) then
        tmpVar := _ResVar.New(FCompiler.getIntConst(Right.VarPos.GlobalVar.AsInteger - FRange.Lo))
      else
      begin
        if Right.HasType() and (Right.VarType.BaseType in LapeOrdinalTypes) then
          Right.VarType := FCompiler.getBaseType(DetermineIntType(Right.VarType.Size, Right.VarType.BaseType in LapeSignedOrdinalTypes));

        IndexLow := FCompiler.getIntConst(FRange.Lo, ltSizeInt);
        tmpVar := Right.VarType.Eval(op_Minus, tmpVar, Right, _ResVar.New(IndexLow), []);
      end;

      Result := //Result := @Pointer[Index - Lo]^
        inherited Eval(Op, Dest, LeftVar, tmpVar, Flags - [lefRangeCheck]);
    end;
  finally
    Result.isConstant := Left.isConstant;
  end
  else if (op = op_Assign) and (BaseType = ltStaticArray) and CompatibleWith(Right.VarType) then
    if (not NeedInitialization) and Equals(Right.VarType) and (Size > 0) then
    begin
      Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
      begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op_Assign, Dest, Left, Right, []);
      end
      else
      begin
        Left.VarType := Self;
        if (Left.VarPos.MemPos = mpStack) and (not Left.VarPos.ForceVariable) then
        begin
          FCompiler.Emitter._GrowStack(Size);
          Left.VarPos.ForceVariable := True;

          IndexLow := FCompiler.getTempVar(ltPointer);
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), _ResVar.New(IndexLow), Left, NullResVar);

          wasConstant := True;
        end
        else
          wasConstant := False;

        IndexHigh := FCompiler.getIntConst(Size, ltSizeInt);
        tmpVar := Compiler.getTempStackVar(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar);
        if wasConstant then
          FCompiler.Emitter._Eval(getEvalProc(op_Assign, ltPointer, ltPointer), tmpVar, _ResVar.New(IndexLow), NullResVar)
        else
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, _ResVar.New(IndexHigh), NullResVar);
        FCompiler.Emitter._InvokeImportedProc(_ResVar.New(FCompiler['!move']), SizeOf(Pointer) * 3);
        Result := Left;

        if wasConstant then
          Left.VarPos.ForceVariable := False;
      end;
    end
    else
    begin
      Assert(HasType());
      if (Left.VarPos.MemPos = mpStack) and (not Left.VarPos.ForceVariable) then
      begin
        FCompiler.Emitter._GrowStack(Size);
        Left.VarPos.ForceVariable := True;
        wasConstant := True;
      end
      else
        wasConstant := False;

      CounterVar := _ResVar.New(FCompiler.getTempVar(ltSizeInt));
      CounterVar := CounterVar.VarType.Eval(op_Assign, tmpVar, CounterVar, _ResVar.New(FCompiler.getIntConst(FRange.Lo)), []);

      with TLapeTree_For.Create(FCompiler) do
      try
        Counter := TLapeTree_Var.Create(CounterVar, FCompiler);
        Limit := TLapeTree_Var.Create(_ResVar.New(FCompiler.getIntConst(FRange.Hi)), FCompiler);

        Body := TLapeTree_Operator.Create(op_Assign, FCompiler);
        with TLapeTree_Operator(Body) do
        begin
          Left := TLapeTree_Operator.Create(op_Index, Body);
          Right := TLapeTree_Operator.Create(op_Index, Body);
        end;

        IndexOperator := TLapeTree_Operator(Body).Left as TLapeTree_Operator;
        IndexOperator.Left := TLapeTree_Var.Create(Left, IndexOperator);
        IndexOperator.Right := TLapeTree_Var.Create(CounterVar, IndexOperator);

        IndexOperator := TLapeTree_Operator(Body).Right as TLapeTree_Operator;
        IndexOperator.Left := TLapeTree_Var.Create(Right, IndexOperator);
        IndexOperator.Right := TLapeTree_Var.Create(CounterVar, IndexOperator);

        Emit();
      finally

        Free();
      end;

      Result := Left;
      if wasConstant then
        Left.VarPos.ForceVariable := False;
    end
  else
    Result := inherited;
end;

procedure TLapeType_StaticArray.FinalizeConst(AVar: TLapeGlobalVar);
var
  i: Integer;
  Element: TLapeGlobalVar;
begin
  if (AVar = nil) or (not NeedFinalization) then
    Exit;

  for i := 0 to FRange.Hi - FRange.Lo do
  begin
    Element := FPType.NewGlobalVarP(Pointer(PtrInt(AVar.Ptr) + (FPType.Size * i)));
    try
      FPType.FinalizeConst(Element);
    finally
      Element.Free();
    end;
  end;
end;

procedure TLapeType_StaticArray.addArrayHelpers;
var
  Typ: TLapeType;
begin
  if (lcoArrayHelpers in FCompiler.Options) and HasType() then
  begin
    Typ := FCompiler.getGlobalType('!arrayhelpers_static');
    if (Typ <> nil) then
      FParentType := TLapeType(Typ);
  end;
end;

function TLapeType_String.VarToString(AVar: Pointer): lpString;
begin
  if (FBaseType = ltAnsiString) then
    Result := lpString('"'+PAnsiString(AVar)^+'"')
  else if (FBaseType = ltWideString) then
    Result := lpString('" '+PWideString(AVar)^+'"')
  else if (FBaseType = ltUnicodeString) then
    Result := lpString('"'+PUnicodeString(AVar)^+'"')
  else
    Result := lpString('"'+PlpString(AVar)^+'"');
end;

procedure TLapeType_String.VarUnique(var AVar: Pointer);
begin
  if (BaseType in LapeStringTypes) then
    case BaseType of
      ltAnsiString:    UniqueString(AnsiString(AVar));
      ltWideString:    UniqueString(WideString(AVar));
      ltUnicodeString: UniqueString(UnicodeString(AVar));
      else LapeException(lpeImpossible);
    end;
end;

procedure TLapeType_String.VarUnique(AVar: TResVar);
begin
  Assert(FCompiler <> nil);
  //FCompiler.EmitCode('System.UniqueString(AVar);', ['AVar'], [], [AVar], Offset, FCompiler.DocPos);

  with TLapeTree_Invoke.Create('UniqueString', FCompiler) do
  try
    addParam(TLapeTree_Var.Create(AVar, FCompiler));
    Emit();
  finally
    Free();
  end;
end;

function TLapeType_String.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(1, ltSizeInt);
end;

function TLapeType_String.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (AVar = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(Length(PString(AVar)^), ltSizeInt);
end;

function TLapeType_String.NewGlobalVarStr(Str: AnsiString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
  PAnsiString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: AnsiString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName);
end;

function TLapeType_String.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
  PUnicodeString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName);
end;

function TLapeType_String.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Right <> nil) then
  begin
    if (lefAssigning in Flags) then
      VarUnique(PPointer(Left.Ptr)^);

    if (Right <> nil) and Right.HasType() and (Right.VarType.BaseType in LapeOrdinalTypes) then
      Right := FCompiler.getIntConst(Right.AsInteger - 1);

    Result := //Result := Pointer[Index - 1]^
      inherited EvalConst(
        Op,
        Left,
        Right,
        Flags
      );
  end
  else
    Result := inherited;
end;

function TLapeType_String.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);

  if (op = op_Index) and (lefAssigning in Flags) then
    VarUnique(Left);
  Result := inherited;
  if (op = op_Index) and HasType() then
  begin
    if (not Result.VarPos.isPointer) then
      LapeException(lpeImpossible);
    Result.IncOffset(-(FPType.Size));
  end;
end;

procedure TLapeType_String.addArrayHelpers;
begin
  { nothing }
end;

constructor TLapeType_AnsiString.Create(ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltAnsiChar), ACompiler, AName);
  FBaseType := ltAnsiString;
end;

constructor TLapeType_WideString.Create(ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideChar), ACompiler, AName);
  FBaseType := ltWideString;
end;

constructor TLapeType_UnicodeString.Create(ACompiler: TLapeCompilerBase; AName: lpString = '');
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideChar), ACompiler, AName);
  FBaseType := ltUnicodeString;
end;

function TLapeType_ShortString.getAsString: lpString;
begin
  if (FAsString = '') then
  begin
    FAsString := inherited;
    FAsString := FAsString + '[' + lpString(IntToStr(FRange.Hi)) + ']';
  end;
  Result := inherited;
end;

constructor TLapeType_ShortString.Create(ACompiler: TLapeCompilerBase; ASize: UInt8 = High(UInt8); AName: lpString = '');
var
  StrRange: TLapeRange;
begin
  StrRange.Lo := 0;
  StrRange.Hi := ASize;
  Assert(ACompiler <> nil);
  inherited Create(StrRange, ACompiler.getBaseType(ltAnsiChar), ACompiler, AName);
  FBaseType := ltShortString;
end;

function TLapeType_ShortString.VarToString(AVar: Pointer): lpString;
begin
  Result := '"'+lpString(PShortString(AVar)^)+'"';
end;

function TLapeType_ShortString.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getIntConst(1, ltUInt8);
end;

function TLapeType_ShortString.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (AVar <> nil) then
    Result := FCompiler.getIntConst(Length(PShortString(AVar)^), ltUInt8)
  else
    Result := FCompiler.getIntConst(FRange.Hi, ltUInt8);
end;

function TLapeType_ShortString.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
  if (Length(Str) >= FRange.Hi) then
    Delete(Str, FRange.Hi, Length(Str) - FRange.Hi + 1);
  PShortString(Result.Ptr)^ := ShortString(Str);
end;

function TLapeType_ShortString.NewGlobalVar(Str: ShortString; AName: lpString = ''): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName);
  PShortString(Result.Ptr)^ := Str;
end;

function TLapeType_ShortString.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  EvalProc: TLapeEvalProc;
  tmpString: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Assign) and (Right <> nil) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (Right.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Right.VarType).Range.Hi) then
    begin
      EvalProc := getEvalProc(op_Assign, ltShortString, ltUInt8);
      Assert(ValidEvalFunction(EvalProc));
      EvalProc(Left.Ptr, Right.Ptr, @FRange.Hi);
      Result := Left;
    end
    else
    begin
      if (FRange.Hi < High(UInt8)) then
      begin
        tmpString := FCompiler.getBaseType(ltShortString).NewGlobalVarP();
        Result := tmpString.VarType.EvalConst(op_Assign, tmpString, Right, Flags);
      end
      else
        Result := inherited;

      if (Result <> nil) and Result.HasType() and (Result.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Result.VarType).Range.Hi) then
      begin
        Result := EvalConst(op_Assign, Left, Result, []);
        tmpString.Free();
      end;
    end
  else
    Result := inherited;
end;

function TLapeType_ShortString.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags): TResVar;
var
  EvalProc: TLapeEvalProc;
  tmpString: TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  Result := NullResVar;
  tmpString := NullResVar;

  if (op = op_Assign) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (Right.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Right.VarType).Range.Hi) then
    begin
      EvalProc := getEvalProc(op_Assign, ltShortString, ltUInt8);
      Assert(ValidEvalFunction(EvalProc));
      FCompiler.Emitter._Eval(EvalProc, Left, Right, _ResVar.New(VarHi()));
      Result := Left;
    end
    else
    begin
      if (FRange.Hi < High(UInt8)) then
      begin
        tmpString := FCompiler.getTempStackVar(ltShortString);
        FCompiler.getDestVar(Dest, tmpString, op_Unknown);
        Result := tmpString.VarType.Eval(op_Assign, Dest, tmpString, Right, Flags)
      end
      else
        Result := inherited;

      if Result.HasType() and (Result.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Result.VarType).Range.Hi) then
        Result := Eval(op_Assign, Dest, Left, Result, []);
    end
  else
    Result := inherited;
end;

end.

