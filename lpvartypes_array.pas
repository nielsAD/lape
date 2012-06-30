{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Array types.
}
unit lpvartypes_array;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lptree, lpvartypes;

type
  TLapeType_DynArray = class(TLapeType_Pointer)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    procedure VarSetLength(var AVar: Pointer; ALen: Integer); overload; virtual;
    procedure VarSetLength(AVar, ALen: TResVar; var Offset: Integer; Pos: PDocPos = nil); overload; virtual;

    function EvalRes(Op: EOperator; ARight: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; ALeft, ARight: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; ALeft, ARight: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_StaticArray = class(TLapeType_DynArray)
  protected
    FRange: TLapeRange;

    function getSize: Integer; override;
    function getAsString: lpString; override;
  public
    constructor Create(ARange: TLapeRange; ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function VarToString(AVar: Pointer): lpString; override;
    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;
    function NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); override;

    property Range: TLapeRange read FRange;
  end;

  TLapeType_String = class(TLapeType_DynArray)
  public
    function VarToString(AVar: Pointer): lpString; override;
    function NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_AnsiString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_WideString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UnicodeString = class(TLapeType_String)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_ShortString = class(TLapeType_StaticArray)
  protected
    function getAsString: lpString; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ASize: UInt8 = High(UInt8); AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function NewGlobalVar(Str: ShortString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; reintroduce; overload; virtual;

    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

implementation

uses
  lpparser, lpeval, lpexceptions;

function TLapeType_DynArray.getAsString: lpString;
begin
  if (FAsString = '') and (FBaseType = ltDynArray) then
    if HasType() then
      FAsString := 'array of ' + FPType.AsString
    else
      FAsString := 'array';
  Result := inherited;
end;

constructor TLapeType_DynArray.Create(ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, ArrayType, AName, ADocPos);
  FBaseType := ltDynArray;
end;

function TLapeType_DynArray.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_DynArray;
begin
  Result := TLapeClassType(Self.ClassType).Create(FPType, FCompiler, Name, @_DocPos);
  with TLapeType_DynArray(Result) do
  begin
    copyManagedDecls(Self.ManagedDecls, not DeepCopy);
    FBaseType := Self.BaseType;
  end;
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

  Result :=
    '  function _ElementToString(const p: System.Pointer): System.string;'                  + LineEnding +
    '  begin'                                                                               + LineEnding +
    '    Result := System.ToString['+IntToStr(Index)+'](p^);'                               + LineEnding +
    '  end;'                                                                                + LineEnding +
    'type'                                                                                  + LineEnding +
    '  TArrayToString = function(const Arr; AToString: System.Pointer; Len, Size: System.Int32): System.string;' + LineEnding +
    'var'                                                                                   + LineEnding +
    '  Len: System.Int32;'                                                                  + LineEnding +
    'begin'                                                                                 + LineEnding +
    '  Len := System.Length(Param0);'                                                       + LineEnding +
    '  if (Len <= 0) then'                                                                  + LineEnding +
    '    Result := '#39'[]'#39''                                                            + LineEnding +
    '  else'                                                                                + LineEnding +
    '    Result := TArrayToString('+AIA+'System._ArrayToString)('                           + LineEnding +
    '      Param0['+IntToStr(VarLo().AsInteger)+'],'                                        + LineEnding +
    '      '+AIA+'_ElementToString, Len, System.SizeOf(Param0[0]));'                        + LineEnding +
    'end;';
end;

function TLapeType_DynArray.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
  p: Pointer;
begin
  Result := '[';
  if (AVar <> nil) and HasType() then
  begin
    p := PPointer(AVar)^;
    for i := 0 to Length(PCodeArray(AVar)^) - 1 do
    begin
      if (i > 0) then
        Result := Result + ', ';
      Result := Result + FPType.VarToString(Pointer(PtrUInt(p) + UInt32(FPType.Size * i)));
    end;
  end;
  Result := Result + ']';
end;

function TLapeType_DynArray.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(0);
end;

function TLapeType_DynArray.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) or (AVar = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(High(PCodeArray(AVar)^));
end;

procedure TLapeType_DynArray.VarSetLength(var AVar: Pointer; ALen: Integer);
var
  i, OldLen, NewSize: SizeInt;
  NewP: Pointer;
  DoFree: Boolean;
  tmpLeft, tmpRight: TLapeGlobalVar;
begin
  if (not (BaseType in LapeArrayTypes - [ltStaticArray, ltShortString])) then
    LapeException(lpeInvalidEvaluation);
  if (not HasType()) then
    Exit;

  if (BaseType in LapeStringTypes) then
  begin
    case BaseType of
      ltAnsiString:    SetLength(AnsiString(AVar), ALen);
      ltWideString:    SetLength(WideString(AVar), ALen);
      ltUnicodeString: SetLength(UnicodeString(AVar), ALen);
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
          PType.Finalize(tmpLeft);
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
        PType.EvalConst(op_Assign, tmpLeft, tmpRight);
      finally
        tmpLeft.Free();
        tmpRight.Free();
      end;
      Dec(i);
    end;

    AVar := NewP;
  end;
end;

procedure TLapeType_DynArray.VarSetLength(AVar, ALen: TResVar; var Offset: Integer; Pos: PDocPos = nil);
begin
  Assert(FCompiler <> nil);
  //FCompiler.EmitCode('System.SetLength(AVar, ALen);', ['AVar', 'ALen'], [], [AVar, ALen], Offset, Pos);

  with TLapeTree_InternalMethod_SetLength.Create(FCompiler, Pos) do
  try
    addParam(TLapeTree_ResVar.Create(AVar, FCompiler, Pos));
    addParam(TLapeTree_ResVar.Create(ALen, FCompiler, Pos));
    Compile(Offset);
  finally
    Free();
  end;
end;

function TLapeType_DynArray.EvalRes(Op: EOperator; ARight: TLapeType = nil): TLapeType;
begin
  if (op = op_Index) then
    Result := FPType
  else if (op = op_Assign) and (BaseType = ltDynArray) and HasType() and (ARight <> nil) and (ARight is ClassType) and FPType.Equals(TLapeType_DynArray(ARight).FPType) then
    Result := Self
  else if (op = op_Plus) and (BaseType = ltDynArray) and HasType() and FPType.CompatibleWith(ARight) then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_DynArray.EvalConst(Op: EOperator; ALeft, ARight: TLapeGlobalVar): TLapeGlobalVar;
var
  tmpType: ELapeBaseType;
  IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((ALeft = nil) or (ALeft.VarType is TLapeType_Pointer));

  if (op = op_Index) then
  begin
    tmpType := FBaseType;
    FBaseType := ltPointer;
    IndexVar := nil;
    try
      IndexVar := inherited EvalConst(Op, ALeft, ARight);
      Result := //Result := Pointer[Index]^
        EvalConst(
          op_Deref,
          IndexVar,
          nil
        );
    finally
      if (IndexVar <> nil) then
        IndexVar.Free();
      FBaseType := tmpType;
    end;
  end
  else if (op = op_Assign) and (BaseType = ltDynArray) and (ALeft <> nil) and (ARight <> nil) and CompatibleWith(ARight.VarType) then
    if (ARight.BaseType = ltDynArray) then
    begin
      if (PPointer(ALeft.Ptr)^ = PPointer(ARight.Ptr)^) then
        Exit(ALeft);

      VarSetLength(PPointer(ALeft.Ptr)^, 0);
      Result := inherited;
      if (Result <> nil) and (Result.Ptr <> nil) and (PPointer(Result.Ptr)^ <> nil) then
        Inc(PtrInt(Pointer(PtrInt(Result.Ptr^) - SizeOf(SizeInt) - SizeOf(PtrInt))^));
    end
    else if (ARight.VarType is TLapeType_StaticArray) then
    begin
      VarSetLength(PPointer(ALeft.Ptr)^, TLapeType_StaticArray(ARight.VarType).Range.Hi - TLapeType_StaticArray(ARight.VarType).Range.Lo + 1);
      Result := ALeft;

      IndexVar := EvalConst(op_Index, ALeft, FCompiler.getConstant(0));
      try
        IndexVar.VarType := ARight.VarType;
        IndexVar.VarType.EvalConst(op_Assign, IndexVar, ARight);
      finally
        IndexVar.Free();
      end;
    end
    else
      LapeException(lpeImpossible)
  else if (op = op_Plus) and (BaseType = ltDynArray) and (ALeft <> nil) and (ARight <> nil) and HasType() and FPType.CompatibleWith(ARight.VarType) then
  begin
    Result := EvalConst(op_Assign, NewGlobalVarP(), ALeft);
    IndexVar := FCompiler.getConstant(Length(PCodeArray(Result.Ptr)^));
    VarSetLength(PPointer(Result.Ptr)^, IndexVar.AsInteger + 1);

    IndexVar := EvalConst(op_Index, Result, IndexVar);
    try
      PType.EvalConst(op_Assign, IndexVar, ARight);
    finally
      IndexVar.Free();
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_DynArray.Eval(Op: EOperator; var Dest: TResVar; ALeft, ARight: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpType: ELapeBaseType;
  tmpVar: TLapeStackTempVar;
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
  try
    tmpType := FBaseType;
    FBaseType := ltPointer;
    if (not ALeft.VarPos.isPointer) then
    begin
      if (Dest.VarPos.MemPos = mpStack) then
        Dest.Spill();
      Result := inherited Eval(Op, Dest, ALeft, ARight, Offset, Pos);
      Result.VarPos.isPointer := True;
      Result.VarType := FPType;
    end
    else
    begin
      IndexVar := inherited Eval(Op, IndexVar, ALeft, ARight, Offset, Pos);
      Result := //Result := Pointer[Index]^
        Eval(
          op_Deref,
          Dest,
          IndexVar,
          NullResVar,
          Offset,
          Pos
        );
      IndexVar.Spill(1);
    end;
  finally
    FBaseType := tmpType;
  end
  else if (op = op_Assign) and (BaseType = ltDynArray) and CompatibleWith(ARight.VarType) then
  begin
    Result := ALeft;
    if (ALeft.VarPos.MemPos = mpStack) then
    begin
      tmpVar := FCompiler.getTempVar(Self, BigLock);
      tmpVar.isConstant := False;
      ALeft := _ResVar.New(tmpVar);
    end;

    if (ARight.VarType.BaseType = ltDynArray) then
    begin
      {FCompiler.EmitCode(
        'if (System.Pointer(Left) <> System.Pointer(Right)) then begin'     +
        '  System.SetLength(Left, 0);'                                      +
        '  System.Pointer(Left) := System.Pointer(Right);'                  +
        '  if (System.Pointer(Left) <> nil) then'                           +
        '    System.Inc(System.PtrInt(System.Pointer(Left)[-'               +
               IntToStr(SizeOf(SizeInt)+SizeOf(PtrInt))                     +
             ']^));'                                                        +
        'end;'
      , ['Left', 'Right'], [], [ALeft, ARight], Offset, Pos)}

      with TLapeTree_If.Create(FCompiler, Pos) do
      try
        Body := TLapeTree_StatementList.Create(Self.FCompiler, Pos);
        with TLapeTree_StatementList(Body) do
        begin
          Node := TLapeTree_InternalMethod_SetLength.Create(Body);
          with TLapeTree_InternalMethod_SetLength(Node) do
          begin
            addParam(TLapeTree_ResVar.Create(ALeft.IncLock(), Body));
            addParam(TLapeTree_Integer.Create(0, Body));
          end;
          addStatement(Node);
        end;

        ALeft.VarType := FCompiler.getPointerType(ltNativeInt);
        ARight.VarType := ALeft.VarType;

        Condition := TLapeTree_Operator.Create(op_cmp_NotEqual, Body);
        with TLapeTree_Operator(Condition) do
        begin
          Left := TLapeTree_ResVar.Create(ALeft.IncLock(), Condition);
          Right := TLapeTree_ResVar.Create(ARight.IncLock(), Condition);
        end;

        with TLapeTree_StatementList(Body) do
        begin
          Node := TLapeTree_Operator.Create(op_Assign, Condition);
          with TLapeTree_Operator(Node) do
          begin
            Left := TLapeTree_ResVar.Create(ALeft.IncLock(2), Condition);
            Right := TLapeTree_ResVar.Create(ARight.IncLock(), Condition);
          end;
          addStatement(Node);

          Node := TLapeTree_If.Create(Condition);
          with TLapeTree_If(Node) do
          begin
            Condition := TLapeTree_Operator.Create(op_cmp_NotEqual, Node);
            with TLapeTree_Operator(Condition) do
            begin
              Left := TLapeTree_ResVar.Create(ALeft.IncLock(), Condition);
              Right := TLapeTree_GlobalVar.Create('nil', ltPointer, Condition);
            end;

            Body := TLapeTree_InternalMethod_Inc.Create(Condition);
            with TLapeTree_InternalMethod(Body) do
            begin
              with TLapeTree_Operator.Create(op_Deref, Condition) do
              begin
                Left := TLapeTree_Operator.Create(op_Index, Condition);
                with TLapeTree_Operator(Left) do
                begin
                  Left := TLapeTree_ResVar.Create(ALeft.IncLock(), Condition);
                  Right := TLapeTree_Integer.Create(-2, Condition);
                end;
                addParam(Left.Parent as TLapeTree_ExprBase);
              end;
            end;
           end;
           addStatement(Node);
        end;

        Compile(Offset);
      finally
        Free();
      end;
    end
    else if (ARight.VarType is TLapeType_StaticArray) then
    begin
      {FCompiler.EmitCode(
        'System.SetLength(Left, '+IntToStr(
          TLapeType_StaticArray(ARight.VarType).Range.Hi -
          TLapeType_StaticArray(ARight.VarType).Range.Lo + 1)               +
        ');'                                                                +
        'PType(@Left[0])^ := Right;',
        ['PType', 'Left', 'Right'], [FCompiler.getTypeVar(FCompiler.getPointerType(ARight.VarType))],
        [ALeft, ARight], Offset, Pos)}

      with TLapeType_StaticArray(ARight.VarType).Range do
        VarSetLength(ALeft.IncLock(), _ResVar.New(FCompiler.getConstant(Hi - Lo + 1)), Offset, Pos);

      with TLapeTree_Operator.Create(op_Index, FCompiler, Pos) do
      try
        Left := TLapeTree_ResVar.Create(ALeft.IncLock(), FCompiler, Pos);
        Right := TLapeTree_Integer.Create(0, Left);
        IndexVar := Compile(Offset);
      finally
        Free();
      end;

      IndexVar.VarType := ARight.VarType;
      with TLapeTree_Operator.Create(op_Assign, FCompiler, Pos) do
      try
        Left := TLapeTree_ResVar.Create(IndexVar.IncLock(), FCompiler, Pos);
        Right := TLapeTree_ResVar.Create(ARight.IncLock(), Left);
        Compile(Offset);
      finally
        Free();
      end;
    end
    else
      LapeException(lpeImpossible);

    if (tmpVar <> nil) then
    begin
      FCompiler.Emitter._PopVarToStack(Size, tmpVar.Offset, Offset, Pos);
      ALeft.Spill(BigLock);
    end;
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

    wasConstant := not Result.Writeable;
    if wasConstant then
      Result.Writeable := True;

    Result := Eval(op_Assign, tmpResVar, Result, ALeft, Offset, Pos);

    IndexVar := _ResVar.New(FCompiler.getTempVar(ltInt32, BigLock));
    try
    {FCompiler.EmitCode(
      'System.SetLength(Result, System.Length(Result) + 1);' +
      'Result[System.High(Result)] := Right;'
    , ['Result', 'Right'], [], [Result, ARight], Offset, Pos);}

      IndexVar.isConstant := False;
      with TLapeTree_Operator.Create(op_Plus, FCompiler, Pos) do
      try
        Left := TLapeTree_Operator.Create(op_Assign, FCompiler, Pos);
        Right := TLapeTree_Integer.Create(1, Left);
        with TLapeTree_Operator(Left) do
        begin
          Left := TLapeTree_ResVar.Create(IndexVar, FCompiler, Pos);
          Right := TLapeTree_InternalMethod_Length.Create(Left);
          TLapeTree_InternalMethod_Length(Right).addParam(TLapeTree_ResVar.Create(Result.IncLock(), Left));
        end;

        Dest := VarResVar;
        tmpResVar := Compile(Offset);
      finally
        Free();
      end;

      VarSetLength(Result.IncLock(), tmpResVar, Offset, Pos);
      with TLapeTree_Operator.Create(op_Assign, FCompiler, Pos) do
      try
        Left := TLapeTree_Operator.Create(op_Index, FCompiler, Pos);
        with TLapeTree_Operator(Left) do
        begin
          Left := TLapeTree_ResVar.Create(Result.IncLock(), FCompiler, Pos);
          Right := TLapeTree_ResVar.Create(IndexVar, Left);
        end;
        Right := TLapeTree_ResVar.Create(ARight.IncLock(), Left);
        Compile(Offset);
      finally
        Free();
      end;
    finally
      IndexVar.Spill(BigLock);
    end;

    if wasConstant then
      Result.Writeable := False;
  end
  else
    Result := inherited;
end;

function TLapeType_StaticArray.getSize: Integer;
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
    FAsString := 'array [' + IntToStr(FRange.Lo) + '..' + IntToStr(FRange.Hi) + ']';
    if HasType() then
      FAsString := FAsString + ' of ' + FPType.AsString;
  end;
  Result := inherited;
end;

constructor TLapeType_StaticArray.Create(ARange: TLapeRange; ArrayType: TLapeType; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ArrayType, ACompiler, AName, ADocPos);
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
begin
  Result := '[';
  if (AVar <> nil) and HasType() then
    for i := 0 to FRange.Hi - FRange.Lo do
    begin
      if (i > 0) then
        Result := Result + ', ';
      Result := Result + FPType.VarToString(Pointer(PtrInt(AVar) + (FPType.Size * i)));
    end;
  Result := Result + ']';
end;

function TLapeType_StaticArray.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(FRange.Lo);
end;

function TLapeType_StaticArray.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FCompiler = nil) then
    Result := nil
  else
    Result := FCompiler.getConstant(FRange.Hi);
end;

function TLapeType_StaticArray.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_StaticArray;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FPType, FCompiler, Name, @_DocPos);
  with TLapeType_StaticArray(Result) do
  begin
    copyManagedDecls(Self.ManagedDecls, not DeepCopy);
    FBaseType := Self.BaseType;
  end;
end;

function TLapeType_StaticArray.NewGlobalVar(AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
end;

function TLapeType_StaticArray.EvalRes(Op: EOperator; Right: TLapeType = nil): TLapeType;
begin
  if (op = op_Assign) and (BaseType = ltStaticArray) and HasType() and (Right <> nil) and (Right is ClassType) and
     ((TLapeType_StaticArray(Right).Range.Hi - TLapeType_StaticArray(Right).Range.Lo) = (Range.Hi - Range.Lo)) and
     FPType.CompatibleWith(TLapeType_StaticArray(Right).FPType)
  then
    Result := Self
  else
    Result := inherited;
end;

function TLapeType_StaticArray.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  i: Integer;
  LeftVar, RightVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Left <> nil) and (Right <> nil) then
  begin
    Assert(HasType());

    i := Right.AsInteger;
    if (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else if (i < FRange.Lo) or (i > FRange.Hi) then
      LapeException(lpeOutOfTypeRange);

    Result := FPType.NewGlobalVarP(Pointer(PtrInt(Left.Ptr) + (FPType.Size * (i - FRange.Lo))));
    Result.CopyFlags(Left);
  end
  else if (op = op_Assign) and (BaseType = ltStaticArray) and (Left <> nil) and (Right <> nil) and CompatibleWith(Right.VarType) then
  begin
    LeftVar := nil;
    RightVar := nil;

    for i := 0 to FRange.Hi - FRange.Lo do
    try
      LeftVar := FPType.NewGlobalVarP(Pointer(PtrInt(Left.Ptr) + (FPType.Size * i)));
      RightVar := FPType.NewGlobalVarP(Pointer(PtrInt(Right.Ptr) + (FPType.Size * i)));
      FPType.EvalConst(op_Assign, LeftVar, RightVar);
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

function TLapeType_StaticArray.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpVar, LeftVar: TResVar;
  wasConstant: Boolean;
  tmpType: TLapeType;
  CounterVar, IndexLow, IndexHigh: TLapeVar;
  LoopOffset: Integer;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);
  tmpVar := NullResVar;
  LeftVar := NullResVar;

  if (op = op_Index) then
  try
    wasConstant := not Left.Writeable;
    if wasConstant then
      Left.Writeable := True;

    if (not Left.VarPos.isPointer) or (Left.VarPos.Offset > 0) then
      LeftVar := Eval(op_Addr, tmpVar, Left, NullResVar, Offset, Pos)
    else
    begin
      LeftVar := Left.IncLock();
      LeftVar.VarPos.isPointer := False;
      LeftVar.VarType := FCompiler.getPointerType(PType);
    end;

    if (FRange.Lo = 0) then
      Result := inherited Eval(Op, Dest, LeftVar, Right, Offset, Pos)
    else
    try
      tmpType := Right.VarType;
      if (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
        if Right.HasType() then
          LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
        else
          LapeException(lpeInvalidEvaluation)
      else
        Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

      Result := //Result := @Pointer[Index - Lo]^
        inherited Eval(
          Op,
          Dest,
          LeftVar,
          Right.VarType.Eval(op_Minus, tmpVar, Right, _ResVar.New(
            FCompiler.getConstant(FRange.Lo, DetermineIntType(FRange.Lo, Right.VarType.BaseType, False), False, True)
          ), Offset, Pos),
          Offset,
          Pos
        );
    finally
      Right.VarType := tmpType;
    end;
  finally
    if (not Left.VarPos.isPointer) or (Left.VarPos.Offset > 0) then
      LeftVar.Spill(1);

    Left.Writeable := not wasConstant;
    Result.CopyFlags(Left);
  end
  else if (op = op_Assign) and (BaseType = ltStaticArray) and CompatibleWith(Right.VarType) then
    if (not NeedInitialization) and Equals(Right.VarType) and (Size > 0) then
    try
      tmpType := Right.VarType;
      Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, False));

      if Left.HasType() then
      begin
        Right.VarType := Left.VarType;
        Result := Left.VarType.Eval(op_Assign, Dest, Left, Right, Offset, Pos);
      end
      else
      begin
        Left.VarType := Self;
        if (Left.VarPos.MemPos = mpStack) and (not Left.VarPos.ForceVariable) then
        begin
          FCompiler.Emitter._GrowStack(Size, Offset, Pos);
          Left.VarPos.ForceVariable := True;

          IndexLow := FCompiler.getTempVar(ltPointer, 1);
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), _ResVar.New(IndexLow), Left, NullResVar, Offset, Pos);

          wasConstant := True;
        end
        else
          wasConstant := False;

        IndexHigh := FCompiler.getConstant(Size);
        tmpVar := Compiler.getTempStackVar(ltPointer);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Right, NullResVar, Offset, Pos);
        if wasConstant then
          FCompiler.Emitter._Eval(getEvalProc(op_Assign, ltPointer, ltPointer), tmpVar, _ResVar.New(IndexLow), NullResVar, Offset, Pos)
        else
          FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, Left, NullResVar, Offset, Pos);
        FCompiler.Emitter._Eval(getEvalProc(op_Addr, ltUnknown, ltUnknown), tmpVar, _ResVar.New(IndexHigh), NullResVar, Offset, Pos);
        FCompiler.Emitter._InvokeImportedProc(_ResVar.New(FCompiler['!move']), SizeOf(Pointer)*3, Offset, Pos);
        Result := Left;

        if wasConstant then
        begin
          TLapeStackTempVar(IndexLow).Declock(1);
          Left.VarPos.ForceVariable := False;
        end;
      end;

    finally
      Left.VarType := Self;
      Right.VarType := tmpType;
    end
    else
    begin
      Assert(HasType());
      if (Left.VarPos.MemPos = mpStack) and (not Left.VarPos.ForceVariable) then
      begin
        FCompiler.Emitter._GrowStack(Size, Offset, Pos);
        Left.VarPos.ForceVariable := True;
        wasConstant := True;
      end
      else
        wasConstant := False;

      CounterVar := FCompiler.getTempVar(DetermineIntType(FRange.Lo, FRange.Hi, ltNativeInt), BigLock);
      IndexLow := FCompiler.getConstant(FRange.Lo, CounterVar.VarType.BaseType, False, True);
      IndexHigh := FCompiler.getConstant(FRange.Hi, CounterVar.VarType.BaseType, False, True);
      LeftVar := CounterVar.VarType.Eval(op_Assign, LeftVar, _ResVar.New(CounterVar), _ResVar.New(IndexLow), Offset, Pos);
      LoopOffset := Offset;
      FPType.Eval(op_Assign, tmpVar, Eval(op_Index, tmpVar, Left, LeftVar, Offset, Pos), Right.VarType.Eval(op_Index, tmpVar, Right, LeftVar, Offset, Pos), Offset, Pos);
      CounterVar.VarType.Eval(op_Assign, tmpVar, LeftVar, CounterVar.VarType.Eval(op_Plus, tmpVar, LeftVar, _ResVar.New(FCompiler.getConstant(1, CounterVar.VarType.BaseType, False, True)), Offset, Pos), Offset, Pos);
      FCompiler.Emitter._JmpRIf(LoopOffset - Offset, CounterVar.VarType.Eval(op_cmp_LessThanOrEqual, tmpVar, LeftVar, _ResVar.New(IndexHigh), Offset, Pos), Offset, Pos);
      LeftVar.Spill(BigLock);

      Result := Left;
      if wasConstant then
        Left.VarPos.ForceVariable := False;
    end
  else
    Result := inherited;
end;

procedure TLapeType_StaticArray.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);
var
  i, LoopOffset: Integer;
  tmpVar, IndexVar: TResVar;
  Counter, LowIndex, HighIndex: TLapeVar;
begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) {or (not NeedFinalization)} then
    Exit;

  IndexVar := NullResVar;
  tmpVar := NullResVar;
  if UseCompiler and (FCompiler <> nil) then
  begin
    Counter := FCompiler.getTempVar(DetermineIntType(FRange.Lo, FRange.Hi, ltNativeInt), BigLock);
    LowIndex := FCompiler.addManagedVar(Counter.VarType.NewGlobalVarStr(IntToStr(FRange.Lo)));
    HighIndex := FCompiler.addManagedVar(Counter.VarType.NewGlobalVarStr(IntToStr(FRange.Hi)));
    IndexVar := Counter.VarType.Eval(op_Assign, IndexVar, _ResVar.New(Counter), _ResVar.New(LowIndex), Offset, Pos);
    LoopOffset := Offset;
    FCompiler.FinalizeVar(Eval(op_Index, tmpVar, AVar, IndexVar, Offset, Pos), Offset, Pos);
    Counter.VarType.Eval(op_Assign, tmpVar, IndexVar, Counter.VarType.Eval(op_Plus, tmpVar, IndexVar, _ResVar.New(FCompiler.addManagedVar(Counter.VarType.NewGlobalVarStr('1'))), Offset, Pos), Offset, Pos);
    FCompiler.Emitter._JmpRIf(LoopOffset - Offset, Counter.VarType.Eval(op_cmp_LessThanOrEqual, tmpVar, IndexVar, _ResVar.New(HighIndex), Offset, Pos), Offset, Pos);
    IndexVar.Spill(BigLock);
  end
  else if (AVar.VarPos.MemPos <> mpMem) then
    LapeException(lpeImpossible)
  else if NeedFinalization then
    for i := 0 to FRange.Hi - FRange.Lo do
    try
      IndexVar := AVar;
      IndexVar.VarType := FPType;
      IndexVar.VarPos.GlobalVar := IndexVar.VarType.NewGlobalVarP(Pointer(PtrInt(IndexVar.VarPos.GlobalVar.Ptr) + (FPType.Size * i)));
      FPType.Finalize(IndexVar, Offset, UseCompiler, Pos);
    finally
      FreeAndNil(IndexVar.VarPos.GlobalVar);
    end;
end;

function TLapeType_String.VarToString(AVar: Pointer): lpString;
begin
  if (FBaseType = ltAnsiString) then
    Result := '"'+PAnsiString(AVar)^+'"'
  else if (FBaseType = ltWideString) then
    Result := '" '+PWideString(AVar)^+'"'
  else if (FBaseType = ltUnicodeString) then
    Result := '"'+PUnicodeString(AVar)^+'"'
  else
    Result := '"'+PlpString(AVar)^+'"';
end;

function TLapeType_String.NewGlobalVarStr(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PAnsiString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: AnsiString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName, ADocPos);
end;

function TLapeType_String.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PUnicodeString(Result.Ptr)^ := Str;
end;

function TLapeType_String.NewGlobalVar(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarStr(Str, AName, ADocPos);
end;

function TLapeType_String.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  tmpType: TLapeType;
  IndexVar: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Index) and (Right <> nil) then
  begin
    tmpType := Right.VarType;
    if (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
      if Right.HasType() then
        LapeExceptionFmt(lpeInvalidIndex, [Right.VarType.AsString])
      else
        LapeException(lpeInvalidEvaluation)
    else if (Right.AsInteger < 1) then
      LapeException(lpeOutOfTypeRange)
    else
      Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    IndexVar := nil;
    try
      IndexVar := Right.VarType.EvalConst(op_Minus, Right, FCompiler.getConstant(1, ltUInt8, False, True));
      Result := //Result := Pointer[Index - 1]^
        inherited EvalConst(
          Op,
          Left,
          IndexVar
        );
    finally
      if (IndexVar <> nil) then
        IndexVar.Free();
      Right.VarType := tmpType;
    end;
  end
  else
    Result := inherited;
end;

function TLapeType_String.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType is TLapeType_Pointer);

  Result := inherited;
  if (op = op_Index) and HasType() then
  begin
    if (not Result.VarPos.isPointer) then
      LapeException(lpeImpossible);
    Result.DecOffset(FPType.Size);
  end;
end;

constructor TLapeType_AnsiString.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltAnsiChar), ACompiler, AName, ADocPos);
  FBaseType := ltAnsiString;
end;

constructor TLapeType_WideString.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideChar), ACompiler, AName, ADocPos);
  FBaseType := ltWideString;
end;

constructor TLapeType_UnicodeString.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  inherited Create(ACompiler.getBaseType(ltWideChar), ACompiler, AName, ADocPos);
  FBaseType := ltUnicodeString;
end;

function TLapeType_ShortString.getAsString: lpString;
begin
  if (FAsString = '') then
  begin
    FAsString := inherited;
    FAsString := FAsString + '[' + IntToStr(FRange.Hi) + ']';
  end;
  Result := inherited;
end;

constructor TLapeType_ShortString.Create(ACompiler: TLapeCompilerBase; ASize: UInt8 = High(UInt8); AName: lpString = ''; ADocPos: PDocPos = nil);
var
  StrRange: TLapeRange;
begin
  StrRange.Lo := 0;
  StrRange.Hi := ASize;
  Assert(ACompiler <> nil);
  inherited Create(StrRange, ACompiler.getBaseType(ltAnsiChar), ACompiler, AName, ADocPos);
  FBaseType := ltShortString;
end;

function TLapeType_ShortString.VarToString(AVar: Pointer): lpString;
begin
  Result := '"'+PShortString(AVar)^+'"';
end;

function TLapeType_ShortString.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  if (Length(Str) >= FRange.Hi) then
    Delete(Str, FRange.Hi, Length(Str) - FRange.Hi + 1);
  PShortString(Result.Ptr)^ := Str;
end;

function TLapeType_ShortString.NewGlobalVar(Str: ShortString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PShortString(Result.Ptr)^ := Str;
end;

function TLapeType_ShortString.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): TLapeGlobalVar;
var
  EvalProc: TLapeEvalProc;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType is TLapeType_Pointer));

  if (op = op_Assign) and (Right <> nil) and Right.HasType() and CompatibleWith(Right.VarType) then
    if (Right.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Right.VarType).Range.Hi) then
    begin
      EvalProc := getEvalProc(op_Assign, ltShortString, ltUInt8);
      Assert(ValidEvalFunction(EvalProc));
      EvalProc(Left.Ptr, Right.Ptr, @FRange.Hi);
    end
    else
    begin
      if (FRange.Hi < High(UInt8)) then
      begin
        Result := FCompiler.getBaseType(ltShortString).NewGlobalVarP();
        Result := Result.VarType.EvalConst(Op, Result, Right);
      end
      else
        Result := inherited;

      Result := inherited;
      if (Result <> nil) and Result.HasType() and (Result.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Result.VarType).Range.Hi) then
        with Result do
        begin
          Result := EvalConst(op_Assign, Left, Result);
          Free();
        end;
    end
  else
    Result := inherited;
end;

function TLapeType_ShortString.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; var Offset: Integer; Pos: PDocPos = nil): TResVar;
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
      FCompiler.Emitter._Eval(EvalProc, Left, Right, _ResVar.New(FCompiler.getConstant(FRange.Hi, ltUInt8, False, True)), Offset, Pos);
      Result := Left;
    end
    else
    begin
      if (FRange.Hi < High(UInt8)) then
      begin
        tmpString := FCompiler.getTempStackVar(ltShortString);
        FCompiler.getDestVar(Dest, tmpString, op_Unknown);
        Result := tmpString.VarType.Eval(Op, Dest, tmpString, Right, Offset, Pos)
      end
      else
        Result := inherited;

      if Result.HasType() and (Result.VarType is TLapeType_ShortString) and (FRange.Hi <> TLapeType_ShortString(Result.VarType).Range.Hi) then
      begin
        Result := Eval(op_Assign, Dest, Left, Result, Offset, Pos);
        tmpString.Spill(1);
      end;
    end
  else
    Result := inherited;
end;

end.

