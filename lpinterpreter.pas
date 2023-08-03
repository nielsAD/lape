{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Opcodes and bytecode interpreter.
}
unit lpinterpreter;

{$I lape.inc}
{$IFNDEF FPC}
  {$UNDEF Lape_Inline}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes,
  lpinterpreter_types;

const
  StackSize = 2048 * SizeOf(Pointer);
  VarStackSize = 512 * SizeOf(Pointer);
  VarStackStackSize = 32;
  TryStackSize = 512;
  CallStackSize = 512;

procedure RunCode(const Emitter: TLapeCodeEmitter; var DoContinue: TInitBool; const InitialVarStack: TByteArray = nil; const InitialJump: TCodePos = 0); overload;
procedure RunCode(const Emitter: TLapeCodeEmitter; const InitialVarStack: TByteArray = nil; const InitialJump: TCodePos = 0); overload;

implementation

uses
  lpmessages;

{$OverFlowChecks Off}

type
  TStackTraceInfo = array of record
    Address: TCodePos;
    CalledFrom: PByte;
  end;

  TJumpException = class
    Obj: Exception;
    Pos: PDocPos;
    StackTraceInfo: TStackTraceInfo;
  end;

  TInJump = record
    JumpException: TJumpException;
    JumpSafe: PByte;
  end;

const
  InEmptyJump: TInJump = (JumpException: nil; JumpSafe: nil);

function GetStackTrace(Emitter: TLapeCodeEmitter; StackTraceInfo: TStackTraceInfo; Pos: PDocPos): lpString;

  function FormatLine(Number: Integer; FuncName: String; Pos: PDocPos): lpString;
  begin
    if (FuncName = 'main') then
      FuncName := '"' + FuncName + '"'
    else
      FuncName := 'function "' + FuncName + '"';

    if (Pos <> nil) and (Pos^.Col <> NullDocPos.Col) and (Pos^.Line <> NullDocPos.Line) then
    begin
      Result := LineEnding + '  ' + IntToStr(Number) + ') Line ' + IntToStr(Pos^.Line) + ' in ' + FuncName;
      if (Pos^.FileName <> '') then
        Result := Result + ' in file "' + Pos^.FileName + '"';
    end else
      Result := LineEnding + '  ' + IntToStr(Number) + ') ' + FuncName;
  end;

var
  i, Number: Integer;
  DocPos: PDocPos;
begin
  Result := 'Stack Trace:';

  if (Length(StackTraceInfo) = 0) then
    Result := Result + FormatLine(0, 'main', Pos)
  else
  begin
    Result := Result + FormatLine(0, Emitter.CodePointerName[StackTraceInfo[High(StackTraceInfo)].Address], Pos);

    Number := 1;
    for i := High(StackTraceInfo) downto 0 do
    begin
      DocPos := @NullDocPos;
      {$IFDEF Lape_EmitPos}
      DocPos := PPointer(StackTraceInfo[i].CalledFrom + SizeOf(opCode))^;
      {$ENDIF}

      if (i > 0) then
        Result := Result + FormatLine(Number, Emitter.CodePointerName[StackTraceInfo[i - 1].Address], DocPos)
      else
        Result := Result + FormatLine(Number, 'main', DocPos);

      Inc(Number);
    end;
  end;
end;

procedure MergeJumps(var AJump: TInJump; const Merge: TInJump); {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (Merge.JumpException <> nil) then
  begin
    if (AJump.JumpException <> nil) and (AJump.JumpException <> Merge.JumpException) then
      Merge.JumpException.Free()
    else
      AJump.JumpException := Merge.JumpException;
  end;

  if (Merge.JumpSafe > AJump.JumpSafe) then
    AJump.JumpSafe := Merge.JumpSafe;
end;

function IsLapeException(const E: Exception): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  Result := (E <> nil) and (E.ClassType = lpException) and (lpException(e).DocPos.Col <> NullDocPos.Col) and (lpException(e).DocPos.Line <> NullDocPos.Line);
end;

procedure RunCode(const Emitter: TLapeCodeEmitter; var DoContinue: TInitBool; const InitialVarStack: TByteArray; const InitialJump: TCodePos);
const
  opNone: opCode = ocNone;
var
  Code: PByte;
  CodeBase: PByte;
  CodeUpper: PByte;
  Stack: TByteArray;
  StackPos: UInt32;

  VarStack: TByteArray;
  VarStackIndex, VarStackPos, VarStackLen: UInt32;
  VarStackStack: array of record
    Stack: TByteArray;
    Pos: UInt32;
  end;

  TryStack: array of record
    Jmp: PByte;
    JmpFinally: PByte;
  end;
  TryStackPos: UInt32;
  InJump: TInJump;

  CallStack: array of record
    Address: TCodePos;
    CalledFrom: PByte;
    JumpBack: PByte;
    StackP, VarStackP: UInt32;
    Jump: TInJump;
  end;
  CallStackPos: UInt32;

  PreviousException: record
    Exists: Boolean;
    Msg: lpString;
    Loc: TDocPos;
  end;

  function NeedMoreVarStack(const Size: UInt32): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := VarStackLen + Size > Length(VarStack);
    if not Result then
    begin
      VarStackPos := VarStackLen;
      VarStackLen := VarStackLen + Size;
    end;
  end;

  function NeedMoreGrowVarStack(const Size: UInt32): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := VarStackLen + Size > Length(VarStack);
    if not Result then
      VarStackLen := VarStackLen + Size;
  end;

  procedure ExpandVarStack(const Size: UInt32);
  begin
    Assert(VarStackLen + Size > Length(VarStack));

    VarStackStack[VarStackIndex].Pos := VarStackLen;

    Inc(VarStackIndex);
    if (VarStackIndex >= UInt32(Length(VarStackStack))) then
      SetLength(VarStackStack, VarStackIndex + (VarStackStackSize div 2));

    VarStackPos := 0;
    VarStackLen := Size;
    VarStack := VarStackStack[VarStackIndex].Stack;

    if (Size > VarStackSize) then
      SetLength(VarStack, Size)
    else
    if (VarStack = nil) then
      SetLength(VarStack, VarStackSize);

    VarStackStack[VarStackIndex].Stack := VarStack;
  end;

  procedure GrowVarStack(const Size: UInt32);
  var
    OldLen: UInt32;
  begin
    Assert(VarStackLen + Size > Length(VarStack));

    if (VarStackPos = 0) then
    begin
      Inc(VarStackLen, Size);
      SetLength(VarStack, VarStackLen);
      VarStackStack[VarStackIndex].Stack := VarStack;
    end else
    begin
      OldLen := VarStackLen - VarStackPos;
      ExpandVarStack(Size + OldLen);
      with VarStackStack[VarStackIndex - 1] do
      begin
        Dec(Pos, OldLen);
        Move(Stack[Pos], VarStack[0], OldLen);
      end;
    end;
  end;

  procedure JumpTo(const Target: TCodePos); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (Target = 0) then
      LapeException(lpeInvalidJump);
    Code := PByte(PtrUInt(CodeBase) + Target);
  end;

  procedure JumpToRelative(const Offset: TCodeOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Inc(Code, Offset);
  end;

  procedure HandleException;
  begin
    if (TryStackPos > 0) then
    begin
      Dec(TryStackPos);
      Code := TryStack[TryStackPos].Jmp;
    end else
      raise InJump.JumpException.Obj;
  end;

  procedure HandleSafeJump; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    IsEndJump: Boolean;
  begin
    IsEndJump := (CodeBase = PByte(PtrUInt(InJump.JumpSafe) - EndJump));

    while (TryStackPos > 0) and (IsEndJump or (TryStack[TryStackPos - 1].Jmp < InJump.JumpSafe)) and (TryStack[TryStackPos - 1].JmpFinally = nil) do
      Dec(TryStackPos);

    if (TryStackPos > 0) and (IsEndJump or (TryStack[TryStackPos - 1].Jmp < InJump.JumpSafe)) and  (TryStack[TryStackPos - 1].JmpFinally <> nil) then
    begin
      Assert(IsEndJump or (TryStack[TryStackPos - 1].JmpFinally >= Code));
      Dec(TryStackPos);
      Code := TryStack[TryStackPos].JmpFinally;
    end
    else if (CodeBase = PByte(PtrUInt(InJump.JumpSafe) - EndJump)) then
      Code := @opNone
    else
    begin
      Code := InJump.JumpSafe;
      InJump.JumpSafe := nil;
    end;
  end;

  procedure PushToVar(const Size: TStackOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if NeedMoreVarStack(Size) then
      ExpandVarStack(Size);

    if (Size > 0) then
    begin
      Dec(StackPos, Size);

      Move(Stack[StackPos], VarStack[VarStackPos], Size);
    end;
  end;

  procedure DoCheckInternal; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    Check: PtrUInt;
  begin
    Check := PtrUInt(CodeBase) + PtrUInt(PCodePos(@Stack[StackPos - SizeOf(Pointer)])^);
    PEvalBool(@Stack[StackPos - SizeOf(Pointer)])^ := (Check >= PtrUInt(CodeBase)) and (Check < PtrUInt(CodeUpper)) and (opCodeP(Check)^ = ocIncTry);
    Dec(StackPos, SizeOf(Pointer) - SizeOf(EvalBool));
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionMessage;
  begin
    if PreviousException.Exists then
      PlpString(PPointer(@Stack[StackPos - SizeOf(Pointer)])^)^ := PreviousException.Msg
    else
      PlpString(PPointer(@Stack[StackPos - SizeOf(Pointer)])^)^ := '';

    Dec(StackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionLocation;
  begin
    if PreviousException.Exists then
      PPointer(@Stack[StackPos])^ := @PreviousException.Loc
    else
      PPointer(@Stack[StackPos])^ := nil;

    Inc(StackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetCallerLocation;
  begin
    {$IFDEF Lape_EmitPos}
    if (CallStackPos > 0) then
      PPointer(@Stack[StackPos])^ := PPointer(CallStack[CallStackPos - 1].CalledFrom + SizeOf(opCode))^
    else
      PPointer(@Stack[StackPos])^ := nil;
    {$ELSE}
    PPointer(@Stack[StackPos])^ := nil;
    {$ENDIF}

    Inc(StackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetCallerAddress;
  begin
    if (CallStackPos < 2) then
      PPointer(@Stack[StackPos])^ := nil
    else
      PPointer(@Stack[StackPos])^ := Pointer(CallStack[CallStackPos - 2].Address);

    Inc(StackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoDumpCallStack;
  var
    StackTraceInfo: TStackTraceInfo;
    Offset, i: Integer;
    Pos: PDocPos;
  begin
    Offset := PInt32(@Stack[StackPos - (SizeOf(Pointer) + SizeOf(Int32))])^;

    if (Offset >= 0) and (Offset <= CallStackPos) then
    begin
      SetLength(StackTraceInfo, CallStackPos - Offset);
      for i := 0 to High(StackTraceInfo) do
      begin
        StackTraceInfo[i].Address := CallStack[i].Address;
        StackTraceInfo[i].CalledFrom := CallStack[i].CalledFrom;
      end;

      {$IFDEF Lape_EmitPos}
      if (Offset > 0) then
        Pos := PPointer(CallStack[CallStackPos - Offset].CalledFrom + SizeOf(opCode))^
      else
        Pos := PPointer(Code + SizeOf(opCode))^;
      {$ELSE}
      Pos := nil;
      {$ENDIF}

      PlpString(PPointer(@Stack[StackPos - SizeOf(Pointer)])^)^ := GetStackTrace(Emitter, StackTraceInfo, Pos);
    end;

    Dec(StackPos, SizeOf(Int32) + SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetScriptMethodName;
  begin
    {$IFDEF Delphi}
    PShortString(@Stack[StackPos - SizeOf(Pointer)])^ := UTF8EncodeToShortString(Emitter.CodePointerName[PCodePos(@Stack[StackPos - SizeOf(Pointer)])^]);
    {$ELSE}
    PShortString(@Stack[StackPos - SizeOf(Pointer)])^ := Emitter.CodePointerName[PCodePos(@Stack[StackPos - SizeOf(Pointer)])^];
    {$ENDIF}

    Inc(StackPos, SizeOf(ShortString) - SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoReRaiseException;
  begin
    Inc(Code, ocSize);

    if (not PreviousException.Exists) then
      LapeException(lpeImpossible)
    else
      LapeException(PreviousException.Msg, PreviousException.Loc);
  end;

  procedure DoCatchException;
  begin
    if (InJump.JumpException <> nil) then
    begin
      PreviousException.Exists := True;

      if IsLapeException(InJump.JumpException.Obj) then
      begin
        PreviousException.Msg := lpException(InJump.JumpException.Obj).Error;
        PreviousException.Loc := lpException(InJump.JumpException.Obj).DocPos;
      end else
      begin
        PreviousException.Msg := InJump.JumpException.Obj.Message;
        if (InJump.JumpException.Pos <> nil) then
          PreviousException.Loc := InJump.JumpException.Pos^;
      end;

      InJump.JumpException.Obj.Free();

      FreeAndNil(InJump.JumpException);
    end;

    Inc(Code, ocSize);
  end;

  procedure DoDynArrayRangeCheck; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    Arr: Pointer;
    Len, Index: SizeInt;
  begin
    Dec(StackPos, SizeOf(Pointer) + SizeOf(SizeInt));
    Inc(Code, ocSize);

    Arr := PPointer(@Stack[StackPos])^;
    Index := PSizeInt(@Stack[StackPos + SizeOf(SizeInt)])^;

    if Assigned(Arr) then
      Len := PSizeInt(Arr)[-1] {$IFDEF FPC}+1{$ENDIF}
    else
      Len := 0;

    if (Index < 0) or (Index >= Len) then
      LapeExceptionFmt(lpeIndexOutOfRange, [Index, 0, Len - 1]);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    InitStackSize: TStackOffset;
  begin
    InitStackSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if (StackPos + InitStackSize > UInt32(Length(Stack))) then
      SetLength(Stack, StackPos + InitStackSize + (StackSize div 2));
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    InitStackSize: TStackOffset;
  begin
    InitStackSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if (StackPos + InitStackSize > UInt32(Length(Stack))) then
      SetLength(Stack, StackPos + InitStackSize + (StackSize div 2));
    FillChar(Stack[StackPos], InitStackSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if (StackPos + GrowSize > UInt32(Length(Stack))) then
      SetLength(Stack, StackPos + GrowSize + (StackSize div 2));
    Inc(StackPos, GrowSize);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    ExpandSize: TStackOffset;
  begin
    ExpandSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if NeedMoreVarStack(ExpandSize) then
      ExpandVarStack(PStackOffset(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    ExpandSize: TStackOffset;
  begin
    ExpandSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if NeedMoreVarStack(ExpandSize) then
      ExpandVarStack(ExpandSize);
    FillChar(VarStack[VarStackPos], ExpandSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if NeedMoreGrowVarStack(GrowSize) then
      GrowVarStack(GrowSize);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    if NeedMoreGrowVarStack(GrowSize) then
      GrowVarStack(GrowSize);
    FillChar(VarStack[VarStackLen - GrowSize], GrowSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoPopVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (VarStackPos = 0) and (VarStackIndex > 0) then
    begin
      Dec(VarStackIndex);
      with VarStackStack[VarStackIndex] do
      begin
        VarStack := Stack;
        VarStackPos := Pos;
      end;
    end;

    VarStackLen := VarStackPos;
    Inc(Code, ocSize);
  end;

  procedure DoPopStackToVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      Dec(StackPos, Size);
      Move(Stack[StackPos], VarStack[VarStackPos + VOffset], Size);
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoPopVarToStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      Move(VarStack[VarStackPos + VOffset], Stack[StackPos], Size);
      FillChar(VarStack[VarStackPos + VOffset], Size, 0);
      Inc(StackPos, Size);
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoJmpVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(StackPos, SizeOf(TCodePos));
    //JumpTo(PCodePos(@Stack[StackPos])^);
    InJump.JumpSafe := @Stack[StackPos];
    if (PCodePos(InJump.JumpSafe)^ = 0) then
      LapeException(lpeInvalidJump);

    InJump.JumpSafe := PByte(PtrUInt(CodeBase) + PCodePos(InJump.JumpSafe)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafe; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    InJump.JumpSafe := PByte(PtrUInt(CodeBase) + PCodePos(PtrUInt(Code) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafeR; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    InJump.JumpSafe := PByte(PtrInt(Code) + PCodeOffset(PtrUInt(Code) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (TryStackPos >= UInt32(Length(TryStack))) then
      SetLength(TryStack, TryStackPos + (TryStackSize div 2));

    with POC_IncTry(PtrUInt(Code) + ocSize)^ do
    begin
      TryStack[TryStackPos].Jmp := PByte(PtrInt(Code) + Jmp);
      if (JmpFinally = Try_NoFinally) then
        TryStack[TryStackPos].JmpFinally := nil
      else if (JmpFinally = Try_NoExcept) then
        TryStack[TryStackPos].JmpFinally := TryStack[TryStackPos].Jmp
      else
        TryStack[TryStackPos].JmpFinally := PByte(PtrInt(Code) + Int32(JmpFinally) + Jmp);
    end;

    Inc(TryStackPos);
    Inc(Code, ocSize + SizeOf(TOC_IncTry));
  end;

  procedure DoDecTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(TryStackPos);
    Inc(Code, ocSize);
  end;

  procedure DoEndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (InJump.JumpException <> nil) then
      HandleException()
    else if (InJump.JumpSafe <> nil) then
      HandleSafeJump()
    else
      Inc(Code, ocSize);

    PreviousException.Exists := False;
  end;

  procedure DoIncCall(const RecSize: Integer; const Jmp: TCodePos; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (CallStackPos >= UInt32(Length(CallStack))) then
      SetLength(CallStack, CallStackPos + (CallStackSize div 2));

    with CallStack[CallStackPos] do
    begin
      Address := Jmp;
      CalledFrom := Code;
      JumpBack := PByte(PtrInt(Code) + ocSize + RecSize);
      VarStackP := VarStackPos;
      PushToVar(ParamSize);
      StackP := StackPos + UInt32(StackPosOffset);

      Jump := InJump;
      InJump := InEmptyJump;
      JumpTo(Jmp);
    end;
    Inc(CallStackPos);
  end;

  procedure DoDecCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (CallStackPos <= 0) then
      Code := @opNone
    else
    begin
      DoPopVar();
      Dec(CallStackPos);
      with CallStack[CallStackPos] do
      begin
        Code := JumpBack;
        VarStackPos := VarStackP;
        StackPos := StackP;
        MergeJumps(InJump, Jump);
      end;
    end;
  end;

  procedure DoDecCall_EndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    tmp: TInJump;
  begin
    tmp := InJump;
    DoDecCall();
    if (tmp.JumpException = InJump.JumpException) and (InJump.JumpException <> nil) then
      HandleException()
    else if (tmp.JumpSafe = InJump.JumpSafe) and (InJump.JumpSafe <> nil) then
      HandleSafeJump();
  end;

  procedure DoInvokeImportedProc(const RecSize: Integer; const Ptr: Pointer; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedProc(Ptr)(@Stack[StackPos - ParamSize]);
    Dec(StackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  procedure DoInvokeImportedFunc(const RecSize: Integer; const Ptr, Res: Pointer; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedFunc(Ptr)(@Stack[StackPos - ParamSize], Res);
    Dec(StackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  {$I lpinterpreter_doinvoke.inc}
  {$I lpinterpreter_dojump.inc}
  {$I lpinterpreter_doeval.inc}

  procedure DaLoop; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GoBack: Boolean;
    i: Integer;
  label
    Start;
  begin
    Start:
    GoBack := False;

    try
      while (DoContinue = bTrue) do {$I lpinterpreter_opcodecase.inc}
    except
      if (InJump.JumpException = nil) or (ExceptObject <> InJump.JumpException.Obj) then
      begin
        if (InJump.JumpException = nil) then
          InJump.JumpException := TJumpException.Create();

        SetLength(InJump.JumpException.StackTraceInfo, CallStackPos);
        for i := 0 to CallStackPos - 1 do
        begin
          InJump.JumpException.StackTraceInfo[i].Address    := CallStack[i].Address;
          InJump.JumpException.StackTraceInfo[i].CalledFrom := CallStack[i].CalledFrom;
        end;

        {$IFDEF Lape_EmitPos}
        InJump.JumpException.Pos := PPointer(PtrUInt(Code) + SizeOf(opCode))^;
        {$ENDIF}
      end;
      InJump.JumpException.Obj := Exception(AcquireExceptionObject());

      HandleException();
      GoBack := True;
    end;

    if (DoContinue = bUnknown) then
    begin
      Sleep(1);
      goto Start;
    end
    else if (DoContinue = bFalse) then
    begin
      DoContinue := bTrue;
      InJump.JumpSafe := PByte(PtrUInt(CodeBase) + EndJump);
      HandleSafeJump();
      goto Start;
    end;

    if GoBack then
      goto Start;
  end;

var
  StackTrace: lpString;
begin
  Code := PByte(PtrUInt(Emitter.Code) + InitialJump);
  CodeBase := Emitter.Code;
  CodeUpper := Emitter.Code + Emitter.CodeLen;
  SetLength(Stack, StackSize);
  SetLength(TryStack, TryStackSize);
  SetLength(CallStack, CallStackSize);

  VarStackIndex := 0;
  SetLength(VarStackStack, VarStackStackSize);

  VarStackStack[0].Stack := InitialVarStack;
  if (Length(VarStackStack[0].Stack) < VarStackSize) then
    SetLength(VarStackStack[0].Stack, VarStackSize);
  VarStack := VarStackStack[0].Stack;
  VarStackLen := Length(VarStack);

  PreviousException.Exists := False;
  PreviousException.Msg := '';
  PreviousException.Loc := NullDocPos;

  StackPos := 0;
  VarStackPos := 0;
  TryStackPos := 0;
  CallStackPos := 0;
  InJump := InEmptyJump;

  try
    DaLoop();
  except
    on E: Exception do
    begin
      StackTrace := GetStackTrace(Emitter, InJump.JumpException.StackTraceInfo, InJump.JumpException.Pos);

      if IsLapeException(E) then
        LapeExceptionRuntime(E as lpException, StackTrace)
      else
      if (InJump.JumpException <> nil) and (E = InJump.JumpException.Obj) and (InJump.JumpException.Pos <> nil) then
        LapeExceptionRuntime(E, StackTrace, InJump.JumpException.Pos^)
      else
        LapeExceptionRuntime(E, StackTrace, NullDocPos);
    end;
  end;
end;

procedure RunCode(const Emitter: TLapeCodeEmitter; const InitialVarStack: TByteArray; const InitialJump: TCodePos);
var
  DoContinue: TInitBool;
begin
  DoContinue := bTrue;
  RunCode(Emitter, DoContinue, InitialVarStack, InitialJump);
end;

end.

