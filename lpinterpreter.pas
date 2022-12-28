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
  SysUtils,
  lptypes;

type
  opCode = (
    ocNone,
    ocIsInternal,                                              //IsInternal
    ocGetExceptionMessage,                                     //GetExceptionMessage
    ocGetExceptionLocation,
    ocGetCallerLocation,
    ocInitStackLen,                                            //InitStackLen TStackOffset
    ocInitVarLen,                                              //InitVarLen TStackOffset
    ocInitStack,                                               //InitStack TStackOffset
    ocGrowStack,                                               //GrowStack TStackOffset
    ocExpandVar,                                               //ExpandVar TStackOffset
    ocExpandVarAndInit,                                        //ExpandVarAndInit TStackOffset
    ocGrowVar,                                                 //GrowVar TStackOffset
    ocGrowVarAndInit,                                          //GrowVarAndInit TStackOffset
    ocPopStackToVar,                                           //PopStackToVar TStackOffset TVarStackOffset
    ocPopVarToStack,                                           //PopVarToStack TStackOffset TVarStackOffset
    ocPopVar,                                                  //PopVar
    ocJmpVar,                                                  //JmpVar
    ocJmpSafe,                                                 //JmpSafe TCodePos
    ocJmpSafeR,                                                //JmpSafeR TCodeOffset

    ocIncTry,                                                  //IncTry TCodeOffset UInt32
    ocDecTry,                                                  //DecTry
    ocEndTry,                                                  //EndTry
    ocCatchException,                                          //CatchException
    ocReRaiseException,                                        //ReRaiseException

    ocDecCall,                                                 //DecCall
    ocDecCall_EndTry,                                          //DecCall_EndTry

    {$I lpinterpreter_invokeopcodes.inc}
    {$I lpinterpreter_jumpopcodes.inc}
    {$I lpinterpreter_evalopcodes.inc}
  );
  opCodeTypeP = ^opCodeType;
  {$IFDEF Lape_SmallCode}
  opCodeType = Byte;
  {$ELSE}
  opCodeType = Integer; //Better alignment
  {$ENDIF}

  POC_PopStackToVar = ^TOC_PopStackToVar;
  TOC_PopStackToVar = record
    Size: TStackOffset;
    VOffset: TVarStackOffset;
  end;

  POC_IncTry = ^TOC_IncTry;
  TOC_IncTry = record
    Jmp: TCodeOffset;
    JmpFinally: UInt32;
  end;

  {$I lpinterpreter_invokerecords.inc}
  {$I lpinterpreter_jumprecords.inc}
  {$I lpinterpreter_evalrecords.inc}

const
  ocSize = SizeOf(opCodeType) {$IFDEF Lape_EmitPos}+SizeOf(TDocPos){$ENDIF};

  Try_NoFinally: UInt32 = UInt32(-1);
  Try_NoExcept: UInt32 = UInt32(-2);
  EndJump: TCodePos = TCodePos(-1);

  StackSize = 2048 * SizeOf(Pointer);
  VarStackSize = 512 * SizeOf(Pointer);
  VarStackStackSize = 32;
  TryStackSize = 512;
  CallStackSize = 512;

type
  TInJump = record
    JumpException: record
      Obj: Exception;
      Pos: PDocPos;
    end;
    JumpSafe: PByte;
  end;

type
  TLapeCodeRunner = class(TLapeBaseClass)
  protected
    FCode: PByte;
    FCodeBase: PByte;
    FCodeUpper: PByte;
    FStack: TByteArray;
    FStackPos: UInt32;

    FVarStack: TByteArray;
    FVarStackIndex, FVarStackPos, FVarStackLen: UInt32;
    FVarStackStack: array of record
      Stack: TByteArray;
      Pos: UInt32;
    end;

    FTryStack: array of record
      Jmp: PByte;
      JmpFinally: PByte;
      ExceptionMessage: String;
      ExceptionLocation: TDocPos;
    end;
    FTryStackPos: UInt32;
    FInJump: TInJump;

    FCallStack: array of record
      CalledFrom: PByte;
      JumpBack: PByte;
      StackP, VarStackP: UInt32;
      Jump: TInJump;
    end;
    FCallStackPos: UInt32;
  public
    DoContinue: TInitBool;

    constructor Create(CodeBase: PByte; CodeLen: Integer); reintroduce; overload;
    constructor Create(Emitter: TLapeBaseClass); reintroduce; overload;

    procedure Run(InitialVarStack: TByteArray = nil; InitialJump: TCodePos = 0);
  end;

implementation

uses
  lpmessages, lpvartypes;

{$OverFlowChecks Off}

const
  opNone: opCodeType = opCodeType(ocNone);

  InEmptyJump: TInJump = (JumpException: (Obj: nil; Pos: nil); JumpSafe: nil);

procedure MergeJumps(var AJump: TInJump; const Merge: TInJump);
begin
  if (Merge.JumpException.Obj <> nil) then
  begin
    if (AJump.JumpException.Obj <> nil) and (AJump.JumpException.Obj <> Merge.JumpException.Obj) then
      Merge.JumpException.Obj.Free()
    else
      AJump.JumpException := Merge.JumpException;
  end;

  if (Merge.JumpSafe > AJump.JumpSafe) then
    AJump.JumpSafe := Merge.JumpSafe;
end;

constructor TLapeCodeRunner.Create(CodeBase: PByte; CodeLen: Integer);
begin
  inherited Create();

  SetLength(FStack, StackSize);
  SetLength(FTryStack, TryStackSize);
  SetLength(FCallStack, CallStackSize);
  SetLength(FVarStackStack, VarStackStackSize);

  FCodeBase := CodeBase;
  FCodeUpper := PByte(CodeBase + CodeLen);
end;

constructor TLapeCodeRunner.Create(Emitter: TLapeBaseClass);
begin
  if (not (Emitter is TLapeCodeEmitter)) then
    LapeException(lpeImpossible);

  Create(
    TLapeCodeEmitter(Emitter).Code,
    TLapeCodeEmitter(Emitter).CodeLen
  );
end;

procedure TLapeCodeRunner.Run(InitialVarStack: TByteArray; InitialJump: TCodePos);

  procedure ExpandVarStack(Size: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FVarStackLen + Size > UInt32(Length(FVarStack))) then
    begin
      FVarStackStack[FVarStackIndex].Pos := FVarStackLen;

      Inc(FVarStackIndex);
      if (FVarStackIndex >= UInt32(Length(FVarStackStack))) then
        SetLength(FVarStackStack, FVarStackIndex + (VarStackStackSize div 2));

      FVarStackPos := 0;
      FVarStackLen := Size;
      FVarStack := FVarStackStack[FVarStackIndex].Stack;

      if (Size > VarStackSize) then
        SetLength(FVarStack, Size)
      else if (FVarStack = nil) then
        SetLength(FVarStack, VarStackSize);

      FVarStackStack[FVarStackIndex].Stack := FVarStack;
    end
    else
    begin
      FVarStackPos := FVarStackLen;
      Inc(FVarStackLen, Size);
    end;
  end;

  procedure GrowVarStack(Size: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    OldLen: UInt32;
  begin
    if (FVarStackLen + Size > UInt32(Length(FVarStack))) then
      if (FVarStackPos = 0) then
      begin
        Inc(FVarStackLen, Size);
        SetLength(FVarStack, FVarStackLen);
        FVarStackStack[FVarStackIndex].Stack := FVarStack;
      end
      else
      begin
        OldLen := FVarStackLen - FVarStackPos;
        ExpandVarStack(Size + OldLen);
        with FVarStackStack[FVarStackIndex - 1] do
        begin
          Dec(Pos, OldLen);
          Move(FStack[Pos], FVarStack[0], OldLen);
        end;
      end
    else
      Inc(FVarStackLen, Size);
  end;

  procedure JumpTo(const Target: TCodePos); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (Target = 0) then
      LapeException(lpeInvalidJump);
    FCode := PByte(PtrUInt(FCodeBase) + Target);
  end;

  procedure JumpToRelative(const Offset: TCodeOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Inc(FCode, Offset);
  end;

  procedure HandleException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FTryStackPos > 0) then
    begin
      FTryStack[FTryStackPos].ExceptionMessage := '';

      Dec(FTryStackPos);
      FCode := FTryStack[FTryStackPos].Jmp;

      if (FInJump.JumpException.Obj <> nil) then
      begin
        if (FInJump.JumpException.Obj is lpException) then
          FTryStack[FTryStackPos].ExceptionMessage := lpException(FInJump.JumpException.Obj).Error
        else
          FTryStack[FTryStackPos].ExceptionMessage := FInJump.JumpException.Obj.Message;

        if (FInJump.JumpException.Obj is lpException) and lpException(FInJump.JumpException.Obj).hasDocPos then
          FTryStack[FTryStackPos].ExceptionLocation := lpException(FInJump.JumpException.Obj).DocPos
        else
        if (FInJump.JumpException.Pos <> nil) then
          FTryStack[FTryStackPos].ExceptionLocation := FInJump.JumpException.Pos^;
      end;
    end
    else
      raise FInJump.JumpException.Obj;
  end;

  procedure HandleSafeJump; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    IsEndJump: Boolean;
  begin
    IsEndJump := (FCodeBase = PByte(PtrUInt(FInJump.JumpSafe) - EndJump));

    while (FTryStackPos > 0) and (IsEndJump or (FTryStack[FTryStackPos - 1].Jmp < FInJump.JumpSafe)) and (FTryStack[FTryStackPos - 1].JmpFinally = nil) do
      Dec(FTryStackPos);

    if (FTryStackPos > 0) and (IsEndJump or (FTryStack[FTryStackPos - 1].Jmp < FInJump.JumpSafe)) and  (FTryStack[FTryStackPos - 1].JmpFinally <> nil) then
    begin
      Assert(IsEndJump or (FTryStack[FTryStackPos - 1].JmpFinally >= FCode));
      Dec(FTryStackPos);
      FCode := FTryStack[FTryStackPos].JmpFinally;
    end
    else if (FCodeBase = PByte(PtrUInt(FInJump.JumpSafe) - EndJump)) then
      FCode := @opNone
    else
    begin
      FCode := FInJump.JumpSafe;
      FInJump.JumpSafe := nil;
    end;
  end;

  procedure PushToVar(const Size: TStackOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(FStackPos, Size);
    ExpandVarStack(Size);
    Move(FStack[FStackPos], FVarStack[FVarStackPos], Size);
  end;

  procedure DoCheckInternal; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    Check: PtrUInt;
  begin
    Check := PtrUInt(FCodeBase) + PtrUInt(PCodePos(@FStack[FStackPos - SizeOf(Pointer)])^);
    PEvalBool(@FStack[FStackPos - SizeOf(Pointer)])^ := (Check >= PtrUInt(FCodeBase)) and (Check < PtrUInt(FCodeUpper)) and (opCodeTypeP(Check)^ = opCodeType(ocIncTry));
    Dec(FStackPos, SizeOf(Pointer) - SizeOf(EvalBool));
    Inc(FCode, ocSize);
  end;

  procedure DoGetExceptionMessage;
  begin
    PShortString(@FStack[FStackPos])^ := FTryStack[FTryStackPos].ExceptionMessage;

    Inc(FStackPos, SizeOf(ShortString));
    Inc(FCode, ocSize);
  end;

  procedure DoGetExceptionLocation;
  begin
    PPointer(@FStack[FStackPos])^ := @FTryStack[FTryStackPos].ExceptionLocation;

    Inc(FStackPos, SizeOf(Pointer));
    Inc(FCode, ocSize);
  end;

  procedure DoGetCallerLocation;
  begin
    PPointer(@FStack[FStackPos])^ := FCallStack[FCallStackPos - 1].CalledFrom + SizeOf(opCodeType);

    Inc(FStackPos, SizeOf(Pointer));
    Inc(FCode, ocSize);
  end;

  procedure DoReRaiseException;
  var
    Pos: Int32;
  begin
    Inc(FCode, ocSize);

    Pos := FTryStackPos;
    while (Pos > 0) and
          (FTryStack[Pos].ExceptionLocation.Line = NullDocPos.Line) and
          (FTryStack[Pos].ExceptionLocation.Col = NullDocPos.Line) and
          (FTryStack[Pos].ExceptionMessage = '') do
      Dec(Pos);

    with FTryStack[Pos] do
      LapeException(ExceptionMessage, ExceptionLocation);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    InitStackSize: TStackOffset;
  begin
    InitStackSize := PStackOffset(PtrUInt(FCode) + ocSize)^;
    if (FStackPos + InitStackSize > UInt32(Length(FStack))) then
      SetLength(FStack, FStackPos + InitStackSize + (StackSize div 2));
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitVarLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    SetLength(FVarStack, PStackOffset(PtrUInt(FCode) + ocSize)^);
    FVarStackStack[FVarStackIndex].Stack := FVarStack;
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    InitStackSize: TStackOffset;
  begin
    InitStackSize := PStackOffset(PtrUInt(FCode) + ocSize)^;
    if (FStackPos + InitStackSize > UInt32(Length(FStack))) then
      SetLength(FStack, FStackPos + InitStackSize + (StackSize div 2));
    FillChar(FStack[FStackPos], InitStackSize, 0);
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(FCode) + ocSize)^;
    if (FStackPos + GrowSize > UInt32(Length(FStack))) then
      SetLength(FStack, FStackPos + GrowSize + (StackSize div 2));
    Inc(FStackPos, GrowSize);
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    ExpandVarStack(PStackOffset(PtrUInt(FCode) + ocSize)^);
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    ExpandSize: TStackOffset;
  begin
    ExpandSize := PStackOffset(PtrUInt(FCode) + ocSize)^;
    ExpandVarStack(ExpandSize);
    FillChar(FVarStack[FVarStackPos], ExpandSize, 0);
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    GrowVarStack(PStackOffset(PtrUInt(FCode) + ocSize)^);
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(FCode) + ocSize)^;
    GrowVarStack(GrowSize);
    FillChar(FVarStack[FVarStackLen - GrowSize], GrowSize, 0);
    Inc(FCode, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoPopVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FVarStackPos = 0) and (FVarStackIndex > 0) then
    begin
      Dec(FVarStackIndex);
      with FVarStackStack[FVarStackIndex] do
      begin
        FVarStack := Stack;
        FVarStackPos := Pos;
      end;
    end;

    FVarStackLen := FVarStackPos;
    Inc(FCode, ocSize);
  end;

  procedure DoPopStackToVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(FCode) + ocSize)^ do
    begin
      Dec(FStackPos, Size);
      Move(FStack[FStackPos], FVarStack[FVarStackPos + VOffset], Size);
    end;
    Inc(FCode, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoPopVarToStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(FCode) + ocSize)^ do
    begin
      Move(FVarStack[FVarStackPos + VOffset], FStack[FStackPos], Size);
      FillChar(FVarStack[FVarStackPos + VOffset], Size, 0);
      Inc(FStackPos, Size);
    end;
    Inc(FCode, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoJmpVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(FStackPos, SizeOf(TCodePos));
    //JumpTo(PCodePos(@FStack[FStackPos])^);
    FInJump.JumpSafe := @FStack[FStackPos];
    if (PCodePos(FInJump.JumpSafe)^ = 0) then
      LapeException(lpeInvalidJump);

    FInJump.JumpSafe := PByte(PtrUInt(FCodeBase) + PCodePos(FInJump.JumpSafe)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafe; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FInJump.JumpSafe := PByte(PtrUInt(FCodeBase) + PCodePos(PtrUInt(FCode) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafeR; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FInJump.JumpSafe := PByte(PtrUInt(FCode) + PCodeOffset(PtrUInt(FCode) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FTryStackPos >= UInt32(Length(FTryStack))) then
      SetLength(FTryStack, FTryStackPos + (TryStackSize div 2));

    with POC_IncTry(PtrUInt(FCode) + ocSize)^ do
    begin
      FTryStack[FTryStackPos].Jmp := PByte(PtrUInt(FCode) + Jmp);
      if (JmpFinally = Try_NoFinally) then
        FTryStack[FTryStackPos].JmpFinally := nil
      else if (JmpFinally = Try_NoExcept) then
        FTryStack[FTryStackPos].JmpFinally := FTryStack[FTryStackPos].Jmp
      else
        FTryStack[FTryStackPos].JmpFinally := PByte(PtrUInt(FCode) + Int32(JmpFinally) + Jmp);
    end;

    Inc(FTryStackPos);
    Inc(FCode, ocSize + SizeOf(TOC_IncTry));
  end;

  procedure DoDecTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(FTryStackPos);
    Inc(FCode, ocSize);
  end;

  procedure DoEndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FTryStack[FTryStackPos].ExceptionMessage := '';
    FTryStack[FTryStackPos].ExceptionLocation := NullDocPos;

    if (FInJump.JumpException.Obj <> nil) then
      HandleException()
    else if (FInJump.JumpSafe <> nil) then
      HandleSafeJump()
    else
      Inc(FCode, ocSize);
  end;

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FreeAndNil(FInJump.JumpException.Obj);
    Inc(FCode, ocSize);
  end;

  procedure DoIncCall(RecSize: Integer; Jmp: TCodePos; ParamSize: TParamSize; StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FCallStackPos >= UInt32(Length(FCallStack))) then
      SetLength(FCallStack, FCallStackPos + (CallStackSize div 2));

    with FCallStack[FCallStackPos] do
    begin
      CalledFrom := FCode;
      JumpBack := PByte(PtrUInt(FCode) + ocSize + RecSize);
      VarStackP := FVarStackPos;
      PushToVar(ParamSize);
      StackP := FStackPos + UInt32(StackPosOffset);

      Jump := FInJump;
      FInJump := InEmptyJump;
      JumpTo(Jmp);
    end;
    Inc(FCallStackPos);
  end;

  procedure DoDecCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FCallStackPos <= 0) then
      FCode := @opNone
    else
    begin
      DoPopVar();
      Dec(FCallStackPos);
      with FCallStack[FCallStackPos] do
      begin
        FCode := JumpBack;
        FVarStackPos := VarStackP;
        FStackPos := StackP;
        MergeJumps(FInJump, Jump);
      end;
    end;
  end;

  procedure DoDecCall_EndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    tmp: TInJump;
  begin
    tmp := FInJump;
    DoDecCall();
    if (tmp.JumpException.Obj = FInJump.JumpException.Obj) and (FInJump.JumpException.Obj <> nil) then
      HandleException()
    else if (tmp.JumpSafe = FInJump.JumpSafe) and (FInJump.JumpSafe <> nil) then
      HandleSafeJump();
  end;

  procedure DoInvokeImportedProc(RecSize: Integer; Ptr: Pointer; ParamSize: TParamSize; StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedProc(Ptr)(@FStack[FStackPos - ParamSize]);
    Dec(FStackPos, ParamSize - StackPosOffset);
    Inc(FCode, RecSize + ocSize);
  end;

  procedure DoInvokeImportedFunc(RecSize: Integer; Ptr, Res: Pointer; ParamSize: TParamSize; StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedFunc(Ptr)(@FStack[FStackPos - ParamSize], Res);
    Dec(FStackPos, ParamSize - StackPosOffset);
    Inc(FCode, RecSize + ocSize);
  end;

  {$I lpinterpreter_doinvoke.inc}
  {$I lpinterpreter_dojump.inc}
  {$I lpinterpreter_doeval.inc}

  procedure DaLoop;
  var
    GoBack: Boolean;
  label
    Start;
  begin
    Start:
    GoBack := False;

    try
      while (DoContinue = bTrue) do {$I lpinterpreter_opcodecase.inc}
    except
      {$IFDEF Lape_EmitPos}
      if (ExceptObject <> FInJump.JumpException.Obj) then
        FInJump.JumpException.Pos := PDocPos(PtrUInt(FCode) + SizeOf(opCodeType));
      {$ENDIF}

      FInJump.JumpException.Obj := Exception(AcquireExceptionObject());
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
      FInJump.JumpSafe := PByte(PtrUInt(FCodeBase) + EndJump);
      HandleSafeJump();
      goto Start;
    end;

    if GoBack then
      goto Start;
  end;

begin
  DoContinue := bTrue;

  FInJump := InEmptyJump;

  FStackPos := 0;
  FVarStackPos := 0;
  FTryStackPos := 0;
  FCallStackPos := 0;

  FVarStackIndex := 0;
  FVarStackLen := Length(InitialVarStack);
  if (FVarStackLen > 0) then
  begin
    if (FVarStackLen < VarStackSize) then
      SetLength(InitialVarStack, VarStackSize);
    FVarStackStack[0].Stack := InitialVarStack;
  end;
  FVarStack := FVarStackStack[0].Stack;

  try
    FCode := PByte(PtrUInt(FCodeBase) + InitialJump);

    DaLoop();
  except
    on E: Exception do
    begin
      if (E is lpException) and lpException(E).hasDocPos() then
        LapeExceptionFmt(lpeRuntime, [lpException(E).Error], lpException(E).DocPos)
      else
      if (E = FInJump.JumpException.Obj) and (FInJump.JumpException.Pos <> nil) then
        LapeExceptionFmt(lpeRuntime, [E.Message], FInJump.JumpException.Pos^)
      else
        LapeExceptionFmt(lpeRuntime, [E.Message]);
    end;
  end;
end;

end.

