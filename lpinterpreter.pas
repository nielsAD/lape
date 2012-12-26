{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
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
  TryStackSize = 256;
  CallStackSize = 512;

procedure RunCode(Code: PByte; var DoContinue: TInitBool; InitialVarStack: TByteArray = nil; InitialJump: TCodePos = 0); overload;
procedure RunCode(Code: PByte; InitialVarStack: TByteArray = nil; InitialJump: TCodePos = 0); overload;

implementation

uses
  lpexceptions;

{$OverFlowChecks Off}

type
  TInJump = record
    JumpException: record
      Obj: Exception;
      Pos: TDocPos;
    end;
    JumpSafe: PByte;
  end;

const
  InEmptyJump: TInJump = (JumpException: (Obj: nil; Pos: (Line: 0; Col: 0; FileName: '')); JumpSafe: nil);

procedure MergeJumps(var AJump: TInJump; const Merge: TInJump);
begin
  if (Merge.JumpException.Obj <> nil) then
  begin
    if (AJump.JumpException.Obj <> nil) and (AJump.JumpException.Obj <> Merge.JumpException.Obj) then
      //FreeAndNil(AJump.JumpException.Obj);
      Merge.JumpException.Obj.Free()
    else
      AJump.JumpException := Merge.JumpException;
  end;

  if (Merge.JumpSafe > AJump.JumpSafe) then
    AJump.JumpSafe := Merge.JumpSafe;
end;

procedure RunCode(Code: PByte; var DoContinue: TInitBool; InitialVarStack: TByteArray = nil; InitialJump: TCodePos = 0);
const
  opNone: opCodeType = opCodeType(ocNone);
var
  CodeBase: PByte;
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
    CalledFrom: PByte;
    StackP, VarStackP: UInt32;
    Jump: TInJump;
  end;
  CallStackPos: UInt32;

  procedure ExpandVarStack(Size: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (VarStackLen + Size > UInt32(Length(VarStack))) then
    begin
      VarStackStack[VarStackIndex].Pos := VarStackLen;

      Inc(VarStackIndex);
      if (VarStackIndex >= Length(VarStackStack)) then
        SetLength(VarStackStack, VarStackIndex + (VarStackStackSize div 2));

      VarStackPos := 0;
      VarStackLen := Size;
      VarStack := VarStackStack[VarStackIndex].Stack;

      if (Size > VarStackSize) then
        SetLength(VarStack, Size)
      else if (VarStack = nil) then
        SetLength(VarStack, VarStackSize);

      VarStackStack[VarStackIndex].Stack := VarStack;
    end
    else
    begin
      VarStackPos := VarStackLen;
      Inc(VarStackLen, Size);
    end;
  end;

  procedure GrowVarStack(Size: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    OldLen: UInt32;
  begin
    if (VarStackLen + Size > UInt32(Length(VarStack))) then
      if (VarStackPos = 0) then
      begin
        Inc(VarStackLen, Size);
        SetLength(VarStack, VarStackLen);
        VarStackStack[VarStackIndex].Stack := VarStack;
      end
      else
      begin
        OldLen := VarStackLen - VarStackPos;
        ExpandVarStack(Size + OldLen);
        with VarStackStack[VarStackIndex - 1] do
        begin
          Dec(Pos, OldLen);
          Move(Stack[Pos], VarStack[0], OldLen);
        end;
      end
    else
      Inc(VarStackLen, Size);
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

  procedure HandleException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (TryStackPos > 0) then
    begin
      Dec(TryStackPos);
      Code := TryStack[TryStackPos].Jmp;
    end
    else
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
    Dec(StackPos, Size);
    ExpandVarStack(Size);
    Move(Stack[StackPos], VarStack[VarStackPos], Size);
  end;

  procedure DoCheckInternal; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    try
      PEvalBool(@Stack[StackPos - SizeOf(Pointer)])^ := opCodeTypeP(PtrUInt(CodeBase) + PtrUInt(PCodePos(@Stack[StackPos - SizeOf(Pointer)])^))^ = opCodeType(ocIncTry);
    except
      PEvalBool(@Stack[StackPos - SizeOf(Pointer)])^ := False;
    end;
    Dec(StackPos, SizeOf(Pointer) - SizeOf(EvalBool));
    Inc(Code, ocSize);
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

  procedure DoInitVarLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    SetLength(VarStack, PStackOffset(PtrUInt(Code) + ocSize)^);
    VarStackStack[VarStackIndex].Stack := VarStack;
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
  begin
    ExpandVarStack(PStackOffset(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    ExpandSize: TStackOffset;
  begin
    ExpandSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    ExpandVarStack(ExpandSize);
    FillChar(VarStack[VarStackPos], ExpandSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    GrowVarStack(PStackOffset(PtrUInt(Code) + ocSize)^);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
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
    if (TryStackPos >= Length(TryStack)) then
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
    if (InJump.JumpException.Obj <> nil) then
      HandleException()
    else if (InJump.JumpSafe <> nil) then
      HandleSafeJump()
    else
      Inc(Code, ocSize);
  end;

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FreeAndNil(InJump.JumpException.Obj);
    Inc(Code, ocSize);
  end;

  procedure DoIncCall(RecSize: Integer; Jmp: TCodePos; ParamSize: TParamSize; StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (CallStackPos >= Length(CallStack)) then
      SetLength(CallStack, CallStackPos + (CallStackSize div 2));

    with CallStack[CallStackPos] do
    begin
      CalledFrom := PByte(PtrInt(Code) + ocSize + RecSize);
      VarStackP := VarStackPos;
      PushToVar(ParamSize);
      StackP := StackPos + StackPosOffset;

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
        Code := CalledFrom;
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
    if (tmp.JumpException.Obj = InJump.JumpException.Obj) and (InJump.JumpException.Obj <> nil) then
      HandleException()
    else if (tmp.JumpSafe = InJump.JumpSafe) and (InJump.JumpSafe <> nil) then
      HandleSafeJump();
  end;

  procedure DoInvokeImportedProc(RecSize: Integer; Ptr: Pointer; ParamSize: UInt16; StackPosOffset: Integer = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedProc(Ptr)(@Stack[StackPos - ParamSize]);
    Dec(StackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  procedure DoInvokeImportedFunc(RecSize: Integer; Ptr, Res: Pointer; ParamSize: UInt16; StackPosOffset: Integer = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
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
  label
    Start;
  begin
    Start:
    GoBack := False;

    try
      while (DoContinue = bTrue) do {$I lpinterpreter_opcodecase.inc}
    except
      {$IFDEF Lape_EmitPos}
      if (ExceptObject <> InJump.JumpException.Obj) then
        InJump.JumpException.Pos := PDocPos(PtrUInt(Code) + SizeOf(opCodeType))^;
      {$ENDIF}

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

begin
  CodeBase := Code;
  SetLength(Stack, StackSize);
  SetLength(TryStack, TryStackSize);
  SetLength(CallStack, CallStackSize);

  VarStackIndex := 0;
  SetLength(VarStackStack, VarStackStackSize);

  VarStackLen := Length(InitialVarStack);
  if (VarStackLen < VarStackSize) then
    SetLength(InitialVarStack, VarStackSize);
  VarStackStack[0].Stack := InitialVarStack;
  VarStack := VarStackStack[0].Stack;

  StackPos := 0;
  VarStackPos := 0;
  TryStackPos := 0;
  CallStackpos := 0;
  InJump := InEmptyJump;

  try
    Code := PByte(PtrUInt(CodeBase) + InitialJump);
    DaLoop();
  except
    on E: Exception do
      if (E = InJump.JumpException.Obj) then
        LapeExceptionFmt(lpeRuntime, [E.Message], InJump.JumpException.Pos)
      else
        LapeExceptionFmt(lpeRuntime, [E.Message]);
  end;
end;

procedure RunCode(Code: PByte; InitialVarStack: TByteArray = nil; InitialJump: TCodePos = 0);
var
  DoContinue: TInitBool;
begin
  DoContinue := bTrue;
  RunCode(Code, DoContinue, InitialVarStack, InitialJump);
end;

end.

