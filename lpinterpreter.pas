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
  lptypes, lpvartypes, lpinterpreter_types;

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

  TVarStackStack = array of record
    Stack: TByteArray;
    Pos: UInt32;
  end;

  TTryStack = array of record
    Jmp: PByte;
    JmpFinally: PByte;
  end;

  TInJump = record
    JumpException: TJumpException;
    JumpSafe: PByte;
  end;

  TCallStack = array of record
    Address: TCodePos;
    CalledFrom: PByte;
    JumpBack: PByte;
    StackP, VarStackP: UInt32;
    Jump: TInJump;
  end;

  TLapeCodeRunner = class(TLapeBaseClass)
  protected
    FEmitter: TLapeCodeEmitter;
    FRunning: TInitBool;
    FStack: TByteArray;
    FStackPos: UInt32;

    FVarStack: TByteArray;
    FVarStackIndex, FVarStackPos, FVarStackLen: UInt32;
    FVarStackStack: TVarStackStack;

    FTryStack: TTryStack;
    FTryStackPos: UInt32;
    FInJump: TInJump;

    FCallStack: TCallStack;
    FCallStackPos: UInt32;

    FDefStackSize: SizeUInt;
    FDefVarStackSize: SizeUInt;
    FDefVarStackStackSize: SizeUInt;
    FDefTryStackSize: SizeUInt;
    FDefCallStackSize: SizeUInt;

    FStacksAllocated: Boolean;

    function getRunning: Boolean;
    function getPaused: Boolean;
    function getStopped: Boolean;
  public
    constructor Create(Emitter: TLapeCodeEmitter); reintroduce;
    destructor Destroy; override;

    procedure Run(const InitialJump: TCodePos = 0; const InitialVarStack: TByteArray = nil);

    procedure Resume;
    procedure Pause;
    procedure Stop;

    property isRunning: Boolean read getRunning;
    property isStopped: Boolean read getStopped;
    property isPaused: Boolean read getPaused;

    property DefStackSize: SizeUInt read FDefStackSize write FDefStackSize;
    property DefVarStackSize: SizeUInt read FDefVarStackSize write FDefVarStackSize;
    property DefVarStackStackSize: SizeUInt read FDefVarStackStackSize write FDefVarStackStackSize;
    property DefTryStackSize: SizeUInt read FDefTryStackSize write FDefTryStackSize;
    property DefCallStackSize: SizeUInt read FDefCallStackSize write FDefCallStackSize;
  end;

procedure RunCode(const Emitter: TLapeCodeEmitter; const InitialVarStack: TByteArray = nil; const InitialJump: TCodePos = 0);

implementation

uses
  lpmessages;

{$IFDEF Lape_InterpreterDebug_ScriptMethodInvokes} {$DEFINE DEBUG_INVOKE}      {$ENDIF}
{$IFDEF Lape_InterpreterDebug_StackResize}         {$DEFINE DEBUG_STACKRESIZE} {$ENDIF}
{$IFDEF Lape_InterpreterDebug_StackUsage}          {$DEFINE DEBUG_STACKUSAGE}  {$ENDIF}

{$OverflowChecks Off}

{$IFDEF FPC}
  {$IFDEF Lape_UseFPCTrunk_FillChar}
    {$i extensions/fpctrunkfillchar.inc}
  {$ENDIF}
{$ENDIF}

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

function IsLapeException(const E: Exception): Boolean;
begin
  Result := (E <> nil) and (E.ClassType = lpException) and (lpException(e).DocPos.Col <> NullDocPos.Col) and (lpException(e).DocPos.Line <> NullDocPos.Line);
end;

function TLapeCodeRunner.getRunning: Boolean;
begin
  Result := FRunning = bTrue;
end;

function TLapeCodeRunner.getPaused: Boolean;
begin
  Result := FRunning = bUnknown;
end;

function TLapeCodeRunner.getStopped: Boolean;
begin
  Result := FRunning = bFalse;
end;

constructor TLapeCodeRunner.Create(Emitter: TLapeCodeEmitter);
begin
  inherited Create();

  FEmitter := Emitter;

  FDefStackSize         := 1024*8;
  FDefVarStackSize      := 1024*16;
  FDefVarStackStackSize := 64;
  FDefTryStackSize      := 128;
  FDefCallStackSize     := 128;
end;

destructor TLapeCodeRunner.Destroy;
{$IFDEF DEBUG_STACKUSAGE}
var I: Integer;
{$ENDIF}
begin
  inherited Destroy();

  {$IFDEF DEBUG_STACKUSAGE}
  for I := 0 to High(FCallStack) do
    if FCallStack[I].Address = 0 then
    begin
      WriteLn('FCallStack MaxDepth=', I);
      Break;
    end;
  for I := 0 to High(FTryStack) do
    if FTryStack[I].Jmp = nil then
    begin
      WriteLn('FTryStack MaxDepth=', I);
      Break;
    end;

  WriteLn('FStack Length=', Length(FStack));

  for I := 0 to High(FVarStackStack) do
    if (Length(FVarStackStack[I].FStack) > 0) then
      WriteLn('FVarStackStack[',I,'] Length=', Length(FVarStackStack[I].FStack));
  {$ENDIF}
end;

procedure TLapeCodeRunner.Run(const InitialJump: TCodePos; const InitialVarStack: TByteArray = nil);
const
  opNone: opCode = ocNone;
var
  Code: PByte;
  CodeBase: PByte;
  CodeUpper: PByte;

  PreviousException: record
    Exists: Boolean;
    Msg: lpString;
    Loc: TDocPos;
  end;

  function NeedMoreVarStack(const Size: UInt32): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := FVarStackLen + Size > Length(FVarStack);
    if not Result then
    begin
      FVarStackPos := FVarStackLen;
      FVarStackLen := FVarStackLen + Size;
    end;
  end;

  function NeedMoreGrowVarStack(const Size: UInt32): Boolean; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Result := FVarStackLen + Size > Length(FVarStack);
    if not Result then
      FVarStackLen := FVarStackLen + Size;
  end;

  procedure ExpandVarStack(const Size: UInt32);
  begin
    Assert(FVarStackLen + Size > Length(FVarStack));
    FVarStackStack[FVarStackIndex].Pos := FVarStackLen;

    Inc(FVarStackIndex);
    if (FVarStackIndex >= UInt32(Length(FVarStackStack))) then
      SetLength(FVarStackStack, FVarStackIndex + FDefVarStackStackSize);

    FVarStackPos := 0;
    FVarStackLen := Size;
    FVarStack := FVarStackStack[FVarStackIndex].Stack;

    {$IFDEF DEBUG_STACKRESIZE}
    if (Size > Length(FVarStack)) then
      WriteLn('[Resize]: ExpandVarStack Index=', FVarStackIndex, ' Need=', Size, ' Current=', Length(FVarStack));
    {$ENDIF}

    if (Size > Length(FVarStack)) then
      if (Size > FDefVarStackSize) then
        SetLength(FVarStack, Size)
      else
      if (FVarStack = nil) then
        SetLength(FVarStack, FDefVarStackSize);

    FVarStackStack[FVarStackIndex].Stack := FVarStack;
  end;

  procedure GrowVarStack(const Size: UInt32);
  var
    OldLen: UInt32;
  begin
    Assert(FVarStackLen + Size > Length(FVarStack));

    if (FVarStackPos = 0) then
    begin
      Inc(FVarStackLen, Size);
      SetLength(FVarStack, FVarStackLen);
      FVarStackStack[FVarStackIndex].Stack := FVarStack;
    end
    else
    if (FVarStackLen = FVarStackPos) then
      ExpandVarStack(Size)
    else
    begin
      OldLen := FVarStackLen - FVarStackPos;
      ExpandVarStack(Size + OldLen);
      with FVarStackStack[FVarStackIndex - 1] do
      begin
        Dec(Pos, OldLen);
        Move(Stack[Pos], FVarStack[0], OldLen);
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
    if (FTryStackPos > 0) then
    begin
      Dec(FTryStackPos);
      Code := FTryStack[FTryStackPos].Jmp;
    end else
      raise FInJump.JumpException.Obj;
  end;

  procedure HandleSafeJump; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    IsEndJump: Boolean;
  begin
    IsEndJump := (CodeBase = PByte(PtrUInt(FInJump.JumpSafe) - EndJump));

    while (FTryStackPos > 0) and (IsEndJump or (FTryStack[FTryStackPos - 1].Jmp < FInJump.JumpSafe)) and (FTryStack[FTryStackPos - 1].JmpFinally = nil) do
      Dec(FTryStackPos);

    if (FTryStackPos > 0) and (IsEndJump or (FTryStack[FTryStackPos - 1].Jmp < FInJump.JumpSafe)) and  (FTryStack[FTryStackPos - 1].JmpFinally <> nil) then
    begin
      Assert(IsEndJump or (FTryStack[FTryStackPos - 1].JmpFinally >= Code));
      Dec(FTryStackPos);
      Code := FTryStack[FTryStackPos].JmpFinally;
    end
    else if (CodeBase = PByte(PtrUInt(FInJump.JumpSafe) - EndJump)) then
      Code := @opNone
    else
    begin
      Code := FInJump.JumpSafe;
      FInJump.JumpSafe := nil;
    end;
  end;

  procedure PushToVar(const Size: TStackOffset); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    // force inlining of:
    // if NeedMoreVarStack(Size) then
    //   ExpandVarStack(Size);
    if FVarStackLen + Size <= Length(FVarStack) then
    begin
      FVarStackPos := FVarStackLen;
      FVarStackLen := FVarStackLen + Size;
    end else
      ExpandVarStack(Size);

    if (Size > 0) then
    begin
      Dec(FStackPos, Size);
      Move(FStack[FStackPos], FVarStack[FVarStackPos], Size);
    end;
  end;

  procedure DoIsScriptMethod; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    Check: PtrUInt;
  begin
    Check := PtrUInt(CodeBase) + PtrUInt(PCodePos(@FStack[FStackPos - SizeOf(Pointer)])^);
    PEvalBool(@FStack[FStackPos - SizeOf(Pointer)])^ := (Check >= PtrUInt(CodeBase)) and (Check < PtrUInt(CodeUpper)) and (opCodeP(Check)^ = ocIncTry);
    Dec(FStackPos, SizeOf(Pointer) - SizeOf(EvalBool));
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionMessage;
  begin
    if PreviousException.Exists then
      PlpString(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ := PreviousException.Msg
    else
      PlpString(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ := '';

    Dec(FStackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionLocation;
  begin
    if PreviousException.Exists then
      PPointer(@FStack[FStackPos])^ := @PreviousException.Loc
    else
      PPointer(@FStack[FStackPos])^ := nil;

    Inc(FStackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetCallerLocation;
  begin
    {$IFDEF Lape_EmitPos}
    if (FCallStackPos > 0) then
      PPointer(@FStack[FStackPos])^ := PPointer(FCallStack[FCallStackPos - 1].CalledFrom + SizeOf(opCode))^
    else
      PPointer(@FStack[FStackPos])^ := nil;
    {$ELSE}
    PPointer(@FStack[FStackPos])^ := nil;
    {$ENDIF}

    Inc(FStackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetCallerAddress;
  begin
    if (FCallStackPos < 2) then
      PPointer(@FStack[FStackPos])^ := nil
    else
      PPointer(@FStack[FStackPos])^ := Pointer(FCallStack[FCallStackPos - 2].Address);

    Inc(FStackPos, SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoDumpCallStack;
  var
    StackTraceInfo: TStackTraceInfo;
    Offset, i: Integer;
    Pos: PDocPos;
  begin
    Offset := PInt32(@FStack[FStackPos - (SizeOf(Pointer) + SizeOf(Int32))])^;

    if (Offset >= 0) and (Offset <= FCallStackPos) then
    begin
      SetLength(StackTraceInfo, FCallStackPos - Offset);
      for i := 0 to High(StackTraceInfo) do
      begin
        StackTraceInfo[i].Address := FCallStack[i].Address;
        StackTraceInfo[i].CalledFrom := FCallStack[i].CalledFrom;
      end;

      {$IFDEF Lape_EmitPos}
      if (Offset > 0) then
        Pos := PPointer(FCallStack[FCallStackPos - Offset].CalledFrom + SizeOf(opCode))^
      else
        Pos := PPointer(Code + SizeOf(opCode))^;
      {$ELSE}
      Pos := nil;
      {$ENDIF}

      PlpString(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ := GetStackTrace(FEmitter, StackTraceInfo, Pos);
    end;

    Dec(FStackPos, SizeOf(Int32) + SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetScriptMethodName;
  begin
    {$IFDEF Delphi}
    PShortString(@FStack[FStackPos - SizeOf(Pointer)])^ := UTF8EncodeToShortString(FEmitter.CodePointerName[PCodePos(@FStack[FStackPos - SizeOf(Pointer)])^]);
    {$ELSE}
    PShortString(@FStack[FStackPos - SizeOf(Pointer)])^ := FEmitter.CodePointerName[PCodePos(@FStack[FStackPos - SizeOf(Pointer)])^];
    {$ENDIF}

    Inc(FStackPos, SizeOf(ShortString) - SizeOf(Pointer));
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
    if (FInJump.JumpException <> nil) then
    begin
      PreviousException.Exists := True;

      if IsLapeException(FInJump.JumpException.Obj) then
      begin
        PreviousException.Msg := lpException(FInJump.JumpException.Obj).Error;
        PreviousException.Loc := lpException(FInJump.JumpException.Obj).DocPos;
      end else
      begin
        PreviousException.Msg := FInJump.JumpException.Obj.Message;
        if (FInJump.JumpException.Pos <> nil) then
          PreviousException.Loc := FInJump.JumpException.Pos^;
      end;

      FInJump.JumpException.Obj.Free();

      FreeAndNil(FInJump.JumpException);
    end;

    Inc(Code, ocSize);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    InitStackSize: TStackOffset;
  begin
    InitStackSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    {$IFDEF DEBUG_STACKRESIZE}
    if (FStackPos + InitStackSize > Length(FStack)) then
      WriteLn('[Resize]: DoInitStackLen Need=', FStackPos + InitStackSize, 'Current=', Length(FStack));
    {$ENDIF}

    if (FStackPos + InitStackSize > UInt32(Length(FStack))) then
      SetLength(FStack, FStackPos + InitStackSize + FDefStackSize);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    InitStackSize: TStackOffset;
  begin
    InitStackSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    {$IFDEF DEBUG_STACKRESIZE}
    if (FStackPos + InitStackSize > Length(FStack)) then
      WriteLn('[Resize]: DoInitStack Need=', FStackPos + InitStackSize, 'Current=', Length(FStack));
    {$ENDIF}

    Assert(InitStackSize > 0);
    if (FStackPos + InitStackSize > UInt32(Length(FStack))) then
      SetLength(FStack, FStackPos + InitStackSize + FDefStackSize);
    FillChar(FStack[FStackPos], InitStackSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    {$IFDEF DEBUG_STACKRESIZE}
    if (FStackPos + GrowSize > Length(FStack)) then
      WriteLn('[Resize]: DoGrowStack Need=', FStackPos + GrowSize, 'Current=', Length(FStack));
    {$ENDIF}

    Assert(GrowSize > 0);
    if (FStackPos + GrowSize > UInt32(Length(FStack))) then
      SetLength(FStack, FStackPos + GrowSize + FDefStackSize);
    Inc(FStackPos, GrowSize);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    ExpandSize: TStackOffset;
  begin
    ExpandSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    Assert(ExpandSize > 0);
    if NeedMoreVarStack(ExpandSize) then
      ExpandVarStack(ExpandSize);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    ExpandSize: TStackOffset;
  begin
    ExpandSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    Assert(ExpandSize > 0);
    if NeedMoreVarStack(ExpandSize) then
      ExpandVarStack(ExpandSize);
    FillChar(FVarStack[FVarStackPos], ExpandSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    Assert(GrowSize > 0);
    if NeedMoreGrowVarStack(GrowSize) then
      GrowVarStack(GrowSize);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  var
    GrowSize: TStackOffset;
  begin
    GrowSize := PStackOffset(PtrUInt(Code) + ocSize)^;
    Assert(GrowSize > 0);
    if NeedMoreGrowVarStack(GrowSize) then
      GrowVarStack(GrowSize);
    FillChar(FVarStack[FVarStackLen - GrowSize], GrowSize, 0);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
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
    Inc(Code, ocSize);
  end;

  procedure DoPopStackToVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      Assert(Size > 0);
      Dec(FStackPos, Size);
      Move(FStack[FStackPos], FVarStack[FVarStackPos + VOffset], Size);
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoPopVarToStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      Assert(Size > 0);
      Move(FVarStack[FVarStackPos + VOffset], FStack[FStackPos], Size);
      FillChar(FVarStack[FVarStackPos + VOffset], Size, 0);
      Inc(FStackPos, Size);
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoJmpVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(FStackPos, SizeOf(TCodePos));
    //JumpTo(PCodePos(@FStack[FStackPos])^);
    FInJump.JumpSafe := @FStack[FStackPos];
    if (PCodePos(FInJump.JumpSafe)^ = 0) then
      LapeException(lpeInvalidJump);

    FInJump.JumpSafe := PByte(PtrUInt(CodeBase) + PCodePos(FInJump.JumpSafe)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafe; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FInJump.JumpSafe := PByte(PtrUInt(CodeBase) + PCodePos(PtrUInt(Code) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoJmpSafeR; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    FInJump.JumpSafe := PByte(PtrInt(Code) + PCodeOffset(PtrUInt(Code) + ocSize)^);
    HandleSafeJump();
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FTryStackPos >= UInt32(Length(FTryStack))) then
      SetLength(FTryStack, FTryStackPos + FDefTryStackSize);

    with POC_IncTry(PtrUInt(Code) + ocSize)^ do
    begin
      FTryStack[FTryStackPos].Jmp := PByte(PtrInt(Code) + Jmp);
      if (JmpFinally = Try_NoFinally) then
        FTryStack[FTryStackPos].JmpFinally := nil
      else if (JmpFinally = Try_NoExcept) then
        FTryStack[FTryStackPos].JmpFinally := FTryStack[FTryStackPos].Jmp
      else
        FTryStack[FTryStackPos].JmpFinally := PByte(PtrInt(Code) + Int32(JmpFinally) + Jmp);
    end;

    Inc(FTryStackPos);
    Inc(Code, ocSize + SizeOf(TOC_IncTry));
  end;

  procedure DoDecTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    Dec(FTryStackPos);
    Inc(Code, ocSize);
  end;

  procedure DoEndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FInJump.JumpException <> nil) then
      HandleException()
    else if (FInJump.JumpSafe <> nil) then
      HandleSafeJump()
    else
      Inc(Code, ocSize);

    PreviousException.Exists := False;
  end;

  procedure DoIncCall(const RecSize: Integer; const Jmp: TCodePos; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FCallStackPos >= UInt32(Length(FCallStack))) then
      SetLength(FCallStack, FCallStackPos + FDefCallStackSize);

    with FCallStack[FCallStackPos] do
    begin
      {$IFDEF DEBUG_INVOKE}
      WriteLn('[INVOKE] Name=', FEmitter.CodePointerName[Jmp], ' Depth=', FCallStackPos);
      {$ENDIF}

      Address := Jmp;
      CalledFrom := Code;
      JumpBack := PByte(PtrInt(Code) + ocSize + RecSize);
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
      Code := @opNone
    else
    begin
      DoPopVar();
      Dec(FCallStackPos);
      with FCallStack[FCallStackPos] do
      begin
        Code := JumpBack;
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
    if (tmp.JumpException = FInJump.JumpException) and (FInJump.JumpException <> nil) then
      HandleException()
    else if (tmp.JumpSafe = FInJump.JumpSafe) and (FInJump.JumpSafe <> nil) then
      HandleSafeJump();
  end;

  procedure DoInvokeImportedProc(const RecSize: Integer; const Ptr: Pointer; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedProc(Ptr)(@FStack[FStackPos - ParamSize]);
    Dec(FStackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  procedure DoInvokeImportedFunc(const RecSize: Integer; const Ptr, Res: Pointer; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedFunc(Ptr)(@FStack[FStackPos - ParamSize], Res);
    Dec(FStackPos, ParamSize - StackPosOffset);
    Inc(Code, RecSize + ocSize);
  end;

  {$I lpinterpreter_doinvoke.inc}
  {$I lpinterpreter_dojump.inc}
  {$I lpinterpreter_doeval.inc}

var
  StackTrace: lpString;
  GoBack: Boolean;
  i: Integer;
label
  Start;
begin
  FRunning := bTrue;

  Code := PByte(PtrUInt(FEmitter.Code) + InitialJump);
  CodeBase := FEmitter.Code;
  CodeUpper := FEmitter.Code + FEmitter.CodeLen;

  if not FStacksAllocated then
  begin
    SetLength(FStack, FDefStackSize);
    SetLength(FTryStack, FDefTryStackSize);
    SetLength(FCallStack, FDefCallStackSize);
    SetLength(FVarStackStack, FDefVarStackStackSize);
    SetLength(FVarStackStack[0].Stack, FDefVarStackSize);

    FStacksAllocated := True;
  end;

  FVarStack := FVarStackStack[0].Stack;
  FVarStackIndex := 0;
  FVarStackLen := Length(InitialVarStack);
  if (FVarStackLen > 0) then
  begin
    if (Length(FVarStack) < FVarStackLen) then
      SetLength(FVarStack, FVarStackLen);
    Move(InitialVarStack[0], FVarStack[0], FVarStackLen);
  end;

  PreviousException.Exists := False;
  PreviousException.Msg := '';
  PreviousException.Loc := NullDocPos;

  FStackPos := 0;
  FVarStackPos := 0;
  FTryStackPos := 0;
  FCallStackPos := 0;
  FInJump := InEmptyJump;

  try
    Start:
    GoBack := False;

    try
      while (FRunning = bTrue) do {$I lpinterpreter_opcodecase.inc}
    except
      if (FInJump.JumpException = nil) or (ExceptObject <> FInJump.JumpException.Obj) then
      begin
        if (FInJump.JumpException = nil) then
          FInJump.JumpException := TJumpException.Create();

        SetLength(FInJump.JumpException.StackTraceInfo, FCallStackPos);
        for i := 0 to FCallStackPos - 1 do
        begin
          FInJump.JumpException.StackTraceInfo[i].Address    := FCallStack[i].Address;
          FInJump.JumpException.StackTraceInfo[i].CalledFrom := FCallStack[i].CalledFrom;
        end;

        {$IFDEF Lape_EmitPos}
        FInJump.JumpException.Pos := PPointer(PtrUInt(Code) + SizeOf(opCode))^;
        {$ENDIF}
      end;
      FInJump.JumpException.Obj := Exception(AcquireExceptionObject());

      HandleException();
      GoBack := True;
    end;

    if (FRunning = bUnknown) then
    begin
      Sleep(1);
      goto Start;
    end
    else if (FRunning = bFalse) then
    begin
      FRunning := bTrue;
      FInJump.JumpSafe := PByte(PtrUInt(CodeBase) + EndJump);
      HandleSafeJump();
      goto Start;
    end;

    if GoBack then
      goto Start;
  except
    on E: Exception do
    begin
      FRunning := bFalse;

      StackTrace := GetStackTrace(FEmitter, FInJump.JumpException.StackTraceInfo, FInJump.JumpException.Pos);
      if IsLapeException(E) then
        LapeExceptionRuntime(E as lpException, StackTrace)
      else
      if (FInJump.JumpException <> nil) and (E = FInJump.JumpException.Obj) and (FInJump.JumpException.Pos <> nil) then
        LapeExceptionRuntime(E, StackTrace, FInJump.JumpException.Pos^)
      else
        LapeExceptionRuntime(E, StackTrace, NullDocPos);
    end;
  end;

  FRunning := bFalse;
end;

procedure TLapeCodeRunner.Resume;
begin
  FRunning := bTrue;
end;

procedure TLapeCodeRunner.Pause;
begin
  FRunning := bUnknown;
end;

procedure TLapeCodeRunner.Stop;
begin
  FRunning := bFalse
end;

procedure RunCode(const Emitter: TLapeCodeEmitter; const InitialVarStack: TByteArray; const InitialJump: TCodePos);
begin
  with TLapeCodeRunner.Create(Emitter) do
  try
    Run(InitialJump, InitialVarStack);
  finally
    Free();
  end;
end;

end.

