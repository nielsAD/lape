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
  lptypes, lpvartypes, lpinterpreter_types, lpinterpreter_debugevals;

type
  TStackTraceInfo = array of record
    Address: TCodePos;
    CalledFrom: TCodePos;
  end;

  // What was raised and is on its way: what it says and where
  TJumpException = class
    Msg: lpString;
    Loc: TDocPos;
    // (where it was raised; Loc is where it says it was)
    Pos: TDocPos;
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
    CalledFrom: TCodePos;
    JumpBack: PByte;
    StackP, VarStackP: UInt32;
    Jump: TInJump;
  end;

  TLapeCodeRunner = class(TLapeBaseClass)
  protected
    FCode: PByte;
    FCodeLen: Integer;
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

    {$IFDEF Lape_InterpreterDebug_EvalUsage}
    FDebugEvals: TLapeDebugEvals;
    {$ENDIF}

    function getRunning: Boolean;
    function getPaused: Boolean;
    function getStopped: Boolean;
    function RaisedAt(const Given: TDocPos): TDocPos; virtual;
  public
    // (the code as it is now: compile before, nothing is emitted after)
    constructor Create(ACode: PByte; ACodeLen: Integer); reintroduce; overload;
    constructor Create(Emitter: TLapeCodeEmitter); reintroduce; overload;
    destructor Destroy; override;

    // (code that was still being compiled when the runner was made)
    procedure SetCode(ACode: PByte; ACodeLen: Integer); virtual;
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
{$IFDEF Lape_InterpreterDebug_EvalUsage}           {$DEFINE DEBUG_EVALUSAGE}   {$ENDIF}

{$OverflowChecks Off}

{$IFDEF FPC}
  {$IFDEF Lape_UseFPCTrunk_FillChar}
    {$i extensions/fpctrunkfillchar.inc}
  {$ENDIF}
{$ENDIF}

const
  InEmptyJump: TInJump = (JumpException: nil; JumpSafe: nil);

function GetStackTrace(Code: PByte; CodeLen: Integer; StackTraceInfo: TStackTraceInfo; const Pos: TDocPos): lpString;

  function FormatLine(Number: Integer; FuncName: String; const Pos: TDocPos): lpString;
  begin
    if Pos.isInternal then
    begin
      // (a method of a type with no name)
      if (FuncName <> '') and (FuncName[1] = '.') then
        Delete(FuncName, 1, 1);
      // (1:1 is a method built without source: no line to show)
      if (Pos.Line = 1) and (Pos.Col = 1) then
        Exit(LineEnding + '  ' + IntToStr(Number) + ') internal function "' + FuncName + '"');
      Exit(LineEnding + '  ' + IntToStr(Number) + ') Line ' + IntToStr(Pos.Line) + ', Col ' + IntToStr(Pos.Col) + ' in internal function "' + FuncName + '"');
    end;

    if (FuncName = 'main') then
      FuncName := '"' + FuncName + '"'
    else
      FuncName := 'function "' + FuncName + '"';

    if (not Pos.isEmpty) then
    begin
      Result := LineEnding + '  ' + IntToStr(Number) + ') Line ' + IntToStr(Pos.Line) + ' in ' + FuncName;
      if (Pos.FileName <> '') then
        Result := Result + ' in file "' + Pos.FileName + '"';
    end else
      Result := LineEnding + '  ' + IntToStr(Number) + ') ' + FuncName;
  end;

var
  i, Number: Integer;
begin
  Result := 'Stack Trace:';

  if (Length(StackTraceInfo) = 0) then
    Result := Result + FormatLine(0, 'main', Pos)
  else
  begin
    Result := Result + FormatLine(0, CodeMethodName(Code, CodeLen, StackTraceInfo[High(StackTraceInfo)].Address), Pos);

    Number := 1;
    for i := High(StackTraceInfo) downto 0 do
    begin
      if (i > 0) then
        Result := Result + FormatLine(Number, CodeMethodName(Code, CodeLen, StackTraceInfo[i - 1].Address), CodeDocPos(Code, CodeLen, StackTraceInfo[i].CalledFrom))
      else
        Result := Result + FormatLine(Number, 'main', CodeDocPos(Code, CodeLen, StackTraceInfo[i].CalledFrom));

      Inc(Number);
    end;
  end;
end;

type
  ELapeUncaught = class(Exception);

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
  Result := (E <> nil) and (E.ClassType = lpException) and (not lpException(e).DocPos.isEmpty);
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

// Where something raised at Given is said to be: in an internal method, the nearest call that the script wrote
function TLapeCodeRunner.RaisedAt(const Given: TDocPos): TDocPos;
var
  Pos: TDocPos;
  i: Integer;
begin
  Result := Given;
  i := FCallStackPos;
  while Result.isInternal and (i > 0) do
  begin
    Dec(i);
    Pos := CodeDocPos(FCode, FCodeLen, FCallStack[i].CalledFrom);
    if (not Pos.isEmpty) then
      Result := Pos;
  end;

  // (code of Lape that nothing of the script called: as it is)
  if Result.isInternal then
    Result := Given;
end;

constructor TLapeCodeRunner.Create(Emitter: TLapeCodeEmitter);
begin
  Create(Emitter.Code, Emitter.CodeLen);
end;

constructor TLapeCodeRunner.Create(ACode: PByte; ACodeLen: Integer);
begin
  inherited Create();

  SetCode(ACode, ACodeLen);

  FDefStackSize         := 1024*8;
  FDefVarStackSize      := 1024*16;
  FDefVarStackStackSize := 64;
  FDefTryStackSize      := 128;
  FDefCallStackSize     := 128;

  {$IFDEF DEBUG_EVALUSAGE}
  FDebugEvals := TLapeDebugEvals.Create();
  {$ENDIF}
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

  {$IFDEF DEBUG_EVALUSAGE}
  FDebugEvals.Free();
  {$ENDIF}
end;

procedure TLapeCodeRunner.SetCode(ACode: PByte; ACodeLen: Integer);
begin
  FCode := ACode;
  FCodeLen := ACodeLen;
end;

procedure TLapeCodeRunner.Run(const InitialJump: TCodePos = 0; const InitialVarStack: TByteArray = nil);
const
  opNone: opCode = ocNone;
var
  Code: PByte;
  CodeBase: PByte;
  CodeUpper: PByte;
  Trailer: TCodeTrailer;

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
      raise ELapeUncaught.Create('');
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
    Pos: TDocPos;
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

      if (Offset > 0) then
        Pos := CodeDocPos(CodeBase, FCodeLen, FCallStack[FCallStackPos - Offset].CalledFrom)
      else
        Pos := CodeDocPos(CodeBase, FCodeLen, PtrUInt(Code) - PtrUInt(CodeBase));

      PlpString(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ := GetStackTrace(FCode, FCodeLen, StackTraceInfo, Pos);
    end;

    Dec(FStackPos, SizeOf(Int32) + SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetScriptMethodName;
  var
    At: TCodePos;
    Name: lpString;
  begin
    // (any method value: only one that is in the code has a name there)
    At := PCodePos(@FStack[FStackPos - SizeOf(Pointer)])^;
    Name := '';
    if (At < TCodePos(CodeUpper - CodeBase)) then
      Name := CodeMethodName(CodeBase, FCodeLen, At);
    {$IFDEF Delphi}
    PShortString(@FStack[FStackPos - SizeOf(Pointer)])^ := UTF8EncodeToShortString(Name);
    {$ELSE}
    PShortString(@FStack[FStackPos - SizeOf(Pointer)])^ := Name;
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
      PreviousException.Msg := FInJump.JumpException.Msg;
      PreviousException.Loc := FInJump.JumpException.Loc;

      FreeAndNil(FInJump.JumpException);
    end;

    Inc(Code, ocSize);
  end;

  procedure DoUnused;
  begin
    LapeException(lpeImpossible);
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
      WriteLn('[INVOKE] Name=', CodeMethodName(CodeBase, FCodeLen, Jmp), ' Depth=', FCallStackPos);
      {$ENDIF}

      Address := Jmp;
      CalledFrom := PtrUInt(Code) - PtrUInt(CodeBase);
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
  StackTrace, UncaughtMsg: lpString;
  UncaughtLoc: TDocPos;
  GoBack: Boolean;
  i: Integer;
label
  Start;
begin
  FRunning := bTrue;

  // (0: the program)
  Code := PByte(PtrUInt(FCode) + InitialJump);
  CodeBase := FCode;
  // (where the program ends: a method is only ever in there)
  if CodeTrailer(FCode, FCodeLen, Trailer) then
    CodeUpper := FCode + Trailer.ProgramSize
  else
    CodeUpper := FCode + FCodeLen;

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
      while (FRunning = bTrue) do
      begin
        {$IFDEF DEBUG_EVALUSAGE}
        if (opCodeP(Code)^ in [ocEval_PtrD_PtrL_PtrR..ocEval_PVarD_PStkL_PStkR]) then
          FDebugEvals.Add(TLapeEvalProc(PPointer(PtrUInt(Code) + ocSize)^), opCodeP(Code)^);
        {$ENDIF}

        {$I lpinterpreter_opcodecase.inc}
      end;
    except
      // nothing handles it
      if (ExceptObject is ELapeUncaught) then
        raise;

      if (FInJump.JumpException = nil) then
        FInJump.JumpException := TJumpException.Create();

      SetLength(FInJump.JumpException.StackTraceInfo, FCallStackPos);
      for i := 0 to FCallStackPos - 1 do
      begin
        FInJump.JumpException.StackTraceInfo[i].Address    := FCallStack[i].Address;
        FInJump.JumpException.StackTraceInfo[i].CalledFrom := FCallStack[i].CalledFrom;
      end;

      FInJump.JumpException.Pos := CodeDocPos(CodeBase, FCodeLen, PtrUInt(Code) - PtrUInt(CodeBase));

      if IsLapeException(Exception(ExceptObject)) then
      begin
        FInJump.JumpException.Msg := lpException(ExceptObject).Error;
        FInJump.JumpException.Loc := lpException(ExceptObject).DocPos;
      end else
      begin
        FInJump.JumpException.Msg := Exception(ExceptObject).Message;
        FInJump.JumpException.Loc := FInJump.JumpException.Pos;
      end;

      FInJump.JumpException.Loc := RaisedAt(FInJump.JumpException.Loc);

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
      if (not (E is ELapeUncaught)) or (FInJump.JumpException = nil) then
        LapeExceptionRuntime(E, '', NullDocPos);

      try
        StackTrace := GetStackTrace(FCode, FCodeLen, FInJump.JumpException.StackTraceInfo, FInJump.JumpException.Pos);
        UncaughtMsg := FInJump.JumpException.Msg;
        UncaughtLoc := FInJump.JumpException.Loc;
      finally
        for i := FCallStackPos - 1 downto 0 do
          if (FCallStack[i].Jump.JumpException <> FInJump.JumpException) then
            FreeAndNil(FCallStack[i].Jump.JumpException);
        FreeAndNil(FInJump.JumpException);
      end;
      raise lpException.Create(Format(lpeRuntime, [UncaughtMsg]), UncaughtLoc, StackTrace);
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

