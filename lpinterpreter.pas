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
    ocGetScriptMethodName,
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
      StackTraceInfo: array of record
        Address: TCodePos;
        CalledFrom: PByte;
      end;
    end;
    JumpSafe: PByte;
  end;

type
  TLapeCodeRunner = class(TLapeBaseClass)
  protected
  type
    TMethodMap = {$IFDEF FPC}specialize{$ENDIF} TLapePointerMap<lpString>;
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
    end;
    FTryStackPos: UInt32;
    FInJump: TInJump;
    FPreviousException: record
      Msg: String;
      Loc: TDocPos;
    end;

    FCallStack: array of record
      Address: TCodePos;
      CalledFrom: PByte;
      JumpBack: PByte;
      StackP, VarStackP: UInt32;
      Jump: TInJump;
    end;
    FCallStackPos: UInt32;

    FMethodMap: TMethodMap;
    FExceptionStackTrace: String;

    function GetScriptMethodName(const CodePos: TCodePos): String;
    function GetExceptionStackTrace: lpString;
  public
    DoContinue: TInitBool;

    constructor Create(CodeBase: PByte; CodeLen: Integer); reintroduce; overload;
    constructor Create(Emitter: TLapeBaseClass); reintroduce; overload;
    destructor Destroy; override;

    procedure Run(InitialVarStack: TByteArray = nil; InitialJump: TCodePos = 0);

    property ExceptionStackTrace: String read FExceptionStackTrace;
  end;

implementation

uses
  lpmessages, lpvartypes;

{$OverFlowChecks Off}

const
  opNone: opCodeType = opCodeType(ocNone);

  InEmptyJump: TInJump = (JumpException: (Obj: nil; Pos: nil; StackTraceInfo: nil); JumpSafe: nil);

procedure MergeJumps(var AJump: TInJump; const Merge: TInJump); {$IFDEF Lape_Inline}inline;{$ENDIF}
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

function IsLapeException(const E: Exception): Boolean;
begin
  Result := (E <> nil) and (E.ClassType = lpException) and HasDocPos(lpException(e).DocPos);
end;

function TLapeCodeRunner.GetScriptMethodName(const CodePos: TCodePos): String;
begin
  Result := FMethodMap[Pointer(PtrUInt(CodePos))];
end;

constructor TLapeCodeRunner.Create(CodeBase: PByte; CodeLen: Integer);
begin
  inherited Create();

  FMethodMap := TMethodMap.Create();

  SetLength(FStack, StackSize);
  SetLength(FTryStack, TryStackSize);
  SetLength(FCallStack, CallStackSize);
  SetLength(FVarStackStack, VarStackStackSize);

  FCodeBase := CodeBase;
  FCodeUpper := PByte(CodeBase + CodeLen);
end;

constructor TLapeCodeRunner.Create(Emitter: TLapeBaseClass);
var
  i: Integer;
  CodeEmitter: TLapeCodeEmitter absolute Emitter;
begin
  if (not (Emitter is TLapeCodeEmitter)) then
    LapeException(lpeImpossible);

  Create(CodeEmitter.Code, CodeEmitter.CodeLen);

  with CodeEmitter.CodePointers do
    for i := 0 to Count - 1 do
      FMethodMap.Items[Pointer(PtrUInt(Items[i].Pos^))] := Items[i].Name;
end;

destructor TLapeCodeRunner.Destroy;
begin
  if (FMethodMap <> nil) then
    FreeAndNil(FMethodMap);

  inherited Destroy();
end;

function TLapeCodeRunner.GetExceptionStackTrace: lpString;

  function FormatLine(Number: Integer; FuncName: String; DocPos: TDocPos): lpString;
  begin
    if (FuncName = 'main') then
      FuncName := '"' + FuncName + '"'
    else
      FuncName := 'function "' + FuncName + '"';

    if HasDocPos(DocPos) then
    begin
      Result := LineEnding + '  ' + IntToStr(Number) + ') Line ' + IntToStr(DocPos.Line) + ' in ' + FuncName;
      if (DocPos.FileName <> '') then
        Result := Result + ' in file "' + DocPos.FileName + '"';
    end else
      Result := LineEnding + '  ' + IntToStr(Number) + ') ' + FuncName;
  end;

var
  i, Number: Integer;
  DocPos: TDocPos;
begin
  Result := 'Stack trace:';

  Number := 1;
  with FInJump.JumpException do
  begin
    Result := Result + FormatLine(0, GetScriptMethodName(StackTraceInfo[High(StackTraceInfo)].Address), Pos^);

    for i := High(StackTraceInfo) downto 0 do
    begin
      DocPos := PDocPos(StackTraceInfo[i].CalledFrom + SizeOf(opCodeType))^;

      if (i > 0) then
        Result := Result + FormatLine(Number, GetScriptMethodName(StackTraceInfo[i - 1].Address), DocPos)
      else
        Result := Result + FormatLine(Number, 'main', DocPos);

      Inc(Number);
    end;
  end;
end;

procedure TLapeCodeRunner.Run(InitialVarStack: TByteArray; InitialJump: TCodePos);

  procedure ExpandVarStack(const Size: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
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

  procedure GrowVarStack(const Size: UInt32); {$IFDEF Lape_Inline}inline;{$ENDIF}
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
          Move(Stack[Pos], FVarStack[0], OldLen);
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
      Dec(FTryStackPos);
      FCode := FTryStack[FTryStackPos].Jmp;
    end else
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
    if (Size > 0) then
      Dec(FStackPos, Size);
    ExpandVarStack(Size);
    if (Size > 0) then
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

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FInJump.JumpException.Obj <> nil) then
    begin
      if IsLapeException(FInJump.JumpException.Obj) then
      begin
        FPreviousException.Msg := lpException(FInJump.JumpException.Obj).Error;
        FPreviousException.Loc := lpException(FInJump.JumpException.Obj).DocPos;
      end else
      begin
        FPreviousException.Msg := FInJump.JumpException.Obj.Message;
        if (FInJump.JumpException.Pos <> nil) then
          FPreviousException.Loc := FInJump.JumpException.Pos^;
      end;
    end;

    FreeAndNil(FInJump.JumpException.Obj);

    Inc(FCode, ocSize);
  end;

  procedure DoGetExceptionMessage;
  begin
    PShortString(@FStack[FStackPos])^ := FPreviousException.Msg;

    Inc(FStackPos, SizeOf(ShortString));
    Inc(FCode, ocSize);
  end;

  procedure DoGetExceptionLocation;
  begin
    PPointer(@FStack[FStackPos])^ := @FPreviousException.Loc;

    Inc(FStackPos, SizeOf(Pointer));
    Inc(FCode, ocSize);
  end;

  procedure DoGetCallerLocation;
  begin
    PPointer(@FStack[FStackPos])^ := FCallStack[FCallStackPos - 1].CalledFrom + SizeOf(opCodeType);

    Inc(FStackPos, SizeOf(Pointer));
    Inc(FCode, ocSize);
  end;

  procedure DoGetScriptMethodName;
  begin
    PShortString(@FStack[FStackPos - SizeOf(Pointer)])^ := FMethodMap[PPointer(@FStack[FStackPos - SizeOf(Pointer)])^];

    Dec(FStackPos, SizeOf(Pointer) - SizeOf(ShortString));
    Inc(FCode, ocSize);
  end;

  procedure DoReRaiseException;
  begin
    Inc(FCode, ocSize);

    LapeException(FPreviousException.Msg, FPreviousException.Loc);
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
    if (FInJump.JumpException.Obj <> nil) then
      HandleException()
    else if (FInJump.JumpSafe <> nil) then
      HandleSafeJump()
    else
      Inc(FCode, ocSize);

    FPreviousException.Msg := '';
    FPreviousException.Loc := NullDocPos;
  end;

  procedure DoIncCall(const RecSize: Integer; const Jmp: TCodePos; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    if (FCallStackPos >= UInt32(Length(FCallStack))) then
      SetLength(FCallStack, FCallStackPos + (CallStackSize div 2));

    with FCallStack[FCallStackPos] do
    begin
      Address := Jmp;
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

  procedure DoInvokeImportedProc(const RecSize: Integer; const Ptr: Pointer; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    TLapeImportedProc(Ptr)(@FStack[FStackPos - ParamSize]);
    Dec(FStackPos, ParamSize - StackPosOffset);
    Inc(FCode, RecSize + ocSize);
  end;

  procedure DoInvokeImportedFunc(const RecSize: Integer; const Ptr, Res: Pointer; const ParamSize: TParamSize; const StackPosOffset: TStackInc = 0); {$IFDEF Lape_Inline}inline;{$ENDIF}
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
    i: Integer;
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
      begin
        FInJump.JumpException.Pos := PDocPos(PtrUInt(FCode) + SizeOf(opCodeType));

        SetLength(FInJump.JumpException.StackTraceInfo, FCallStackPos);
        for i := 0 to FCallStackPos - 1 do
        begin
          FInJump.JumpException.StackTraceInfo[i].Address    := FCallStack[i].Address;
          FInJump.JumpException.StackTraceInfo[i].CalledFrom := FCallStack[i].CalledFrom;
        end;
      end;
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
      FExceptionStackTrace := GetExceptionStackTrace();

      if IsLapeException(E) then
        LapeExceptionFmt(lpeRunTime, [lpException(E).Error], lpException(E).DocPos)
      else
      if (E = FInJump.JumpException.Obj) and (FInJump.JumpException.Pos <> nil) then
        LapeExceptionFmt(lpeRuntime, [E.Message], FInJump.JumpException.Pos^)
      else
        LapeExceptionFmt(lpeRuntime, [E.Message]);
    end;
  end;
end;

end.

