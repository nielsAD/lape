{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Bytecode emitter.
}
unit lpcodeemitter;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpinterpreter_types, lptokenizer;

type
  PLapeEvalProc = ^TLapeEvalProc;

  TLapeCodeEmitterBase = class(TLapeBaseClass)
  protected
  type
    // Where the instructions from Offset on are from in the script
    TCodeDocPos = record
      Offset: Integer;
      Pos: TDocPos;
    end;
    // A script method that starts at Offset
    TCodeMethod = record
      Offset: Integer;
      Name: Integer; // in the string table
    end;
  protected
    FCode: TCodeArray;
    FCodePtr: PByte;
    FCodeCur: Integer;
    // (while compiling: a DocPos where it changes, the methods, and the strings; at the end they go after the program)
    FDocPos: array of TCodeDocPos;
    FDocPosCount: Integer;
    FMethods: array of TCodeMethod;
    FMethodCount: Integer;
    // (as they go in the string table: UTF-8 in the Unicode build)
    FStrings: array of RawByteString;
    FStringCount: Integer;
    FCurrentPos: PDocPos;

    FMaxStack: Integer;
    FStackPos: Integer;

    procedure _Int8(v: Int8; Pos: PInt8); overload; virtual;
    procedure _UInt8(v: UInt8; Pos: PUInt8); overload; virtual;
    procedure _Int16(v: Int16; Pos: PInt16); overload; virtual;
    procedure _UInt16(v: UInt16; Pos: PUInt16); overload; virtual;
    procedure _Int32(v: Int32; Pos: PInt32); overload; virtual;
    procedure _UInt32(v: UInt32; Pos: PUInt32); overload; virtual;
    procedure _Int64(v: Int64; Pos: PInt64); overload; virtual;
    procedure _UInt64(v: UInt64; Pos: PUInt64); overload; virtual;
    procedure _Pointer(v: Pointer; Pos: PPointer); overload; virtual;

    procedure _Opcode(v: opCode; Pos: opCodeP); overload; virtual;
    procedure _CodePos(v: TCodePos; Pos: PCodePos); overload; virtual;
    procedure _CodeOffset(v: TCodeOffset; Pos: PCodeOffset); overload; virtual;

    procedure _EvalProc(v: TLapeEvalProc; Pos: PLapeEvalProc); overload; virtual;
    procedure _StackInc(v: TStackInc; Pos: PStackInc); overload; virtual;
    procedure _StackOffset(v: TStackOffset; Pos: PStackOffset); overload; virtual;
    procedure _VarStackOffset(v: TVarStackOffset; Pos: PVarStackOffset); overload; virtual;
    procedure _PointerOffset(v: TPointerOffset; Pos: PPointerOffset); overload; virtual;
    procedure _ParamSize(v: TParamSize; Pos: PParamSize); overload; virtual;

    procedure IncStack(Size: TStackInc); virtual;
    procedure DecStack(Size: TStackInc); virtual;

    procedure addDocPos(CodeOffset: Integer);
    // Len bytes more at the end: where they start
    function Reserve(Len: Integer): Integer; virtual;
    // Op with Value at At: where it ends
    function PatchOp(At: Integer; Op: opCode; Value: TStackOffset): Integer; virtual;
  public
    constructor Create; override;
    procedure Reset; virtual;

    // Where S is in the string table (each string once)
    function addString(const S: lpString): Integer; virtual;
    // The script method Name starts at CodeOffset
    procedure addMethod(CodeOffset: Integer; const Name: lpString); virtual;
    // When the program is done: the string, DocPos and method tables after it, then the trailer
    procedure WriteTables; virtual;

    procedure EnsureCodeGrowth(Len: Integer); virtual;

    function NewStack(Size: Integer = 0; Max: Integer = 0): Integer; virtual;

    procedure PatchJump(At, Target: Integer); virtual;
    function PatchTry(At, Target: Integer; JmpFinally: UInt32): Integer; virtual;
    function PatchUnused(At: Integer): Integer; virtual;
    function PatchJmpR(At, Target: Integer): Integer; virtual;
    function PatchExpandVar(At: Integer; Len: TStackOffset): Integer; virtual;
    function PatchExpandVarAndInit(At: Integer; Len: TStackOffset): Integer; virtual;
    function PatchGrowVar(At: Integer; Len: TStackOffset): Integer; virtual;
    function PatchGrowVarAndInit(At: Integer; Len: TStackOffset): Integer; virtual;
    function PatchInitStackLen(At: Integer; Len: TStackOffset): Integer; virtual;

    // Where what is emitted now is from (nil: nowhere); the node compiling sets it
    property CurrentPos: PDocPos read FCurrentPos write FCurrentPos;

    function _Int8(v: Int8): Integer; overload; virtual;
    function _UInt8(v: UInt8): Integer; overload; virtual;
    function _Int16(v: Int16): Integer; overload; virtual;
    function _UInt16(v: UInt16): Integer; overload; virtual;
    function _Int32(v: Int32): Integer; overload; virtual;
    function _UInt32(v: UInt32): Integer; overload; virtual;
    function _Int64(v: Int64): Integer; overload; virtual;
    function _UInt64(v: UInt64): Integer; overload; virtual;
    function _Pointer(v: Pointer): Integer; overload; virtual;

    function _Opcode(v: opCode): Integer; overload; virtual;
    function _CodePos(v: TCodePos): Integer; overload; virtual;
    function _CodeOffset(v: TCodeOffset): Integer; overload; virtual;

    function _EvalProc(v: TLapeEvalProc): Integer; overload; virtual;
    function _StackInc(v: TStackInc): Integer; overload; virtual;
    function _StackOffset(v: TStackOffset): Integer; overload; virtual;
    function _VarStackOffset(v: TVarStackOffset): Integer; overload; virtual;
    function _PointerOffset(v: TPointerOffset): Integer; overload; virtual;
    function _ParamSize(v: TParamSize): Integer; overload; virtual;

    function _op(op: opCode): Integer;

    function _IsScriptMethod: Integer;
    function _GetExceptionMessage: Integer;
    function _GetExceptionLocation: Integer;
    function _GetCallerAddress: Integer;
    function _GetScriptMethodName: Integer;
    function _DumpCallStack: Integer;
    function _ReRaiseException: Integer;
    function _Unused: Integer;
    function _InitStackLen(Len: TStackOffset): Integer;
    function _InitStack(Len: TStackOffset): Integer;
    function _GrowStack(Len: TStackOffset): Integer;
    function _ExpandVar(Len: TStackOffset): Integer;
    function _ExpandVarAndInit(Len: TStackOffset): Integer;
    function _GrowVar(Len: TStackOffset): Integer;
    function _GrowVarAndInit(Len: TStackOffset): Integer;
    function _PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer;
    function _PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer;
    function _PopVar: Integer;
    function _JmpVar: Integer;
    function _JmpSafe(Target: TCodePos): Integer;
    function _JmpSafeR(Jmp: TCodeOffset): Integer;

    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32): Integer;
    function _DecTry: Integer;
    function _EndTry: Integer;
    function _CatchException: Integer;

    function _DecCall: Integer;
    function _DecCall_EndTry: Integer;

    {$I lpcodeemitter_invokeheader.inc}
    {$I lpcodeemitter_jumpheader.inc}
    {$I lpcodeemitter_evalheader.inc}

    property Code: PByte read FCodePtr;
    property CodeLen: Integer read FCodeCur;
    // The deepest the eval stack gets in the method being compiled
    property MaxStack: Integer read FMaxStack;
  end;

implementation

uses
  lpmessages;
procedure TLapeCodeEmitterBase._Int8(v: Int8; Pos: PInt8);          begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._UInt8(v: UInt8; Pos: PUInt8);       begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._Int16(v: Int16; Pos: PInt16);       begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._UInt16(v: UInt16; Pos: PUInt16);    begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._Int32(v: Int32; Pos: PInt32);       begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._UInt32(v: UInt32; Pos: PUInt32);    begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._Int64(v: Int64; Pos: PInt64);       begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._UInt64(v: UInt64; Pos: PUInt64);    begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._Pointer(v: Pointer; Pos: PPointer); begin Pos^ := v; end;

procedure TLapeCodeEmitterBase._Opcode(v: opCode; Pos: opCodeP);              begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._CodePos(v: TCodePos; Pos: PCodePos);          begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._CodeOffset(v: TCodeOffset; Pos: PCodeOffset); begin Pos^ := v; end;

procedure TLapeCodeEmitterBase._EvalProc(v: TLapeEvalProc; Pos: PLapeEvalProc);           begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._StackInc(v: TStackInc; Pos: PStackInc);                   begin Pos^ := v; IncStack(v); end;
procedure TLapeCodeEmitterBase._VarStackOffset(v: TVarStackOffset; Pos: PVarStackOffset); begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._PointerOffset(v: TPointerOffset; Pos: PPointerOffset);    begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._StackOffset(v: TStackOffset; Pos: PStackOffset);          begin Pos^ := v; end;
procedure TLapeCodeEmitterBase._ParamSize(v: TParamSize; Pos: PParamSize);                begin Pos^ := v; DecStack(v); end;

procedure TLapeCodeEmitterBase.IncStack(Size: TStackInc);
begin
  Inc(FStackPos, Size);
  if (FStackPos < 0) then
    FStackPos := 0;
  if (FStackPos > FMaxStack) then
    FMaxStack := FStackPos;
end;

procedure TLapeCodeEmitterBase.DecStack(Size: TStackInc);
begin
  IncStack(-Size);
end;

procedure TLapeCodeEmitterBase.addDocPos(CodeOffset: Integer);
var
  NewPos: PDocPos;
begin
  NewPos := FCurrentPos;
  if (NewPos = nil) then
    NewPos := @NullDocPos;

  if (FDocPosCount > 0) then
    with FDocPos[FDocPosCount - 1] do
    begin
      Assert(Offset < CodeOffset);
      if (Pos.Line = NewPos^.Line) and (Pos.Col = NewPos^.Col) and (Pos.FileName = NewPos^.FileName) then
        Exit;
    end;

  if (FDocPosCount >= Length(FDocPos)) then
    SetLength(FDocPos, 1024 + Length(FDocPos) * 2);
  FDocPos[FDocPosCount].Offset := CodeOffset;
  FDocPos[FDocPosCount].Pos := NewPos^;
  Inc(FDocPosCount);
end;
function TLapeCodeEmitterBase.addString(const S: lpString): Integer;
var
  Raw: RawByteString;
  i: Integer;
begin
  {$IFDEF Lape_Unicode}
  Raw := UTF8Encode(S);
  {$ELSE}
  Raw := S;
  {$ENDIF}

  Result := 0;
  for i := 0 to FStringCount - 1 do
  begin
    if (FStrings[i] = Raw) then
      Exit;
    Inc(Result, Length(FStrings[i]) + 1);
  end;

  if (FStringCount >= Length(FStrings)) then
    SetLength(FStrings, 64 + FStringCount * 2);
  FStrings[FStringCount] := Raw;
  Inc(FStringCount);
end;

procedure TLapeCodeEmitterBase.addMethod(CodeOffset: Integer; const Name: lpString);
begin
  if (FMethodCount >= Length(FMethods)) then
    SetLength(FMethods, 64 + FMethodCount * 2);
  FMethods[FMethodCount].Offset := CodeOffset;
  FMethods[FMethodCount].Name := addString(Name);
  Inc(FMethodCount);
end;

procedure TLapeCodeEmitterBase.WriteTables;
var
  i: Integer;
  Entry: TCodeDocPosEntry;
  MethodEntry: TCodeMethodEntry;
  FileNames: array of Integer;
  Trailer: TCodeTrailer;
begin
  SetLength(FileNames, FDocPosCount);
  for i := 0 to FDocPosCount - 1 do
    if (i > 0) and (FDocPos[i].Pos.FileName = FDocPos[i - 1].Pos.FileName) then
      FileNames[i] := FileNames[i - 1]
    else
      FileNames[i] := addString(FDocPos[i].Pos.FileName);

  Trailer.Version := LapeCodeVersion;
  Trailer.ProgramSize := FCodeCur;

  // (the strings, each null-terminated; their offsets count from the start of the table)
  Trailer.StringTableStart := FCodeCur;
  Trailer.StringTableCount := FStringCount;
  for i := 0 to FStringCount - 1 do
  begin
    EnsureCodeGrowth(Length(FStrings[i]) + 1);
    if (FStrings[i] <> '') then
      Move(FStrings[i][1], FCode[FCodeCur], Length(FStrings[i]));
    Inc(FCodeCur, Length(FStrings[i]));
    FCode[FCodeCur] := 0;
    Inc(FCodeCur);
  end;
  Trailer.StringTableSize := UInt32(FCodeCur) - Trailer.StringTableStart;

  Trailer.DocPosTableStart := FCodeCur;
  Trailer.DocPosTableCount := FDocPosCount;
  EnsureCodeGrowth(FDocPosCount * SizeOf(Entry) + SizeOf(Trailer));
  for i := 0 to FDocPosCount - 1 do
  begin
    Entry.CodeOffset := FDocPos[i].Offset;
    Entry.Line := FDocPos[i].Pos.Line;
    Entry.Col := FDocPos[i].Pos.Col;
    Entry.FileName := FileNames[i];
    Move(Entry, FCode[FCodeCur], SizeOf(Entry));
    Inc(FCodeCur, SizeOf(Entry));
  end;
  Trailer.DocPosTableSize := UInt32(FCodeCur) - Trailer.DocPosTableStart;

  Trailer.MethodTableStart := FCodeCur;
  Trailer.MethodTableCount := FMethodCount;
  EnsureCodeGrowth(FMethodCount * SizeOf(MethodEntry) + SizeOf(Trailer));
  for i := 0 to FMethodCount - 1 do
  begin
    MethodEntry.CodeOffset := FMethods[i].Offset;
    MethodEntry.Name := FMethods[i].Name;
    Move(MethodEntry, FCode[FCodeCur], SizeOf(MethodEntry));
    Inc(FCodeCur, SizeOf(MethodEntry));
  end;
  Trailer.MethodTableSize := UInt32(FCodeCur) - Trailer.MethodTableStart;

  Assert(Trailer.StringTableStart = Trailer.ProgramSize);
  Assert(Trailer.DocPosTableStart = Trailer.StringTableStart + Trailer.StringTableSize);
  Assert(Trailer.DocPosTableSize = Trailer.DocPosTableCount * SizeOf(Entry));
  Assert(Trailer.MethodTableStart = Trailer.DocPosTableStart + Trailer.DocPosTableSize);
  Assert(Trailer.MethodTableSize = Trailer.MethodTableCount * SizeOf(MethodEntry));

  Move(Trailer, FCode[FCodeCur], SizeOf(Trailer));
  Inc(FCodeCur, SizeOf(Trailer));
end;

constructor TLapeCodeEmitterBase.Create;
begin
  inherited;

  Reset();
end;

procedure TLapeCodeEmitterBase.Reset;
begin
  FCodeCur := 0;
  FCurrentPos := nil;
  FDocPos := nil;
  FDocPosCount := 0;
  FMethods := nil;
  FMethodCount := 0;
  FStrings := nil;
  FStringCount := 0;

  EnsureCodeGrowth(0);
  NewStack();
end;

procedure TLapeCodeEmitterBase.EnsureCodeGrowth(Len: Integer);
begin
  // (half as much again as is needed)
  if (FCodeCur + Len >= Length(FCode)) then
  begin
    SetLength(FCode, (FCodeCur + Len) * 3 div 2 + 1024);
    FCodePtr := @FCode[0];
  end;
end;

function TLapeCodeEmitterBase.Reserve(Len: Integer): Integer;
begin
  Result := FCodeCur;
  EnsureCodeGrowth(Len);
  Inc(FCodeCur, Len);
end;

function TLapeCodeEmitterBase.PatchOp(At: Integer; Op: opCode; Value: TStackOffset): Integer;
begin
  Result := At + ocSize + SizeOf(TStackOffset);
  Assert((At >= 0) and (Result <= FCodeCur));
  opCodeP(@FCode[At])^ := Op;
  PStackOffset(@FCode[At + ocSize])^ := Value;
end;

// (every jump has its distance, from where it starts, right after its opcode)
procedure TLapeCodeEmitterBase.PatchJump(At, Target: Integer);
begin
  Assert((At >= 0) and (At + ocSize + SizeOf(TCodeOffset) <= FCodeCur));
  PCodeOffset(@FCode[At + ocSize])^ := Target - At;
end;

function TLapeCodeEmitterBase.PatchTry(At, Target: Integer; JmpFinally: UInt32): Integer;
begin
  Result := At + ocSize + SizeOf(TOC_IncTry);
  Assert((At >= 0) and (Result <= FCodeCur));
  opCodeP(@FCode[At])^ := ocIncTry;
  POC_IncTry(@FCode[At + ocSize])^.Jmp := Target - At;
  POC_IncTry(@FCode[At + ocSize])^.JmpFinally := JmpFinally;
end;

function TLapeCodeEmitterBase.PatchUnused(At: Integer): Integer;
begin
  Result := PatchOp(At, ocUnused, 0);
end;

function TLapeCodeEmitterBase.PatchJmpR(At, Target: Integer): Integer;
begin
  Result := PatchOp(At, ocJmpR, 0);
  PatchJump(At, Target);
end;

function TLapeCodeEmitterBase.PatchExpandVar(At: Integer; Len: TStackOffset): Integer;
begin
  Result := PatchOp(At, ocExpandVar, Len);
end;

function TLapeCodeEmitterBase.PatchExpandVarAndInit(At: Integer; Len: TStackOffset): Integer;
begin
  Result := PatchOp(At, ocExpandVarAndInit, Len);
end;

function TLapeCodeEmitterBase.PatchGrowVar(At: Integer; Len: TStackOffset): Integer;
begin
  Result := PatchOp(At, ocGrowVar, Len);
end;

function TLapeCodeEmitterBase.PatchGrowVarAndInit(At: Integer; Len: TStackOffset): Integer;
begin
  Result := PatchOp(At, ocGrowVarAndInit, Len);
end;

function TLapeCodeEmitterBase.PatchInitStackLen(At: Integer; Len: TStackOffset): Integer;
begin
  Result := PatchOp(At, ocInitStackLen, Len);
end;

function TLapeCodeEmitterBase.NewStack(Size: Integer = 0; Max: Integer = 0): Integer;
begin
  Result := FStackPos;
  FStackPos := Size;
  FMaxStack := Max;
end;

function TLapeCodeEmitterBase._Int8(v: Int8): Integer;
begin
  Result := Reserve(SizeOf(Int8));
  _Int8(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._UInt8(v: UInt8): Integer;
begin
  Result := Reserve(SizeOf(UInt8));
  _UInt8(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._Int16(v: Int16): Integer;
begin
  Result := Reserve(SizeOf(Int16));
  _Int16(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._UInt16(v: UInt16): Integer;
begin
  Result := Reserve(SizeOf(UInt16));
  _UInt16(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._Int32(v: Int32): Integer;
begin
  Result := Reserve(SizeOf(Int32));
  _Int32(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._UInt32(v: UInt32): Integer;
begin
  Result := Reserve(SizeOf(UInt32));
  _UInt32(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._Int64(v: Int64): Integer;
begin
  Result := Reserve(SizeOf(Int64));
  _Int64(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._UInt64(v: UInt64): Integer;
begin
  Result := Reserve(SizeOf(UInt64));
  _UInt64(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._Pointer(v: Pointer): Integer;
begin
  Result := Reserve(SizeOf(Pointer));
  _Pointer(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._Opcode(v: opCode): Integer;
begin
  Result := Reserve(SizeOf(opCode));
  _Opcode(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._CodePos(v: TCodePos): Integer;
begin
  Result := Reserve(SizeOf(TCodePos));
  _CodePos(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._CodeOffset(v: TCodeOffset): Integer;
begin
  Result := Reserve(SizeOf(TCodeOffset));
  _CodeOffset(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._EvalProc(v: TLapeEvalProc): Integer;
begin
  Result := Reserve(SizeOf(TLapeEvalProc));
  _EvalProc(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._StackInc(v: TStackInc): Integer;
begin
  Result := Reserve(SizeOf(TStackInc));
  _StackInc(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._StackOffset(v: TStackOffset): Integer;
begin
  Result := Reserve(SizeOf(TStackOffset));
  _StackOffset(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._VarStackOffset(v: TVarStackOffset): Integer;
begin
  Result := Reserve(SizeOf(TVarStackOffset));
  _VarStackOffset(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._PointerOffset(v: TPointerOffset): Integer;
begin
  Result := Reserve(SizeOf(TPointerOffset));
  _PointerOffset(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._ParamSize(v: TParamSize): Integer;
begin
  Result := Reserve(SizeOf(TParamSize));
  _ParamSize(v, @FCode[Result]);
end;

function TLapeCodeEmitterBase._op(op: opCode): Integer;
begin
  Result := _Opcode(op);
  {$IFDEF Lape_EmitPos}
  addDocPos(Result);
  {$ENDIF}
end;

function TLapeCodeEmitterBase._IsScriptMethod: Integer;
begin
  Result := _op(ocIsScriptMethod);
  DecStack(SizeOf(Pointer) - SizeOf(EvalBool));
end;

function TLapeCodeEmitterBase._GetExceptionMessage: Integer;
begin
  Result := _op(ocGetExceptionMessage);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetExceptionLocation: Integer;
begin
  Result := _op(ocGetExceptionLocation);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetCallerAddress: Integer;
begin
  Result := _op(ocGetCallerAddress);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetScriptMethodName: Integer;
begin
  Result := _op(ocGetScriptMethodName);
  DecStack(SizeOf(Pointer) - SizeOf(ShortString));
end;

function TLapeCodeEmitterBase._DumpCallStack: Integer;
begin
  Result := _op(ocDumpCallStack);
  DecStack(SizeOf(Int32) + SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._ReRaiseException: Integer;
begin
  Result := _op(ocReRaiseException);
end;

function TLapeCodeEmitterBase._Unused: Integer;
begin
  Result := _op(ocUnused);
  _StackOffset(0);
end;

function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset): Integer;
begin
  Result := _op(ocInitStackLen);
  _StackOffset(Len);
  IncStack(Integer(Len) - FStackPos);
end;

function TLapeCodeEmitterBase._InitStack(Len: TStackOffset): Integer;
begin
  Result := _op(ocInitStack);
  _StackOffset(Len);
end;

function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset): Integer;
begin
  Result := _op(ocGrowStack);
  _StackOffset(Len);
  IncStack(Len);
end;

function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset): Integer;
begin
  Result := _op(ocExpandVar);
  _StackOffset(Len);
end;

function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset): Integer;
begin
  Result := _op(ocExpandVarAndInit);
  _StackOffset(Len);
end;

function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset): Integer;
begin
  Result := _op(ocGrowVar);
  _StackOffset(Len);
end;

function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset): Integer;
begin
  Result := _op(ocGrowVarAndInit);
  _StackOffset(Len);
end;

function TLapeCodeEmitterBase._PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocPopStackToVar);
  Reserve(SizeOf(TOC_PopStackToVar));
  with POC_PopStackToVar(@FCode[Result + ocSize])^ do
  begin
    _StackOffset(ASize, @Size);
    _VarStackOffset(VarStackOffset, @VOffset);
  end;
  DecStack(ASize);
end;

function TLapeCodeEmitterBase._PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocPopVarToStack);
  Reserve(SizeOf(TOC_PopStackToVar));
  with POC_PopStackToVar(@FCode[Result + ocSize])^ do
  begin
    _StackOffset(ASize, @Size);
    _VarStackOffset(VarStackOffset, @VOffset);
  end;
  IncStack(ASize);
end;

function TLapeCodeEmitterBase._PopVar: Integer;
begin
  Result := _op(ocPopVar);
end;

function TLapeCodeEmitterBase._JmpVar: Integer;
begin
  Result := _op(ocJmpVar);
end;

function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos): Integer;
begin
  Result := _op(ocJmpSafe);
  _CodePos(Target);
end;

function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpSafeR);
  _CodeOffset(Jmp);
end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32): Integer;
begin
  Result := _op(ocIncTry);
  Reserve(SizeOf(TOC_IncTry));
  with POC_IncTry(@FCode[Result + ocSize])^ do
  begin
    _CodeOffset(AJmp, @Jmp);
    _UInt32(AJmpFinally, @JmpFinally);
  end;
end;

function TLapeCodeEmitterBase._DecTry: Integer;
begin
  Result := _op(ocDecTry);
end;

function TLapeCodeEmitterBase._EndTry: Integer;
begin
  Result := _op(ocEndTry);
end;

function TLapeCodeEmitterBase._CatchException: Integer;
begin
  Result := _op(ocCatchException);
end;

function TLapeCodeEmitterBase._DecCall: Integer;
begin
  Result := _op(ocDecCall);
end;

function TLapeCodeEmitterBase._DecCall_EndTry: Integer;
begin
  Result := _op(ocDecCall_EndTry);
end;




{$I lpcodeemitter_invokebody.inc}
{$I lpcodeemitter_jumpbody.inc}
{$I lpcodeemitter_evalbody.inc}

end.

