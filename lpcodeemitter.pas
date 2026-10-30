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
  public
    FullEmit: Boolean;

    constructor Create; override;
    procedure Reset; virtual;

    // Where S is in the string table (each string once)
    function addString(const S: lpString): Integer; virtual;
    // The script method Name starts at CodeOffset
    procedure addMethod(CodeOffset: Integer; const Name: lpString); virtual;
    // When the program is done: the string, DocPos and method tables after it, then the trailer
    procedure WriteTables; virtual;

    procedure EnsureCodeGrowth(Len: Integer); virtual;
    function CheckOffset(var Offset: Integer; Len: Word = 0): Integer; overload; virtual;
    function CheckOffset(Len: Word = 0): Integer; overload; virtual;

    function NewStack(Size: Integer = 0; Max: Integer = 0): Integer; virtual;

    // Where what is emitted now is from (nil: nowhere); the node compiling sets it
    property CurrentPos: PDocPos read FCurrentPos write FCurrentPos;

    function _Int8(v: Int8; var Offset: Integer): Integer; overload; virtual;
    function _UInt8(v: UInt8; var Offset: Integer): Integer; overload; virtual;
    function _Int16(v: Int16; var Offset: Integer): Integer; overload; virtual;
    function _UInt16(v: UInt16; var Offset: Integer): Integer; overload; virtual;
    function _Int32(v: Int32; var Offset: Integer): Integer; overload; virtual;
    function _UInt32(v: UInt32; var Offset: Integer): Integer; overload; virtual;
    function _Int64(v: Int64; var Offset: Integer): Integer; overload; virtual;
    function _UInt64(v: UInt64; var Offset: Integer): Integer; overload; virtual;
    function _Pointer(v: Pointer; var Offset: Integer): Integer; overload; virtual;

    function _Opcode(v: opCode; var Offset: Integer): Integer; overload; virtual;
    function _CodePos(v: TCodePos; var Offset: Integer): Integer; overload; virtual;
    function _CodeOffset(v: TCodeOffset; var Offset: Integer): Integer; overload; virtual;

    function _EvalProc(v: TLapeEvalProc; var Offset: Integer): Integer; overload; virtual;
    function _StackInc(v: TStackInc; var Offset: Integer): Integer; overload; virtual;
    function _StackOffset(v: TStackOffset; var Offset: Integer): Integer; overload; virtual;
    function _VarStackOffset(v: TVarStackOffset; var Offset: Integer): Integer; overload; virtual;
    function _PointerOffset(v: TPointerOffset; var Offset: Integer): Integer; overload; virtual;
    function _ParamSize(v: TParamSize; var Offset: Integer): Integer; overload; virtual;

    function _op(op: opCode; var Offset: Integer): Integer; overload;
    function _op(op: opCode): Integer; overload;

    function _IsScriptMethod(var Offset: Integer): Integer; overload;
    function _IsScriptMethod: Integer; overload;
    function _GetExceptionMessage(var Offset: Integer): Integer; overload;
    function _GetExceptionMessage: Integer; overload;
    function _GetExceptionLocation(var Offset: Integer): Integer; overload;
    function _GetExceptionLocation: Integer; overload;
    function _GetCallerAddress(var Offset: Integer): Integer; overload;
    function _GetCallerAddress: Integer; overload;
    function _GetScriptMethodName(var Offset: Integer): Integer; overload;
    function _GetScriptMethodName: Integer; overload;
    function _DumpCallStack(var Offset: Integer): Integer; overload;
    function _DumpCallStack: Integer; overload;
    function _ReRaiseException(var Offset: Integer): Integer; overload;
    function _ReRaiseException: Integer; overload;
    function _Unused(var Offset: Integer): Integer;
    function _InitStackLen(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _InitStackLen(Len: TStackOffset): Integer; overload;
    function _InitStack(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _InitStack(Len: TStackOffset): Integer; overload;
    function _GrowStack(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _GrowStack(Len: TStackOffset): Integer; overload;
    function _ExpandVar(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _ExpandVar(Len: TStackOffset): Integer; overload;
    function _ExpandVarAndInit(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _ExpandVarAndInit(Len: TStackOffset): Integer; overload;
    function _GrowVar(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _GrowVar(Len: TStackOffset): Integer; overload;
    function _GrowVarAndInit(Len: TStackOffset; var Offset: Integer): Integer; overload;
    function _GrowVarAndInit(Len: TStackOffset): Integer; overload;
    function _PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer): Integer; overload;
    function _PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer; overload;
    function _PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer): Integer; overload;
    function _PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer; overload;
    function _PopVar(var Offset: Integer): Integer; overload;
    function _PopVar: Integer; overload;
    function _JmpVar(var Offset: Integer): Integer; overload;
    function _JmpVar: Integer; overload;
    function _JmpSafe(Target: TCodePos; var Offset: Integer): Integer; overload;
    function _JmpSafe(Target: TCodePos): Integer; overload;
    function _JmpSafeR(Jmp: TCodeOffset; var Offset: Integer): Integer; overload;
    function _JmpSafeR(Jmp: TCodeOffset): Integer; overload;

    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; var Offset: Integer): Integer; overload;
    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32): Integer; overload;
    function _DecTry(var Offset: Integer): Integer; overload;
    function _DecTry: Integer; overload;
    function _EndTry(var Offset: Integer): Integer; overload;
    function _EndTry: Integer; overload;
    function _CatchException(var Offset: Integer): Integer; overload;
    function _CatchException: Integer; overload;

    function _DecCall(var Offset: Integer): Integer; overload;
    function _DecCall: Integer; overload;
    function _DecCall_EndTry(var Offset: Integer): Integer; overload;
    function _DecCall_EndTry: Integer; overload;

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
  if (not FullEmit) then
    Exit;

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

// (an instruction added at the end; one from the same place as the one before it needs no entry)
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

// (where it is: the lengths of the ones before it, each with its terminating 0)
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
  // (every file name in the string list first: the DocPos entries point to them; mostly the one before)
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

  FullEmit := True;
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

function TLapeCodeEmitterBase.CheckOffset(var Offset: Integer; Len: Word = 0): Integer;
begin
  if (Offset < 0) or (Offset + Len > FCodeCur) then
  begin
    Offset := FCodeCur;
    EnsureCodeGrowth(Len);
    Inc(FCodeCur, Len);
  end;

  Result := Offset;
end;

function TLapeCodeEmitterBase.NewStack(Size: Integer = 0; Max: Integer = 0): Integer;
begin
  Result := FStackPos;
  FStackPos := Size;
  FMaxStack := Max;
end;

function TLapeCodeEmitterBase._Int8(v: Int8; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int8));
  _Int8(v, @FCode[Offset]);
  Inc(Offset, SizeOf(Int8));
end;

function TLapeCodeEmitterBase._UInt8(v: UInt8; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt8));
  _UInt8(v, @FCode[Offset]);
  Inc(Offset, SizeOf(UInt8));
end;

function TLapeCodeEmitterBase._Int16(v: Int16; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int16));
  _Int16(v, @FCode[Offset]);
  Inc(Offset, SizeOf(Int16));
end;

function TLapeCodeEmitterBase._UInt16(v: UInt16; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt16));
  _UInt16(v, @FCode[Offset]);
  Inc(Offset, SizeOf(UInt16));
end;

function TLapeCodeEmitterBase._Int32(v: Int32; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int32));
  _Int32(v, @FCode[Offset]);
  Inc(Offset, SizeOf(Int32));
end;

function TLapeCodeEmitterBase._UInt32(v: UInt32; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt32));
  _UInt32(v, @FCode[Offset]);
  Inc(Offset, SizeOf(UInt32));
end;

function TLapeCodeEmitterBase._Int64(v: Int64; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Int64));
  _Int64(v, @FCode[Offset]);
  Inc(Offset, SizeOf(Int64));
end;

function TLapeCodeEmitterBase._UInt64(v: UInt64; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(UInt64));
  _UInt64(v, @FCode[Offset]);
  Inc(Offset, SizeOf(UInt64));
end;

function TLapeCodeEmitterBase._Pointer(v: Pointer; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(Pointer));
  _Pointer(v, @FCode[Offset]);
  Inc(Offset, SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._Opcode(v: opCode; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(opCode));
  _Opcode(v, @FCode[Offset]);
  Inc(Offset, SizeOf(opCode));
end;

function TLapeCodeEmitterBase._CodePos(v: TCodePos; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TCodePos));
  _CodePos(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TCodePos));
end;

function TLapeCodeEmitterBase._CodeOffset(v: TCodeOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TCodeOffset));
  _CodeOffset(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TCodeOffset));
end;

function TLapeCodeEmitterBase._EvalProc(v: TLapeEvalProc; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TLapeEvalProc));
  _EvalProc(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TLapeEvalProc));
end;

function TLapeCodeEmitterBase._StackInc(v: TStackInc; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TStackInc));
  _StackInc(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TStackInc));
end;

function TLapeCodeEmitterBase._StackOffset(v: TStackOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TStackOffset));
  _StackOffset(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TStackOffset));
end;

function TLapeCodeEmitterBase._VarStackOffset(v: TVarStackOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TVarStackOffset));
  _VarStackOffset(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TVarStackOffset));
end;

function TLapeCodeEmitterBase._PointerOffset(v: TPointerOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TVarStackOffset));
  _PointerOffset(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TPointerOffset));
end;

function TLapeCodeEmitterBase._ParamSize(v: TParamSize; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TParamSize));
  _ParamSize(v, @FCode[Offset]);
  Inc(Offset, SizeOf(TParamSize));
end;

function TLapeCodeEmitterBase._op(op: opCode; var Offset: Integer): Integer;
{$IFDEF Lape_EmitPos}
var
  CodeEnd: Integer;
{$ENDIF}
begin
  {$IFDEF Lape_EmitPos}
  CodeEnd := FCodeCur;
  {$ENDIF}
  Result := _Opcode(op, Offset);
  {$IFDEF Lape_EmitPos}
  // (written again in place, an instruction stays where it was first written)
  if (Result >= CodeEnd) then
    addDocPos(Result);
  {$ENDIF}
end;

function TLapeCodeEmitterBase._IsScriptMethod(var Offset: Integer): Integer;
begin
  Result := _op(ocIsScriptMethod, Offset);
  DecStack(SizeOf(Pointer) - SizeOf(EvalBool));
end;

function TLapeCodeEmitterBase._GetExceptionMessage(var Offset: Integer): Integer;
begin
  Result := _op(ocGetExceptionMessage, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetExceptionLocation(var Offset: Integer): Integer;
begin
  Result := _op(ocGetExceptionLocation, Offset);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetCallerAddress(var Offset: Integer): Integer;
begin
  Result := _op(ocGetCallerAddress, Offset);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetScriptMethodName(var Offset: Integer): Integer;
begin
  Result := _op(ocGetScriptMethodName, Offset);
  DecStack(SizeOf(Pointer) - SizeOf(ShortString));
end;

function TLapeCodeEmitterBase._DumpCallStack(var Offset: Integer): Integer;
begin
  Result := _op(ocDumpCallStack, Offset);
  DecStack(SizeOf(Int32) + SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._ReRaiseException(var Offset: Integer): Integer;
begin
  Result := _op(ocReRaiseException, Offset);
end;

function TLapeCodeEmitterBase._Unused(var Offset: Integer): Integer;
begin
  Result := _op(ocUnused, Offset);
  _StackOffset(0, Offset);
end;

function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocInitStackLen, Offset);
  _StackOffset(Len, Offset);
  IncStack(Integer(Len) - FStackPos);
end;

function TLapeCodeEmitterBase._InitStack(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocInitStack, Offset);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocGrowStack, Offset);
  _StackOffset(Len, Offset);
  IncStack(Len);
end;

function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocExpandVar, Offset);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocExpandVarAndInit, Offset);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocGrowVar, Offset);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocGrowVarAndInit, Offset);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocPopStackToVar, Offset);
  CheckOffset(Offset, SizeOf(TOC_PopStackToVar));
  with POC_PopStackToVar(@FCode[Offset])^ do
  begin
    _StackOffset(ASize, @Size);
    _VarStackOffset(VarStackOffset, @VOffset);
  end;
  DecStack(ASize);
  Inc(Offset, SizeOf(TOC_PopStackToVar));
end;

function TLapeCodeEmitterBase._PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocPopVarToStack, Offset);
  CheckOffset(Offset, SizeOf(TOC_PopStackToVar));
  with POC_PopStackToVar(@FCode[Offset])^ do
  begin
    _StackOffset(ASize, @Size);
    _VarStackOffset(VarStackOffset, @VOffset);
  end;
  IncStack(ASize);
  Inc(Offset, SizeOf(TOC_PopStackToVar));
end;

function TLapeCodeEmitterBase._PopVar(var Offset: Integer): Integer;
begin
  Result := _op(ocPopVar, Offset);
end;

function TLapeCodeEmitterBase._JmpVar(var Offset: Integer): Integer;
begin
  Result := _op(ocJmpVar, Offset);
end;

function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos; var Offset: Integer): Integer;
begin
  Result := _op(ocJmpSafe, Offset);
  _CodePos(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset; var Offset: Integer): Integer;
begin
  Result := _op(ocJmpSafeR, Offset);
  _CodeOffset(Jmp, Offset);
end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; var Offset: Integer): Integer;
begin
  Result := _op(ocIncTry, Offset);
  CheckOffset(Offset, SizeOf(TOC_IncTry));
  with POC_IncTry(@FCode[Offset])^ do
  begin
    _CodeOffset(AJmp, @Jmp);
    _UInt32(AJmpFinally, @JmpFinally);
  end;
  Inc(Offset, SizeOf(TOC_IncTry));
end;

function TLapeCodeEmitterBase._DecTry(var Offset: Integer): Integer;
begin
  Result := _op(ocDecTry, Offset);
end;

function TLapeCodeEmitterBase._EndTry(var Offset: Integer): Integer;
begin
  Result := _op(ocEndTry, Offset);
end;

function TLapeCodeEmitterBase._CatchException(var Offset: Integer): Integer;
begin
  Result := _op(ocCatchException, Offset);
end;

function TLapeCodeEmitterBase._DecCall(var Offset: Integer): Integer;
begin
  Result := _op(ocDecCall, Offset);
end;

function TLapeCodeEmitterBase._DecCall_EndTry(var Offset: Integer): Integer;
begin
  Result := _op(ocDecCall_EndTry, Offset);
end;

function TLapeCodeEmitterBase.CheckOffset(Len: Word = 0): Integer;
  var o: Integer; begin o := -1; Result := CheckOffset(o, Len); end;
function TLapeCodeEmitterBase._op(op: opCode): Integer;
  var o: Integer; begin o := -1; Result := _op(op, o); end;
function TLapeCodeEmitterBase._IsScriptMethod: Integer;
  var o: Integer; begin o := -1; Result := _IsScriptMethod(o); end;
function TLapeCodeEmitterBase._GetExceptionMessage: Integer;
  var o: Integer; begin o := -1; Result := _GetExceptionMessage(o); end;
function TLapeCodeEmitterBase._GetExceptionLocation: Integer;
  var o: Integer; begin o := -1; Result := _GetExceptionLocation(o); end;
function TLapeCodeEmitterBase._GetCallerAddress: Integer;
  var o: Integer; begin o := -1; Result := _GetCallerAddress(o); end;
function TLapeCodeEmitterBase._GetScriptMethodName: Integer;
  var o: Integer; begin o := -1; Result := _GetScriptMethodName(o); end;
function TLapeCodeEmitterBase._DumpCallStack: Integer;
  var o: Integer; begin o := -1; Result := _DumpCallStack(o); end;
function TLapeCodeEmitterBase._ReRaiseException: Integer;
  var o: Integer; begin o := -1; Result := _ReRaiseException(o); end;
function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _InitStackLen(Len, o); end;
function TLapeCodeEmitterBase._InitStack(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _InitStack(Len, o); end;
function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _GrowStack(Len, o); end;
function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVar(Len, o); end;
function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVarAndInit(Len, o); end;
function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _GrowVar(Len, o); end;
function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _GrowVarAndInit(Len, o); end;
function TLapeCodeEmitterBase._PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _PopStackToVar(ASize, VarStackOffset, o); end;
function TLapeCodeEmitterBase._PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset): Integer;
  var o: Integer; begin o := -1; Result := _PopVarToStack(ASize, VarStackOffset, o); end;
function TLapeCodeEmitterBase._PopVar: Integer;
  var o: Integer; begin o := -1; Result := _PopVar(o); end;
function TLapeCodeEmitterBase._JmpVar: Integer;
  var o: Integer; begin o := -1; Result := _JmpVar(o); end;
function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos): Integer;
  var o: Integer; begin o := -1; Result := _JmpSafe(Target, o); end;
function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset): Integer;
  var o: Integer; begin o := -1; Result := _JmpSafeR(Jmp, o); end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32): Integer;
  var o: Integer; begin o := -1; Result := _IncTry(AJmp, AJmpFinally, o); end;
function TLapeCodeEmitterBase._DecTry: Integer;
  var o: Integer; begin o := -1; Result := _DecTry(o); end;
function TLapeCodeEmitterBase._EndTry: Integer;
  var o: Integer; begin o := -1; Result := _EndTry(o); end;
function TLapeCodeEmitterBase._CatchException: Integer;
  var o: Integer; begin o := -1; Result := _CatchException(o); end;

function TLapeCodeEmitterBase._DecCall: Integer;
  var o: Integer; begin o := -1; Result := _DecCall(o); end;
function TLapeCodeEmitterBase._DecCall_EndTry: Integer;
  var o: Integer; begin o := -1; Result := _DecCall_EndTry(o); end;

{$I lpcodeemitter_invokebody.inc}
{$I lpcodeemitter_jumpbody.inc}
{$I lpcodeemitter_evalbody.inc}

end.

