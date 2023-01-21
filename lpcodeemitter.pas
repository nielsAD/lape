{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Bytecode emitter.
}
unit lpcodeemitter;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpinterpreter_types, lpparser;

type
  PLapeEvalProc = ^TLapeEvalProc;

  TLapeCodeEmitterBase = class(TLapeBaseClass)
  protected
  type
    TCodePosList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<PCodePos>;
    TCodePosNameList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<lpString>;
    TDocPosList = {$IFDEF FPC}specialize{$ENDIF} TLapeList<PDocPos>;
  protected
    FCode: TCodeArray;
    FCodePtr: PByte;
    FCodeCur: Integer;
    FCodePointers: TCodePosList;
    FCodePointerNames: TCodePosNameList;
    FDocPosList: TDocPosList;

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

    function GetDocPos(DocPos: TDocPos): PDocPos;
    function GetCodePointerName(CodePos: TCodePos): lpString;
  public
    FullEmit: Boolean;

    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; virtual;

    function addCodePointer(p: PCodePos; Name: lpString = ''): Integer; virtual;
    procedure adjustCodePointers(Pos, Offset: Integer); virtual;

    procedure Delete(StartOffset, Len: Integer); overload; virtual;
    procedure Delete(StartOffset, Len: Integer; var Offset: Integer); overload; virtual;

    procedure EnsureCodeGrowth(Len: Integer); virtual;
    function getCodeOffset(Offset: Integer): Integer; virtual;
    function CheckOffset(var Offset: Integer; Len: Word = 0): Integer; overload; virtual;
    function CheckOffset(Len: Word = 0): Integer; overload; virtual;

    function NewStack(Size: Integer = 0; Max: Integer = 0): Integer; virtual;

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

    function _DocPos(Pos: TDocPos; var Offset: Integer): Integer; overload;
    function _DocPos(Pos: TDocPos): Integer; overload;
    function _DocPos(Pos: PDocPos; var Offset: Integer): Integer; overload;
    function _DocPos(Pos: PDocPos): Integer; overload;

    function _op(op: opCode; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _op(op: opCode; Pos: PDocPos = nil): Integer; overload;

    function _IsInternal(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _IsInternal(Pos: PDocPos = nil): Integer; overload;
    function _GetExceptionMessage(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GetExceptionMessage(Pos: PDocPos = nil): Integer; overload;
    function _GetExceptionLocation(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GetExceptionLocation(Pos: PDocPos = nil): Integer; overload;
    function _GetCallerLocation(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GetCallerLocation(Pos: PDocPos = nil): Integer; overload;
    function _GetCallerAddress(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GetCallerAddress(Pos: PDocPos = nil): Integer; overload;
    function _GetScriptMethodName(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GetScriptMethodName(Pos: PDocPos = nil): Integer; overload;
    function _DumpCallStack(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DumpCallStack(Pos: PDocPos = nil): Integer; overload;
    function _ReRaiseException(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _ReRaiseException(Pos: PDocPos = nil): Integer; overload;
    function _InitStackLen(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InitStackLen(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _InitStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _InitStack(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _GrowStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GrowStack(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVar(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _ExpandVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _GrowVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GrowVar(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _GrowVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _GrowVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
    function _PopVar(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _PopVar(Pos: PDocPos = nil): Integer; overload;
    function _JmpVar(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _JmpVar(Pos: PDocPos = nil): Integer; overload;
    function _JmpSafe(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafe(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafeR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _JmpSafeR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;

    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; Pos: PDocPos = nil): Integer; overload;
    function _DecTry(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DecTry(Pos: PDocPos = nil): Integer; overload;
    function _EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _EndTry(Pos: PDocPos = nil): Integer; overload;
    function _CatchException(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _CatchException(Pos: PDocPos = nil): Integer; overload;

    function _DecCall(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DecCall(Pos: PDocPos = nil): Integer; overload;
    function _DecCall_EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
    function _DecCall_EndTry(Pos: PDocPos = nil): Integer; overload;

    {$I lpcodeemitter_invokeheader.inc}
    {$I lpcodeemitter_jumpheader.inc}
    {$I lpcodeemitter_evalheader.inc}

    property Code: PByte read FCodePtr;
    property CodeLen: Integer read FCodeCur;
    property MaxStack: Integer read FMaxStack;

    property CodePointerName[Pos: TCodePos]: lpString read GetCodePointerName;
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

function TLapeCodeEmitterBase.GetDocPos(DocPos: TDocPos): PDocPos;
begin
  New(Result);
  Result^ := DocPos;

  FDocPosList.Add(Result);
end;

function TLapeCodeEmitterBase.GetCodePointerName(CodePos: TCodePos): lpString;
var
  i: Integer;
begin
  Assert(FCodePointers.Count = FCodePointerNames.Count);

  for i := 0 to FCodePointers.Count - 1 do
    if (FCodePointers[i]^ = CodePos) then
    begin
      Result := FCodePointerNames[i];
      Exit;
    end;

  Result := '';
end;

constructor TLapeCodeEmitterBase.Create;
begin
  inherited;

  FCodePointers := TCodePosList.Create(nil, dupAccept, False);
  FCodePointerNames := TCodePosNameList.Create('', dupAccept, False);
  FDocPosList := TDocPosList.Create(nil, dupAccept, False);

  FullEmit := True;
  Reset();
end;

destructor TLapeCodeEmitterBase.Destroy;
begin
  Reset();

  FCodePointers.Free();
  FCodePointerNames.Free();
  FDocPosList.Free();

  inherited;
end;

procedure TLapeCodeEmitterBase.Reset;
var
  i: Integer;
begin
  FCodeCur := 0;
  FCodePointers.Clear();
  FCodePointerNames.Clear();

  for i := 0 to FDocPosList.Count - 1 do
    if (FDocPosList[i] <> nil) then
      Dispose(FDocPosList[I]);
  FDocPosList.Clear();
  
  EnsureCodeGrowth(0);
  NewStack();
end;

function TLapeCodeEmitterBase.addCodePointer(p: PCodePos; Name: lpString): Integer;
begin
  if (p <> nil) then
  begin
    Result := FCodePointers.Add(p);
              FCodePointerNames.Add(Name);
  end else
    Result := -1;
end;

procedure TLapeCodeEmitterBase.adjustCodePointers(Pos, Offset: Integer);
var
  i: Integer;
begin
  for i := 0 to FCodePointers.Count - 1 do
    if (Integer(FCodePointers[i]^) > Pos) then
      Inc(FCodePointers[i]^, Offset);
end;

procedure TLapeCodeEmitterBase.Delete(StartOffset, Len: Integer);
var
  i: Integer;
begin
  if (Len <= 0) or (StartOffset < 0) or (StartOffset + Len > FCodeCur) then
    Exit;
  for i := StartOffset + Len to FCodeCur - 1 do
    FCode[i - Len] := FCode[i];

  Dec(FCodeCur, Len);
  adjustCodePointers(StartOffset, -Len);
end;

procedure TLapeCodeEmitterBase.Delete(StartOffset, Len: Integer; var Offset: Integer);
begin
  Delete(StartOffset, Len);
  if (Offset > StartOffset) then
    Dec(Offset, Len);
end;

procedure TLapeCodeEmitterBase.EnsureCodeGrowth(Len: Integer);
begin
  if (FCodeCur + Len >= Length(FCode)) then
  begin
    if (Length(FCode) = 0) then
      SetLength(FCode, 32768)
    else
      SetLength(FCode, Length(FCode) * 2);

    FCodePtr := @FCode[0];
  end;
end;

function TLapeCodeEmitterBase.getCodeOffset(Offset: Integer): Integer;
begin
  if (Offset < 0) then
    Result := 0
  else
    Result := Offset;
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

function TLapeCodeEmitterBase._DocPos(Pos: TDocPos; var Offset: Integer): Integer;
begin
{$IFDEF Lape_EmitPos}
  Result := _Pointer(GetDocPos(Pos), Offset)
{$ENDIF}
end;

function TLapeCodeEmitterBase._DocPos(Pos: PDocPos; var Offset: Integer): Integer;
begin
  if (Pos = nil) then
    Result := _DocPos(NullDocPos, Offset)
  else
    Result := _DocPos(Pos^, Offset);
end;

function TLapeCodeEmitterBase._op(op: opCode; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _Opcode(op, Offset);
  _DocPos(Pos, Offset);
end;

function TLapeCodeEmitterBase._IsInternal(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIsInternal, Offset, Pos);
  DecStack(SizeOf(Pointer) - SizeOf(EvalBool));
end;

function TLapeCodeEmitterBase._GetExceptionMessage(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGetExceptionMessage, Offset, Pos);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetExceptionLocation(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGetExceptionLocation, Offset, Pos);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetCallerLocation(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGetCallerLocation, Offset, Pos);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetCallerAddress(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGetCallerAddress, Offset, Pos);
  IncStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._GetScriptMethodName(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGetScriptMethodName, Offset, Pos);
  DecStack(SizeOf(Pointer) - SizeOf(ShortString));
end;

function TLapeCodeEmitterBase._DumpCallStack(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDumpCallStack, Offset, Pos);
  DecStack(SizeOf(Int32) + SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._ReRaiseException(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocReRaiseException, Offset, Pos);
end;

function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInitStackLen, Offset, Pos);
  _StackOffset(Len, Offset);
  IncStack(Integer(Len) - FStackPos);
end;

function TLapeCodeEmitterBase._InitStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocInitStack, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGrowStack, Offset, Pos);
  _StackOffset(Len, Offset);
  IncStack(Len);
end;

function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocExpandVar, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocExpandVarAndInit, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGrowVar, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocGrowVarAndInit, Offset, Pos);
  _StackOffset(Len, Offset);
end;

function TLapeCodeEmitterBase._PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocPopStackToVar, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_PopStackToVar));
  with POC_PopStackToVar(@FCode[Offset])^ do
  begin
    _StackOffset(ASize, @Size);
    _VarStackOffset(VarStackOffset, @VOffset);
  end;
  DecStack(ASize);
  Inc(Offset, SizeOf(TOC_PopStackToVar));
end;

function TLapeCodeEmitterBase._PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocPopVarToStack, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_PopStackToVar));
  with POC_PopStackToVar(@FCode[Offset])^ do
  begin
    _StackOffset(ASize, @Size);
    _VarStackOffset(VarStackOffset, @VOffset);
  end;
  IncStack(ASize);
  Inc(Offset, SizeOf(TOC_PopStackToVar));
end;

function TLapeCodeEmitterBase._PopVar(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocPopVar, Offset, Pos);
end;

function TLapeCodeEmitterBase._JmpVar(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpVar, Offset, Pos);
end;

function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpSafe, Offset, Pos);
  _CodePos(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpSafeR, Offset, Pos);
  _CodeOffset(Jmp, Offset);
end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocIncTry, Offset, Pos);
  CheckOffset(Offset, SizeOf(TOC_IncTry));
  with POC_IncTry(@FCode[Offset])^ do
  begin
    _CodeOffset(AJmp, @Jmp);
    _UInt32(AJmpFinally, @JmpFinally);
  end;
  Inc(Offset, SizeOf(TOC_IncTry));
end;

function TLapeCodeEmitterBase._DecTry(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDecTry, Offset, Pos);
end;

function TLapeCodeEmitterBase._EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocEndTry, Offset, Pos);
end;

function TLapeCodeEmitterBase._CatchException(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocCatchException, Offset, Pos);
end;

function TLapeCodeEmitterBase._DecCall(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDecCall, Offset, Pos);
end;

function TLapeCodeEmitterBase._DecCall_EndTry(var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocDecCall_EndTry, Offset, Pos);
end;

function TLapeCodeEmitterBase.CheckOffset(Len: Word = 0): Integer;
  var o: Integer; begin o := -1; Result := CheckOffset(o, Len); end;
function TLapeCodeEmitterBase._DocPos(Pos: TDocPos): Integer;
  var o: Integer; begin o := -1; Result := _DocPos(Pos, o); end;
function TLapeCodeEmitterBase._DocPos(Pos: PDocPos): Integer;
  var o: Integer; begin o := -1; Result := _DocPos(Pos, o); end;
function TLapeCodeEmitterBase._op(op: opCode; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _op(op, o, Pos); end;
function TLapeCodeEmitterBase._IsInternal(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _IsInternal(o, Pos); end;
function TLapeCodeEmitterBase._GetExceptionMessage(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GetExceptionMessage(o, Pos); end;
function TLapeCodeEmitterBase._GetExceptionLocation(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GetExceptionLocation(o, Pos); end;
function TLapeCodeEmitterBase._GetCallerLocation(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GetCallerLocation(o, Pos); end;
function TLapeCodeEmitterBase._GetCallerAddress(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GetCallerAddress(o, Pos); end;
function TLapeCodeEmitterBase._GetScriptMethodName(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GetScriptMethodName(o, Pos); end;
function TLapeCodeEmitterBase._DumpCallStack(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DumpCallStack(o, Pos); end;
function TLapeCodeEmitterBase._ReRaiseException(Pos: PDocPos): Integer;
  var o: Integer; begin o := -1; Result := _ReRaiseException(o, Pos); end;

function TLapeCodeEmitterBase._InitStackLen(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InitStackLen(Len, o, Pos); end;
function TLapeCodeEmitterBase._InitStack(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _InitStack(Len, o, Pos); end;
function TLapeCodeEmitterBase._GrowStack(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GrowStack(Len, o, Pos); end;
function TLapeCodeEmitterBase._ExpandVar(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVar(Len, o, Pos); end;
function TLapeCodeEmitterBase._ExpandVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVarAndInit(Len, o, Pos); end;
function TLapeCodeEmitterBase._GrowVar(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _ExpandVar(Len, o, Pos); end;
function TLapeCodeEmitterBase._GrowVarAndInit(Len: TStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _GrowVarAndInit(Len, o, Pos); end;
function TLapeCodeEmitterBase._PopStackToVar(ASize: TStackOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _PopStackToVar(ASize, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._PopVarToStack(ASize: TStackOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _PopVarToStack(ASize, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._PopVar(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _PopVar(o, Pos); end;
function TLapeCodeEmitterBase._JmpVar(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpVar(o, Pos); end;
function TLapeCodeEmitterBase._JmpSafe(Target: TCodePos; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpSafe(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpSafeR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpSafeR(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._IncTry(AJmp: TCodeOffset; AJmpFinally: UInt32; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _IncTry(AJmp, AJmpFinally, o, Pos); end;
function TLapeCodeEmitterBase._DecTry(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DecTry(o, Pos); end;
function TLapeCodeEmitterBase._EndTry(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _EndTry(o, Pos); end;
function TLapeCodeEmitterBase._CatchException(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _CatchException(o, Pos); end;

function TLapeCodeEmitterBase._DecCall(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DecCall(o, Pos); end;
function TLapeCodeEmitterBase._DecCall_EndTry(Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _DecCall_EndTry(o, Pos); end;

{$I lpcodeemitter_invokebody.inc}
{$I lpcodeemitter_jumpbody.inc}
{$I lpcodeemitter_evalbody.inc}

end.

