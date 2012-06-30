{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Jump bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function TLapeCodeEmitterBase._Jmp(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmp, Offset, Pos);
  _CodePos(Target, Offset);
end;

function TLapeCodeEmitterBase._JmpR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpR, Offset, Pos);
  _CodeOffset(Jmp, Offset);
end;

function TLapeCodeEmitterBase._JmpR_Var(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_JmpR_Var));
  with POC_JmpR_Var(@FCode[Offset])^ do begin
    _CodeOffset(Jmp, @Offset);
    _VarStackOffset(AVarStackOffset, @VarStackOffset);
  end;
  Inc(Offset, SizeOf(TOC_JmpR_Var));
end;

function TLapeCodeEmitterBase._JmpR_Ptr(Jmp: TCodeOffset; APtr: Pointer; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_JmpR_Ptr));
  with POC_JmpR_Ptr(@FCode[Offset])^ do begin
    _CodeOffset(Jmp, @Offset);
    _Pointer(APtr, @Ptr);
  end;
  Inc(Offset, SizeOf(TOC_JmpR_Ptr));
end;

function TLapeCodeEmitterBase._JmpR_PVar(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;
begin
  Result := CheckOffset(Offset, SizeOf(TOC_JmpR_PVar));
  with POC_JmpR_PVar(@FCode[Offset])^ do begin
    _CodeOffset(Jmp, @Offset);
    _VarStackOffset(AVarStackOffset, @VarStackOffset);
    _PointerOffset(APointerOffset, @PointerOffset);
  end;
  Inc(Offset, SizeOf(TOC_JmpR_PVar));
end;

//jump relative if
function TLapeCodeEmitterBase._JmpRIf8_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt8));
end;

function TLapeCodeEmitterBase._JmpRIf16_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt16));
end;

function TLapeCodeEmitterBase._JmpRIf32_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt32));
end;

function TLapeCodeEmitterBase._JmpRIf64_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt64));
end;

function TLapeCodeEmitterBase._JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf8_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf16_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf32_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf64_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf8_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf16_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf32_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIf64_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

//jump relative if not
function TLapeCodeEmitterBase._JmpRIfNot8_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt8));
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt16));
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt32));
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_Stk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(UInt64));
end;

function TLapeCodeEmitterBase._JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_Var, Offset, Pos);
  _JmpR_Var(Jmp, VarStackOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_Ptr, Offset, Pos);
  _JmpR_Ptr(Jmp, Ptr, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot16_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot32_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot64_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_PStk, Offset, Pos);
  _CodeOffset(Jmp, Offset);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot8_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot16_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot32_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer;
begin
  Result := _op(ocJmpRIfNot64_PVar, Offset, Pos);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset, Offset);
end;

function TLapeCodeEmitterBase._Jmp(Target: TCodePos; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _Jmp(Target, o, Pos); end;
function TLapeCodeEmitterBase._JmpR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpR(Jmp, o, Pos); end;

//jump relative if
function TLapeCodeEmitterBase._JmpRIf8_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_Stk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_Var(Jmp, VarStackOffset, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_Ptr(Jmp, Ptr, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_PStk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf8_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf16_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf32_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIf64_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;

//jump relative if not
function TLapeCodeEmitterBase._JmpRIfNot8_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_Stk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_Stk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_Var(Jmp, VarStackOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_Var(Jmp, VarStackOffset, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_Ptr(Jmp, Ptr, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_Ptr(Jmp, Ptr, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_PStk(Jmp, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_PStk(Jmp, o, Pos); end;

function TLapeCodeEmitterBase._JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot8_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot16_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot32_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
function TLapeCodeEmitterBase._JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer;
  var o: Integer; begin o := -1; Result := _JmpRIfNot64_PVar(Jmp, VarStackOffset, PointerOffset, o, Pos); end;
