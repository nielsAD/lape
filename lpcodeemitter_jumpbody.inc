{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Jump bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function TLapeCodeEmitterBase._Jmp(Target: TCodePos): Integer;
begin
  Result := _op(ocJmp);
  _CodePos(Target);
end;

function TLapeCodeEmitterBase._JmpR(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpR);
  _CodeOffset(Jmp);
end;

function TLapeCodeEmitterBase._JmpR_Var(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset): Integer;
begin
  Result := Reserve(SizeOf(TOC_JmpR_Var));
  with POC_JmpR_Var(@FCode[Result])^ do begin
    _CodeOffset(Jmp, @Offset);
    _VarStackOffset(AVarStackOffset, @VarStackOffset);
  end;
end;

function TLapeCodeEmitterBase._JmpR_Ptr(Jmp: TCodeOffset; APtr: Pointer): Integer;
begin
  Result := Reserve(SizeOf(TOC_JmpR_Ptr));
  with POC_JmpR_Ptr(@FCode[Result])^ do begin
    _CodeOffset(Jmp, @Offset);
    _Pointer(APtr, @Ptr);
  end;
end;

function TLapeCodeEmitterBase._JmpR_PVar(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset): Integer;
begin
  Result := Reserve(SizeOf(TOC_JmpR_PVar));
  with POC_JmpR_PVar(@FCode[Result])^ do begin
    _CodeOffset(Jmp, @Offset);
    _VarStackOffset(AVarStackOffset, @VarStackOffset);
    _PointerOffset(APointerOffset, @PointerOffset);
  end;
end;

//jump relative if
function TLapeCodeEmitterBase._JmpRIf8_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf8_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt8));
end;

function TLapeCodeEmitterBase._JmpRIf16_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf16_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt16));
end;

function TLapeCodeEmitterBase._JmpRIf32_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf32_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt32));
end;

function TLapeCodeEmitterBase._JmpRIf64_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf64_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt64));
end;

function TLapeCodeEmitterBase._JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIf8_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIf16_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIf32_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIf64_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIf8_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIf16_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIf32_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIf64_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIf8_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf8_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf16_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf16_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf32_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf32_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf64_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIf64_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIf8_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

function TLapeCodeEmitterBase._JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIf16_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

function TLapeCodeEmitterBase._JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIf32_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

function TLapeCodeEmitterBase._JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIf64_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

//jump relative if not
function TLapeCodeEmitterBase._JmpRIfNot8_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot8_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt8));
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot16_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt16));
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot32_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt32));
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Stk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot64_Stk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(UInt64));
end;

function TLapeCodeEmitterBase._JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIfNot8_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIfNot16_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIfNot32_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
begin
  Result := _op(ocJmpRIfNot64_Var);
  _JmpR_Var(Jmp, VarStackOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIfNot8_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIfNot16_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIfNot32_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
begin
  Result := _op(ocJmpRIfNot64_Ptr);
  _JmpR_Ptr(Jmp, Ptr);
end;

function TLapeCodeEmitterBase._JmpRIfNot8_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot8_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot16_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot16_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot32_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot32_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot64_PStk(Jmp: TCodeOffset): Integer;
begin
  Result := _op(ocJmpRIfNot64_PStk);
  _CodeOffset(Jmp);
  DecStack(SizeOf(Pointer));
end;

function TLapeCodeEmitterBase._JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIfNot8_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIfNot16_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIfNot32_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;

function TLapeCodeEmitterBase._JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
begin
  Result := _op(ocJmpRIfNot64_PVar);
  _JmpR_PVar(Jmp, VarStackOffset, PointerOffset);
end;


//jump relative if





//jump relative if not




