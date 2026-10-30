{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Invoke bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function TLapeCodeEmitterBase._IncCall_Var(AVOffset: TVarStackOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocIncCall_Var);
  Reserve(SizeOf(TOC_Invoke_Var));
  with POC_Invoke_Var(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffset, @VOffset);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._IncCall_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocIncCall_PVar);
  Reserve(SizeOf(TOC_Invoke_PVar));
  with POC_Invoke_PVar(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffset, @VOffset);
    _PointerOffset(APOffset, @POffset);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._IncCall_Ptr(APtr: Pointer; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocIncCall_Ptr);
  Reserve(SizeOf(TOC_Invoke_Ptr));
  with POC_Invoke_Ptr(@FCode[Result + ocSize])^ do begin
    _Pointer(APtr, @Ptr);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Var(AVOffset: TVarStackOffset; AParamSize: TParamSize): Integer;
begin
  // (the operands of IncCall, its opcode replaced)
  Result := _IncCall_Var(AVOffset, AParamSize);
  opCodeP(@FCode[Result])^ := ocInvokeImported_Var;
end;

function TLapeCodeEmitterBase._InvokeImported_PVar(AVOffset: TVarStackOffset; APOffset: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  // (the operands of IncCall, its opcode replaced)
  Result := _IncCall_PVar(AVOffset, APOffset, AParamSize);
  opCodeP(@FCode[Result])^ := ocInvokeImported_PVar;
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr(APtr: Pointer; AParamSize: TParamSize): Integer;
begin
  // (the operands of IncCall, its opcode replaced)
  Result := _IncCall_Ptr(APtr, AParamSize);
  opCodeP(@FCode[Result])^ := ocInvokeImported_Ptr;
end;

function TLapeCodeEmitterBase._InvokeImported_Var_Stk(AVOffsetS: TVarStackOffset; AParamSize: TParamSize; AStackInc: TStackInc): Integer;
begin
  Result := _op(ocInvokeImported_Var_Stk);
  Reserve(SizeOf(TOC_Invoke_Var_Stk));
  with POC_Invoke_Var_Stk(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _ParamSize(AParamSize, @ParamSize);
    _StackInc(AStackInc, @StackInc);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Var_PStk(AVOffsetS: TVarStackOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Var_PStk);
  Reserve(SizeOf(TOC_Invoke_Var_PStk));
  with POC_Invoke_Var_PStk(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Var_Var(AVOffsetS, AVOffsetD: TVarStackOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Var_Var);
  Reserve(SizeOf(TOC_Invoke_Var_Var));
  with POC_Invoke_Var_Var(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Var_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Var_PVar);
  Reserve(SizeOf(TOC_Invoke_Var_PVar));
  with POC_Invoke_Var_PVar(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APOffset, @POffset);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Var_Ptr(AVOffsetS: TVarStackOffset; APtrD: Pointer; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Var_Ptr);
  Reserve(SizeOf(TOC_Invoke_Var_Ptr));
  with POC_Invoke_Var_Ptr(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _Pointer(APtrD, @PtrD);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;


function TLapeCodeEmitterBase._InvokeImported_PVar_Stk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: TParamSize; AStackInc: TStackInc): Integer;
begin
  Result := _op(ocInvokeImported_PVar_Stk);
  Reserve(SizeOf(TOC_Invoke_PVar_Stk));
  with POC_Invoke_PVar_Stk(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _PointerOffset(APOffsetS, @POffsetS);
    _ParamSize(AParamSize, @ParamSize);
    _StackInc(AStackInc, @StackInc);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_PStk(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_PVar_PStk);
  Reserve(SizeOf(TOC_Invoke_PVar_PStk));
  with POC_Invoke_PVar_PStk(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _PointerOffset(APOffsetS, @POffsetS);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_Var(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_PVar_Var);
  Reserve(SizeOf(TOC_Invoke_PVar_Var));
  with POC_Invoke_PVar_Var(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APOffsetS, @POffsetS);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_PVar(AVOffsetS, AVOffsetD: TVarStackOffset; APOffsetS, APOffsetD: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_PVar_PVar);
  Reserve(SizeOf(TOC_Invoke_PVar_PVar));
  with POC_Invoke_PVar_PVar(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APOffsetS, @POffsetS);
    _PointerOffset(APOffsetD, @POffsetD);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_PVar_Ptr(AVOffsetS: TVarStackOffset; APOffsetS: TPointerOffset; APtrD: Pointer; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_PVar_Ptr);
  Reserve(SizeOf(TOC_Invoke_PVar_Ptr));
  with POC_Invoke_PVar_Ptr(@FCode[Result + ocSize])^ do begin
    _VarStackOffset(AVOffsetS, @VOffsetS);
    _PointerOffset(APOffsetS, @POffsetS);
    _Pointer(APtrD, @PtrD);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;


function TLapeCodeEmitterBase._InvokeImported_Ptr_Stk(APtrS: Pointer; AParamSize: TParamSize; AStackInc: TStackInc): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_Stk);
  Reserve(SizeOf(TOC_Invoke_Ptr_Stk));
  with POC_Invoke_Ptr_Stk(@FCode[Result + ocSize])^ do begin
    _Pointer(APtrS, @PtrS);
    _ParamSize(AParamSize, @ParamSize);
    _StackInc(AStackInc, @StackInc);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_PStk(APtrS: Pointer; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_PStk);
  Reserve(SizeOf(TOC_Invoke_Ptr_PStk));
  with POC_Invoke_Ptr_PStk(@FCode[Result + ocSize])^ do begin
    _Pointer(APtrS, @PtrS);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_Var(APtrS: Pointer; AVOffsetD: TVarStackOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_Var);
  Reserve(SizeOf(TOC_Invoke_Ptr_Var));
  with POC_Invoke_Ptr_Var(@FCode[Result + ocSize])^ do begin
    _Pointer(APtrS, @PtrS);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_PVar(APtrS: Pointer; AVOffsetD: TVarStackOffset; APOffset: TPointerOffset; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_PVar);
  Reserve(SizeOf(TOC_Invoke_Ptr_PVar));
  with POC_Invoke_Ptr_PVar(@FCode[Result + ocSize])^ do begin
    _Pointer(APtrS, @PtrS);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APOffset, @POffset);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;

function TLapeCodeEmitterBase._InvokeImported_Ptr_Ptr(APtrS, APtrD: Pointer; AParamSize: TParamSize): Integer;
begin
  Result := _op(ocInvokeImported_Ptr_Ptr);
  Reserve(SizeOf(TOC_Invoke_Ptr_Ptr));
  with POC_Invoke_Ptr_Ptr(@FCode[Result + ocSize])^ do begin
    _Pointer(APtrS, @PtrS);
    _Pointer(APtrD, @PtrD);
    _ParamSize(AParamSize, @ParamSize);
  end;
end;







