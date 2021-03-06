{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All the invoke related handling functions.
}
{%MainUnit lpinterpreter.pas}

procedure DoIncCall_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk(PtrUInt(Code) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_Stk), PCodePos(@Stack[StackPos - SOffset])^, ParamSize, ParamSize - SOffset);
end;

procedure DoIncCall_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
var
  ParamSize: TParamSize;
begin
  ParamSize := PParamSize(PtrUInt(Code) + ocSize)^;
  DoIncCall(SizeOf(TParamSize), PCodePos(PPointer(@Stack[StackPos - ParamSize - SizeOf(Pointer)])^)^, ParamSize, -SizeOf(Pointer));
end;

procedure DoIncCall_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var(PtrUInt(Code) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_Var), PCodePos(@VarStack[VarStackPos + VOffset])^, ParamSize);
end;

procedure DoIncCall_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar(PtrUInt(Code) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_PVar), PCodePos(PtrInt(PPointer(@VarStack[VarStackPos + VOffset])^) + POffset)^, ParamSize);
end;

procedure DoIncCall_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr(PtrUInt(Code) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_Ptr), PCodePos(Ptr)^, ParamSize);
end;

procedure DoInvokeImported_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_Stk), PPointer(@Stack[StackPos - SOffset])^, ParamSize, ParamSize - SOffset);
end;

procedure DoInvokeImported_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
var
  ParamSize: TParamSize;
begin
  ParamSize := PParamSize(PtrUInt(Code) + ocSize)^;
  DoInvokeImportedProc(SizeOf(TParamSize), PPointer(PPointer(@Stack[StackPos - ParamSize - SizeOf(Pointer)])^)^, ParamSize, -SizeOf(Pointer));
end;

procedure DoInvokeImported_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_Var), PPointer(@VarStack[VarStackPos + VOffset])^, ParamSize);
end;

procedure DoInvokeImported_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_PVar), PPointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffset])^) + POffset)^, ParamSize);
end;

procedure DoInvokeImported_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_Ptr), PPointer(Ptr)^, ParamSize);
end;

procedure DoInvokeImported_Stk_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Stk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_Stk), PPointer(@Stack[StackPos - SOffset])^, @Stack[StackPos - SOffset], ParamSize, ParamSize - SOffset + UInt32(StackInc));
end;

procedure DoInvokeImported_Stk_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_PStk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_PStk), PPointer(@Stack[StackPos - SOffset])^, PPointer(@Stack[StackPos - SOffset])^, ParamSize, ParamSize - SOffset);
end;

procedure DoInvokeImported_Stk_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Var(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_Var), PPointer(@Stack[StackPos - SOffset])^, @VarStack[VarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_Stk_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_PVar(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_PVar), PPointer(@Stack[StackPos - SOffset])^, Pointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + POffset), ParamSize);
end;

procedure DoInvokeImported_Stk_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Ptr(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_Ptr), PPointer(@Stack[StackPos - SOffset])^, PtrD, ParamSize);
end;

procedure DoInvokeImported_Var_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Stk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_Stk), PPointer(@VarStack[VarStackPos + VOffsetS])^, @Stack[StackPos - ParamSize], ParamSize, StackInc);
end;

procedure DoInvokeImported_Var_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_PStk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_PStk), PPointer(@VarStack[VarStackPos + VOffsetS])^, PPointer(@Stack[StackPos - ParamSize])^, ParamSize);
end;

procedure DoInvokeImported_Var_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Var(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_Var), PPointer(@VarStack[VarStackPos + VOffsetS])^, @VarStack[VarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_Var_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_PVar(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_PVar), PPointer(@VarStack[VarStackPos + VOffsetS])^, Pointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + POffset), ParamSize);
end;

procedure DoInvokeImported_Var_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Ptr(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_Ptr), PPointer(@VarStack[VarStackPos + VOffsetS])^, PtrD, ParamSize);
end;

procedure DoInvokeImported_PVar_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Stk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_Stk), PPointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetS])^) + POffsetS)^, @Stack[StackPos - ParamSize], ParamSize, StackInc);
end;

procedure DoInvokeImported_PVar_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_PStk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_PStk), PPointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetS])^) + POffsetS)^, PPointer(@Stack[StackPos - ParamSize])^, ParamSize);
end;

procedure DoInvokeImported_PVar_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Var(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_Var), PPointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetS])^) + POffsetS)^, @VarStack[VarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_PVar_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_PVar(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_PVar), PPointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetS])^) + POffsetS)^, Pointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + POffsetD), ParamSize);
end;

procedure DoInvokeImported_PVar_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Ptr(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_Ptr), PPointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetS])^) + POffsetS)^, PtrD, ParamSize);
end;

procedure DoInvokeImported_Ptr_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Stk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_Stk), PPointer(PtrS)^, @Stack[StackPos - ParamSize], ParamSize, StackInc);
end;

procedure DoInvokeImported_Ptr_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_PStk(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_PStk), PPointer(PtrS)^, PPointer(@Stack[StackPos - ParamSize])^, ParamSize);
end;

procedure DoInvokeImported_Ptr_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Var(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_Var), PPointer(PtrS)^, @VarStack[VarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_Ptr_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_PVar(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_PVar), PPointer(PtrS)^, Pointer(PtrInt(PPointer(@VarStack[VarStackPos + VOffsetD])^) + POffset), ParamSize);
end;

procedure DoInvokeImported_Ptr_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Ptr(PtrUInt(Code) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_Ptr), PPointer(PtrS)^, PtrD, ParamSize);
end;
