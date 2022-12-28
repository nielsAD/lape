{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All the invoke related handling functions.
}
{%MainUnit lpinterpreter.pas}

procedure DoIncCall_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk(PtrUInt(FCode) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_Stk), PCodePos(@FStack[FStackPos - SOffset])^, ParamSize, ParamSize - SOffset);
end;

procedure DoIncCall_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
var
  ParamSize: TParamSize;
begin
  ParamSize := PParamSize(PtrUInt(FCode) + ocSize)^;
  DoIncCall(SizeOf(TParamSize), PCodePos(PPointer(@FStack[FStackPos - ParamSize - SizeOf(Pointer)])^)^, ParamSize, -SizeOf(Pointer));
end;

procedure DoIncCall_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var(PtrUInt(FCode) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_Var), PCodePos(@FVarStack[FVarStackPos + VOffset])^, ParamSize);
end;

procedure DoIncCall_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar(PtrUInt(FCode) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_PVar), PCodePos(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffset])^) + POffset)^, ParamSize);
end;

procedure DoIncCall_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr(PtrUInt(FCode) + ocSize)^ do
    DoIncCall(SizeOf(TOC_Invoke_Ptr), PCodePos(Ptr)^, ParamSize);
end;

procedure DoInvokeImported_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_Stk), PPointer(@FStack[FStackPos - SOffset])^, ParamSize, ParamSize - SOffset);
end;

procedure DoInvokeImported_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
var
  ParamSize: TParamSize;
begin
  ParamSize := PParamSize(PtrUInt(FCode) + ocSize)^;
  DoInvokeImportedProc(SizeOf(TParamSize), PPointer(PPointer(@FStack[FStackPos - ParamSize - SizeOf(Pointer)])^)^, ParamSize, -SizeOf(Pointer));
end;

procedure DoInvokeImported_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_Var), PPointer(@FVarStack[FVarStackPos + VOffset])^, ParamSize);
end;

procedure DoInvokeImported_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_PVar), PPointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffset])^) + POffset)^, ParamSize);
end;

procedure DoInvokeImported_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedProc(SizeOf(TOC_Invoke_Ptr), PPointer(Ptr)^, ParamSize);
end;

procedure DoInvokeImported_Stk_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Stk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_Stk), PPointer(@FStack[FStackPos - SOffset])^, @FStack[FStackPos - SOffset], ParamSize, ParamSize - SOffset + UInt32(StackInc));
end;

procedure DoInvokeImported_Stk_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_PStk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_PStk), PPointer(@FStack[FStackPos - SOffset])^, PPointer(@FStack[FStackPos - SOffset])^, ParamSize, ParamSize - SOffset);
end;

procedure DoInvokeImported_Stk_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Var(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_Var), PPointer(@FStack[FStackPos - SOffset])^, @FVarStack[FVarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_Stk_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_PVar(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_PVar), PPointer(@FStack[FStackPos - SOffset])^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + POffset), ParamSize);
end;

procedure DoInvokeImported_Stk_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Stk_Ptr(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Stk_Ptr), PPointer(@FStack[FStackPos - SOffset])^, PtrD, ParamSize);
end;

procedure DoInvokeImported_Var_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Stk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_Stk), PPointer(@FVarStack[FVarStackPos + VOffsetS])^, @FStack[FStackPos - ParamSize], ParamSize, StackInc);
end;

procedure DoInvokeImported_Var_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_PStk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_PStk), PPointer(@FVarStack[FVarStackPos + VOffsetS])^, PPointer(@FStack[FStackPos - ParamSize])^, ParamSize);
end;

procedure DoInvokeImported_Var_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Var(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_Var), PPointer(@FVarStack[FVarStackPos + VOffsetS])^, @FVarStack[FVarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_Var_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_PVar(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_PVar), PPointer(@FVarStack[FVarStackPos + VOffsetS])^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + POffset), ParamSize);
end;

procedure DoInvokeImported_Var_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Var_Ptr(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Var_Ptr), PPointer(@FVarStack[FVarStackPos + VOffsetS])^, PtrD, ParamSize);
end;

procedure DoInvokeImported_PVar_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Stk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_Stk), PPointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetS])^) + POffsetS)^, @FStack[FStackPos - ParamSize], ParamSize, StackInc);
end;

procedure DoInvokeImported_PVar_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_PStk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_PStk), PPointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetS])^) + POffsetS)^, PPointer(@FStack[FStackPos - ParamSize])^, ParamSize);
end;

procedure DoInvokeImported_PVar_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Var(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_Var), PPointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetS])^) + POffsetS)^, @FVarStack[FVarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_PVar_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_PVar(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_PVar), PPointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetS])^) + POffsetS)^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + POffsetD), ParamSize);
end;

procedure DoInvokeImported_PVar_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_PVar_Ptr(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_PVar_Ptr), PPointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetS])^) + POffsetS)^, PtrD, ParamSize);
end;

procedure DoInvokeImported_Ptr_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Stk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_Stk), PPointer(PtrS)^, @FStack[FStackPos - ParamSize], ParamSize, StackInc);
end;

procedure DoInvokeImported_Ptr_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_PStk(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_PStk), PPointer(PtrS)^, PPointer(@FStack[FStackPos - ParamSize])^, ParamSize);
end;

procedure DoInvokeImported_Ptr_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Var(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_Var), PPointer(PtrS)^, @FVarStack[FVarStackPos + VOffsetD], ParamSize);
end;

procedure DoInvokeImported_Ptr_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_PVar(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_PVar), PPointer(PtrS)^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + POffset), ParamSize);
end;

procedure DoInvokeImported_Ptr_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_Invoke_Ptr_Ptr(PtrUInt(FCode) + ocSize)^ do
    DoInvokeImportedFunc(SizeOf(TOC_Invoke_Ptr_Ptr), PPointer(PtrS)^, PtrD, ParamSize);
end;
