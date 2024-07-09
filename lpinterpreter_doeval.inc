{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All the evaluation related handling functions.
}
{%MainUnit lpinterpreter.pas}

procedure DoEval__PtrD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, PtrL, @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PtrL, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, PtrL, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PtrL, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, @FVarStack[FVarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, @FVarStack[FVarStackPos + VOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_VarR) + ocSize);
end;
procedure DoEval__PtrD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FVarStack[FVarStackPos + VOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_StkR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, @FVarStack[FVarStackPos + VOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FVarStack[FVarStackPos + VOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FStack[FStackPos - SOffsetL], PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FStack[FStackPos - SOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_VarR) + ocSize);
end;
procedure DoEval__PtrD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_StkR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FStack[FStackPos - SOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, @FStack[FStackPos - SOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(PtrD, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, PtrR);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, @FVarStack[FVarStackPos + VOffsetR]);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@FStack[FStackPos - SOffsetL])^, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PtrD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PtrD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(PtrD, PPointer(@FStack[FStackPos - 2*SizeOf(Pointer)])^, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PtrD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], PtrL, @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_VarR) + ocSize);
end;
procedure DoEval__VarD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PtrL, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_StkR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], PtrL, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PtrL, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__VarD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FVarStack[FVarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PtrR) + ocSize);
end;
procedure DoEval__VarD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FVarStack[FVarStackPos + VOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_VarR) + ocSize);
end;
procedure DoEval__VarD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FVarStack[FVarStackPos + VOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_StkR) + ocSize);
end;
procedure DoEval__VarD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FVarStack[FVarStackPos + VOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PVarR) + ocSize);
end;
procedure DoEval__VarD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FVarStack[FVarStackPos + VOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_VarL_PStkR) + ocSize);
end;
procedure DoEval__VarD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FStack[FStackPos - SOffsetL], PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PtrR) + ocSize);
end;
procedure DoEval__VarD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FStack[FStackPos - SOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_VarR) + ocSize);
end;
procedure DoEval__VarD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_StkR) + ocSize);
end;
procedure DoEval__VarD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FStack[FStackPos - SOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PVarR) + ocSize);
end;
procedure DoEval__VarD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], @FStack[FStackPos - SOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_StkL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_VarR) + ocSize);
end;
procedure DoEval__VarD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_StkR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(@FVarStack[FVarStackPos + VOffsetD], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, PtrR);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__VarD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, @FVarStack[FVarStackPos + VOffsetR]);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_VarR) + ocSize);
end;
procedure DoEval__VarD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PPointer(@FStack[FStackPos - SOffsetL])^, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_StkR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__VarD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__VarD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FVarStack[FVarStackPos + VOffsetD], PPointer(@FStack[FStackPos - 2*SizeOf(Pointer)])^, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__VarD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], PtrL, PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], PtrL, @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_VarR) + ocSize);
end;
procedure DoEval__StkD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetR], PtrL, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_StkR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], PtrL, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SizeOf(Pointer)], PtrL, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__StkD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], @FVarStack[FVarStackPos + VOffsetL], PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PtrR) + ocSize);
end;
procedure DoEval__StkD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], @FVarStack[FVarStackPos + VOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_VarR) + ocSize);
end;
procedure DoEval__StkD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetR], @FVarStack[FVarStackPos + VOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_StkR) + ocSize);
end;
procedure DoEval__StkD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], @FVarStack[FVarStackPos + VOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PVarR) + ocSize);
end;
procedure DoEval__StkD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SizeOf(Pointer)], @FVarStack[FVarStackPos + VOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_VarL_PStkR) + ocSize);
end;
procedure DoEval__StkD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetL], PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PtrR) + ocSize);
end;
procedure DoEval__StkD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_VarR) + ocSize);
end;
procedure DoEval__StkD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_StkR) + ocSize);
end;
procedure DoEval__StkD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PVarR) + ocSize);
end;
procedure DoEval__StkD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_StkL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_VarR) + ocSize);
end;
procedure DoEval__StkD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetR], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_StkR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SizeOf(Pointer)], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SizeOf(Pointer)], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__StkD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SizeOf(Pointer)], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_VarR) + ocSize);
end;
procedure DoEval__StkD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SOffsetL], PPointer(@FStack[FStackPos - SOffsetL])^, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_StkR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - SizeOf(Pointer)], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__StkD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__StkD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(@FStack[FStackPos - 2*SizeOf(Pointer)], PPointer(@FStack[FStackPos - 2*SizeOf(Pointer)])^, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__StkD_PStkL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PtrL, PtrR);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PtrL, @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PtrL, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PtrL, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PtrL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PtrL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PtrL, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PtrL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FVarStack[FVarStackPos + VOffsetL], PtrR);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_VarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FVarStack[FVarStackPos + VOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_VarR) + ocSize);
end;
procedure DoEval__PVarD_VarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FVarStack[FVarStackPos + VOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_StkR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FVarStack[FVarStackPos + VOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_VarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_VarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FVarStack[FVarStackPos + VOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_VarL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FStack[FStackPos - SOffsetL], PtrR);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_StkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FStack[FStackPos - SOffsetL], @FVarStack[FVarStackPos + VOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_VarR) + ocSize);
end;
procedure DoEval__PVarD_StkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FStack[FStackPos - SOffsetL], @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_StkR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FStack[FStackPos - SOffsetL], Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_StkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_StkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), @FStack[FStackPos - SOffsetL], PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_StkL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PtrR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PtrR);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_VarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FVarStack[FVarStackPos + VOffsetR]);
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PVarR(PtrUInt(Code) + ocSize)^ do
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PVarL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PVarL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetL])^) + PLeftOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PVarL_PStkR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PtrR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PtrR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, PtrR);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PtrR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_VarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_VarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, @FVarStack[FVarStackPos + VOffsetR]);
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_VarR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_StkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_StkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PPointer(@FStack[FStackPos - SOffsetL])^, @FStack[FStackPos - SOffsetR]);
    Inc(FStackPos, StackInc);
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_StkR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PVarR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PVarR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PPointer(@FStack[FStackPos - SizeOf(Pointer)])^, Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetR])^) + PRightOffset));
    Dec(FStackPos, SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PVarR) + ocSize);
end;
procedure DoEval__PVarD_PStkL_PStkR; {$IFDEF Lape_Inline}inline;{$ENDIF} begin
  with POC_Eval__PVarD_PStkL_PStkR(PtrUInt(Code) + ocSize)^ do begin
    Proc(Pointer(PtrInt(PPointer(@FVarStack[FVarStackPos + VOffsetD])^) + PDestOffset), PPointer(@FStack[FStackPos - 2*SizeOf(Pointer)])^, PPointer(@FStack[FStackPos - SizeOf(Pointer)])^);
    Dec(FStackPos, 2*SizeOf(Pointer));
  end;
  Inc(Code, SizeOf(TOC_Eval__PVarD_PStkL_PStkR) + ocSize);
end;
