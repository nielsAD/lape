{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Evaluation bodies for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PtrR(AProc: TLapeEvalProc; APtrD, APtrL, APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_PtrR);
  Reserve(SizeOf(TOC_Eval__PtrD_PtrL_PtrR));
  with POC_Eval__PtrD_PtrL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _Pointer(APtrL, @PtrL);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_VarR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_VarR);
  Reserve(SizeOf(TOC_Eval__PtrD_PtrL_VarR));
  with POC_Eval__PtrD_PtrL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_StkR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_StkR);
  Reserve(SizeOf(TOC_Eval__PtrD_PtrL_StkR));
  with POC_Eval__PtrD_PtrL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _Pointer(APtrL, @PtrL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PVarR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_PVarR);
  Reserve(SizeOf(TOC_Eval__PtrD_PtrL_PVarR));
  with POC_Eval__PtrD_PtrL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PtrL_PStkR(AProc: TLapeEvalProc; APtrD, APtrL: Pointer): Integer; begin
  Result := _op(ocEval_PtrD_PtrL_PStkR);
  Reserve(SizeOf(TOC_Eval__PtrD_PtrL_PStkR));
  with POC_Eval__PtrD_PtrL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _Pointer(APtrL, @PtrL);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PtrD_VarL_PtrR);
  Reserve(SizeOf(TOC_Eval__PtrD_VarL_PtrR));
  with POC_Eval__PtrD_VarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL, AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PtrD_VarL_VarR);
  Reserve(SizeOf(TOC_Eval__PtrD_VarL_VarR));
  with POC_Eval__PtrD_VarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_VarL_StkR);
  Reserve(SizeOf(TOC_Eval__PtrD_VarL_StkR));
  with POC_Eval__PtrD_VarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PtrD_VarL_PVarR);
  Reserve(SizeOf(TOC_Eval__PtrD_VarL_PVarR));
  with POC_Eval__PtrD_VarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_VarL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PtrD_VarL_PStkR);
  Reserve(SizeOf(TOC_Eval__PtrD_VarL_PStkR));
  with POC_Eval__PtrD_VarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_StkL_PtrR);
  Reserve(SizeOf(TOC_Eval__PtrD_StkL_PtrR));
  with POC_Eval__PtrD_StkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_StkL_VarR);
  Reserve(SizeOf(TOC_Eval__PtrD_StkL_VarR));
  with POC_Eval__PtrD_StkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_StkL_StkR);
  Reserve(SizeOf(TOC_Eval__PtrD_StkL_StkR));
  with POC_Eval__PtrD_StkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_StkL_PVarR);
  Reserve(SizeOf(TOC_Eval__PtrD_StkL_PVarR));
  with POC_Eval__PtrD_StkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_StkL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_StkL_PStkR);
  Reserve(SizeOf(TOC_Eval__PtrD_StkL_PStkR));
  with POC_Eval__PtrD_StkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PtrR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_PtrR);
  Reserve(SizeOf(TOC_Eval__PtrD_PVarL_PtrR));
  with POC_Eval__PtrD_PVarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_VarR);
  Reserve(SizeOf(TOC_Eval__PtrD_PVarL_VarR));
  with POC_Eval__PtrD_PVarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_StkR);
  Reserve(SizeOf(TOC_Eval__PtrD_PVarL_StkR));
  with POC_Eval__PtrD_PVarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_PVarR);
  Reserve(SizeOf(TOC_Eval__PtrD_PVarL_PVarR));
  with POC_Eval__PtrD_PVarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PVarL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PtrD_PVarL_PStkR);
  Reserve(SizeOf(TOC_Eval__PtrD_PVarL_PStkR));
  with POC_Eval__PtrD_PVarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PtrR(AProc: TLapeEvalProc; APtrD, APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_PtrR);
  Reserve(SizeOf(TOC_Eval__PtrD_PStkL_PtrR));
  with POC_Eval__PtrD_PStkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_VarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_VarR);
  Reserve(SizeOf(TOC_Eval__PtrD_PStkL_VarR));
  with POC_Eval__PtrD_PStkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_StkR(AProc: TLapeEvalProc; APtrD: Pointer; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_StkR);
  Reserve(SizeOf(TOC_Eval__PtrD_PStkL_StkR));
  with POC_Eval__PtrD_PStkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PVarR(AProc: TLapeEvalProc; APtrD: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_PVarR);
  Reserve(SizeOf(TOC_Eval__PtrD_PStkL_PVarR));
  with POC_Eval__PtrD_PStkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PtrD_PStkL_PStkR(AProc: TLapeEvalProc; APtrD: Pointer): Integer; begin
  Result := _op(ocEval_PtrD_PStkL_PStkR);
  Reserve(SizeOf(TOC_Eval__PtrD_PStkL_PStkR));
  with POC_Eval__PtrD_PStkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrD, @PtrD);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL, APtrR: Pointer): Integer; begin
  Result := _op(ocEval_VarD_PtrL_PtrR);
  Reserve(SizeOf(TOC_Eval__VarD_PtrL_PtrR));
  with POC_Eval__VarD_PtrL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _Pointer(APtrL, @PtrL);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_VarD_PtrL_VarR);
  Reserve(SizeOf(TOC_Eval__VarD_PtrL_VarR));
  with POC_Eval__VarD_PtrL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_PtrL_StkR);
  Reserve(SizeOf(TOC_Eval__VarD_PtrL_StkR));
  with POC_Eval__VarD_PtrL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _Pointer(APtrL, @PtrL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_VarD_PtrL_PVarR);
  Reserve(SizeOf(TOC_Eval__VarD_PtrL_PVarR));
  with POC_Eval__VarD_PtrL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PtrL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrL: Pointer): Integer; begin
  Result := _op(ocEval_VarD_PtrL_PStkR);
  Reserve(SizeOf(TOC_Eval__VarD_PtrL_PStkR));
  with POC_Eval__VarD_PtrL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _Pointer(APtrL, @PtrL);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_VarD_VarL_PtrR);
  Reserve(SizeOf(TOC_Eval__VarD_VarL_PtrR));
  with POC_Eval__VarD_VarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL, AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_VarD_VarL_VarR);
  Reserve(SizeOf(TOC_Eval__VarD_VarL_VarR));
  with POC_Eval__VarD_VarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_VarL_StkR);
  Reserve(SizeOf(TOC_Eval__VarD_VarL_StkR));
  with POC_Eval__VarD_VarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_VarD_VarL_PVarR);
  Reserve(SizeOf(TOC_Eval__VarD_VarL_PVarR));
  with POC_Eval__VarD_VarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset): Integer; begin
  Result := _op(ocEval_VarD_VarL_PStkR);
  Reserve(SizeOf(TOC_Eval__VarD_VarL_PStkR));
  with POC_Eval__VarD_VarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_StkL_PtrR);
  Reserve(SizeOf(TOC_Eval__VarD_StkL_PtrR));
  with POC_Eval__VarD_StkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_StkL_VarR);
  Reserve(SizeOf(TOC_Eval__VarD_StkL_VarR));
  with POC_Eval__VarD_StkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_StkL_StkR);
  Reserve(SizeOf(TOC_Eval__VarD_StkL_StkR));
  with POC_Eval__VarD_StkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_StkL_PVarR);
  Reserve(SizeOf(TOC_Eval__VarD_StkL_PVarR));
  with POC_Eval__VarD_StkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_StkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_StkL_PStkR);
  Reserve(SizeOf(TOC_Eval__VarD_StkL_PStkR));
  with POC_Eval__VarD_StkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_VarD_PVarL_PtrR);
  Reserve(SizeOf(TOC_Eval__VarD_PVarL_PtrR));
  with POC_Eval__VarD_PVarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_VarD_PVarL_VarR);
  Reserve(SizeOf(TOC_Eval__VarD_PVarL_VarR));
  with POC_Eval__VarD_PVarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_PVarL_StkR);
  Reserve(SizeOf(TOC_Eval__VarD_PVarL_StkR));
  with POC_Eval__VarD_PVarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_VarD_PVarL_PVarR);
  Reserve(SizeOf(TOC_Eval__VarD_PVarL_PVarR));
  with POC_Eval__VarD_PVarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_VarD_PVarL_PStkR);
  Reserve(SizeOf(TOC_Eval__VarD_PVarL_PStkR));
  with POC_Eval__VarD_PVarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_VarD_PStkL_PtrR);
  Reserve(SizeOf(TOC_Eval__VarD_PStkL_PtrR));
  with POC_Eval__VarD_PStkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_VarD_PStkL_VarR);
  Reserve(SizeOf(TOC_Eval__VarD_PStkL_VarR));
  with POC_Eval__VarD_PStkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_VarD_PStkL_StkR);
  Reserve(SizeOf(TOC_Eval__VarD_PStkL_StkR));
  with POC_Eval__VarD_PStkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetD, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_VarD_PStkL_PVarR);
  Reserve(SizeOf(TOC_Eval__VarD_PStkL_PVarR));
  with POC_Eval__VarD_PStkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__VarD_PStkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset): Integer; begin
  Result := _op(ocEval_VarD_PStkL_PStkR);
  Reserve(SizeOf(TOC_Eval__VarD_PStkL_PStkR));
  with POC_Eval__VarD_PStkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PtrR(AProc: TLapeEvalProc; APtrL, APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PtrL_PtrR);
  Reserve(SizeOf(TOC_Eval__StkD_PtrL_PtrR));
  with POC_Eval__StkD_PtrL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrL, @PtrL);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_VarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PtrL_VarR);
  Reserve(SizeOf(TOC_Eval__StkD_PtrL_VarR));
  with POC_Eval__StkD_PtrL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_StkR(AProc: TLapeEvalProc; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PtrL_StkR);
  Reserve(SizeOf(TOC_Eval__StkD_PtrL_StkR));
  with POC_Eval__StkD_PtrL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrL, @PtrL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PVarR(AProc: TLapeEvalProc; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PtrL_PVarR);
  Reserve(SizeOf(TOC_Eval__StkD_PtrL_PVarR));
  with POC_Eval__StkD_PtrL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PtrL_PStkR(AProc: TLapeEvalProc; APtrL: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PtrL_PStkR);
  Reserve(SizeOf(TOC_Eval__StkD_PtrL_PStkR));
  with POC_Eval__StkD_PtrL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrL, @PtrL);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_VarL_PtrR);
  Reserve(SizeOf(TOC_Eval__StkD_VarL_PtrR));
  with POC_Eval__StkD_VarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_VarL_VarR);
  Reserve(SizeOf(TOC_Eval__StkD_VarL_VarR));
  with POC_Eval__StkD_VarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_VarL_StkR);
  Reserve(SizeOf(TOC_Eval__StkD_VarL_StkR));
  with POC_Eval__StkD_VarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_VarL_PVarR);
  Reserve(SizeOf(TOC_Eval__StkD_VarL_PVarR));
  with POC_Eval__StkD_VarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_VarL_PStkR);
  Reserve(SizeOf(TOC_Eval__StkD_VarL_PStkR));
  with POC_Eval__StkD_VarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PtrR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_StkL_PtrR);
  Reserve(SizeOf(TOC_Eval__StkD_StkL_PtrR));
  with POC_Eval__StkD_StkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackOffset(ASOffsetL, @SOffsetL);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_VarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_StkL_VarR);
  Reserve(SizeOf(TOC_Eval__StkD_StkL_VarR));
  with POC_Eval__StkD_StkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_StkL_StkR);
  Reserve(SizeOf(TOC_Eval__StkD_StkL_StkR));
  with POC_Eval__StkD_StkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PVarR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_StkL_PVarR);
  Reserve(SizeOf(TOC_Eval__StkD_StkL_PVarR));
  with POC_Eval__StkD_StkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_StkL_PStkR(AProc: TLapeEvalProc; ASOffsetL: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_StkL_PStkR);
  Reserve(SizeOf(TOC_Eval__StkD_StkL_PStkR));
  with POC_Eval__StkD_StkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PVarL_PtrR);
  Reserve(SizeOf(TOC_Eval__StkD_PVarL_PtrR));
  with POC_Eval__StkD_PVarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PVarL_VarR);
  Reserve(SizeOf(TOC_Eval__StkD_PVarL_VarR));
  with POC_Eval__StkD_PVarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PVarL_StkR);
  Reserve(SizeOf(TOC_Eval__StkD_PVarL_StkR));
  with POC_Eval__StkD_PVarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PVarL_PVarR);
  Reserve(SizeOf(TOC_Eval__StkD_PVarL_PVarR));
  with POC_Eval__StkD_PVarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PVarL_PStkR);
  Reserve(SizeOf(TOC_Eval__StkD_PVarL_PStkR));
  with POC_Eval__StkD_PVarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PtrR(AProc: TLapeEvalProc; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PStkL_PtrR);
  Reserve(SizeOf(TOC_Eval__StkD_PStkL_PtrR));
  with POC_Eval__StkD_PStkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PStkL_VarR);
  Reserve(SizeOf(TOC_Eval__StkD_PStkL_VarR));
  with POC_Eval__StkD_PStkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_StkR(AProc: TLapeEvalProc; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PStkL_StkR);
  Reserve(SizeOf(TOC_Eval__StkD_PStkL_StkR));
  with POC_Eval__StkD_PStkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PStkL_PVarR);
  Reserve(SizeOf(TOC_Eval__StkD_PStkL_PVarR));
  with POC_Eval__StkD_PStkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__StkD_PStkL_PStkR(AProc: TLapeEvalProc; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_StkD_PStkL_PStkR);
  Reserve(SizeOf(TOC_Eval__StkD_PStkL_PStkR));
  with POC_Eval__StkD_PStkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL, APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_PtrR);
  Reserve(SizeOf(TOC_Eval__PVarD_PtrL_PtrR));
  with POC_Eval__PVarD_PtrL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _Pointer(APtrL, @PtrL);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_VarR);
  Reserve(SizeOf(TOC_Eval__PVarD_PtrL_VarR));
  with POC_Eval__PVarD_PtrL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_StkR);
  Reserve(SizeOf(TOC_Eval__PVarD_PtrL_StkR));
  with POC_Eval__PVarD_PtrL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _Pointer(APtrL, @PtrL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_PVarR);
  Reserve(SizeOf(TOC_Eval__PVarD_PtrL_PVarR));
  with POC_Eval__PVarD_PtrL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _Pointer(APtrL, @PtrL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PtrL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrL: Pointer): Integer; begin
  Result := _op(ocEval_PVarD_PtrL_PStkR);
  Reserve(SizeOf(TOC_Eval__PVarD_PtrL_PStkR));
  with POC_Eval__PVarD_PtrL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _Pointer(APtrL, @PtrL);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PVarD_VarL_PtrR);
  Reserve(SizeOf(TOC_Eval__PVarD_VarL_PtrR));
  with POC_Eval__PVarD_VarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL, AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PVarD_VarL_VarR);
  Reserve(SizeOf(TOC_Eval__PVarD_VarL_VarR));
  with POC_Eval__PVarD_VarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_VarL_StkR);
  Reserve(SizeOf(TOC_Eval__PVarD_VarL_StkR));
  with POC_Eval__PVarD_VarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL, AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PVarD_VarL_PVarR);
  Reserve(SizeOf(TOC_Eval__PVarD_VarL_PVarR));
  with POC_Eval__PVarD_VarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_VarL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PVarD_VarL_PStkR);
  Reserve(SizeOf(TOC_Eval__PVarD_VarL_PStkR));
  with POC_Eval__PVarD_VarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; APtrR: Pointer; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_StkL_PtrR);
  Reserve(SizeOf(TOC_Eval__PVarD_StkL_PtrR));
  with POC_Eval__PVarD_StkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _StackOffset(ASOffsetL, @SOffsetL);
    _Pointer(APtrR, @PtrR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_StkL_VarR);
  Reserve(SizeOf(TOC_Eval__PVarD_StkL_VarR));
  with POC_Eval__PVarD_StkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_StkL_StkR);
  Reserve(SizeOf(TOC_Eval__PVarD_StkL_StkR));
  with POC_Eval__PVarD_StkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_StkL_PVarR);
  Reserve(SizeOf(TOC_Eval__PVarD_StkL_PVarR));
  with POC_Eval__PVarD_StkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _StackOffset(ASOffsetL, @SOffsetL);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_StkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_StkL_PStkR);
  Reserve(SizeOf(TOC_Eval__PVarD_StkL_PStkR));
  with POC_Eval__PVarD_StkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_PtrR);
  Reserve(SizeOf(TOC_Eval__PVarD_PVarL_PtrR));
  with POC_Eval__PVarD_PVarL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_VarR);
  Reserve(SizeOf(TOC_Eval__PVarD_PVarL_VarR));
  with POC_Eval__PVarD_PVarL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_StkR);
  Reserve(SizeOf(TOC_Eval__PVarD_PVarL_StkR));
  with POC_Eval__PVarD_PVarL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_PVarR);
  Reserve(SizeOf(TOC_Eval__PVarD_PVarL_PVarR));
  with POC_Eval__PVarD_PVarL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PVarL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetL: TVarStackOffset; APLeftOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PVarD_PVarL_PStkR);
  Reserve(SizeOf(TOC_Eval__PVarD_PVarL_PStkR));
  with POC_Eval__PVarD_PVarL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetL, @VOffsetL);
    _PointerOffset(APLeftOffset, @PLeftOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PtrR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; APtrR: Pointer): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_PtrR);
  Reserve(SizeOf(TOC_Eval__PVarD_PStkL_PtrR));
  with POC_Eval__PVarD_PStkL_PtrR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _Pointer(APtrR, @PtrR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_VarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetR: TVarStackOffset): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_VarR);
  Reserve(SizeOf(TOC_Eval__PVarD_PStkL_VarR));
  with POC_Eval__PVarD_PStkL_VarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_StkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; ASOffsetL, ASOffsetR: TStackOffset; AStackInc: TStackInc): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_StkR);
  Reserve(SizeOf(TOC_Eval__PVarD_PStkL_StkR));
  with POC_Eval__PVarD_PStkL_StkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _StackOffset(ASOffsetL, @SOffsetL);
    _StackOffset(ASOffsetR, @SOffsetR);
    _StackInc(AStackInc, @StackInc);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PVarR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset; AVOffsetR: TVarStackOffset; APRightOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_PVarR);
  Reserve(SizeOf(TOC_Eval__PVarD_PStkL_PVarR));
  with POC_Eval__PVarD_PStkL_PVarR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
    _VarStackOffset(AVOffsetR, @VOffsetR);
    _PointerOffset(APRightOffset, @PRightOffset);
  end;
end;
function TLapeCodeEmitterBase._Eval__PVarD_PStkL_PStkR(AProc: TLapeEvalProc; AVOffsetD: TVarStackOffset; APDestOffset: TPointerOffset): Integer; begin
  Result := _op(ocEval_PVarD_PStkL_PStkR);
  Reserve(SizeOf(TOC_Eval__PVarD_PStkL_PStkR));
  with POC_Eval__PVarD_PStkL_PStkR(@FCode[Result + ocSize])^ do begin
    _EvalProc(AProc, {$IFNDEF FPC}@{$ENDIF}@Proc);
    _VarStackOffset(AVOffsetD, @VOffsetD);
    _PointerOffset(APDestOffset, @PDestOffset);
  end;
end;
