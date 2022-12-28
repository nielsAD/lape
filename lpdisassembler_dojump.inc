{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include prints all the jump related opcodes in the bytecode.
}
{%MainUnit lpdisassembler.pas}

procedure DoJmp; {$IFDEF FPC}{$IFDEF Lape_Inline}inline;{$ENDIF}{$ENDIF}
begin
  _WriteLn('JumpTo $%x', [PCodePos(PtrUInt(FCode) + ocSize)^]);
  Inc(FCode, SizeOf(TCodePos) + ocSize);
end;

procedure DoJmpR; {$IFDEF FPC}{$IFDEF Lape_Inline}inline;{$ENDIF}{$ENDIF}
begin
  _WriteLn('JumpToRelative $%x', [PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

//jump relative if
procedure DoJmpRIf8_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf Stack[StackPos - %d] $%x', [SizeOf(UInt8), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt8)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf16_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf Stack[StackPos - %d] $%x', [SizeOf(UInt16), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt16)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf32_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf Stack[StackPos - %d] $%x', [SizeOf(UInt32), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt32)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf64_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf Stack[StackPos - %d] $%x', [SizeOf(UInt64), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt64)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf8_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf16_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf32_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf64_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf8_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf16_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf32_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf64_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf8_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf16_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf32_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf64_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIf (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIf8_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIf16_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIf32_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIf64_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIf (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

//jump relative if not
procedure DoJmpRIfNot8_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot Stack[StackPos - %d] $%x', [SizeOf(UInt8), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt8)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot16_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot Stack[StackPos - %d] $%x', [SizeOf(UInt16), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt16)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot32_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot Stack[StackPos - %d] $%x', [SizeOf(UInt32), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt32)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot64_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot Stack[StackPos - %d] $%x', [SizeOf(UInt64), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(UInt64)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot8_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot16_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot32_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot64_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot VarStack[VarStackPos + %d] $%x', [VarStackOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot8_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot16_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot32_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot64_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot %s $%x', [IntToStr(Ptr), PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot8_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot16_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot32_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot64_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  _WriteLn('JumpToRelativeIfNot (Stack[StackPos - %d])^ $%x', [SizeOf(Pointer), PtrInt(FCode - FCodeBase) + PCodeOffset(PtrUInt(FCode) + ocSize)^]);
  _WriteLn('DecStackPos %d', [SizeOf(Pointer)]);
  Inc(FCode, SizeOf(TCodeOffset) + ocSize);
end;

procedure DoJmpRIfNot8_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIfNot16_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIfNot32_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIfNot64_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    _WriteLn('JumpToRelativeIfNot (VarStack[VarStackPos + %d])^+%d $%x', [VarStackOffset, PointerOffset, PtrInt(FCode - FCodeBase) + Offset]);
  Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;
