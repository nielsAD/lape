{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All the jump related handling functions.
}
{%MainUnit lpinterpreter.pas}

procedure DoJmp; {$IFDEF FPC}{$IFDEF Lape_Inline}inline;{$ENDIF}{$ENDIF}
begin
  JumpTo(PCodePos(PtrUInt(FCode) + ocSize)^);
end;

procedure DoJmpR; {$IFDEF FPC}{$IFDEF Lape_Inline}inline;{$ENDIF}{$ENDIF}
begin
  JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^);
end;

//jump relative if
procedure DoJmpRIf8_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt8(@FStack[FStackPos - SizeOf(UInt8)])^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt8));
end;

procedure DoJmpRIf16_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt16(@FStack[FStackPos - SizeOf(UInt16)])^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt16));
end;

procedure DoJmpRIf32_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt32(@FStack[FStackPos - SizeOf(UInt32)])^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt32));
end;

procedure DoJmpRIf64_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt64(@FStack[FStackPos - SizeOf(UInt64)])^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt64));
end;

procedure DoJmpRIf8_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt8(@FVarStack[FVarStackPos + VarStackOffset])^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf16_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt16(@FVarStack[FVarStackPos + VarStackOffset])^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf32_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt32(@FVarStack[FVarStackPos + VarStackOffset])^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf64_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt64(@FVarStack[FVarStackPos + VarStackOffset])^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIf8_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt8(Ptr)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf16_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt16(Ptr)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf32_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt32(Ptr)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf64_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt64(Ptr)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIf8_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt8(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIf16_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt16(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIf32_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt32(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIf64_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt64(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ <> 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIf8_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt8(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIf16_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt16(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIf32_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt32(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIf64_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt64(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ <> 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

//jump relative if not
procedure DoJmpRIfNot8_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt8(@FStack[FStackPos - SizeOf(UInt8)])^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt8));
end;

procedure DoJmpRIfNot16_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt16(@FStack[FStackPos - SizeOf(UInt16)])^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt16));
end;

procedure DoJmpRIfNot32_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt32(@FStack[FStackPos - SizeOf(UInt32)])^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt32));
end;

procedure DoJmpRIfNot64_Stk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt64(@FStack[FStackPos - SizeOf(UInt64)])^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodeOffset) + ocSize);
  Dec(FStackPos, SizeOf(UInt64));
end;

procedure DoJmpRIfNot8_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt8(@FVarStack[FVarStackPos + VarStackOffset])^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot16_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt16(@FVarStack[FVarStackPos + VarStackOffset])^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot32_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt32(@FVarStack[FVarStackPos + VarStackOffset])^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot64_Var; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Var(PtrUInt(FCode) + ocSize)^ do
    if (PUInt64(@FVarStack[FVarStackPos + VarStackOffset])^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Var) + ocSize);
end;

procedure DoJmpRIfNot8_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt8(Ptr)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot16_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt16(Ptr)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot32_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt32(Ptr)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot64_Ptr; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_Ptr(PtrUInt(FCode) + ocSize)^ do
    if (PUInt64(Ptr)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_Ptr) + ocSize);
end;

procedure DoJmpRIfNot8_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt8(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIfNot16_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt16(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIfNot32_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt32(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIfNot64_PStk; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  if (PUInt64(PPointer(@FStack[FStackPos - SizeOf(Pointer)])^)^ = 0) then
    JumpToRelative(PCodeOffset(PtrUInt(FCode) + ocSize)^)
  else
    Inc(FCode, SizeOf(TCodePos) + ocSize);
  Dec(FStackPos, SizeOf(Pointer));
end;

procedure DoJmpRIfNot8_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt8(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIfNot16_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt16(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIfNot32_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt32(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;

procedure DoJmpRIfNot64_PVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
begin
  with POC_JmpR_PVar(PtrUInt(FCode) + ocSize)^ do
    if (PUInt64(PtrInt(PPointer(@FVarStack[FVarStackPos + VarStackOffset])^) + PointerOffset)^ = 0) then
      JumpToRelative(Offset)
    else
      Inc(FCode, SizeOf(TOC_JmpR_PVar) + ocSize);
end;
