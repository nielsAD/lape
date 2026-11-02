{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Bytecode disassembler.
}
unit lpdisassembler;

{$I lape.inc}
{$IFNDEF FPC}
  {$UNDEF Lape_Inline}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes;

type
  TLapeDisassemblerPointerMap = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<string>;

// (Code, CodeLen: what a compiler made; its tables say where each instruction is from)
procedure DisassembleCode(Code: PByte; CodeLen: Integer; PointerNames: TLapeDisassemblerPointerMap); overload;
procedure DisassembleCode(Code: PByte; CodeLen: Integer; PointerNames: array of TLapeDeclArray); overload;

implementation

uses
  lpmessages, lpinterpreter_types, lpeval, lpeval_extra;

procedure DisassembleCode(Code: PByte; CodeLen: Integer; PointerNames: TLapeDisassemblerPointerMap);
var
  CodeBase: PByte;
  p, Pos: TDocPos;
  // (the instruction being written, and what it does to the stack)
  Pending, PendingEffect: string;
  PendingAt: Integer;

  function IntToStr(i: Int64): string; overload;
  begin
    Result := SysUtils.IntToStr(i);
  end;

  function IntToStr(p: Pointer): string; overload;
  var s: string;
  begin
    if (p = nil) then
      Result := 'nil'
    else
    begin
      s := IntToHex(PtrUInt(p), 0);
      if (PointerNames <> nil) and PointerNames.ExistsKey(lpString(s)) then
        Result := PointerNames[lpString(s)]
      else
        Result := '$' + s;
    end;
  end;

  // Its name in a column of its own
  function PadName(const s: string): string;
  const
    NameWidth = 22;
  var
    i: Integer;
  begin
    i := System.Pos(' ', s);
    if (i = 0) or (i > NameWidth) then
      Exit(s);
    Result := Copy(s, 1, i - 1) + StringOfChar(' ', NameWidth - i + 1) + Copy(s, i + 1, MaxInt);
  end;

  procedure Flush;
  begin
    if (Pending = '') then
      Exit;
    WriteLn('$', IntToHex(PendingAt, 8), '  ', PadName(Pending), PendingEffect);
    Pending := '';
    PendingEffect := '';
  end;

  // What it does to the stack goes on the instruction's line
  function StackEffect(const s: string; out Effect: string): Boolean;
  var
    Name: string;
    n: Int64;
  begin
    Name := Copy(s, 1, System.Pos(' ', s) - 1);
    Result := ((Name = 'IncStackPos') or (Name = 'DecStackPos') or (Name = 'IncStack')) and
              TryStrToInt64(Copy(s, Length(Name) + 2, MaxInt), n);
    if (not Result) then
      Exit;
    if (Name = 'DecStackPos') then
      n := -n;
    if (n >= 0) then
      Effect := '  [stack +' + SysUtils.IntToStr(n) + ']'
    else
      Effect := '  [stack ' + SysUtils.IntToStr(n) + ']';
  end;

  procedure _WriteLn(const s: string); overload;
  var
    Effect: string;
  begin
    if (Pending <> '') and (PendingAt = Code - CodeBase) and StackEffect(s, Effect) then
      PendingEffect := PendingEffect + Effect
    else
    begin
      Flush();
      Pending := s;
      PendingAt := Code - CodeBase;
    end;
  end;

  procedure _WriteLn(const s: string; const Args: array of const); overload;
  begin
    _WriteLn(Format(s, args));
  end;

  procedure DoIsScriptMethod; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('IsScriptMethod');
    _WriteLn('IncStack %d', [SizeOf(EvalBool) - SizeOf(Pointer)]);
    Inc(Code, ocSize);
  end;

  procedure DoDumpCallStack;
  begin
    _WriteLn('DumpCallStack');
    _WriteLn('IncStack %d', [SizeOf(lpString)]);
    Inc(Code, ocSize);
  end;

  procedure DoGetScriptMethodName;
  begin
    _WriteLn('GetScriptMethodName');
    _WriteLn('IncStack %d', [SizeOf(ShortString) - SizeOf(Pointer)]);
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionMessage;
  begin
    _WriteLn('GetExceptionMessage');
    _WriteLn('IncStack %d', [SizeOf(ShortString)]);
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionLocation;
  begin
    _WriteLn('GetExceptionLocation');
    _WriteLn('IncStack %d', [SizeOf(Pointer)]);
    Inc(Code, ocSize);
  end;

  procedure DoGetCallerAddress;
  begin
    _WriteLn('GetCallerAddress');
    _WriteLn('IncStack %d', [SizeOf(Pointer)]);
    Inc(Code, ocSize);
  end;

  procedure DoDynArrayRangeCheck; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DynArrayRangeCheck');
    _WriteLn('IncStack %d', [SizeOf(Pointer) + SizeOf(SizeInt)]);
    Inc(Code, ocSize);
  end;

  procedure DoUnused; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('Unused');
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitStackLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitStackLen %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitVarLen; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitVarStackLen %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitStack %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('GrowStack %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('ExpandVarStack %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoExpandVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('ExpandVarStackAndInit %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('GrowVarStack %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowVarAndInit; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('GrowVarStackAndInit %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoPopVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('PopVarStack');
    Inc(Code, ocSize);
  end;

  procedure DoPopStackToVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      _WriteLn('PopStackToVar %d %d', [Size, VOffset]);
      _WriteLn('DecStackPos %d', [Size]);
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoPopVarToStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      _WriteLn('PopVarToStack %d %d', [Size, VOffset]);
      _WriteLn('IncStackPos %d', [Size]);
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoJmpVar; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('JmpVar');
    Inc(Code, ocSize);
  end;

  procedure DoJmpSafe; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('JmpSafe $%x', [PCodePos(PtrUInt(Code) + ocSize)^]);
    Inc(Code, ocSize + SizeOf(TCodePos));
  end;

  procedure DoJmpSafeR; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('JmpSafeR $%x', [PtrInt(Code - CodeBase) + PCodeOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, ocSize + SizeOf(TCodeOffset));
  end;

  procedure DoIncTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_IncTry(PtrUInt(Code) + ocSize)^ do
      if (JmpFinally = Try_NoFinally) then
        _WriteLn('IncTry $%x (NoFinally)', [PtrInt(Code - CodeBase) + Jmp])
      else if (JmpFinally = Try_NoExcept) then
        _WriteLn('IncTry $%x (NoExcept)',  [PtrInt(Code - CodeBase) + Jmp])
      else
        _WriteLn('IncTry $%x $%x', [PtrInt(Code - CodeBase) + Jmp, PtrInt(Code - CodeBase) + Jmp + Int32(JmpFinally)]);
    Inc(Code, ocSize + SizeOf(TOC_IncTry));
  end;

  procedure DoDecTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DecTry');
    Inc(Code, ocSize);
  end;

  procedure DoEndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('EndTry');
    Inc(Code, ocSize);
  end;

  procedure DoCatchException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('CatchException');
    Inc(Code, ocSize);
  end;

  procedure DoReRaiseException; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('ReRaiseException');
    Inc(Code, ocSize);
  end;

  procedure DoDecCall; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DecCall');
    Inc(Code, ocSize);
  end;

  procedure DoDecCall_EndTry; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('DecCall_EndTry');
    Inc(Code, ocSize);
  end;

  {$I lpdisassembler_doinvoke.inc}
  {$I lpdisassembler_dojump.inc}
  {$I lpdisassembler_doeval.inc}

  // The code's tables, after it: its strings by index, its DocPos entries, its methods; and its trailer
  procedure WriteTables;
  var
    Trailer: TCodeTrailer;
    At: TCodePos;
    i: UInt32;
    Entry: TCodeDocPosEntry;
    MethodEntry: TCodeMethodEntry;
  begin
    if (not CodeTrailer(CodeBase, CodeLen, Trailer)) then
      Exit;

    WriteLn;
    WriteLn('-- strings (', Trailer.StringTableCount, ', ', Trailer.StringTableSize, ' bytes)');
    At := Trailer.StringTableStart;
    for i := 1 to Trailer.StringTableCount do
    begin
      WriteLn('$', IntToHex(At, 8), '  [', At - Trailer.StringTableStart, '] "', string(CodeString(CodeBase, CodeLen, At - Trailer.StringTableStart)), '"');
      Inc(At, StrLen(PAnsiChar(PtrUInt(CodeBase) + At)) + 1);
    end;

    WriteLn;
    WriteLn('-- DocPos (', Trailer.DocPosTableCount, ', ', Trailer.DocPosTableSize, ' bytes)');
    At := Trailer.DocPosTableStart;
    for i := 1 to Trailer.DocPosTableCount do
    begin
      Move(PByte(PtrUInt(CodeBase) + At)^, Entry, SizeOf(Entry));
      if (Entry.Line = 0) then
        WriteLn('$', IntToHex(At, 8), '  from $', IntToHex(Entry.CodeOffset, 8), '  (no position)')
      else
        WriteLn('$', IntToHex(At, 8), '  from $', IntToHex(Entry.CodeOffset, 8), '  ', string(CodeString(CodeBase, CodeLen, Entry.FileName)), ':', Entry.Line, ':', Entry.Col);
      Inc(At, SizeOf(Entry));
    end;

    WriteLn;
    WriteLn('-- methods (', Trailer.MethodTableCount, ', ', Trailer.MethodTableSize, ' bytes)');
    At := Trailer.MethodTableStart;
    for i := 1 to Trailer.MethodTableCount do
    begin
      Move(PByte(PtrUInt(CodeBase) + At)^, MethodEntry, SizeOf(MethodEntry));
      WriteLn('$', IntToHex(At, 8), '  at $', IntToHex(MethodEntry.CodeOffset, 8), '  ', string(CodeString(CodeBase, CodeLen, MethodEntry.Name)));
      Inc(At, SizeOf(MethodEntry));
    end;

    WriteLn;
    WriteLn('-- trailer');
    WriteLn('$', IntToHex(At, 8), '  version ', Trailer.Version, ', program ', Trailer.ProgramSize, ' bytes');
  end;

  // Before the instruction at Code: the program's or a method's start, where it is from
  procedure WriteHeaders;
  var
    Offset: Integer;
    Name: lpString;
  begin
    Offset := Code - CodeBase;

    if (Offset = 0) then
      Name := 'main'
    else
      Name := CodeMethodName(CodeBase, CodeLen, Offset);
    if (Name <> '') then
    begin
      WriteLn;
      WriteLn('-- ', string(Name));
    end;

    Pos := CodeDocPos(CodeBase, CodeLen, Offset);
    if (p.FileName <> Pos.FileName) or (p.Line <> Pos.Line) or (p.Col <> Pos.Col) then
    begin
      p := Pos;
      if Pos.isInternal then
        WriteLn('   internal ', string(Copy(Pos.FileName, 2, MaxInt)), ':', Pos.Line, ':', Pos.Col)
      else if (not Pos.isEmpty) then
        WriteLn('   ', string(Pos.FileName), ':', Pos.Line, ':', Pos.Col);
    end;
  end;

begin
  CodeBase := Code;
  p := NullDocPos;
  Pending := '';
  PendingEffect := '';

  try
    while True do
    begin
      Flush();
      WriteHeaders();
      {$I lpinterpreter_opcodecase.inc}
    end;
    Flush();
    WriteLn('$', IntToHex(Code - CodeBase, 8), '  End');
    WriteTables();
  except
    on E: Exception do
    begin
      Flush();
      LapeExceptionFmt(lpeRuntime, [E.Message], CodeDocPos(CodeBase, CodeLen, Code - CodeBase));
    end;
  end;
end;

procedure Disassemble__EvalProcs(pMap: TLapeDisassemblerPointerMap);

  procedure Add(Proc: TLapeEvalProc; Name: string);
  begin
    if ValidEvalFunction(Proc) then
      pMap[lpString(IntToHex(PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc), 0))] := Name;
  end;

var
  op: EOperator;
  t1, t2: ELapeBaseType;
  proc: TLapeEvalProc;
  Size: Integer;
begin
  Assert(pMap <> nil);

  // (the ones the compiler uses directly, not by operator)
  Add({$IFDEF FPC}@{$ENDIF}lpeTMethod_ASGN, '(TMethod :=)');
  Add(getEvalProc_StringLength(), '(Length of string)');
  Add(getEvalProc_DynArrayLength(), '(Length of array)');
  Add(getEvalProc_DynArrayHigh(), '(High of array)');
  for t1 := Low(LapeIntegerTypeRange) to High(LapeIntegerTypeRange) do
  begin
    Add(getEvalProc_DynArrayRangeCheck(t1), '(range check, ' + string(LapeTypeToString(t1)) + ' index)');
    for Size in LapePointerIndexEvalSizes do
      Add(getEvalProc_PointerIndex(Size, t1), '(index of ' + IntToStr(Size) + '-byte items, ' + string(LapeTypeToString(t1)) + ')');
  end;

  for op := Low(EOperator) to High(EOperator) do
  begin
    if (op_name[op] = '') then
      Continue;

    for t1 := High(ELapeBaseType) downto Low(ELapeBaseType)  do
      for t2 := High(ELapeBaseType) downto Low(ELapeBaseType) do
      begin
        proc := getEvalProc(op, t1, t2);
        // (as the operation reads: `(Int32 := Int64)`, `(-Int32)`, `(@)`)
        if ValidEvalFunction(proc) then
          if (t1 = ltUnknown) then
            pMap[lpString(IntToHex(PtrUInt({$IFNDEF FPC}@{$ENDIF}proc), 0))] := '(' + string(op_str[op]) + ')'
          else if (t2 = ltUnknown) then
            if (op_str[op][1] in ['a'..'z']) then
              pMap[lpString(IntToHex(PtrUInt({$IFNDEF FPC}@{$ENDIF}proc), 0))] := '(' + string(op_str[op]) + ' ' + string(LapeTypeToString(t1)) + ')'
            else
              pMap[lpString(IntToHex(PtrUInt({$IFNDEF FPC}@{$ENDIF}proc), 0))] := '(' + string(op_str[op]) + string(LapeTypeToString(t1)) + ')'
          else
            pMap[lpString(IntToHex(PtrUInt({$IFNDEF FPC}@{$ENDIF}proc), 0))] := '(' + string(LapeTypeToString(t1)) + ' ' + string(op_str[op]) + ' ' + string(LapeTypeToString(t2)) + ')';
      end;
  end;
end;

procedure DisassembleCode(Code: PByte; CodeLen: Integer; PointerNames: array of TLapeDeclArray);
var
  pMap: TLapeDisassemblerPointerMap;
  i,j: Integer;
begin
  pMap := TLapeDisassemblerPointerMap.Create('');
  try
    Disassemble__EvalProcs(pMap);

    for i := 0 to High(PointerNames) do
      for j := 0 to High(PointerNames[i]) do
        if (PointerNames[i][j].Name = '') and (PointerNames[i][j] is TLapeGlobalVar) then
          pMap[lpString(IntToHex(PtrUInt(TLapeGlobalVar(PointerNames[i][j]).Ptr), 0))] := string(TLapeGlobalVar(PointerNames[i][j]).AsString)
        else if (PointerNames[i][j] is TLapeGlobalVar) then
          pMap[lpString(IntToHex(PtrUInt(TLapeGlobalVar(PointerNames[i][j]).Ptr), 0))] := string(PointerNames[i][j].Name)
        else
          pMap[lpString(IntToHex(PtrUInt(PointerNames[i][j]), 0))] := string(PointerNames[i][j].Name);

    DisassembleCode(Code, CodeLen, pMap);
  finally
    pMap.Free();
  end;
end;

end.

