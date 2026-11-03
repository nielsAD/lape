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
  TLapeDisassemblerPrint = procedure(const Line: string) of object;

procedure DisassembleCode(
  Code: PByte; CodeLen: Integer;
  Decls: array of TLapeDeclArray;
  Print: TLapeDisassemblerPrint = nil
);

implementation

uses
  lpmessages, lpinterpreter_types, lpeval, lpeval_extra;

type
  // (what an address is called: an eval routine, a declaration, a global by where its value is)
  TLapeDisassemblerPointerMap = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<string>;
  // (the stack depth at a jump's target, by its address)
  TLapeDisassemblerDepthMap = {$IFDEF FPC}specialize{$ENDIF} TLapeDictionary<Integer>;

function PointerKey(p: Pointer): lpString;
begin
  Result := lpString(IntToHex(PtrUInt(p), 0));
end;

procedure Disassemble(Code: PByte; CodeLen: Integer; NamedAddresses: TLapeDisassemblerPointerMap; Print: TLapeDisassemblerPrint);
var
  CodeBase: PByte;
  LastFile: lpString; // the file the last instruction is from
  AddressWidth: Integer;
  Pending: string;
  PendingAt: Integer;
  PendingDepth: Integer; // (how much was on the stack before the pending instruction)
  StackDepth: Integer; // (how much the method has put on the stack so far)
  JumpDepths: TLapeDisassemblerDepthMap;
  Section: array of record
    Address, Name, Rest: string;
  end;
  SectionCount: Integer;

  function IntToStr(i: Int64): string; overload;
  begin
    Result := SysUtils.IntToStr(i);
  end;

  function IntToStr(p: Pointer): string; overload;
  begin
    if (p = nil) then
      Result := 'nil'
    else if (NamedAddresses <> nil) and NamedAddresses.ExistsKey(PointerKey(p)) then
      Result := NamedAddresses[PointerKey(p)]
    else
      Result := '$' + string(PointerKey(p));
  end;

  procedure PrintLine(const s: string = '');
  begin
    if Assigned(Print) then
      Print(s)
    else
      WriteLn(s);
  end;

  function Address(At: Integer): string;
  begin
    Result := '$' + IntToHex(At, AddressWidth);
  end;

  procedure SplitName(const s: string; out Name, Rest: string);
  var
    i: Integer;
  begin
    i := System.Pos(') ', s);
    if (Copy(s, 1, 6) = 'Eval (') and (i > 0) then
    begin
      Name := 'Eval ' + Copy(s, 7, i - 7);
      Rest := Copy(s, i + 2, MaxInt);
    end
    else
    begin
      i := System.Pos(' ', s);
      if (i = 0) then
        i := Length(s) + 1;
      Name := Copy(s, 1, i - 1);
      Rest := Copy(s, i + 1, MaxInt);
    end;
  end;

  procedure AddLine(const Address, Name, Rest: string);
  begin
    if (SectionCount = Length(Section)) then
      SetLength(Section, SectionCount * 2 + 64);
    Section[SectionCount].Address := Address;
    Section[SectionCount].Name := Name;
    Section[SectionCount].Rest := Rest;
    Inc(SectionCount);
  end;

  procedure EndSection;
  var
    i, Width: Integer;
  begin
    Width := 0;
    for i := 0 to SectionCount - 1 do
      if (Section[i].Address <> '') and (Length(Section[i].Name) > Width) then
        Width := Length(Section[i].Name);

    for i := 0 to SectionCount - 1 do
      with Section[i] do
        if (Address = '') then
          PrintLine(Name)
        else if (Rest = '') then
          PrintLine(Address + '  ' + Name)
        else
          PrintLine(Address + '  ' + Name + StringOfChar(' ', Width - Length(Name) + 3) + Rest);
    SectionCount := 0;
  end;

  // Stack[StackPos] and Stack[StackPos - N] as where they are from the method's start: Stack[+16]
  function RelativeStack(const s: string; Depth: Integer): string;
  const
    Key = 'Stack[StackPos';
  var
    i, j, n: Integer;
  begin
    Result := '';
    i := 1;
    while (i <= Length(s)) do
      if (Copy(s, i, Length(Key)) = Key) then
      begin
        j := i + Length(Key);
        n := 0;
        if (Copy(s, j, 3) = ' - ') then
        begin
          Inc(j, 3);
          while (j <= Length(s)) and (s[j] in ['0'..'9']) do
          begin
            n := n * 10 + Ord(s[j]) - Ord('0');
            Inc(j);
          end;
        end;
        if (Depth - n >= 0) then
          Result := Result + 'Stack[+' + SysUtils.IntToStr(Depth - n)
        else
          Result := Result + 'Stack[' + SysUtils.IntToStr(Depth - n);
        i := j; // (at its ])
      end
      else
      begin
        Result := Result + s[i];
        Inc(i);
      end;
  end;

  // (a jump's targets, its $ addresses, are where the stack is as deep as after it)
  procedure KeepJumpDepths(const s: string);
  var
    i, j: Integer;
  begin
    if (Copy(s, 1, 4) <> 'Jump') and (Copy(s, 1, 3) <> 'Jmp') and (Copy(s, 1, 6) <> 'IncTry') then
      Exit;
    for i := 1 to Length(s) do
      if (s[i] = '$') then
      begin
        j := i + 1;
        while (j <= Length(s)) and (s[j] in ['0'..'9', 'A'..'F', 'a'..'f']) do
          Inc(j);
        if (j > i + 1) then
          JumpDepths[lpString(IntToHex(StrToInt64('$' + Copy(s, i + 1, j - i - 1)), 0))] := StackDepth;
      end;
  end;

  procedure Flush;
  var
    Name, Rest: string;
  begin
    if (Pending = '') then
      Exit;
    KeepJumpDepths(Pending);
    SplitName(RelativeStack(Pending, PendingDepth), Name, Rest);
    AddLine(Address(PendingAt), Name, Rest);
    Pending := '';
  end;

  procedure _WriteLn(const s: string); overload;
  begin
    Flush();
    Pending := s;
    PendingAt := Code - CodeBase;
    PendingDepth := StackDepth;
  end;

  procedure _WriteLn(const s: string; const Args: array of const); overload;
  begin
    _WriteLn(Format(s, args));
  end;

  // What the instruction just written does to StackPos (as the interpreter does it)
  procedure StackChange(n: Integer);
  begin
    Inc(StackDepth, n);
  end;

  procedure DoIsScriptMethod; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('IsScriptMethod');
    StackChange(-(SizeOf(Pointer) - SizeOf(Boolean)));
    Inc(Code, ocSize);
  end;

  procedure DoDumpCallStack;
  begin
    _WriteLn('DumpCallStack');
    StackChange(-(SizeOf(Int32) + SizeOf(Pointer)));
    Inc(Code, ocSize);
  end;

  procedure DoGetScriptMethodName;
  begin
    _WriteLn('GetScriptMethodName');
    StackChange(SizeOf(ShortString) - SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionMessage;
  begin
    _WriteLn('GetExceptionMessage');
    StackChange(-SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetExceptionLocation;
  begin
    _WriteLn('GetExceptionLocation');
    StackChange(SizeOf(Pointer));
    Inc(Code, ocSize);
  end;

  procedure DoGetCallerAddress;
  begin
    _WriteLn('GetCallerAddress');
    StackChange(SizeOf(Pointer));
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

  procedure DoInitStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('InitStack %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    Inc(Code, SizeOf(TStackOffset) + ocSize);
  end;

  procedure DoGrowStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    _WriteLn('GrowStack %d', [PStackOffset(PtrUInt(Code) + ocSize)^]);
    StackChange(PStackOffset(PtrUInt(Code) + ocSize)^);
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
      StackChange(-Integer(Size));
    end;
    Inc(Code, ocSize + SizeOf(TOC_PopStackToVar));
  end;

  procedure DoPopVarToStack; {$IFDEF Lape_Inline}inline;{$ENDIF}
  begin
    with POC_PopStackToVar(PtrUInt(Code) + ocSize)^ do
    begin
      _WriteLn('PopVarToStack %d %d', [Size, VOffset]);
      StackChange(Size);
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

  // The code's tables, after it: its strings, its DocPos entries, its methods; and its trailer
  procedure WriteTables;
  var
    Trailer: TCodeTrailer;
    At: TCodePos;
    i: UInt32;
    Entry: TCodeDocPosEntry;
    MethodEntry: TCodeMethodEntry;

    procedure Heading(const Name: string; Count, Size: UInt32);
    begin
      PrintLine();
      PrintLine('-- ' + Name + ' (' + IntToStr(Count) + ', ' + IntToStr(Size) + ' bytes)');
    end;

  begin
    if (not CodeTrailer(CodeBase, CodeLen, Trailer)) then
      Exit;

    Heading('strings', Trailer.StringTableCount, Trailer.StringTableSize);
    At := Trailer.StringTableStart;
    for i := 1 to Trailer.StringTableCount do
    begin
      PrintLine('"' + string(CodeString(CodeBase, CodeLen, At - Trailer.StringTableStart)) + '"');
      Inc(At, StrLen(PAnsiChar(PtrUInt(CodeBase) + At)) + 1);
    end;

    Heading('DocPos', Trailer.DocPosTableCount, Trailer.DocPosTableSize);
    At := Trailer.DocPosTableStart;
    for i := 1 to Trailer.DocPosTableCount do
    begin
      Entry := TCodeDocPosEntry(Pointer(PtrUInt(CodeBase) + At)^);
      if (Entry.Line = 0) then
        PrintLine(Address(Entry.CodeOffset) + '  (no position)')
      else
        PrintLine(Address(Entry.CodeOffset) + '  ' + string(CodeString(CodeBase, CodeLen, Entry.FileName)) + ':' + IntToStr(Entry.Line) + ':' + IntToStr(Entry.Col));
      Inc(At, SizeOf(Entry));
    end;

    Heading('methods', Trailer.MethodTableCount, Trailer.MethodTableSize);
    At := Trailer.MethodTableStart;
    for i := 1 to Trailer.MethodTableCount do
    begin
      MethodEntry := TCodeMethodEntry(Pointer(PtrUInt(CodeBase) + At)^);
      PrintLine(Address(MethodEntry.CodeOffset) + '  ' + string(CodeString(CodeBase, CodeLen, MethodEntry.Name)));
      Inc(At, SizeOf(MethodEntry));
    end;

    PrintLine();
    PrintLine('-- trailer');
    PrintLine('version ' + IntToStr(Trailer.Version) + ', program ' + IntToStr(Trailer.ProgramSize) + ' bytes');
  end;

  // Before the instruction at Code: the program's or a method's start, and the file it is from
  procedure WriteHeaders;
  var
    Offset: Integer;
    Name: lpString;
    Pos: TDocPos;
  begin
    Offset := Code - CodeBase;

    if (Offset = 0) then
      Name := 'main'
    else
      Name := CodeMethodName(CodeBase, CodeLen, Offset);

    // (a method's start says which file it is in: `"Name" in File`; a file that changes inside one gets a line of its name)
    Pos := CodeDocPos(CodeBase, CodeLen, Offset);
    if (Name <> '') then
    begin
      EndSection();
      StackDepth := 0;
      if (Offset > 0) then
        PrintLine(); // (between it and the function before it)
      if Pos.isEmpty then
        PrintLine('"' + string(Name) + '"')
      else
      begin
        PrintLine('"' + string(Name) + '" in ' + string(Pos.FileName));
        LastFile := Pos.FileName;
      end;
    end
    else if (not Pos.isEmpty) and (Pos.FileName <> LastFile) then
    begin
      AddLine('', string(Pos.FileName), '');
      LastFile := Pos.FileName;
    end;
  end;

begin
  CodeBase := Code;
  LastFile := '';
  AddressWidth := Length(IntToHex(CodeLen, 0));
  Pending := '';
  PendingDepth := 0;
  StackDepth := 0;
  SectionCount := 0;

  JumpDepths := TLapeDisassemblerDepthMap.Create(0);
  try
    try
      while True do
      begin
        Flush();
        // (where it was jumped to from says how deep the stack is: code after a jump is not reached from above it)
        if JumpDepths.ExistsKey(lpString(IntToHex(Code - CodeBase, 0))) then
          StackDepth := JumpDepths[lpString(IntToHex(Code - CodeBase, 0))];
        WriteHeaders();
        {$I lpinterpreter_opcodecase.inc}
      end;
      Flush();
      AddLine(Address(Code - CodeBase), 'End', '');
      EndSection();
      WriteTables();
    except
      on E: Exception do
      begin
        Flush();
        EndSection();
        LapeExceptionFmt(lpeRuntime, [E.Message], CodeDocPos(CodeBase, CodeLen, Code - CodeBase));
      end;
    end;
  finally
    JumpDepths.Free();
  end;
end;

procedure DisassembleCode(
  Code: PByte; CodeLen: Integer;
  Decls: array of TLapeDeclArray;
  Print: TLapeDisassemblerPrint);
var
  NamedAddresses: TLapeDisassemblerPointerMap;

  procedure addEvalProcNames;
  var
    op: EOperator;
    LeftType, RightType: ELapeBaseType;
    Proc: TLapeEvalProc;
    Size: Integer;
    OpName, Left, Name: string;
  begin
    // lpeval_extra
    NamedAddresses[PointerKey(Pointer({$IFDEF FPC}@{$ENDIF}lpeTMethod_ASGN))]            := '(TMethod := TMethod)';
    Proc := getEvalProc_StringLength();
    NamedAddresses[PointerKey(Pointer(Proc))] := '(String length)';
    Proc := getEvalProc_DynArrayLength();
    NamedAddresses[PointerKey(Pointer(Proc))] := '(Array length)';
    Proc := getEvalProc_DynArrayHigh();
    NamedAddresses[PointerKey(Pointer(Proc))] := '(Array High)';

    for LeftType := Low(LapeIntegerTypeRange) to High(LapeIntegerTypeRange) do
    begin
      for Size in LapePointerIndexEvalSizes do
      begin
        Proc := getEvalProc_PointerIndex(Size, LeftType);
        if (Proc = nil) then
          Continue;
        NamedAddresses[PointerKey(Pointer(Proc))] := '(Pointer Index ' + IntToStr(Size) + ')';
      end;

      Proc := getEvalProc_DynArrayRangeCheck(LeftType);
      if (Proc = nil) then
        Continue;
      NamedAddresses[PointerKey(Pointer(Proc))] := '(Range Check)';
    end;

    // lpeval
    for op := Low(EOperator) to High(EOperator) do
    begin
      if (op_name[op] = '') then
        Continue;
      OpName := string(op_str[op]);

      for LeftType := High(ELapeBaseType) downto Low(ELapeBaseType) do
      begin
        Left := string(LapeTypeToString(LeftType));
        for RightType := High(ELapeBaseType) downto Low(ELapeBaseType) do
        begin
          Proc := getEvalProc(op, LeftType, RightType);
          if (not ValidEvalFunction(Proc)) then
            Continue;

          if (LeftType = ltUnknown) then
            Name := OpName
          else if (RightType = ltUnknown) and (OpName[1] in ['a'..'z']) then
            Name := OpName + ' ' + Left
          else if (RightType = ltUnknown) then
            Name := OpName + Left
          else
            Name := Left + ' ' + OpName + ' ' + string(LapeTypeToString(RightType));

          NamedAddresses[PointerKey(Pointer(Proc))] := '(' + Name + ')';
        end;
      end;
    end;
  end;

  procedure addPointerNames;
  var
    i, j: Integer;
    Decl: TLapeDeclaration;
  begin
    for i := 0 to High(Decls) do
      for j := 0 to High(Decls[i]) do
      begin
        Decl := Decls[i][j];
        if (not (Decl is TLapeGlobalVar)) then
          NamedAddresses[PointerKey(Decl)] := string(Decl.Name)
        else if (Decl.Name = '') then
          NamedAddresses[PointerKey(TLapeGlobalVar(Decl).Ptr)] := string(TLapeGlobalVar(Decl).AsString)
        else
          NamedAddresses[PointerKey(TLapeGlobalVar(Decl).Ptr)] := string(Decl.Name);
      end;
  end;

begin
  NamedAddresses := TLapeDisassemblerPointerMap.Create('');
  try
    addEvalProcNames();
    addPointerNames();

    Disassemble(Code, CodeLen, NamedAddresses, Print);
  finally
    NamedAddresses.Free();
  end;
end;

end.

