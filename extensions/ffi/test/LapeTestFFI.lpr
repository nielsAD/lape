program LapeTestFFI;

uses
  sysutils,
  lptypes, lpvartypes, lpcompiler, lpparser, lpinterpreter, lpexceptions, lpffi, ffi;

type
  TRunFun = function(p: Pointer): Boolean;

  TStrArray = array of lpString;
  TIntArray = array of Int16;
  TStaticStrArray = array[1..5] of lpString;
  TStaticIntArray = array[1..2] of Int16;

  TShortRec  = record a: Int8; end;
  TSmallRec  = record b: Int16; end;
  TStringRec = record c: string; end;
  TLargeRec  = record a,b,c,d: UInt64; end;

var
  Success: Boolean = False;

procedure Proc1;
begin
  Success := True;
end;

function RunProc1(p: Pointer): Boolean;
type
  TP = procedure;
begin
  Success := False;
  TP(p)();
  Result := Success;
end;

procedure Proc2(a, b, c, d, e, f, g, h, i, j: NativeInt);
begin
  Success := (a = 1) and (b =- 2) and (c = 3) and (d = -4) and (e = 5) and (f = -6)
         and (g = 7) and (h = -8) and (i = 9) and (j = -10);
  Assert(Success);
end;

function RunProc2(p: Pointer): Boolean;
type
  TP = procedure(a, b, c, d, e, f, g, h, i, j: NativeInt);
begin
  Success := False;
  TP(p)(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);
  Result := Success;
end;

procedure Proc3(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8);
begin
  Success := (a = 1) and (b = -2) and (c = 3) and (d = -4)
         and (e = 5) and (f = -6) and (g = 7) and (h = -8);
  Assert(Success);
end;

function RunProc3(p: Pointer): Boolean;
type
  TP = procedure(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8);
begin
  Success := False;
  TP(p)(1, -2, 3, -4, 5, -6, 7, -8);
  Result := Success;
end;

procedure Proc4(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64);
begin
  Success := (a = 1.5) and (b = 2) and (c = 3.5) and (d = 4)
         and (e = 5.5) and (f = 6) and (g = 7.5) and (h = 8);
  Assert(Success);
end;

function RunProc4(p: Pointer): Boolean;
type
  TP = procedure(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64);
begin
  Success := False;
  TP(p)(1.5, 2, 3.5, 4, 5.5, 6, 7.5, 8);
  Result := Success;
end;

procedure Proc5(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64);
begin
  Success := (a = '01') and (b = -2) and (c = '03') and (d = -4)
         and (e = '05') and (f = -6) and (g = '07') and (h = -8);
  Assert(Success);
end;

function RunProc5(p: Pointer): Boolean;
type
  TP = procedure(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64);
var
  Str1: shortstring   = '0';
  Str3: AnsiString    = '0';
  Str5: WideString    = '0';
  Str7: UnicodeString = '0';
begin
  Success := False;
  Str1 := Str1 + shortstring(IntToStr(1));
  Str3 := Str3 + AnsiString(IntToStr(3));
  Str5 := Str5 + WideString(IntToStr(5));
  Str7 := Str7 + UnicodeString(IntToStr(7));
  TP(p)(Str1, -2, Str3, -4, Str5, -6, Str7, -8);
  Proc5(Str1, -2, Str3, -4, Str5, -6, Str7, -8);
  Result := Success;
end;

procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64);
begin
  Success := a and (b = 2) and (not c) and (d = 4)
         and e and (f = 6) and (not g) and (h = 8);
  Assert(Success);
end;

function RunProc6(p: Pointer): Boolean;
type
  TP = procedure(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64);
begin
  Success := False;
  TP(p)(True, 2, False, 4, True, 6, False, 8);
  Result := Success;
end;

procedure Proc7(a: ELapeSmallEnum; b: TLapeSmallSet; c: ELapeLargeEnum; d: TLapeLargeSet);
begin
  Success := (a = __LapeSmallEnum3) and (b = [__LapeSmallEnum1, __LapeSmallEnum32])
         and (c = __LapeLargeEnum5) and (d = [__LapeLargeEnum1, __LapeLargeEnum256]);
  Assert(Success);
end;

function RunProc7(p: Pointer): Boolean;
type
  TP = procedure(a: ELapeSmallEnum; b: TLapeSmallSet; c: ELapeLargeEnum; d: TLapeLargeSet);
begin
  Success := False;
  TP(p)(__LapeSmallEnum3, [__LapeSmallEnum1, __LapeSmallEnum32],
        __LapeLargeEnum5, [__LapeLargeEnum1, __LapeLargeEnum256]);
  Result := Success;
end;

procedure Proc8(a: Pointer; b: AnsiChar; c: Variant; d: WideChar);
begin
  Success := (a = nil) and (b = '1') and (c = '234') and (d = '5');
  Assert(Success);
end;

function RunProc8(p: Pointer): Boolean;
type
  TP = procedure(a: Pointer; b: AnsiChar; c: Variant; d: WideChar);
var
  c: Variant;
begin
  Success := False;
  c := IntToSTr(234);
  TP(p)(nil, '1', c, '5');
  Proc8(nil, '1', c, '5');
  Result := Success;
end;

procedure Proc9(a: TStrArray; {b: TStaticStrArray;} c: TIntArray; d: TStaticIntArray);
begin
  Success := (Length(a) = 2) and (Length(c) = 2)
         and (a[0] = 'string0') and (a[1] = 'string1')
         {and (b[1] = 'string2') and (b[5] = 'string3')}
         and (c[0] = 0) and (c[1] = 1)
         and (d[1] = 2) and (d[2] = 3);
  Assert(Success);
end;

function RunProc9(p: Pointer): Boolean;
type
  TP = procedure(a: TStrArray; {b: TStaticStrArray;} c: TIntArray; d: TStaticIntArray);
var
  a: TStrArray;
  b: TStaticStrArray = ('string', '', '', '', 'string');
  c: TIntArray;
  d: TStaticIntArray = (2, 3);
begin
  Success := False;
  SetLength(a, 2);
  SetLength(c, 2);
  a[0] := b[1] + IntToStr(0); a[1] := b[5] + IntToStr(1);
  b[1] := b[1] + IntToStr(2); b[5] := b[5] + IntToStr(3);
  c[0] := 0; c[1] := 1;
  TP(p)(a, {b,} c, d);
  Proc9(a, {b,} c, d);
  Result := Success;
end;

procedure Proc10(a: TShortRec; b: TSmallRec; {c: TStringRec;} d: TLargeRec);
begin
  Success := (a.a = 1) and (b.b = 2) {and (c.c = '03')}
         and (d.a = 4) and (d.b = 5) and (d.c = 6) and (d.d = 7);
  Assert(Success);
end;

function RunProc10(p: Pointer): Boolean;
type
  TP = procedure(a: TShortRec; b: TSmallRec; {c: TStringRec;} d: TLargeRec);
var
  a: TShortRec  = (a: 1);
  b: TSmallRec  = (b: 2);
  //c: TStringRec = (c: '0');
  d: TLargeRec  = (a: 4; b: 5; c: 6; d: 7);
begin
  Success := False;
  //c.c := c.c + IntToStr(3);
  TP(p)(a, b, {c,} d);
  Proc10(a, b, {c,} d);
  Result := Success;
end;

function TestBiDiFFI(Header: lpString; ImportFun: Pointer; RunFun: TRunFun; RunStr: lpString = ''; ImportABI: TFFIABI = FFI_DEFAULT_ABI; ExportABI: TFFIABI = FFI_DEFAULT_ABI): Boolean;
var
  i: TImportClosure;
  e: TExportClosure;
  v: TLapeGlobalVar;
  s: lpString;
  p: Integer;
begin
  i := nil;
  e := nil;
  Result := False;

  try
    with TLapeCompiler.Create(TLapeTokenizerString.Create('begin ' + RunStr + ' end.')) do
    try
      Options := Options + [lcoInitExternalResult];
      addGlobalType('(ESmallFirst = ' + IntToStr(Ord(Low(ELapeSmallEnum))) + ', ESmallLast = ' + IntToStr(Ord(High(ELapeSmallEnum))) + ')', 'TSmallEnum');
      addGlobalType('(ELargeFirst = ' + IntToStr(Ord(Low(ELapeSmallEnum))) + ', ELargeLast = ' + IntToStr(Ord(High(ELapeLargeEnum))) + ')', 'TLargeEnum');
      addGlobalType('set of TSmallEnum', 'TSmallSet');
      addGlobalType('set of TLargeEnum', 'TLargeSet');

      i := LapeImportWrapper(ImportFun, TLapeCompiler(GetSelf()), Header, ImportABI);
      v := addGlobalFunc(Header, i.Func);
      Assert(v.VarType is TLapeType_Method);

      if MethodOfObject(v.VarType) then
        s := 'Self'
      else
        s := '';

      for p := 0 to TLapeType_Method(v.VarType).Params.Count - 1 do
      begin
        if (s <> '') then
          s := s + ',';
        s := s + 'Param' + IntToStr(p);
      end;

      if (TLapeType_Method(v.VarType).Res <> nil) then
        s := 'begin Result := ' + v.Name + '(' + s + '); end;'
      else
        s := 'begin ' + v.Name + '(' + s + '); end;';

      addGlobalFunc(v.VarType as TLapeType_Method, 'TestMe', s);

      if (not Compile()) then
        LapeException('Could not compile ' + Header);

      e := LapeExportWrapper(Globals['TestMe'], ExportABI);
      Result := RunFun(e.Func);
    finally
      Free();
      if (i <> nil) then
        i.Free();
      if (e <> nil) then
        e. Free();
    end;
  except
    on E: Exception do
    begin
      WriteLn('TestFFI Expception: ', e.Message);
      Result := False;
    end
  end;
end;

type
  TRunProc = record
    Fun: Pointer;
    Run: TRunFun;
    Str: lpString;
    Arg: lpString;
  end;

const
  BiDiTests: array[1..9] of TRunProc = (
    (Fun: @Proc1;  Run: @RunProc1;  Str: 'procedure Proc1';                                                                                                                  Arg: 'TestMe();'),
    (Fun: @Proc2;  Run: @RunProc2;  Str: 'procedure Proc2(a, b, c, d, e, f, g, h, i, j: NativeInt)';                                                                         Arg: 'TestMe(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);'),
    (Fun: @Proc3;  Run: @RunProc3;  Str: 'procedure Proc3(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8)';                                Arg: 'TestMe(1, -2, 3, -4, 5, -6, 7, -8);'),
    (Fun: @Proc4;  Run: @RunProc4;  Str: 'procedure Proc4(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64)';                       Arg: 'TestMe(1.5, 2, 3.5, 4, 5.5, 6, 7.5, 8);'),
    //(Fun: @Proc5;  Run: @RunProc5;  Str: 'procedure Proc5(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64)';           Arg: 'TestMe("01", -2, "03", -4, "05", -6, "07", -8);'),
    (Fun: @Proc6;  Run: @RunProc6;  Str: 'procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64)';                    Arg: 'TestMe(True, 2, False, 4, True, 6, False, 8);'),
    (Fun: @Proc7;  Run: @RunProc7;  Str: 'procedure Proc7(a: TSmallEnum; b: TSmallSet; c: TLargeEnum; d: TLargeSet)';                                                        Arg: 'TestMe(TSmallEnum(3), [ESmallFirst, ESmallLast], TLargeEnum(5), [ELargeFirst, ELargeLast]);'),
    (Fun: @Proc8;  Run: @RunProc8;  Str: 'procedure Proc8(a: Pointer; b: AnsiChar; c: Variant; d: WideChar)';                                                                Arg: 'TestMe(nil, "1", "123", "5");'),
    (Fun: @Proc9;  Run: @RunProc9;  Str: 'procedure Proc9(a: array of string; {b: array[1..5] of string;} c: array of Int16; d: array[1..2] of Int16)';                      Arg: 'TestMe(["string0", "string1"], {["string2", "string3"], } [0, 1], [2, 3]);'),
    (Fun: @Proc10; Run: @RunProc10; Str: 'procedure Proc10(a: record a: Int8; end; b: record b: Int16; end; {c: record c: string; end;} d: record a, b, c, d: UInt64; end)'; Arg: 'TestMe([1], [2], [3], [4, 5, 6, 7]);')
  );

var
  t: Integer;
begin
  {$IF DEFINED(MSWINDOWS) AND DECLARED(LoadFFI)}
  if (not FFILoaded()) then
    LoadFFI(
    {$IFDEF Win32}
    '..\bin\win32'
    {$ELSE}
    '..\bin\win64'
    {$ENDIF}
    );
  {$IFEND}

  ExitCode := 0;
  for t := Low(BiDiTests) to High(BiDiTests) do
    if (not TestBiDiFFI(BiDiTests[t].Str, BiDiTests[t].Fun, BiDiTests[t].Run, BiDiTests[t].Arg)) then
    begin
      WriteLn(BiDiTests[t].Str, ' failed');
      ExitCode := -1;
      Exit;
    end;

  WriteLn('Ran ', Length(BiDiTests), ' tests successfully');
end.

