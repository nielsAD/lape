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
  Success := (a = 'string1') and (b = -2) and (c = 'string3') and (d = -4)
         and (e = 'string5') and (f = -6) and (g = 'string7') and (h = -8);
end;

function RunProc5(p: Pointer): Boolean;
type
  TP = procedure(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64);
begin
  Success := False;
  TP(p)('string1', -2, 'string3', -4, 'string5', -6, 'string7', -8);
  Result := Success;
end;

procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64);
begin
  Success := a and (b = 2) and (not c) and (d = 4)
         and e and (f = 6) and (not g) and (h = 8);
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
  Success := (a = nil) and (b = '1') and (c = 2) and (d = '3');
end;

function RunProc8(p: Pointer): Boolean;
type
  TP = procedure(a: Pointer; b: AnsiChar; c: Variant; d: WideChar);
begin
  Success := False;
  TP(p)(nil, '1', 2, '3');
  Result := Success;
end;

procedure Proc9(a: TStrArray; b: TStaticStrArray; c: TIntArray; d: TStaticIntArray);
begin
  WriteLn('In: ', PtrUInt(Pointer(Pointer(@a[0])-8)^), ' ', PtrUInt(Pointer(Pointer(@a[0])-16)^));
  Success := (Length(a) = 2) and (Length(c) = 2)
         and (a[0] = 'string0') and (a[1] = 'string1')
         and (b[1] = 'string2') and (b[5] = 'string3')
         and (c[0] = 0) and (c[1] = 1)
         and (d[1] = 2) and (d[2] = 3);
end;

function RunProc9(p: Pointer): Boolean;
type
  TP = procedure(a: TStrArray; b: TStaticStrArray; c: TIntArray; d: TStaticIntArray);
var
  a: TStrArray;
  c: TIntArray;
const
  b: TStaticStrArray = ('string2', '', '', '', 'string3');
  d: TStaticIntArray = (2, 3);
begin
  Success := False;
  SetLength(a, 2); a[0] := 'string0'; a[1] := 'string1';
  SetLength(c, 2); c[0] := 0; c[1] := 1;
  WriteLn('Before: ', PtrUInt(Pointer(Pointer(@a[0])-8)^), ' ', PtrUInt(Pointer(Pointer(@a[0])-16)^));
  TP(p)(a, b, c, d);
  WriteLn('After: ', PtrUInt(Pointer(Pointer(@a[0])-8)^), ' ', PtrUInt(Pointer(Pointer(@a[0])-16)^));
  Result := Success;
end;

function TestFFI(Header: lpString; ImportFun: Pointer; RunFun: TRunFun; ImportABI: TFFIABI = FFI_DEFAULT_ABI; ExportABI: TFFIABI = FFI_DEFAULT_ABI): Boolean;
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
    with TLapeCompiler.Create(TLapeTokenizerString.Create('begin end.')) do
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
        s := 'begin WriteLn('+s+'); Result := ' + v.Name + '(' + s + '); end;'
      else
        s := 'begin WriteLn('+s+');' + v.Name + '(' + s + '); end;';

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
  except on E: Exception do
    WriteLn('TestFFI Expception: ', e.Message);
  end;
end;

type
  TRunProc = record
    Fun: Pointer;
    Run: TRunFun;
    Str: lpString;
  end;

const
  Tests: array[0..0] of TRunProc = (
    //(Fun: @Proc1; Run: @RunProc1; Str: 'procedure Proc1'),
    //(Fun: @Proc2; Run: @RunProc2; Str: 'procedure Proc2(a, b, c, d, e, f, g, h, i, j: NativeInt)'),
    //(Fun: @Proc3; Run: @RunProc3; Str: 'procedure Proc3(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8)'),
    //(Fun: @Proc4; Run: @RunProc4; Str: 'procedure Proc4(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64)'),
    //(Fun: @Proc5; Run: @RunProc5; Str: 'procedure Proc5(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64)'),
    //(Fun: @Proc6; Run: @RunProc6; Str: 'procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64)'),
    //(Fun: @Proc7; Run: @RunProc7; Str: 'procedure Proc7(a: TSmallEnum; b: TSmallSet; c: TLargeEnum; d: TLargeSet)'),
    //(Fun: @Proc8; Run: @RunProc8; Str: 'procedure Proc8(a: Pointer; b: AnsiChar; c: Variant; d: WideChar)'),
    (Fun: @Proc9; Run: @RunProc9; Str: 'procedure Proc9(a: array of string; b: array[1..5] of string; c: array of Int16; d: array[1..2] of Int16)')
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
  for t := Low(Tests) to High(Tests) do
    if (not TestFFI(Tests[t].Str, Tests[t].Fun, Tests[t].Run)) then
    begin
      WriteLn(Tests[t].Str, ' failed');
      ExitCode := -1;
      Exit;
    end;

  WriteLn('Ran ', Length(Tests), ' tests successfully');
end.

