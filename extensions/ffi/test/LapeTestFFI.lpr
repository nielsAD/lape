program LapeTestFFI;

uses
  sysutils,
  lptypes, lpvartypes, lpcompiler, lpparser, lpinterpreter, lpexceptions, lpffi, ffi;

type
  TRunFun = function(p: Pointer): Boolean;

  TStrArr = array of lpString;
  TIntArr = array of Int16;
  TStatStrArr = array[1..5] of lpString;
  TStatIntArr = array[1..2] of Int16;

  TShortRec =        record a: Int8; end;
  TPackRec  = packed record b: Int16; x: Int8; end;
  TStrRec   =        record c: lpString; end;
  TLargeRec =        record a: UInt8; b,c: UInt64; d: UInt8; end;

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

procedure Proc9(a: TStrArr; constref b: TStatStrArr; c: TIntArr; d: TStatIntArr);
begin
  Success := (Length(a) = 2) and (Length(c) = 2)
         and (a[0] = 'string0') and (a[1] = 'string1')
         and (b[1] = 'string2') and (b[5] = 'string3')
         and (c[0] = 0) and (c[1] = 1)
         and (d[1] = 2) and (d[2] = 3);
  Assert(Success);
end;

function RunProc9(p: Pointer): Boolean;
type
  TP = procedure(a: TStrArr; b: TStatStrArr; c: TIntArr; d: TStatIntArr);
var
  a: TStrArr;
  b: TStatStrArr = ('string', '', '', '', 'string');
  c: TIntArr;
  d: TStatIntArr = (2, 3);
begin
  Success := False;
  SetLength(a, 2);
  SetLength(c, 2);
  a[0] := b[1] + IntToStr(0); a[1] := b[5] + IntToStr(1);
  b[1] := b[1] + IntToStr(2); b[5] := b[5] + IntToStr(3);
  c[0] := 0; c[1] := 1;
  TP(p)(a, b, c, d);
  Proc9(a, b, c, d);
  Result := Success;
end;

procedure Proc10(a: TShortRec; b: TPackRec; constref c: TStrRec; d: TLargeRec);
begin
  Success := (a.a = 1) and (b.b = 2) and (c.c = '03')
         and (d.a = 4) and (d.b = 5) and (d.c = 6) and (d.d = 7);
  Assert(Success);
end;

function RunProc10(p: Pointer): Boolean;
type
  TP = procedure(a: TShortRec; b: TPackRec; constref c: TStrRec; d: TLargeRec);
var
  a: TShortRec  = (a: 1);
  b: TPackRec   = (b: 2; x: 22);
  c: TStrRec    = (c: '0');
  d: TLargeRec  = (a: 4; b: 5; c: 6; d: 7);
begin
  Success := False;
  c.c := c.c + IntToStr(3);
  TP(p)(a, b, c, d);
  Proc10(a, b, c, d);
  Result := Success;
end;

function Func1(const a: NativeInt): NativeInt;
begin
  Result := a + 1;
end;

function RunFunc1(f: Pointer): Boolean;
type
  TF = function(const a: NativeInt): NativeInt;
begin
  Result := TF(f)(10) = 11;
end;

function Func2(const a, b, c: Int8): UInt8;
begin
  Result := a + b + c;
end;

function RunFunc2(f: Pointer): Boolean;
type
  TF = function(const a, b, c: Int8): UInt8;
begin
  Result := TF(f)(15, 14, 13) = 42;
end;

function Func3(const a: Single): Single;
begin
  Result := a * 2.0;
end;

function RunFunc3(f: Pointer): Boolean;
type
  TF = function(const a: Single): Single;
begin
  Result := TF(f)(2.5) = 5.0;
end;

function Func4(const a: Double): Double;
begin
  Result := a * 3.0;
end;

function RunFunc4(f: Pointer): Boolean;
type
  TF = function(const a: Double): Double;
begin
  Result := TF(f)(5) = 15.0;
end;

function Func5(const a: Extended): Extended;
begin
  Result := a * 10.0;
end;

function RunFunc5(f: Pointer): Boolean;
type
  TF = function(const a: Extended): Extended;
begin
  Result := TF(f)(10) = 100.0;
end;

function Func6(const a: Currency): Currency;
begin
  Result := a / 2.0;
end;

function RunFunc6(f: Pointer): Boolean;
type
  TF = function(const a: Currency): Currency;
begin
  Result := TF(f)(8.4) = 4.2;
end;

function Func7(const a: Boolean): Boolean;
begin
  Result := not a;
end;

function RunFunc7(f: Pointer): Boolean;
type
  TF = function(const a: Boolean): Boolean;
begin
  Result := not TF(f)(True);
end;

function Func8(const a: shortstring): shortstring;
begin
  Result := a + a;
end;

function RunFunc8(f: Pointer): Boolean;
type
  TF = function(const a: shortstring): shortstring;
begin
  Result := TF(f)('11') = '1111';
end;

function Func9(const a: lpString): lpString;
begin
  Result := a + a;
end;

function RunFunc9(f: Pointer): Boolean;
type
  TF = function(const a: lpString): lpString;
begin
  Result := TF(f)('22') = '2222';
end;

function Func10(const a: Variant): Variant;
begin
  Result := StrToInt(a) + 1;
end;

function RunFunc10(f: Pointer): Boolean;
type
  TF = function(const a: Variant): Variant;
begin
  Result := TF(f)('42') = 43;
end;

function Func11(const a: TLapeSmallSet): TLapeSmallSet;
begin
  Assert(a = [Low(ELapeSmallEnum)]);
  Result := [High(ELapeSmallEnum)];
end;

function RunFunc11(f: Pointer): Boolean;
type
  TF = function(const a: TLapeSmallSet): TLapeSmallSet;
begin
  Result := TF(f)([Low(ELapeSmallEnum)]) = [High(ELapeSmallEnum)];
end;

function Func12(const a: TLapeLargeSet): TLapeLargeSet;
begin
  Assert(a = [High(ELapeLargeEnum)]);
  Result := [Low(ELapeLargeEnum)];
end;

function RunFunc12(f: Pointer): Boolean;
type
  TF = function(const a: TLapeLargeSet): TLapeLargeSet;
begin
  Result := TF(f)([High(ELapeLargeEnum)]) = [Low(ELapeLargeEnum)];
end;

function Func13(const a: TIntArr): TIntArr;
var
  i: Integer;
begin
  Result := a;
  for i := Low(Result) to High(Result) do
    Result[i] := Result[i] * 10;
end;

function RunFunc13(f: Pointer): Boolean;
type
  TF = function(const a: TIntArr): TIntArr;
var
  a: TIntArr;
begin
  SetLength(a, 3); a[0] := 10; a[1] := 20; a[2] := 30;
  a := TF(f)(a);
  Result := (a[0] = 100) and (a[1] = 200) and (a[2] = 300);
end;

function Func14(const a: TStrArr): TStrArr;
begin
  Assert(Length(a) = 2);
  Result := a;
  Result[0] := StringReplace(a[0], 'hll',  'hello', []);
  Result[1] := StringReplace(a[1], 'wrld', 'world', []);
end;

function RunFunc14(f: Pointer): Boolean;
type
  TF = function(const a: TStrArr): TStrArr;
var
  a: TStrArr;
begin
  SetLength(a, 2); a[0] := 'hll'; a[1] := 'wrld';
  a := TF(f)(a);
  Result := (a[0] = 'hello') and (a[1] = 'world');
end;

function Func15(const a: TStatIntArr): TStatIntArr;
var
  i: Integer;
begin
  for i := Low(Result) to High(Result) do
    Result[i] := a[i] + 2;
end;

function RunFunc15(f: Pointer): Boolean;
type
  TF = function(const a: TStatIntArr): TStatIntArr;
const
  a: TStatIntArr = (1, 2);
var
  b: TStatIntArr;
begin
  b := TF(f)(a);
  Result := (b[1] = 3) and (b[2] = 4);
end;

function Func16(const a: TStatStrArr): TStatStrArr;
var
  i: Integer;
begin
  for i := Low(Result) to High(Result) do
    if (a[i] <> '') then
      Result[i] := a[i] + '!';
end;

function RunFunc16(f: Pointer): Boolean;
type
  TF = function(const a: TStatStrArr): TStatStrArr;
const
  a: TStatStrArr = ('1', '', '', '', '2');
var
  b: TStatStrArr;
begin
  b := TF(f)(a);
  Result := (b[1] = '1!') and (b[5] = '2!');
end;

function Func17(const a: TShortRec): TShortRec;
begin
  Result.a := a.a div 2;
end;

function RunFunc17(f: Pointer): Boolean;
type
  TF = function(const a: TShortRec): TShortRec;
var
  r: TShortRec;
begin
  r.a := 100;
  r := TF(f)(r);
  Result := r.a = 50;
end;

function Func18(const a: TPackRec): TPackRec;
begin
  Result.b := a.b + a.x;
  Result.x := a.x;
end;

function RunFunc18(f: Pointer): Boolean;
type
  TF = function(const a: TPackRec): TPackRec;
var
  r: TPackRec;
begin
  r.b := 1;
  r.x := 10;
  r := TF(f)(r);
  Result := (r.b = 11) and (r.x = 10);
end;

function Func19(constref a: TStrRec): TStrRec;
begin
  Result.c := '0' + a.c;
end;

function RunFunc19(f: Pointer): Boolean;
type
  TF = function(constref a: TStrRec): TStrRec;
var
  r: TStrRec;
begin
  r.c := '123';
  r := TF(f)(r);
  Result := (r.c = '0123');
end;

function Func20(const a: TLargeRec): TLargeRec;
begin
  Result.a := a.d;
  Result.b := a.c;
  Result.c := a.b;
  Result.d := a.a;
end;

function RunFunc20(f: Pointer): Boolean;
type
  TF = function(const a: TLargeRec): TLargeRec;
var
  r: TLargeRec;
begin
  r.a := 1; r.b := 2; r.c := 3; r.d := 4;
  r := TF(f)(r);
  Result := (r.a = 4) and (r.b = 3) and (r.c = 2) and (r.d = 1);
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
      Options := Options + [lcoAssertions, lcoInitExternalResult];

      addGlobalType('(ESmallFirst = ' + IntToStr(Ord(Low(ELapeSmallEnum))) + ', ESmallLast = ' + IntToStr(Ord(High(ELapeSmallEnum))) + ')', 'TSmallEnum');
      addGlobalType('(ELargeFirst = ' + IntToStr(Ord(Low(ELapeSmallEnum))) + ', ELargeLast = ' + IntToStr(Ord(High(ELapeLargeEnum))) + ')', 'TLargeEnum');
      addGlobalType('set of TSmallEnum', 'TSmallSet');
      addGlobalType('set of TLargeEnum', 'TLargeSet');

      addGlobalType('array of Int16',  'TIntArr');
      addGlobalType('array of string', 'TStrArr');
      addGlobalType('array[1..2] of Int16',  'TStatIntArr');
      addGlobalType('array[1..5] of string', 'TStatStrArr');

      addGlobalType('       record a: Int8;           end', 'TShortRec');
      addGlobalType('packed record b: Int16; x: Int8; end', 'TPackRec');
      addGlobalType('       record c: string;         end', 'TStrRec');
      addGlobalType('       record a: UInt8; b,c: UInt64; d: UInt8; end', 'TLargeRec');

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
        s := 'begin '           + v.Name + '(' + s + '); end;';

      addGlobalFunc(v.VarType as TLapeType_Method, 'TestMe', s);

      if (not Compile()) then
        LapeException('Could not compile ' + Header);

      e := LapeExportWrapper(Globals['TestMe'], ExportABI);

      RunCode(Emitter.Code);
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
      WriteLn('TestFFI Exception: ', e.Message);
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
  BiDiTests: array[1..30] of TRunProc = (
    (Fun: @Proc1;  Run: @RunProc1;  Str: 'procedure Proc1';                                                                                                        Arg: 'TestMe();'),
    (Fun: @Proc2;  Run: @RunProc2;  Str: 'procedure Proc2(a, b, c, d, e, f, g, h, i, j: NativeInt)';                                                               Arg: 'TestMe(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);'),
    (Fun: @Proc3;  Run: @RunProc3;  Str: 'procedure Proc3(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8)';                      Arg: 'TestMe(1, -2, 3, -4, 5, -6, 7, -8);'),
    (Fun: @Proc4;  Run: @RunProc4;  Str: 'procedure Proc4(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64)';             Arg: 'TestMe(1.5, 2, 3.5, 4, 5.5, 6, 7.5, 8);'),
    (Fun: @Proc5;  Run: @RunProc5;  Str: 'procedure Proc5(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64)'; Arg: 'TestMe("01", -2, "03", -4, "05", -6, "07", -8);'),
    (Fun: @Proc6;  Run: @RunProc6;  Str: 'procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64)';          Arg: 'TestMe(True, 2, False, 4, True, 6, False, 8);'),
    (Fun: @Proc7;  Run: @RunProc7;  Str: 'procedure Proc7(a: TSmallEnum; b: TSmallSet; c: TLargeEnum; d: TLargeSet)';                                              Arg: 'TestMe(TSmallEnum(2), [ESmallFirst, ESmallLast], TLargeEnum(4), [ELargeFirst, ELargeLast]);'),
    (Fun: @Proc8;  Run: @RunProc8;  Str: 'procedure Proc8(a: Pointer; b: AnsiChar; c: Variant; d: WideChar)';                                                      Arg: 'TestMe(nil, "1", "234", "5");'),
    (Fun: @Proc9;  Run: @RunProc9;  Str: 'procedure Proc9(a: TStrArr; constref b: TStatStrArr; c: TIntArr; d: TStatIntArr)';                                       Arg: 'TestMe(["string0", "string1"], ["string2", "","","", "string3"], [0, 1], [2, 3]);'),
    (Fun: @Proc10; Run: @RunProc10; Str: 'procedure Proc10(a: TShortRec; b: TPackRec; constref c: TStrRec; d: TLargeRec)';                                         Arg: 'TestMe([1], [2, 22], ["03"], [4, 5, 6, 7]);'),

    (Fun: @Func1;  Run: @RunFunc1;  Str: 'function Func1(const a: NativeInt): NativeInt';         Arg: 'Assert(TestMe(10) = 11);'),
    (Fun: @Func2;  Run: @RunFunc2;  Str: 'function Func2(const a, b, c: Int8): UInt8';            Arg: 'Assert(TestMe(15, 14, 13) = 42);'),
    (Fun: @Func3;  Run: @RunFunc3;  Str: 'function Func3(const a: Single): Single';               Arg: 'Assert(TestMe(2.5) = 5);'),
    (Fun: @Func4;  Run: @RunFunc4;  Str: 'function Func4(const a: Double): Double';               Arg: 'Assert(TestMe(5)   = 15);'),
    (Fun: @Func5;  Run: @RunFunc5;  Str: 'function Func5(const a: Extended): Extended';           Arg: 'Assert(TestMe(10)  = 100);'),
    (Fun: @Func6;  Run: @RunFunc6;  Str: 'function Func6(const a: Currency): Currency';           Arg: 'Assert(TestMe(8.4) = 4.2);'),
    (Fun: @Func7;  Run: @RunFunc7;  Str: 'function Func7(const a: Boolean): Boolean';             Arg: 'Assert(not TestMe(True));'),
    (Fun: @Func8;  Run: @RunFunc8;  Str: 'function Func8(const a: shortstring): shortstring';     Arg: 'Assert(TestMe("11") = "1111");'),
    (Fun: @Func9;  Run: @RunFunc9;  Str: 'function Func9(const a: string): string';               Arg: 'Assert(TestMe("22") = "2222");'),
    (Fun: @Func10; Run: @RunFunc10; Str: 'function Func10(const a: Variant): Variant';            Arg: 'Assert(TestMe("42") = 43);'),
    (Fun: @Func11; Run: @RunFunc11; Str: 'function Func11(const a: TSmallSet): TSmallSet';        Arg: 'Assert(TestMe([ESmallFirst]) = [ESmallLast]);'),
    (Fun: @Func12; Run: @RunFunc12; Str: 'function Func12(const a: TLargeSet): TLargeSet';        Arg: 'Assert(TestMe([ELargeLast]) = [ELargeFirst]);'),
    (Fun: @Func13; Run: @RunFunc13; Str: 'function Func13(const a: TIntArr): TIntArr';            Arg: 'Assert(TestMe([10, 20, 30])[2] = 300);'),
    (Fun: @Func14; Run: @RunFunc14; Str: 'function Func14(const a: TStrArr): TStrArr';            Arg: 'Assert(TestMe(["hll", "wrld"])[1] = "world");'),
    (Fun: @Func15; Run: @RunFunc15; Str: 'function Func15(const a: TStatIntArr): TStatIntArr';    Arg: 'Assert(TestMe([1..2])[2] = 4);'),
    (Fun: @Func16; Run: @RunFunc16; Str: 'function Func16(constref a: TStatStrArr): TStatStrArr'; Arg: 'Assert(TestMe(["1", "","","", "2"])[5] = "2!");'),
    (Fun: @Func17; Run: @RunFunc17; Str: 'function Func17(const a: TShortRec): TShortRec';        Arg: 'Assert(TestMe([100]) = [50]);'),
    (Fun: @Func18; Run: @RunFunc18; Str: 'function Func18(const a: TPackRec): TPackRec';          Arg: 'Assert(TestMe([1, 10]) = [11, 10]);'),
    (Fun: @Func19; Run: @RunFunc19; Str: 'function Func19(constref a: TStrRec): TStrRec';         Arg: 'Assert(TestMe(["123"]) = ["0123"]);'),
    (Fun: @Func20; Run: @RunFunc20; Str: 'function Func20(const a: TLargeRec): TLargeRec';        Arg: 'Assert(TestMe([1, 2, 3, 4]) = [4, 3, 2, 1]);')
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

  Assert(FFILoaded());
  ExitCode := 0;

  for t := Low(BiDiTests) to High(BiDiTests) do
    if (not TestBiDiFFI(BiDiTests[t].Str, BiDiTests[t].Fun, BiDiTests[t].Run, BiDiTests[t].Arg)) then
    begin
      WriteLn('"', BiDiTests[t].Str, '" failed');
      Inc(ExitCode);
    end;

  WriteLn('Ran ', Length(BiDiTests) - ExitCode, '/', Length(BiDiTests), ' tests successfully');
end.

