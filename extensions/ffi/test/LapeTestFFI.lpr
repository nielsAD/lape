program LapeTestFFI;

uses
  SysUtils, {$IFDEF FPC}LCLIntf,{$ELSE}{$IFDEF MSWINDOWS}Windows,{$ENDIF}{$ENDIF}
  lptypes, lpvartypes, lpcompiler, lptree, lpparser, lpinterpreter, lpmessages,
  lpffiwrappers, ffi;

type
  TRunFun = function(p: Pointer): Boolean;

  TStrArr = array of lpString;
  TIntArr = array of Int16;
  TStatPackArr = array[0..2] of Int8;
  TStatStrArr  = array[1..5] of lpString;
  TStatIntArr  = array[1..2] of Int16;

  TShortRec =        record a: Int8; end;
  TPackRec  = packed record b: Int16; x: Int8; end;
  TStrRec   =        record c: lpString; end;
  TLargeRec =        record a: UInt8; b,c: UInt64; d: UInt8; end;

{$IFDEF FFITest_REGISTER}
  {$IF DECLARED(FFI_REGISTER)} const TEST_ABI = FFI_REGISTER; {$IFEND}
{$ELSE}{$IFDEF FFITest_PASCAL}
  {$IF DECLARED(FFI_PASCAL)}   const TEST_ABI = FFI_PASCAL;   {$IFEND}
{$ELSE}{$IFDEF FFITest_CDECL}
  {$IF DECLARED(FFI_CDECL)}    const TEST_ABI = FFI_CDECL;    {$IFEND}
{$ELSE}{$IFDEF FFITest_STDCALL}
  {$IF DECLARED(FFI_STDCALL)}  const TEST_ABI = FFI_STDCALL;  {$IFEND}
{$ELSE}
                               const TEST_ABI = FFI_DEFAULT_ABI;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

{$IFNDEF FPC} //Internal error workaround
{$IF NOT DECLARED(GetTickCount)}
function GetTickCount: UInt32;
begin
  Result := Trunc(Now() * 24 * 60 * 60 * 1000);
end;
{$IFEND}
{$ENDIF}

var
  Success: Boolean = False;

procedure Proc1; {$I cconv.inc}
begin
  Success := True;
end;

function RunProc1(p: Pointer): Boolean;
type
  TP = procedure; {$I cconv.inc}
begin
  Success := False;
  TP(p)();
  Result := Success;
end;

procedure Proc2(a, b, c, d, e, f, g, h, i, j: NativeInt); {$I cconv.inc}
begin
  Success := (a = 1) and (b =- 2) and (c = 3) and (d = -4) and (e = 5) and (f = -6)
         and (g = 7) and (h = -8) and (i = 9) and (j = -10);
  Assert(Success);
end;

function RunProc2(p: Pointer): Boolean;
type
  TP = procedure(a, b, c, d, e, f, g, h, i, j: NativeInt); {$I cconv.inc}
begin
  Success := False;
  TP(p)(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);
  Result := Success;
end;

procedure Proc3(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8); {$I cconv.inc}
begin
  Success := (a = 1) and (b = -2) and (c = 3) and (d = -4)
         and (e = 5) and (f = -6) and (g = 7) and (h = -8);
  Assert(Success);
end;

function RunProc3(p: Pointer): Boolean;
type
  TP = procedure(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8); {$I cconv.inc}
begin
  Success := False;
  TP(p)(1, -2, 3, -4, 5, -6, 7, -8);
  Result := Success;
end;

procedure Proc4(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64); {$I cconv.inc}
begin
  Success := (a = 1.5) and (b = 2) and (c = 3.5) and (d = 4)
         and (e = 5.5) and (f = 6) and (g = 7.5) and (h = 8);
  Assert(Success);
end;

function RunProc4(p: Pointer): Boolean;
type
  TP = procedure(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64); {$I cconv.inc}
begin
  Success := False;
  TP(p)(1.5, 2, 3.5, 4, 5.5, 6, 7.5, 8);
  Result := Success;
end;

procedure Proc5(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64); {$I cconv.inc}
begin
  Success := (a = '01') and (b = -2) and (c = '03') and (d = -4)
         and (e = '05') and (f = -6) and (g = '07') and (h = -8);
  Assert(Success);
end;

function RunProc5(p: Pointer): Boolean;
type
  TP = procedure(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64); {$I cconv.inc}
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

procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64); {$I cconv.inc}
begin
  Success := a and (b = 2) and (not c) and (d = 4)
         and e and (f = 6) and (not g) and (h = 8);
  Assert(Success);
end;

function RunProc6(p: Pointer): Boolean;
type
  TP = procedure(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64); {$I cconv.inc}
begin
  Success := False;
  TP(p)(True, 2, False, 4, True, 6, False, 8);
  Result := Success;
end;

procedure Proc7(a: ELapeSmallEnum; b: TLapeSmallSet; c: ELapeLargeEnum; d: TLapeLargeSet); {$I cconv.inc}
begin
  Success := (a = __LapeSmallEnum3) and (b = [__LapeSmallEnum1, __LapeSmallEnum32])
         and (c = __LapeLargeEnum5) and (d = [__LapeLargeEnum1, __LapeLargeEnum256]);
  Assert(Success);
end;

function RunProc7(p: Pointer): Boolean;
type
  TP = procedure(a: ELapeSmallEnum; b: TLapeSmallSet; c: ELapeLargeEnum; d: TLapeLargeSet); {$I cconv.inc}
begin
  Success := False;
  TP(p)(__LapeSmallEnum3, [__LapeSmallEnum1, __LapeSmallEnum32],
        __LapeLargeEnum5, [__LapeLargeEnum1, __LapeLargeEnum256]);
  Result := Success;
end;

procedure Proc8(a: Pointer; b: AnsiChar; c: Variant; d: WideChar); {$I cconv.inc}
begin
  Success := (a = nil) and (b = '1') and (c = '234') and (d = '5');
  Assert(Success);
end;

function RunProc8(p: Pointer): Boolean;
type
  TP = procedure(a: Pointer; b: AnsiChar; c: Variant; d: WideChar); {$I cconv.inc}
var
  c: Variant;
begin
  Success := False;
  c := IntToSTr(234);
  TP(p)(nil, '1', c, '5');
  Proc8(nil, '1', c, '5');
  Result := Success;
end;

procedure Proc9(a: TStatPackArr; b: TStrArr; constref c: TStatStrArr; d: TIntArr; e: TStatIntArr); {$I cconv.inc}
begin
  Success := (Length(b) = 2) and (Length(d) = 2)
         and (a[0] = 1) and (a[2] = 3)
         and (b[0] = 'string0') and (b[1] = 'string1')
         and (c[1] = 'string2') and (c[5] = 'string3')
         and (d[0] = 0) and (d[1] = 1)
         and (e[1] = 2) and (e[2] = 3);
  Assert(Success);
end;

function RunProc9(p: Pointer): Boolean;
type
  TP = procedure(a: TStatPackArr; b: TStrArr; constref c: TStatStrArr; d: TIntArr; e: TStatIntArr); {$I cconv.inc}
var
  a: TStatPackArr = (1, 2, 3);
  b: TStrArr;
  c: TStatStrArr = ('string', '', '', '', 'string');
  d: TIntArr;
  e: TStatIntArr = (2, 3);
begin
  Success := False;
  SetLength(b, 2);
  SetLength(d, 2);
  b[0] := c[1] + IntToStr(0); b[1] := c[5] + IntToStr(1);
  c[1] := c[1] + IntToStr(2); c[5] := c[5] + IntToStr(3);
  d[0] := 0; d[1] := 1;
  TP(p)(a, b, c, d, e);
  Proc9(a, b, c, d, e);
  Result := Success;
end;

procedure Proc10(a: TShortRec; b: TPackRec; constref c: TStrRec; d: TLargeRec); {$I cconv.inc}
begin
  Success := (a.a = 1) and (b.b = 2) and (c.c = '03')
         and (d.a = 4) and (d.b = 5) and (d.c = 6) and (d.d = 7);
  Assert(Success);
end;

function RunProc10(p: Pointer): Boolean;
type
  TP = procedure(a: TShortRec; b: TPackRec; constref c: TStrRec; d: TLargeRec); {$I cconv.inc}
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

function Func1(const a: NativeInt): NativeInt; {$I cconv.inc}
begin
  Result := a + 1;
end;

function RunFunc1(f: Pointer): Boolean;
type
  TF = function(const a: NativeInt): NativeInt; {$I cconv.inc}
begin
  Result := TF(f)(10) = 11;
end;

function Func2(const a, b, c: Int8): UInt8; {$I cconv.inc}
begin
  Result := a + b + c;
end;

function RunFunc2(f: Pointer): Boolean;
type
  TF = function(const a, b, c: Int8): UInt8; {$I cconv.inc}
begin
  Result := TF(f)(15, 14, 13) = 42;
end;

function Func3(const a: Single): Single; {$I cconv.inc}
begin
  Result := a * 2.0;
end;

function RunFunc3(f: Pointer): Boolean;
type
  TF = function(const a: Single): Single; {$I cconv.inc}
begin
  Result := TF(f)(2.5) = 5.0;
end;

function Func4(const a: Double): Double; {$I cconv.inc}
begin
  Result := a * 3.0;
end;

function RunFunc4(f: Pointer): Boolean;
type
  TF = function(const a: Double): Double; {$I cconv.inc}
begin
  Result := TF(f)(5) = 15.0;
end;

function Func5(const a: Extended): Extended; {$I cconv.inc}
begin
  Result := a * 10.0;
end;

function RunFunc5(f: Pointer): Boolean;
type
  TF = function(const a: Extended): Extended; {$I cconv.inc}
begin
  Result := TF(f)(10) = 100.0;
end;

function Func6(const a: Currency): Currency; {$I cconv.inc}
begin
  Result := a / 2.0;
end;

function RunFunc6(f: Pointer): Boolean;
type
  TF = function(const a: Currency): Currency; {$I cconv.inc}
begin
  Result := TF(f)(8.4) = 4.2;
end;

function Func7(const a: Boolean): Boolean; {$I cconv.inc}
begin
  Result := not a;
end;

function RunFunc7(f: Pointer): Boolean;
type
  TF = function(const a: Boolean): Boolean; {$I cconv.inc}
begin
  Result := not TF(f)(True);
end;

function Func8(const a: shortstring): shortstring; {$I cconv.inc}
begin
  Result := a + a;
end;

function RunFunc8(f: Pointer): Boolean;
type
  TF = function(const a: shortstring): shortstring; {$I cconv.inc}
begin
  Result := TF(f)('11') = '1111';
end;

function Func9(const a: lpString): lpString; {$I cconv.inc}
begin
  Result := a + a;
end;

function RunFunc9(f: Pointer): Boolean;
type
  TF = function(const a: lpString): lpString; {$I cconv.inc}
begin
  Result := TF(f)('22') = '2222';
end;

function Func10(const a: Variant): Variant; {$I cconv.inc}
begin
  Result := StrToInt(a) + 1;
end;

function RunFunc10(f: Pointer): Boolean;
type
  TF = function(const a: Variant): Variant; {$I cconv.inc}
begin
  Result := TF(f)('42') = 43;
end;

function Func11(const a: TLapeSmallSet): TLapeSmallSet; {$I cconv.inc}
begin
  Assert(a = [Low(ELapeSmallEnum)]);
  Result := [High(ELapeSmallEnum)];
end;

function RunFunc11(f: Pointer): Boolean;
type
  TF = function(const a: TLapeSmallSet): TLapeSmallSet; {$I cconv.inc}
begin
  Result := TF(f)([Low(ELapeSmallEnum)]) = [High(ELapeSmallEnum)];
end;

function Func12(const a: TLapeLargeSet): TLapeLargeSet; {$I cconv.inc}
begin
  Assert(a = [High(ELapeLargeEnum)]);
  Result := [Low(ELapeLargeEnum)];
end;

function RunFunc12(f: Pointer): Boolean;
type
  TF = function(const a: TLapeLargeSet): TLapeLargeSet; {$I cconv.inc}
begin
  Result := TF(f)([High(ELapeLargeEnum)]) = [Low(ELapeLargeEnum)];
end;

function Func13(const a: TIntArr): TIntArr; {$I cconv.inc}
var
  i: Integer;
begin
  Result := a;
  for i := Low(Result) to High(Result) do
    Result[i] := Result[i] * 10;
end;

function RunFunc13(f: Pointer): Boolean;
type
  TF = function(const a: TIntArr): TIntArr; {$I cconv.inc}
var
  a: TIntArr;
begin
  SetLength(a, 3); a[0] := 10; a[1] := 20; a[2] := 30;
  a := TF(f)(a);
  Result := (a[0] = 100) and (a[1] = 200) and (a[2] = 300);
end;

function Func14(const a: TStrArr): TStrArr; {$I cconv.inc}
begin
  Assert(Length(a) = 2);
  Result := a;
  Result[0] := StringReplace(a[0], 'hll',  'hello', []);
  Result[1] := StringReplace(a[1], 'wrld', 'world', []);
end;

function RunFunc14(f: Pointer): Boolean;
type
  TF = function(const a: TStrArr): TStrArr; {$I cconv.inc}
var
  a: TStrArr;
begin
  SetLength(a, 2); a[0] := 'hll'; a[1] := 'wrld';
  a := TF(f)(a);
  Result := (a[0] = 'hello') and (a[1] = 'world');
end;

function Func15(const a: TStatPackArr): TStatPackArr; {$I cconv.inc}
var
  i: Integer;
begin
  Result[0] := a[0];
  for i := Low(Result)+1 to High(Result) do
    Result[i] := Result[i-1] + a[i];
end;

function RunFunc15(f: Pointer): Boolean;
type
  TF = function(const a: TStatPackArr): TStatPackArr; {$I cconv.inc}
const
  a: TStatPackArr = (1, 2, 3);
var
  b: TStatPackArr;
begin
  b := TF(f)(a);
  Result := (b[2] = 6);
end;

function Func16(const a: TStatIntArr): TStatIntArr; {$I cconv.inc}
var
  i: Integer;
begin
  for i := Low(Result) to High(Result) do
    Result[i] := a[i] + 2;
end;

function RunFunc16(f: Pointer): Boolean;
type
  TF = function(const a: TStatIntArr): TStatIntArr; {$I cconv.inc}
const
  a: TStatIntArr = (1, 2);
var
  b: TStatIntArr;
begin
  b := TF(f)(a);
  Result := (b[1] = 3) and (b[2] = 4);
end;

function Func17(const a: TStatStrArr): TStatStrArr; {$I cconv.inc}
var
  i: Integer;
begin
  for i := Low(Result) to High(Result) do
    if (a[i] <> '') then
      Result[i] := a[i] + '!';
end;

function RunFunc17(f: Pointer): Boolean;
type
  TF = function(const a: TStatStrArr): TStatStrArr; {$I cconv.inc}
const
  a: TStatStrArr = ('1', '', '', '', '2');
var
  b: TStatStrArr;
begin
  b := TF(f)(a);
  Result := (b[1] = '1!') and (b[5] = '2!');
end;

function Func18(const a: TShortRec): TShortRec; {$I cconv.inc}
begin
  Result.a := a.a div 2;
end;

function RunFunc18(f: Pointer): Boolean;
type
  TF = function(const a: TShortRec): TShortRec; {$I cconv.inc}
var
  r: TShortRec;
begin
  r.a := 100;
  r := TF(f)(r);
  Result := r.a = 50;
end;

{$IF DEFINED(FPC) AND DEFINED(FFITest_CDECL) AND DEFINED(CPU86)}
// Work around FPC internal error
type TFunc19 = function(const a: TPackRec): TStatPackArr; cdecl;
function Func19(const a: TPackRec): TStatPackArr; cdecl;
{$ELSE}
// Actual header
type TFunc19 = function(const a: TPackRec): TPackRec; {$I cconv.inc}
function Func19(const a: TPackRec): TPackRec; {$I cconv.inc}
{$IFEND}
begin
  TPackRec(Result).b := a.b + a.x;
  TPackRec(Result).x := a.x;
end;

function RunFunc19(f: Pointer): Boolean;
type
  TF = TFunc19;
var
  r: TPackRec;
begin
  r.b := 1;
  r.x := 10;
  r := TPackRec(TF(f)(r));
  Result := (r.b = 11) and (r.x = 10);
end;

function Func20(constref a: TStrRec): TStrRec; {$I cconv.inc}
begin
  Result.c := '0' + a.c;
end;

function RunFunc20(f: Pointer): Boolean;
type
  TF = function(constref a: TStrRec): TStrRec; {$I cconv.inc}
var
  r: TStrRec;
begin
  r.c := '123';
  r := TF(f)(r);
  Result := (r.c = '0123');
end;

function Func21(const a: TLargeRec): TLargeRec; {$I cconv.inc}
begin
  Result.a := a.d;
  Result.b := a.c;
  Result.c := a.b;
  Result.d := a.a;
end;

function RunFunc21(f: Pointer): Boolean;
type
  TF = function(const a: TLargeRec): TLargeRec; {$I cconv.inc}
var
  r: TLargeRec;
begin
  r.a := 1; r.b := 2; r.c := 3; r.d := 4;
  r := TF(f)(r);
  Result := (r.a = 4) and (r.b = 3) and (r.c = 2) and (r.d = 1);
end;

type
  TTestObject = object
    MagicToken: NativeInt;
    Success: Boolean;

    procedure Meth1; {$I cconv.inc}
    procedure Meth2(a, b, c, d, e, f, g, h, i, j: NativeInt); {$I cconv.inc}
    function Meth3(const a: NativeInt): NativeInt; {$I cconv.inc}
    function Meth4(const a, b, c: Int8): UInt8; {$I cconv.inc}
    function Meth5(const a: TStatPackArr): TStatPackArr; {$I cconv.inc}
    function Meth6(constref a: TStrRec): TStrRec; {$I cconv.inc}
    function Meth7(const a: TLargeRec): TLargeRec; {$I cconv.inc}
  end;

const
  NullTestObject: TTestObject = (MagicToken: 12345; Success: False);

procedure TTestObject.Meth1; {$I cconv.inc}
begin
  Assert(MagicToken = NullTestObject.MagicToken);
  Success := True;
end;

function RunMeth1(p: Pointer): Boolean;
type
  TM = procedure of object; {$I cconv.inc}
var
  m: TM;
  o: TTestObject;
begin
  o := NullTestObject;
  TMethod(m).Code := p;
  TMethod(m).Data := @o;
  TM(m)();
  Result := o.Success;
end;

procedure TTestObject.Meth2(a, b, c, d, e, f, g, h, i, j: NativeInt); {$I cconv.inc}
begin
  Assert(MagicToken = NullTestObject.MagicToken);
  Success := (a = 1) and (b =- 2) and (c = 3) and (d = -4) and (e = 5) and (f = -6)
         and (g = 7) and (h = -8) and (i = 9) and (j = -10);
  Assert(Success);
end;

function RunMeth2(p: Pointer): Boolean;
type
  TM = procedure(a, b, c, d, e, f, g, h, i, j: NativeInt) of object; {$I cconv.inc}
var
  m: TM;
  o: TTestObject;
begin
  o := NullTestObject;
  TMethod(m).Code := p;
  TMethod(m).Data := @o;
  TM(m)(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);
  Result := o.Success;
end;

function TTestObject.Meth3(const a: NativeInt): NativeInt; {$I cconv.inc}
begin
  Assert(MagicToken = NullTestObject.MagicToken);
  Result := a + 1;
end;

function RunMeth3(f: Pointer): Boolean;
type
  TM = function(const a: NativeInt): NativeInt of object; {$I cconv.inc}
var
  m: TM;
  o: TTestObject;
begin
  o := NullTestObject;
  TMethod(m).Code := f;
  TMethod(m).Data := @o;
  Result := TM(m)(10) = 11;
end;

function TTestObject.Meth4(const a, b, c: Int8): UInt8; {$I cconv.inc}
begin
  Assert(MagicToken = NullTestObject.MagicToken);
  Result := a + b + c;
end;

function RunMeth4(f: Pointer): Boolean;
type
  TM = function(const a, b, c: Int8): UInt8 of object; {$I cconv.inc}
var
  m: TM;
  o: TTestObject;
begin
  o := NullTestObject;
  TMethod(m).Code := f;
  TMethod(m).Data := @o;
  Result := TM(m)(15, 14, 13) = 42;
end;

function TTestObject.Meth5(const a: TStatPackArr): TStatPackArr; {$I cconv.inc}
var
  i: Integer;
begin
  Result[0] := a[0];
  for i := Low(Result)+1 to High(Result) do
    Result[i] := Result[i-1] + a[i];
end;

function RunMeth5(f: Pointer): Boolean;
type
  TM = function(const a: TStatPackArr): TStatPackArr of object; {$I cconv.inc}
const
  a: TStatPackArr = (1, 2, 3);
var
  m: TM;
  o: TTestObject;
  b: TStatPackArr;
begin
  o := NullTestObject;
  TMethod(m).Code := f;
  TMethod(m).Data := @o;
  b := TM(m)(a);
  Result := (b[2] = 6);
end;

function TTestObject.Meth6(constref a: TStrRec): TStrRec; {$I cconv.inc}
begin
  Result.c := '0' + a.c;
end;

function RunMeth6(f: Pointer): Boolean;
type
  TM = function(constref a: TStrRec): TStrRec of object; {$I cconv.inc}
var
  m: TM;
  o: TTestObject;
  r: TStrRec;
begin
  o := NullTestObject;
  r.c := '123';
  TMethod(m).Code := f;
  TMethod(m).Data := @o;
  r := TM(m)(r);
  Result := (r.c = '0123');
end;

function TTestObject.Meth7(const a: TLargeRec): TLargeRec; {$I cconv.inc}
begin
  Assert(MagicToken = NullTestObject.MagicToken);
  Result.a := a.d;
  Result.b := a.c;
  Result.c := a.b;
  Result.d := a.a;
end;

function RunMeth7(f: Pointer): Boolean;
type
  TM = function(const a: TLargeRec): TLargeRec of object; {$I cconv.inc}
var
  m: TM;
  o: TTestObject;
  r: TLargeRec;
begin
  o := NullTestObject;
  r.a := 1; r.b := 2; r.c := 3; r.d := 4;
  TMethod(m).Code := f;
  TMethod(m).Data := @o;
  r := TM(m)(r);
  Result := (r.a = 4) and (r.b = 3) and (r.c = 2) and (r.d = 1);
end;

function TestBiDiFFI(Header: lpString; ImportFun: Pointer; RunFun: TRunFun; RunStr: lpString = ''; ImportABI: TFFIABI = FFI_DEFAULT_ABI; ExportABI: TFFIABI = FFI_DEFAULT_ABI): Boolean;
var
  i: TImportClosure;
  e: TExportClosure;
  v: TLapeGlobalVar;
  s: lpString;
  p: Integer;
  m: TLapeTree_Method;
  t: TTestObject;
begin
  i := nil;
  e := nil;
  s := '';
  t := NullTestObject;
  Result := False;

  try
    with TLapeCompiler.Create(TLapeTokenizerString.Create('begin ' + RunStr + ' end.')) do
    try
      Options := Options + [lcoAssertions];

      addGlobalVar(
        addGlobalType('record MagicToken: NativeInt; Success: Boolean; end', 'TTest'),
        @t,
        'Test'
      );

      addGlobalType('(ESmallFirst = ' + IntToStr(Ord(Low(ELapeSmallEnum))) + ', ESmallLast = ' + IntToStr(Ord(High(ELapeSmallEnum))) + ')', 'TSmallEnum');
      addGlobalType('(ELargeFirst = ' + IntToStr(Ord(Low(ELapeSmallEnum))) + ', ELargeLast = ' + IntToStr(Ord(High(ELapeLargeEnum))) + ')', 'TLargeEnum');
      addGlobalType('set of TSmallEnum', 'TSmallSet');
      addGlobalType('set of TLargeEnum', 'TLargeSet');

      addGlobalType('array of Int16',  'TIntArr');
      addGlobalType('array of string', 'TStrArr');
      addGlobalType('array[0..2] of Int8',   'TStatPackArr');
      addGlobalType('array[1..2] of Int16',  'TStatIntArr');
      addGlobalType('array[1..5] of string', 'TStatStrArr');

      addGlobalType('       record a: Int8;           end', 'TShortRec');
      addGlobalType('packed record b: Int16; x: Int8; end', 'TPackRec');
      addGlobalType('       record c: string;         end', 'TStrRec');
      addGlobalType('       record a: UInt8; b,c: UInt64; d: UInt8; end', 'TLargeRec');

      i := LapeImportWrapper(ImportFun, TLapeCompiler(GetSelf()), Header, ImportABI);
      v := addGlobalFunc(Header, i.Func);
      Assert(v.VarType is TLapeType_Method);

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

      m := addGlobalFunc(v.VarType as TLapeType_Method, 'TestMe', s);

      if (m = nil) or (m.Method = nil) or (not Compile()) then
        LapeException('Could not compile ' + Header);

      e := LapeExportWrapper(m.Method, ExportABI);

      Write(Format('Testing  %-6s :: %8s <-> %-8s :: ', [v.Name, ABIToStr(ImportABI), ABIToStr(ExportABI)]));
      RunCode(Emitter.Code);
      Result := RunFun(e.Func);

      if Result then
        WriteLn('Passed')
      else
        WriteLn('Failed');
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
      WriteLn('Failed :: ', e.Message);
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
  BiDiTests: array[1..38] of TRunProc = (
    (Fun: @Proc1;  Run: @RunProc1;  Str: 'procedure Proc1';                                                                                                        Arg: 'TestMe();'),
    (Fun: @Proc2;  Run: @RunProc2;  Str: 'procedure Proc2(a, b, c, d, e, f, g, h, i, j: NativeInt)';                                                               Arg: 'TestMe(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);'),
    (Fun: @Proc3;  Run: @RunProc3;  Str: 'procedure Proc3(a: UInt8; b: Int64; c: UInt32; d: Int16; e: UInt16; f: Int32; g: UInt64; h: Int8)';                      Arg: 'TestMe(1, -2, 3, -4, 5, -6, 7, -8);'),
    (Fun: @Proc4;  Run: @RunProc4;  Str: 'procedure Proc4(a: Single; b: UInt8; c: Double; d: UInt16; e: Currency; f: UInt32; g: Extended; h: UInt64)';             Arg: 'TestMe(1.5, 2, 3.5, 4, 5.5, 6, 7.5, 8);'),
    (Fun: @Proc5;  Run: @RunProc5;  Str: 'procedure Proc5(a: ShortString; b: Int8; c: AnsiString; d: Int16; e: WideString; f: Int32; g: UnicodeString; h: Int64)'; Arg: 'TestMe("01", -2, "03", -4, "05", -6, "07", -8);'),
    (Fun: @Proc6;  Run: @RunProc6;  Str: 'procedure Proc6(a: LongBool; b: UInt8; c: WordBool; d: UInt16; e: ByteBool; f: UInt32; g: Boolean; h: UInt64)';          Arg: 'TestMe(True, 2, False, 4, True, 6, False, 8);'),
    (Fun: @Proc7;  Run: @RunProc7;  Str: 'procedure Proc7(a: TSmallEnum; b: TSmallSet; c: TLargeEnum; d: TLargeSet)';                                              Arg: 'TestMe(TSmallEnum(2), [ESmallFirst, ESmallLast], TLargeEnum(4), [ELargeFirst, ELargeLast]);'),
    (Fun: @Proc8;  Run: @RunProc8;  Str: 'procedure Proc8(a: Pointer; b: AnsiChar; c: Variant; d: WideChar)';                                                      Arg: 'TestMe(nil, "1", "234", "5");'),
    (Fun: @Proc9;  Run: @RunProc9;  Str: 'procedure Proc9(a: TStatPackArr; b: TStrArr; constref c: TStatStrArr; d: TIntArr; e: TStatIntArr)';                      Arg: 'TestMe([1..3], ["string0", "string1"], ["string2", "","","", "string3"], [0, 1], [2, 3]);'),
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
    (Fun: @Func15; Run: @RunFunc15; Str: 'function Func15(const a: TStatPackArr): TStatPackArr';  Arg: 'Assert(TestMe([1..3])[2] = 6);'),
    (Fun: @Func16; Run: @RunFunc16; Str: 'function Func16(const a: TStatIntArr): TStatIntArr';    Arg: 'Assert(TestMe([1..2])[2] = 4);'),
    (Fun: @Func17; Run: @RunFunc17; Str: 'function Func17(constref a: TStatStrArr): TStatStrArr'; Arg: 'Assert(TestMe(["1", "","","", "2"])[5] = "2!");'),
    (Fun: @Func18; Run: @RunFunc18; Str: 'function Func18(const a: TShortRec): TShortRec';        Arg: 'Assert(TestMe([100]) = [50]);'),
    (Fun: @Func19; Run: @RunFunc19; Str: 'function Func19(const a: TPackRec): TPackRec';          Arg: 'Assert(TestMe([1, 10]) = [11, 10]);'),
    (Fun: @Func20; Run: @RunFunc20; Str: 'function Func20(constref a: TStrRec): TStrRec';         Arg: 'Assert(TestMe(["123"]) = ["0123"]);'),
    (Fun: @Func21; Run: @RunFunc21; Str: 'function Func21(const a: TLargeRec): TLargeRec';        Arg: 'Assert(TestMe([1, 2, 3, 4]) = [4, 3, 2, 1]);'),

    (Fun: @TTestObject.Meth1; Run: @RunMeth1; Str: 'procedure TTest.Meth1';                                          Arg: 'Test.TestMe();'),
    (Fun: @TTestObject.Meth2; Run: @RunMeth2; Str: 'procedure TTest.Meth2(a, b, c, d, e, f, g, h, i, j: NativeInt)'; Arg: 'Test.TestMe(1, -2, 3, -4, 5, -6, 7, -8, 9, -10);'),
    (Fun: @TTestObject.Meth3; Run: @RunMeth3; Str: 'function TTest.Meth3(const a: NativeInt): NativeInt';            Arg: 'Assert(Test.TestMe(10) = 11);'),
    (Fun: @TTestObject.Meth4; Run: @RunMeth4; Str: 'function TTest.Meth4(const a, b, c: Int8): UInt8';               Arg: 'Assert(Test.TestMe(15, 14, 13) = 42);'),
    (Fun: @TTestObject.Meth5; Run: @RunMeth5; Str: 'function TTest.Meth5(const a: TStatPackArr): TStatPackArr';      Arg: 'Assert(Test.TestMe([1..3])[2] = 6);'),
    (Fun: @TTestObject.Meth6; Run: @RunMeth6; Str: 'function TTest.Meth6(constref a: TStrRec): TStrRec';             Arg: 'Assert(Test.TestMe(["123"]) = ["0123"]);'),
    (Fun: @TTestObject.Meth7; Run: @RunMeth7; Str: 'function TTest.Meth7(const a: TLargeRec): TLargeRec';            Arg: 'Assert(Test.TestMe([1, 2, 3, 4]) = [4, 3, 2, 1]);')

  );

{$IF DECLARED(TEST_ABI)}
var
  t: Integer;
  StartTime: UInt64;
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

  if (not FFILoaded()) then
  begin
    WriteLn('Cannot find libffi.');
    ExitCode := 221;
    Exit;
  end;

  ExitCode := 0;

  WriteLn('Running ', Length(BiDiTests), ' tests with ', ABIToStr(TEST_ABI), ' calling convention.');
  WriteLn();

  StartTime := GetTickCount();

  for t := Low(BiDiTests) to High(BiDiTests) do
    if (not TestBiDiFFI(BiDiTests[t].Str, BiDiTests[t].Fun, BiDiTests[t].Run, BiDiTests[t].Arg, TEST_ABI, TEST_ABI)) then
    begin
      WriteLn('  Header : "', BiDiTests[t].Str, '"');
      WriteLn('    Call : "', BiDiTests[t].Arg, '"');
      WriteLn();
      Inc(ExitCode);
    end;

  WriteLn();
  WriteLn(Format('Ran %d tests in %.2f seconds', [Length(BiDiTests), ((GetTickCount() - StartTime) / 1000)]));
  WriteLn(Format('%3d / %d tests failed', [ExitCode,                     Length(BiDiTests)]));
  WriteLn(Format('%3d / %d tests passed', [Length(BiDiTests) - ExitCode, Length(BiDiTests)]));
  WriteLn();
{$ELSE}
begin
  WriteLn('Invalid calling convention for platform.');
  ExitCode := 222;
{$ENDIF}
end.

