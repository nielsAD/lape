(*
    Copyright (c) 2012 by Merlijn Wajer and Niels AD

    ffi.pas is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.

    ffi.pas is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ffi.pas. If not, see <http://www.gnu.org/licenses/>.

  ffi.pas unit, Free Pascal interface to the great libffi library.

  The license may become even less restrictive (most likely public domain)
*)

(*
  Features:
      - Supporting ffi_call, ffi_prep_cif, ffi_prep_closure, ffi_prep_closure_loc.
      - All the requires types have been ported to their Pascal equivalent;
        TFFIClosure however requires some extra additions per architecture.
*)
unit ffi;

{$IFDEF FPC}
  {$MODE objfpc}{$H+}
{$ELSE}
  {$IFDEF WIN64}
    {$DEFINE CPUX86_64}
  {$ELSE}
    {$DEFINE CPU86}
  {$ENDIF}
{$ENDIF}

{.$DEFINE StaticFFI}  //Link at compile time into the executable
{$DEFINE DynamicFFI}  //Load library dynamically, use AssertFFILoaded() to check

// If neither StaticFFI or DynamicFFI are defined, linking will be done at start of
// the program (but will fail to start if library is not found)

interface

{$IFDEF StaticFFI}
  {$LINKLIB libffi}
  {$UNDEF DynamicFFI}

  {$IFDEF MSWINDOWS}
    {$LINKLIB libgcc}
    {$LINKLIB libmsvcrt}
    {$LINKLIB libkernel32}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF FPC}
  ctypes, dynlibs
{$ELSE}
  Windows
{$ENDIF};

{$IFNDEF FPC}
type
  PtrInt  = NativeInt;
  PtrUInt = NativeUInt;

  cuint8      = Byte;
  cint8       = SmallInt;
  cuint16     = Word;
  cint16      = ShortInt;
  cuint32     = LongWord;
  cint32      = LongInt;
  cuint64     = UInt64;
  cint64      = Int64;
  cfloat      = Single;
  cdouble     = Double;
  clongdouble = Extended;
  cchar       = cuint8;
  cushort     = cuint16;
  cuint       = cuint32;
  csize_t     = NativeInt;
  cunsigned   = NativeUInt;

  TLibHandle  = THandle;

const
  NilHandle    = 0;
  SharedSuffix = {$IFDEF MSWINDOWS}'dll'{$ELSE}'so'{$ENDIF};
{$ENDIF}

const
  LibFFI = 'libffi.' + SharedSuffix;

type
  TFFIStatus = (
    FFI_OK = 0,
    FFI_BAD_TYPEDEF,
    FFI_BAD_ABI
  );

  TFFIABI = (
    FFI_FIRST_ABI = 0,

    {$IFDEF CPU86}
      {$IFDEF UNIX}
      FFI_SYSV     = 1,
      FFI_THISCALL = 3,
      FFI_FASTCALL = 4,
      FFI_STDCALL  = 5,
      FFI_PASCAL   = 6,
      FFI_REGISTER = 7,
      FFI_MS_CDECL = 8
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      FFI_SYSV     = 1,
      FFI_STDCALL  = 2,
      FFI_THISCALL = 3,
      FFI_FASTCALL = 4,
      FFI_MS_CDECL = 5,
      FFI_PASCAL   = 6,
      FFI_REGISTER = 7,
      {$ENDIF}
    {$ENDIF}

    {$IFDEF CPUX86_64}
      {$IFDEF UNIX}
      FFI_UNIX64 = 2,
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      FFI_WIN64  = 1,
      {$ENDIF}
    {$ENDIF}

    {$IFDEF CPUARM}
      FFI_SYSV = 1,
      FFI_VFP  = 2,
    {$ENDIF}

    FFI_LAST_ABI
  );

const
  FFI_DEFAULT_ABI =
    {$IFDEF CPU86}FFI_REGISTER{$ENDIF}
    {$IFDEF CPUX86_64}{$IFDEF MSWINDOWS}FFI_WIN64{$ELSE}FFI_UNIX64{$ENDIF}{$ENDIF}
    {$IFDEF CPUARM}{$IFDEF FPUVFP}FFI_VFP{$ELSE}FFI_SYSV{$ENDIF}{$ENDIF};

  FFI_TRAMPOLINE_SIZE =
    {$IFDEF CPU86}12{$ENDIF}
    {$IFDEF CPUX86_64}24{$ENDIF}
    {$IFDEF CPUARM}12{$ENDIF};

type
  TFFI_CTYPE = (
    FFI_CTYPE_VOID = 0,
    FFI_CTYPE_INT,
    FFI_CTYPE_FLOAT,
    FFI_CTYPE_DOUBLE,
    FFI_CTYPE_LONGDOUBLE,
    FFI_CTYPE_UINT8,
    FFI_CTYPE_SINT8,
    FFI_CTYPE_UINT16,
    FFI_CTYPE_SINT16,
    FFI_CTYPE_UINT32,
    FFI_CTYPE_SINT32,
    FFI_CTYPE_UINT64,
    FFI_CTYPE_SINT64,
    FFI_CTYPE_STRUCT,
    FFI_CTYPE_POINTER
  );

  PFFIType = ^TFFIType;
  TFFITypeArray = array[0..High(PointerArray)] of PFFIType;
  PFFITypeArray = ^TFFITypeArray;

  TFFIType = record
    size: csize_t;
    alignment: cushort;
    _type: cushort;
    elements: PFFITypeArray;
  end;

  PFFICif = ^TFFICif;
  TFFICif = record
    abi: TFFIABI;
    nargs: cunsigned;
    arg_types: PFFITypeArray;
    rtype: PFFIType;
    bytes: cunsigned;
    flags: cunsigned;
  end;

type
  TClosureBindingFunction = procedure(
      var cif: TFFICif;
      ret: Pointer;
      args: PPointerArray;
      userdata: Pointer
    ); cdecl;

  PFFIClosure = ^TFFIClosure;
  TFFIClosure = record
    //tramp: array [0..FFI_TRAMPOLINE_SIZE - 1] of cchar;
    //cif: PFFICif;
    //fun: TClosureBindingFunction;
    //user_data: Pointer;

    // DO NOT RELY ON THIS LAYOUT, JUST MAKE SURE IT IS LARGE ENOUGH
    _tramp: array [0..55] of cchar;
    _ptrs: array[0..2] of Pointer;
  end;

  TFFIPrepCif = function(
      out cif: TFFICif;
      abi: TFFIABI;
      nargs: cuint;
      rtype: PFFIType;
      atypes: PFFITypeArray
    ): TFFIStatus; cdecl;

  TFFICall = procedure(
      var cif: TFFICif;
      fn: Pointer;
      rvalue: Pointer;
      avalue: PPointerArray
    ); cdecl;

  TFFIClosureAlloc = function(
      size: csize_t;
      var code: Pointer
    ): Pointer; cdecl;

  TFFIClosureFree = procedure(
      closure: Pointer
    ); cdecl;

  TFFIPrepClosureLoc = function(
      out closure: TFFIClosure;
      var CIF: TFFICif;
      fun: TClosureBindingFunction;
      user_data: Pointer;
      codeloc: Pointer
    ): TFFIStatus; cdecl;

var
  ffi_prep_cif: TFFIPrepCif = nil;
  ffi_call: TFFICall = nil;
  ffi_closure_alloc: TFFIClosureAlloc = nil;
  ffi_closure_free: TFFIClosureFree = nil;
  ffi_prep_closure_loc: TFFIPrepClosureLoc = nil;

  {$IFDEF DynamicFFI}
  ffi_libhandle: TLibHandle = NilHandle;
  {$ENDIF}

  ffi_type_void: TFFIType;       {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_uint8: TFFIType;      {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_sint8: TFFIType;      {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_uint16: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_sint16: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_uint32: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_sint32: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_uint64: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_sint64: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_float: TFFIType;      {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_double: TFFIType;     {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_longdouble: TFFIType; {$IFDEF StaticFFI}cvar; external;{$ENDIF}
  ffi_type_pointer: TFFIType;    {$IFDEF StaticFFI}cvar; external;{$ENDIF}

function FFILoaded: Boolean;
procedure AssertFFILoaded;

{$IFDEF DynamicFFI}
procedure LoadFFI(LibPath: string = ''; LibName: string = LibFFI);
procedure UnloadFFI;
{$ENDIF}

implementation

uses
  SysUtils;

function FFILoaded: Boolean;
begin
  {$IFDEF DynamicFFI}
  Result := ffi_libhandle <> NilHandle;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

procedure AssertFFILoaded;
begin
  if (not FFILoaded()) then
    raise EAssertionFailed.Create('libffi is not loaded');
end;

{$IFNDEF DynamicFFI}
  function _ffi_prep_cif(out cif: TFFICif; abi: TFFIABI; nargs: cuint; rtype: PFFIType; atypes: PFFITypeArray): TFFIStatus; cdecl; external {$IFNDEF StaticFFI}LibFFI{$ENDIF} name 'ffi_prep_cif';
  procedure _ffi_call(var cif: TFFICif; fn: Pointer; rvalue: Pointer; avalue: PPointerArray); cdecl; external {$IFNDEF StaticFFI}LibFFI{$ENDIF} name 'ffi_call';
  function _ffi_closure_alloc(size: csize_t; var code: Pointer): Pointer; cdecl; external {$IFNDEF StaticFFI}LibFFI{$ENDIF} name 'ffi_closure_alloc';
  procedure _ffi_closure_free(closure: Pointer); cdecl; external {$IFNDEF StaticFFI}LibFFI{$ENDIF} name 'ffi_closure_free';
  function _ffi_prep_closure_loc(out closure: TFFIClosure; var CIF: TFFICif; fun: TClosureBindingFunction; user_data, codeloc: Pointer): TFFIStatus; cdecl; external {$IFNDEF StaticFFI}LibFFI{$ENDIF} name 'ffi_prep_closure_loc';
{$ENDIF}

{$IFNDEF StaticFFI}
type
  {$IFDEF FPC}generic{$ENDIF} TFFIBaseType<_T> = class
  public type
    TAlignStruct = record
      c: cchar;
      x: _T;
    end;
  var public
    class function getOffset: Integer;
    class function getFFIType(FFI_CType: TFFI_CTYPE = FFI_CTYPE_VOID): TFFIType;
  end;

  TFFIBaseType_UInt8      = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cuint8>;
  TFFIBaseType_SInt8      = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cint8>;
  TFFIBaseType_UInt16     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cuint16>;
  TFFIBaseType_SInt16     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cint16>;
  TFFIBaseType_UInt32     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cuint32>;
  TFFIBaseType_SInt32     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cint32>;
  TFFIBaseType_UInt64     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cuint64>;
  TFFIBaseType_SInt64     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cint64>;
  TFFIBaseType_Float      = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cfloat>;
  TFFIBaseType_Double     = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<cdouble>;
  TFFIBaseType_LongDouble = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<clongdouble>;
  TFFIBaseType_Pointer    = {$IFDEF FPC}specialize{$ENDIF} TFFIBaseType<Pointer>;

class function TFFIBaseType{$IFNDEF FPC}<_T>{$ENDIF}.getOffset: Integer;
var
  t: TAlignStruct;
begin
  Result := PtrUInt(@t.x) - PtrUInt(@t.c);
end;

class function TFFIBaseType{$IFNDEF FPC}<_T>{$ENDIF}.getFFIType(FFI_CType: TFFI_CTYPE = FFI_CTYPE_VOID): TFFIType;
begin
  with Result do
  begin
    size := SizeOf(_T);
    alignment := getOffset();
    _type := Ord(FFI_CType);
    Result.elements := nil;
  end;
end;
{$ENDIF}

{$IFDEF DynamicFFI}
{$IF NOT(DECLARED(GetProcedureAddress)) AND DECLARED(GetProcAddress)}
function GetProcedureAddress(Handle: TLibHandle; Name: PWideChar): FARPROC;
begin
  Result := GetProcAddress(Handle, Name);
end;
{$IFEND}

{$IF NOT(DECLARED(UnloadLibrary)) AND DECLARED(FreeLibrary)}
function UnloadLibrary(Handle: TLibHandle): LongBool;
begin
  Result := FreeLibrary(Handle);
end;
{$IFEND}

procedure LoadFFI(LibPath: string = ''; LibName: string = LibFFI);
begin
  UnloadFFI();
  if (LibPath <> '') then
    LibPath := IncludeTrailingPathDelimiter(LibPath);
  ffi_libhandle := LoadLibrary(PChar(LibPath + LibName));

  if FFILoaded() then
  begin
    Pointer({$IFNDEF FPC}@{$ENDIF}ffi_prep_cif)         := GetProcedureAddress(ffi_libhandle, 'ffi_prep_cif');
    Pointer({$IFNDEF FPC}@{$ENDIF}ffi_call)             := GetProcedureAddress(ffi_libhandle, 'ffi_call');
    Pointer({$IFNDEF FPC}@{$ENDIF}ffi_closure_alloc)    := GetProcedureAddress(ffi_libhandle, 'ffi_closure_alloc');
    Pointer({$IFNDEF FPC}@{$ENDIF}ffi_closure_free)     := GetProcedureAddress(ffi_libhandle, 'ffi_closure_free');
    Pointer({$IFNDEF FPC}@{$ENDIF}ffi_prep_closure_loc) := GetProcedureAddress(ffi_libhandle, 'ffi_prep_closure_loc');
  end;
end;

procedure UnloadFFI;
begin
  if FFILoaded() then
    if UnloadLibrary(ffi_libhandle) then
    begin
      ffi_libhandle := NilHandle;

      Pointer({$IFNDEF FPC}@{$ENDIF}ffi_prep_cif)         := nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}ffi_call)             := nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}ffi_closure_alloc)    := nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}ffi_closure_free)     := nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}ffi_prep_closure_loc) := nil;
    end;
end;
{$ENDIF}

initialization
  {$IFDEF DynamicFFI}
  LoadFFI();
  {$ELSE}
  ffi_prep_cif         := @_ffi_prep_cif;
  ffi_call             := @_ffi_call;
  ffi_closure_alloc    := @_ffi_closure_alloc;
  ffi_closure_free     := @_ffi_closure_free;
  ffi_prep_closure_loc := @_ffi_prep_closure_loc;
  {$ENDIF}

  {$IFNDEF StaticFFI}
  with ffi_type_void do
  begin
    size := 1;
    alignment := 1;
    _type := Ord(FFI_CTYPE_VOID);
    elements := nil;
  end;

  ffi_type_uint8      := TFFIBaseType_UInt8.getFFIType(FFI_CTYPE_UINT8);
  ffi_type_sint8      := TFFIBaseType_SInt8.getFFIType(FFI_CTYPE_SINT8);
  ffi_type_uint16     := TFFIBaseType_UInt16.getFFIType(FFI_CTYPE_UINT16);
  ffi_type_sint16     := TFFIBaseType_SInt16.getFFIType(FFI_CTYPE_SINT16);
  ffi_type_uint32     := TFFIBaseType_UInt32.getFFIType(FFI_CTYPE_UINT32);
  ffi_type_sint32     := TFFIBaseType_SInt32.getFFIType(FFI_CTYPE_SINT32);
  ffi_type_uint64     := TFFIBaseType_UInt64.getFFIType(FFI_CTYPE_UINT64);
  ffi_type_sint64     := TFFIBaseType_SInt64.getFFIType(FFI_CTYPE_SINT64);
  ffi_type_float      := TFFIBaseType_Float.getFFIType(FFI_CTYPE_FLOAT);
  ffi_type_double     := TFFIBaseType_Double.getFFIType(FFI_CTYPE_DOUBLE);
  ffi_type_longdouble := TFFIBaseType_LongDouble.getFFIType(FFI_CTYPE_LONGDOUBLE);
  ffi_type_pointer    := TFFIBaseType_Pointer.getFFIType(FFI_CTYPE_POINTER);
  {$ENDIF}

finalization
  {$IFDEF DynamicFFI}
  UnloadFFI();
  {$ENDIF}
end.
