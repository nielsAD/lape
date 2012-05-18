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
      - Experimental support for ffi_call, ffi_prep_cif, ffi_prep_closure,
        ffi_prep_closure_loc.
      - All the requires types have been ported to their FPC equivalent;
        TFFIClosure however requires some extra additions per architecture.
*)
unit ffi;

{$mode objfpc}{$H+}

{.$DEFINE StaticFFI}  //Link at compile time into the executable
{$DEFINE DynamicFFI}  //Load library dynamically, use AssertFFILoaded() to check

// If neither StaticFFI or DynamicFFI are defined, linking will be done at start of
// the program (but will fail to start if library is not found)

interface

{$IFDEF StaticFFI}
  {$LINKLIB libffi}
  {$UNDEF DynamicFFI}
{$ENDIF}

uses
  ctypes, dynlibs;

(*
TODO:
  -  Add ARM compat. Replace cpu32 ifdef's with better ifdefs.
  -  Mac compat.
  -  Test architectures.
*)

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

    {$IFDEF LINUX}
      FFI_SYSV,
      FFI_UNIX64,   { Unix variants all use the same ABI for x86-64  }
      FFI_LAST_ABI,

      {$IFDEF CPU32}
        FFI_DEFAULT_ABI := FFI_SYSV
      {$ELSE}
        FFI_DEFAULT_ABI := FFI_UNIX64
      {$ENDIF}
    {$ENDIF}

    {$IFDEF MSWINDOWS}
      {$IFDEF CPU32}
        FFI_SYSV,
        FFI_STDCALL,
        FFI_THISCALL,
        FFI_FASTCALL,
        FFI_MS_CDECL,
        FFI_LAST_ABI,
        FFI_DEFAULT_ABI := FFI_SYSV
      {$ELSE}
        FFI_WIN64,
        FFI_LAST_ABI,
        FFI_DEFAULT_ABI := FFI_WIN64
      {$ENDIF}
    {$ENDIF}
  );

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
    FFI_CTYPE_POINTER,
    FFI_CTYPE_LAST := FFI_CTYPE_POINTER
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
    {$IFDEF FFI_EXTRA_FIELDS}
    // TODO
    {$ENDIF}
  end;

const
  FFI_TRAMPOLINE_SIZE =
    {$IFDEF WINDOWS}{$IFDEF CPU32}52{$ELSE}29{$ENDIF}{$ENDIF}
    {$IFDEF LINUX}{$IFDEF CPU32}10{$ELSE}24{$ENDIF}{$ENDIF};

type
  TClosureBindingFunction = procedure(
      var cif: TFFICif;
      ret: Pointer;
      args: PPointerArray;
      userdata: Pointer
    ); cdecl;

  PFFIClosure = ^TFFIClosure;
  TFFIClosure = record
    tramp: array [0..FFI_TRAMPOLINE_SIZE - 1] of cchar; // Let's hope FFI_EXEC_TRAMPOLINE_TABLE is not defined/true
    cif: PFFICif;
    fun: TClosureBindingFunction;
    user_data: Pointer;
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
  sysutils;

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
  generic TFFIBaseType<_T> = class
  public type
    TAlignStruct = record
      c: cchar;
      x: _T;
    end;
  var public
    class function getOffset: Integer;
    class function getFFIType(FFI_CType: TFFI_CTYPE = FFI_CTYPE_VOID): TFFIType;
  end;

  TFFIBaseType_UInt8      = specialize TFFIBaseType<cuint8>;
  TFFIBaseType_SInt8      = specialize TFFIBaseType<cint8>;
  TFFIBaseType_UInt16     = specialize TFFIBaseType<cuint16>;
  TFFIBaseType_SInt16     = specialize TFFIBaseType<cint16>;
  TFFIBaseType_UInt32     = specialize TFFIBaseType<cuint32>;
  TFFIBaseType_SInt32     = specialize TFFIBaseType<cint32>;
  TFFIBaseType_UInt64     = specialize TFFIBaseType<cuint64>;
  TFFIBaseType_SInt64     = specialize TFFIBaseType<cint64>;
  TFFIBaseType_Float      = specialize TFFIBaseType<cfloat>;
  TFFIBaseType_Double     = specialize TFFIBaseType<cdouble>;
  TFFIBaseType_LongDouble = specialize TFFIBaseType<clongdouble>;
  TFFIBaseType_Pointer    = specialize TFFIBaseType<Pointer>;

class function TFFIBaseType.getOffset: Integer;
var
  t: TAlignStruct;
begin
  Result := PtrUInt(@t.x) - PtrUInt(@t.c);
end;

class function TFFIBaseType.getFFIType(FFI_CType: TFFI_CTYPE = FFI_CTYPE_VOID): TFFIType;
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
procedure LoadFFI(LibPath: string = ''; LibName: string = LibFFI);
begin
  UnloadFFI();
  if (LibPath <> '') then
    LibPath := IncludeTrailingPathDelimiter(LibPath);
  ffi_libhandle := SafeLoadLibrary(LibPath + LibName);

  if FFILoaded() then
  begin
    Pointer(ffi_prep_cif) := GetProcedureAddress(ffi_libhandle, 'ffi_prep_cif');
    Pointer(ffi_call) := GetProcedureAddress(ffi_libhandle, 'ffi_call');
    Pointer(ffi_closure_alloc) := GetProcedureAddress(ffi_libhandle, 'ffi_closure_alloc');
    Pointer(ffi_closure_free) := GetProcedureAddress(ffi_libhandle, 'ffi_closure_free');
    Pointer(ffi_prep_closure_loc) := GetProcedureAddress(ffi_libhandle, 'ffi_prep_closure_loc');
  end;
end;

procedure UnloadFFI;
begin
  if FFILoaded() then
    if UnloadLibrary(ffi_libhandle) then
    begin
      ffi_libhandle := NilHandle;

      Pointer(ffi_prep_cif) := nil;
      Pointer(ffi_call) := nil;
      Pointer(ffi_closure_alloc) := nil;
      Pointer(ffi_closure_free) := nil;
      Pointer(ffi_prep_closure_loc) := nil;
    end;
end;
{$ENDIF}

initialization
  {$IFDEF DynamicFFI}
  LoadFFI();
  {$ELSE}
  ffi_prep_cif := @_ffi_prep_cif;
  ffi_call := @_ffi_call;
  ffi_closure_alloc := @_ffi_closure_alloc;
  ffi_closure_free := @_ffi_closure_free;
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
