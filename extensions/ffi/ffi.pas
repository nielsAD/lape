(*
	Copyright (c) 2012 by Merlijn Wajer

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
      Currently only tested on x64 GNU/Linux.
*)
unit ffi;

{$mode objfpc}{$H+}
{$linklib libffi}

interface

uses
    Classes, SysUtils, ctypes;

(*
TODO:
  -  Add ARM compat. Replace cpu32 ifdef's with better ifdefs.
  -  Mac compat.
  -  Test architectures.
  -  Clean up some parameters (I think we can use more arrays and less pointers)
*)

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

  (*
  #if FFI_CLOSURES

  #ifdef _MSC_VER
  __declspec(align(8))
  #endif
  typedef struct {
  #if @FFI_EXEC_TRAMPOLINE_TABLE@
    void *trampoline_table;
    void *trampoline_table_entry;
  #else
    char tramp[FFI_TRAMPOLINE_SIZE];
  #endif
    ffi_cif   *cif;
    void     (*fun)(ffi_cif*,void*,void**,void*);
    void      *user_data;
  #ifdef __GNUC__
  } ffi_closure __attribute__((aligned (8)));
  #else
  } ffi_closure;
  # ifdef __sgi
  #  pragma pack 0
  # endif
  #endif
  *)

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

function ffi_prep_cif(
    out cif: TFFICif;
    abi: TFFIABI;
    nargs: cuint;
    rtype: PFFIType;
    atypes: PFFITypeArray
  ): TFFIStatus; cdecl; external;

procedure ffi_call(
    var cif: TFFICif;
    fn: Pointer;
    rvalue: Pointer;
    avalue: PPointerArray
  ); cdecl; external;

function ffi_closure_alloc(
    size: csize_t;
    var code: Pointer
  ): Pointer; cdecl; external;

procedure ffi_closure_free(
    closure: Pointer
  ); cdecl; external;

function ffi_prep_closure_loc(
    out closure: TFFIClosure;
    var CIF: TFFICif;
    fun: TClosureBindingFunction;
    user_data: Pointer;
    codeloc: Pointer
  ): TFFIStatus; cdecl; external;

var
  ffi_type_void: TFFIType; cvar; external;
  ffi_type_uint8: TFFIType; cvar; external;
  ffi_type_sint8: TFFIType; cvar; external;
  ffi_type_uint16: TFFIType; cvar; external;
  ffi_type_sint16: TFFIType; cvar; external;
  ffi_type_uint32: TFFIType; cvar; external;
  ffi_type_sint32: TFFIType; cvar; external;
  ffi_type_uint64: TFFIType; cvar; external;
  ffi_type_sint64: TFFIType; cvar; external;
  ffi_type_float: TFFIType; cvar; external;
  ffi_type_double: TFFIType; cvar; external;
  ffi_type_longdouble: TFFIType; cvar; external;
  ffi_type_pointer: TFFIType; cvar; external;

implementation

end.
