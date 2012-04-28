{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Foreign function interface extension with help of libffi.
}
unit lpffi;

{$I lape.inc}

interface

uses
  Classes, SysUtils, ffi,
  lptypes, lpvartypes, lpvartypes_record, lpcompiler;

type
  TFFITypeManager = object
  protected
    PElems: array of PFFIType;
  public
    Typ: TFFIType;
    Elems: array of TFFITypeManager;

    class function New(FFIType: TFFIType): TFFITypeManager;
    procedure PrepareType;
  end;

  TFFICifManager = object
  protected
    PArgs: array of PFFIType;
  public
    Cif: TFFICif;
    ABI: TFFIABI;
    Args: array of TFFITypeManager;

    Res: TFFITypeManager;
    HasRes: Boolean;

    class function New(AABI: TFFIABI = FFI_DEFAULT_ABI; ArgCount: Integer = 0): TFFICifManager; overload;
    class function New(ResType: TFFITypeManager; ArgCount: Integer = 0; AABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager; overload;
    class function New(ResType: TFFITypeManager; ArgTypes: array of TFFITypeManager; AABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager; overload;
    function PrepareCif: TFFIStatus;
  end;

function LapeTypeToFFIType(VarType: TLapeType): TFFITypeManager;
function LapeParamToFFIType(Param: TLapeParameter): TFFITypeManager;
function LapeHeaderToFFICif(Header: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager; overload;
function LapeHeaderToFFICif(Compiler: TLapeCompiler; Header: lpString; ABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager; overload;

implementation

uses
  lpexceptions, lpparser;

class function TFFITypeManager.New(FFIType: TFFIType): TFFITypeManager;
begin
  Result.Typ := FFIType;
  Result.Elems := nil;
  Result.PElems := nil;
end;

procedure TFFITypeManager.PrepareType;
var
  i, l: Integer;
begin
  l := Length(Elems);
  if (l <= 1) then
    Exit;

  FillChar(Typ, SizeOf(TFFIType), 0);

  SetLength(PElems, l + 1);
  for i := 0 to l - 1 do
  begin
    Elems[i].PrepareType();
    PElems[i] := @Elems[i].Typ;
  end;
  PElems[l] := nil;

  Typ._type := ffi_type_struct;
  Typ.elements := @PElems[0];
end;

class function TFFICifManager.New(AABI: TFFIABI = FFI_DEFAULT_ABI; ArgCount: Integer = 0): TFFICifManager;
begin
  FillChar(Result, SizeOf(TFFICifManager), 0);
  Result.ABI := AABI;
  Result.HasRes := False;
  SetLength(Result.Args, ArgCount);
end;

class function TFFICifManager.New(ResType: TFFITypeManager; ArgCount: Integer = 0; AABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager;
begin
  Result := New(AABI, ArgCount);
  Result.Res := ResType;
  Result.HasRes := True;
end;

class function TFFICifManager.New(ResType: TFFITypeManager; ArgTypes: array of TFFITypeManager; AABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager;
var
  i: Integer;
begin
  Result := New(ResType, Length(ArgTypes), AABI);
  for i := 0 to High(ArgTypes) do
    Result.Args[i] := ArgTypes[i];
end;

function TFFICifManager.PrepareCif: TFFIStatus;
var
  i: Integer;
  r, a: Pointer;
begin
  SetLength(PArgs, Length(Args));
  for i := 0 to High(Args) do
  begin
    Args[i].PrepareType();
    PArgs[i] := @Args[i].Typ;
  end;

  if HasRes then
    r := @Res.Typ
  else
    r := nil;

  if (Length(Args) > 0) then
    a := @PArgs[0]
  else
    a := nil;

  Result := ffi_prep_cif(Cif, ABI, Length(Args), r, a);
end;

function LapeTypeToFFIType(VarType: TLapeType): TFFITypeManager;

  function ConvertBaseIntType(BaseType: ELapeBaseType): TFFIType;
  begin
    case BaseType of
      ltUInt8:  Result := ffi_type_uint8;
      ltInt8:   Result := ffi_type_sint8;
      ltUInt16: Result := ffi_type_uint16;
      ltInt16:  Result := ffi_type_sint16;
      ltUInt32: Result := ffi_type_uint32;
      ltInt32:  Result := ffi_type_sint32;
      ltUInt64: Result := ffi_type_uint64;
      ltInt64:  Result := ffi_type_sint64;
      else      Result := ffi_type_void;
    end;
  end;

  function FFIByteArray(Size: Integer): TFFITypeManager;
  var
    i: Integer;
  begin
    SetLength(Result.Elems, Size);
    for i := 0 to Size - 1 do
      Result.Elems[i] := TFFITypeManager.New(ffi_type_uint8);
  end;

  function FFIRecord(VarType: TLapeType_Record): TFFITypeManager;
  var
    i: Integer;
  begin
    Result := TFFITypeManager.New(ffi_type_void);
    if (VarType = nil) or (VarType.FieldMap.Count < 1) then
      LapeException(lpeInvalidCast);

    SetLength(Result.Elems, VarType.FieldMap.Count);
    for i := 0 to VarType.FieldMap.Count - 1 do
      Result.Elems[i] := LapeTypeToFFIType(VarType.FieldMap.ItemsI[i].FieldType);
  end;

  function FFIUnion(VarType: TLapeType_Union): TFFITypeManager;
  var
    i, m: Integer;
  begin
    Result := TFFITypeManager.New(ffi_type_void);
    if (VarType = nil) or (VarType.FieldMap.Count < 1) then
      LapeException(lpeInvalidCast);

    with VarType.FieldMap do
    begin
      m := 0;
      for i := 1 to Count - 1 do
        if (ItemsI[i].FieldType.Size > ItemsI[m].FieldType.Size) then
          m := i;

      Result := LapeTypeToFFIType(ItemsI[m].FieldType);
    end;
  end;

begin
  Result := TFFITypeManager.New(ffi_type_void);
  if (VarType = nil) or (VarType.Size <= 0) then
    Exit;

  if (VarType.BaseIntType <> ltUnknown) then
    Result.Typ := ConvertBaseIntType(VarType.BaseIntType)
  else if (VarType.BaseType in LapePointerTypes) then
    Result.Typ := ffi_type_pointer
  else
    case VarType.BaseType of
      ltSingle:      Result.Typ := ffi_type_float;
      ltDouble:      Result.Typ := ffi_type_double;
      ltCurrency:    Result.Typ := ffi_type_uint64;
      ltExtended:    Result.Typ := ffi_type_longdouble;
      ltSmallEnum,
      ltLargeEnum:   Result.Typ := ConvertBaseIntType(DetermineIntType(VarType.Size, False));
      ltSmallSet:    Result.Typ := ffi_type_uint32;
      ltStaticArray,
      ltShortString,
      ltVariant,
      ltLargeSet:    Result := FFIByteArray(VarType.Size);
      ltRecord:      Result := FFIRecord(VarType as TLapeType_Record);
      ltUnion:       Result := FFIUnion(VarType as TLapeType_Union);
    end;

  Result.PrepareType();
end;

function LapeParamToFFIType(Param: TLapeParameter): TFFITypeManager;
begin
  if (Param.ParType in [lptVar, lptOut]) or (Param.VarType = nil) then
    Result := TFFITypeManager.New(ffi_type_pointer)
  else
    Result := LapeTypeToFFIType(Param.VarType);
end;

function LapeHeaderToFFICif(Header: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager;
var
  i: Integer;
begin
  if (Header = nil) then
    Exit;

  if (Header.Res <> nil) then
    Result := TFFICifManager.New(LapeTypeToFFIType(Header.Res), Header.Params.Count, ABI)
  else
    Result := TFFICifManager.New(ABI, Header.Params.Count);

  for i := 0 to Header.Params.Count - 1 do
    Result.Args[i] := LapeParamToFFIType(Header.Params[i]);

  Result.PrepareCif();
end;

type
  __LapeCompiler = class(TLapeCompiler);
function LapeHeaderToFFICif(Compiler: TLapeCompiler; Header: lpString; ABI: TFFIABI = FFI_DEFAULT_ABI): TFFICifManager;
var
  c: __LapeCompiler absolute Compiler;
  s: lpString;
  Method: TLapeType_Method;
  OldState: Pointer;
begin
  if (c = nil) then
    Exit;

  OldState := c.getTempTokenizerState(Header + ';', 'ffi');
  try
    c.Expect([tk_kw_Function, tk_kw_Procedure]);
    Method := c.ParseMethodHeader(s, False);

    if (Method <> nil) then
      Result := LapeHeaderToFFICif(Method, ABI)
    else
      LapeException(lpeInvalidEvaluation);
  finally
    c.resetTokenizerState(OldState);
  end;
end;

end.

