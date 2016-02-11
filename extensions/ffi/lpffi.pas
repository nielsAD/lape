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
  lptypes, lpvartypes, lpvartypes_array, lpvartypes_record, lpcompiler;

type
  TFFITypeManager = class;
  TFFITypeManager = class(TLapeBaseClass)
  private
    PElems: array of PFFIType;
    Prepared: Boolean;
    procedure TryAlter;
  protected
    FTyp: TFFIType;
    FElems: array of record
      Typ: TFFITypeManager;
      DoFree: Boolean;
    end;

    function getTyp: TFFIType;
    procedure setTyp(ATyp: TFFIType);
    function getPTyp: PFFIType;
    procedure PrepareType;
  public
    constructor Create(FFIType: TFFIType); reintroduce;
    destructor Destroy; override;

    procedure addElem(Elem: TFFITypeManager; DoManage: Boolean = True); overload;
    procedure addElem(Elem: TFFIType); overload;

    property Typ: TFFIType read getTyp write setTyp;
    property PTyp: PFFIType read getPTyp;
  end;

  TFFIComplexReturn = (fcrNone, fcrFirstParam, fcrSecondParam, fcrLastParam);
  TArrayOfPointer = array of Pointer;

  TFFICifManager = class(TLapeBaseClass)
  private
    PArgs: array of PFFIType;
    Prepared: Boolean;
    procedure TryAlter;
  protected
    FCif: TFFICif;
    FABI: TFFIABI;
    FArgs: array of record
      Typ: TFFITypeManager;
      DoFree: Boolean;
      TakePointer: Boolean;
    end;
    FRes: record
      Typ: TFFITypeManager;
      DoFree: Boolean;
      Complex: TFFIComplexReturn;
      Eval: TLapeEvalProc;
    end;

    procedure setABI(AABI: TFFIABI);
    function getComplexRes: Boolean;
    function getCif: TFFICif;
    function getPCif: PFFICif;
    procedure PrepareCif;
  public
    constructor Create(AABI: TFFIABI = FFI_DEFAULT_ABI); reintroduce; overload;
    constructor Create(ArgTypes: array of TFFITypeManager; ResType: TFFITypeManager = nil; AABI: TFFIABI = FFI_DEFAULT_ABI); overload;
    constructor Create(ArgTypes: array of TFFIType; ResType: TFFIType; AABI: TFFIABI = FFI_DEFAULT_ABI); overload;
    destructor Destroy; override;

    procedure addArg(Arg: TFFITypeManager; DoManage: Boolean = True; TakePtr: Boolean = False); overload;
    procedure addArg(Arg: TFFIType; TakePtr: Boolean = False); overload;
    procedure setRes(ARes: TFFITypeManager; DoManage: Boolean = True; EvalProc: TLapeEvalProc = nil); overload;
    procedure setRes(ARes: TFFIType; EvalProc: TLapeEvalProc = nil); overload;
    procedure setComplexRes(Complex: TFFIComplexReturn); overload;
    procedure setComplexRes(Complex: Boolean; ImplicitSelf: Boolean); overload;

    function TakePointers(Args: PPointerArray; var Res: Pointer; TakeAddr: Boolean = True): TArrayOfPointer;
    procedure Call(Func, Res: Pointer; Args: PPointerArray; ATakePointers: Boolean = True); overload;
    procedure Call(Func, Res: Pointer); overload;
    procedure Call(Func, Res: Pointer; Args: array of Pointer; ATakePointers: Boolean = True); overload;
    procedure Call(Func: Pointer; Args: array of Pointer; ATakePointers: Boolean = True); overload;

    property ABI: TFFIABI read FABI write setABI;
    property Res: TFFITypeManager read FRes.Typ;
    property ComplexRes: Boolean read getComplexRes;

    property Cif: TFFICif read getCif;
    property PCif: PFFICif read getPCif;
  end;

  {$IFDEF FPC}generic{$ENDIF} TFFIClosureManager<TUserData> = class(TLapeBaseClass)
  public type
    TFreeUserData = procedure(const AData: TUserData);
  var private
    Prepared: Boolean;
    procedure TryAlter;
  protected
    FClosure: PFFIClosure;
    FCif: TFFICifManager;
    FCallback: TClosureBindingFunction;
    FFunc: Pointer;

    function getClosure: TFFIClosure;
    function getPClosure: PFFIClosure;
    procedure setCif(ACif: TFFICifManager);
    procedure setCallback(AFunc: TClosureBindingFunction);
    function getFunc: Pointer;
    procedure PrepareClosure;
  public
    UserData: TUserData;
    ManageCif: Boolean;
    FreeData: TFreeUserData;

    constructor Create(ACif: TFFICifManager = nil; ACallback: TClosureBindingFunction = nil); reintroduce;
    destructor Destroy; override;

    property Closure: TFFIClosure read getClosure;
    property PClosure: PFFIClosure read getPClosure;
    property Cif: TFFICifManager read FCif write setCif;
    property Callback: TClosureBindingFunction read FCallback write setCallback;
    property Func: Pointer read getFunc;
  end;

  PImportClosureData = ^TImportClosureData;
  TImportClosureData = packed record
    NativeFunc: Pointer;
    NativeCif: TFFICifManager;
    FreeCif: Boolean;
  end;
  TImportClosure = {$IFDEF FPC}specialize{$ENDIF} TFFIClosureManager<TImportClosureData>;

  TExportClosureParamInfo = record
    Size: SizeInt;
    Eval: TLapeEvalProc;
  end;

  PExportClosureData = ^TExportClosureData;
  TExportClosureData = record
    CodeBase: PByte;
    CodePos: TCodePos;
    NativeCif: TFFICifManager;
    ParamInfo: array of TExportClosureParamInfo;
    TotalParamSize: Integer;
  end;
  TExportClosure = {$IFDEF FPC}specialize{$ENDIF} TFFIClosureManager<TExportClosureData>;

const
  lpeAlterPrepared   = 'Cannot alter an already prepared object';
  lpeCannotPrepare   = 'Cannot prepare object';
  lpeUnsupportedType = 'Type "%s" cannot be used in this FFI context';

  FFI_LAPE_ABI = {$IF DEFINED(Lape_CDECL) AND DECLARED(FFI_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$IFEND};

function LapeTypeToFFIType(const VarType: TLapeType): TFFITypeManager;
function LapeFFIPointerParam(const Param: TLapeParameter; ABI: TFFIABI): Boolean;
function LapeParamToFFIType(const Param: TLapeParameter; ABI: TFFIABI): TFFITypeManager;
function LapeFFIComplexReturn(const VarType: TLapeType; ABI: TFFIABI): Boolean;
function LapeResultToFFIType(const Res: TLapeType; ABI: TFFIABI): TFFITypeManager;
function LapeResultEvalProc(const Res: TLapeType; ABI: TFFIABI): TLapeEvalProc;

function LapeHeaderToFFICif(Header: TLapeType_Method; ABI: TFFIABI = FFI_LAPE_ABI): TFFICifManager; overload;
function LapeHeaderToFFICif(Compiler: TLapeCompiler; Header: lpString; ABI: TFFIABI = FFI_LAPE_ABI): TFFICifManager; overload;

function LapeImportWrapper(Func: Pointer; NativeCif: TFFICifManager): TImportClosure; overload;
function LapeImportWrapper(Func: Pointer; Header: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI): TImportClosure; overload;
function LapeImportWrapper(Func: Pointer; Compiler: TLapeCompiler; Header: lpString; ABI: TFFIABI = FFI_DEFAULT_ABI): TImportClosure; overload;

function LapeExportWrapper(Code: PByte; Func: TCodePos; NativeCif: TFFICifManager; ParamInfo: array of TExportClosureParamInfo): TExportClosure; overload;
function LapeExportWrapper(Code: PByte; Func: TCodePos; Header: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI): TExportClosure; overload;
function LapeExportWrapper(Func: TLapeGlobalVar; ABI: TFFIABI = FFI_DEFAULT_ABI): TExportClosure; overload;

implementation

uses
  lpexceptions, lpparser, lpeval, lpinterpreter;

const
  PowerTwoRegs = [SizeOf(UInt8), SizeOf(UInt16), SizeOf(UInt32), SizeOf(UInt64)];

{$IF DEFINED(CurrencyImportExport)
  OR DEFINED(CPU86)
  OR (DEFINED(CPUX86_64) AND DEFINED(FPC) AND (NOT DEFINED(MSWINDOWS)))
}
  {$DEFINE CurrencyImportExport}
  {$ASMMODE intel}

  procedure _CurrencyImport(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
  var
    Res: Currency;
  begin
    asm
        fistp Res
    end;
    PCurrency(Dest)^ := Res;
  end;

  procedure _CurrencyExport(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
  var
    Res: Currency;
  begin
    Res := PCurrency(Left)^;
    asm
        fild Res
    end;
  end;
{$IFEND}

procedure TFFITypeManager.TryAlter;
begin
  if Prepared then
    LapeException(lpeAlterPrepared);
end;

function TFFITypeManager.getTyp: TFFIType;
begin
  PrepareType();
  Result := FTyp;
end;

procedure TFFITypeManager.setTyp(ATyp: TFFIType);
begin
  TryAlter();
  FTyp := ATyp;
end;

function TFFITypeManager.getPTyp: PFFIType;
begin
  PrepareType();
  Result := @FTyp;
end;

procedure TFFITypeManager.PrepareType;
var
  i, l: Integer;
begin
  l := Length(FElems);
  if (l < 1) or Prepared then
    Exit;

  FillChar(FTyp, SizeOf(TFFIType), 0);

  SetLength(PElems, l + 1);
  for i := 0 to l - 1 do
    PElems[i] := FElems[i].Typ.PTyp;
  PElems[l] := nil;

  FTyp._type := Ord(FFI_CTYPE_STRUCT);
  FTyp.elements := @PElems[0];
  Prepared := True;
end;

constructor TFFITypeManager.Create(FFIType: TFFIType);
begin
  inherited Create();

  FTyp := FFIType;
  Prepared := TFFI_CTYPE(FFIType._type) <> FFI_CTYPE_VOID;

  FElems := nil;
  PElems := nil;
end;

destructor TFFITypeManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FElems) do
    if FElems[i].DoFree then
      FElems[i].Typ.Free();
  inherited;
end;

procedure TFFITypeManager.addElem(Elem: TFFITypeManager; DoManage: Boolean = True);
begin
  Assert(Elem <> nil);
  TryAlter();

  SetLength(FElems, Length(FElems) + 1);
  with FElems[High(FElems)] do
  begin
    Typ := Elem;
    DoFree := DoManage;
  end;
end;

procedure TFFITypeManager.addElem(Elem: TFFIType);
begin
  addElem(TFFITypeManager.Create(Elem), True);
end;

procedure TFFICifManager.TryAlter;
begin
  if Prepared then
    LapeException(lpeAlterPrepared);
end;

procedure TFFICifManager.setABI(AABI: TFFIABI);
begin
  TryAlter();
  FABI := AABI;
end;

function TFFICifManager.getComplexRes: Boolean;
begin
  Result := FRes.Complex <> fcrNone;
end;

function TFFICifManager.getCif: TFFICif;
begin
  PrepareCif();
  Result := FCif;
end;

function TFFICifManager.getPCif: PFFICif;
begin
  PrepareCif();
  Result := @FCif;
end;

procedure TFFICifManager.PrepareCif;
var
  i: Integer;
  r, a: Pointer;
begin
  if Prepared then
    Exit;

  SetLength(PArgs, Length(FArgs));
  for i := 0 to High(FArgs) do
    PArgs[i] := FArgs[i].Typ.PTyp;

  case FRes.Complex of
    fcrFirstParam, fcrSecondParam:
      begin
        SetLength(PArgs, Length(PArgs) + 1);
        Move(PArgs[0], PArgs[1], SizeOf(PArgs[0]) * Length(FArgs));
        PArgs[0] := @ffi_type_pointer;

        if (FRes.Complex = fcrSecondParam) then
        begin
          Assert(Length(FArgs) > 0);
          Assert(FArgs[0].TakePointer);
          Swap(PArgs[0], PArgs[1]);
        end;
      end;
    fcrLastParam:
      begin
        SetLength(PArgs, Length(PArgs) + 1);
        PArgs[High(PArgs)] := @ffi_type_pointer;
      end;
  end;

  if (FRes.Typ <> nil) then
    r := FRes.Typ.PTyp
  else
    r := @ffi_type_void;

  if (Length(PArgs) > 0) then
    a := @PArgs[0]
  else
    a := @ffi_type_void;

  Assert(FFILoaded());

  if (ffi_prep_cif(FCif, FABI, Length(PArgs), r, a) <> FFI_OK) then
    LapeException(lpeCannotPrepare);

  Prepared := True;
end;

constructor TFFICifManager.Create(AABI: TFFIABI = FFI_DEFAULT_ABI);
begin
  inherited Create();
  FillChar(FCif, SizeOf(TFFICif), 0);

  PArgs := nil;
  Prepared := False;

  FABI := AABI;
  FArgs := nil;
  FRes.Typ := nil;
end;

constructor TFFICifManager.Create(ArgTypes: array of TFFITypeManager; ResType: TFFITypeManager = nil; AABI: TFFIABI = FFI_DEFAULT_ABI);
var
  i: Integer;
begin
  Create(AABI);
  setRes(ResType);
  for i := 0 to High(ArgTypes) do
    addArg(ArgTypes[i]);
end;

constructor TFFICifManager.Create(ArgTypes: array of TFFIType; ResType: TFFIType; AABI: TFFIABI = FFI_DEFAULT_ABI);
var
  i: Integer;
begin
  Create(AABI);
  setRes(ResType);
  for i := 0 to High(ArgTypes) do
    addArg(ArgTypes[i]);
end;

destructor TFFICifManager.Destroy;
var
  i: Integer;
begin
  Prepared := False;
  setRes(nil);

  for i := 0 to High(FArgs) do
    if FArgs[i].DoFree then
      FArgs[i].Typ.Free();

  inherited;
end;

procedure TFFICifManager.addArg(Arg: TFFITypeManager; DoManage: Boolean = True; TakePtr: Boolean = False);
begin
  Assert(Arg <> nil);
  TryAlter();

  SetLength(FArgs, Length(FArgs) + 1);
  with FArgs[High(FArgs)] do
  begin
    Typ := Arg;
    DoFree := DoManage;
    TakePointer := TakePtr;
  end;
end;

procedure TFFICifManager.addArg(Arg: TFFIType; TakePtr: Boolean = False);
begin
  addArg(TFFITypeManager.Create(Arg), True, TakePtr);
end;

procedure TFFICifManager.setRes(ARes: TFFITypeManager; DoManage: Boolean = True; EvalProc: TLapeEvalProc = nil);
begin
  TryAlter();

  with FRes do
  begin
    if (Typ <> nil) and (Typ <> ARes) and DoFree then
      Typ.Free();

    Typ := ARes;
    DoFree := DoManage;
    Complex := fcrNone;
    Eval := EvalProc;
  end;
end;

procedure TFFICifManager.setRes(ARes: TFFIType; EvalProc: TLapeEvalProc = nil);
begin
  setRes(TFFITypeManager.Create(ARes), True, EvalProc);
end;

procedure TFFICifManager.setComplexRes(Complex: TFFIComplexReturn);
begin
  if (Complex <> FRes.Complex) then
  begin
    setRes(nil);
    FRes.Complex := Complex;
  end;
end;

procedure TFFICifManager.setComplexRes(Complex: Boolean; ImplicitSelf: Boolean);
begin
  if (not Complex) then
    setComplexRes(fcrNone)
  else
  {$IF DECLARED(FFI_REGISTER) AND DECLARED(FFI_REGISTER) AND DECLARED(FFI_FASTCALL)}
  if (ABI in [FFI_REGISTER, FFI_PASCAL, FFI_FASTCALL]) then
    setComplexRes(fcrLastParam)
  else
  {$IFEND}
  if ImplicitSelf then
    setComplexRes(fcrSecondParam)
  else
    setComplexRes(fcrFirstParam);
end;

function TFFICifManager.TakePointers(Args: PPointerArray; var Res: Pointer; TakeAddr: Boolean = True): TArrayOfPointer;
var
  i, l, o: Integer;
begin
  Result := nil;

  o := 0;
  l := Length(FArgs);
  if (FRes.Complex <> fcrNone) then
    Inc(l);

  if (Args = nil) or (l <= 0) then
    Exit;

  SetLength(Result, l);
  case FRes.Complex of
    fcrFirstParam,
    fcrSecondParam: begin Dec(l); Inc(o); end;
    fcrLastParam:   begin Dec(l);         end;
  end;

  if TakeAddr then
    for i := 0 to l-1 do
      if FArgs[i].TakePointer then
        Result[i+o] := @Args^[i]
      else
        Result[i+o] := Args^[i]
  else
    for i := 0 to l-1 do
      if FArgs[i].TakePointer then
        Result[i] := PPointer(Args^[i+o])^
      else
        Result[i] := Args^[i+o];

  if TakeAddr then
    case FRes.Complex of
      fcrFirstParam:  begin Result[0] := @Res;                             end;
      fcrSecondParam: begin Result[0] := @Res; Swap(Result[0], Result[1]); end;
      fcrLastParam:   begin Result[l] := @Res;                             end;
    end
  else
  begin
    case FRes.Complex of
      fcrFirstParam:  begin Res := Args^[0]; Result[l] := PPointer(Res)^; end;
      fcrSecondParam: begin Res := Args^[0]; Result[l] := PPointer(Res)^; Swap(Result[0], Result[l]); end;
      fcrLastParam:   begin Res := Args^[l]; Result[l] := PPointer(Res)^; end;
    end;
  end;
end;

procedure TFFICifManager.Call(Func, Res: Pointer; Args: PPointerArray; ATakePointers: Boolean = True);
var
  p: TArrayOfPointer;
  r: UInt64;
begin
  PrepareCif();
  if ATakePointers then
  begin
    p := TakePointers(Args, Res);
    if (Length(p) > 0) then
      Args := @p[0]
    else
      Args := nil;
  end;

  if ({$IFNDEF FPC}@{$ENDIF}FRes.Eval = nil) then
    ffi_call(FCif, Func, Res, Args)
  else
  begin
    //Workaround for libffi quirk that causes integers to always be returned in "full" size
    r := 0;
    ffi_call(FCif, Func, @r, Args);
    FRes.Eval(Res, @r, nil);
  end;
end;

procedure TFFICifManager.Call(Func, Res: Pointer);
begin
  Call(Func, Res, PPointerArray(nil), False);
end;

procedure TFFICifManager.Call(Func, Res: Pointer; Args: array of Pointer; ATakePointers: Boolean = True);
begin
  if (Length(Args) > 0) then
    Call(Func, Res, PPointerArray(@Args[0]), ATakePointers)
  else
    Call(Func, Res);
end;

procedure TFFICifManager.Call(Func: Pointer; Args: array of Pointer; ATakePointers: Boolean = True);
begin
  Call(Func, nil, Args, ATakePointers);
end;

procedure TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.TryAlter;
begin
  if Prepared then
    LapeException(lpeAlterPrepared);
end;

function TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.getClosure: TFFIClosure;
begin
  PrepareClosure();
  Result := FClosure^;
end;

function TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.getPClosure: PFFIClosure;
begin
  PrepareClosure();
  Result := FClosure;
end;

procedure TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.setCif(ACif: TFFICifManager);
begin
  TryAlter();
  if (FCif <> nil) and (FCif <> ACif) and ManageCif then
    FCif.Free();
  FCif := ACif;
end;

procedure TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.setCallback(AFunc: TClosureBindingFunction);
begin
  TryAlter();
  FCallback := AFunc;
end;

function TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.getFunc: Pointer;
begin
  PrepareClosure();
  Result := FFunc;
end;

procedure TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.PrepareClosure;
begin
  if Prepared then
    Exit;

  Assert(FFILoaded());

  if (FCif = nil) or
     ({$IFNDEF FPC}@{$ENDIF}FCallback = nil) or
     (ffi_prep_closure_loc(FClosure^, FCif.PCif^, FCallback, @UserData, FFunc) <> FFI_OK)
  then
    LapeException(lpeCannotPrepare);

  Prepared := True;
end;

constructor TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.Create(ACif: TFFICifManager = nil; ACallback: TClosureBindingFunction = nil);
begin
  inherited Create();
  Assert(FFILoaded());

  Prepared := False;
  ManageCif := True;
  FreeData := nil;

  FClosure := ffi_closure_alloc(SizeOf(TFFIClosure), FFunc);
  FCif := ACif;
  FCallback := ACallback;
end;

destructor TFFIClosureManager{$IFNDEF FPC}<TUserData>{$ENDIF}.Destroy;
begin
  if ({$IFNDEF FPC}@{$ENDIF}FreeData <> nil) then
    FreeData(UserData);
  if (FCif <> nil) and ManageCif then
    FCif.Free();
  if (FClosure <> nil) then
    ffi_closure_free(FClosure);
  inherited;
end;

function LapeTypeToFFIType(const VarType: TLapeType): TFFITypeManager;

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

  procedure FFIArray(Size: SizeInt; FFIType: TFFITypeManager);
  var
    i: Integer;
  begin
    for i := 0 to Size - 1 do
      Result.addElem(FFIType, i = 0);
  end;

  procedure FFIStaticArray(VarType: TLapeType_StaticArray);
  begin
    if (VarType = nil) or (VarType.Range.Lo > VarType.Range.Hi) then
      LapeException(lpeInvalidCast);
    FFIArray(VarType.Range.Hi - VarType.Range.Lo + 1, LapeTypeToFFIType(VarType.PType));
  end;

  procedure FFIRecord(VarType: TLapeType_Record);
  begin
    if (VarType = nil) or (VarType.FieldMap.Count < 1) then
      LapeException(lpeInvalidCast);

    // Treat records as blobs to enforce record alignment
    FFIArray(VarType.Size, TFFITypeManager.Create(ffi_type_uint8));

    //for i := 0 to VarType.FieldMap.Count - 1 do
    //  Result.addElem(LapeTypeToFFIType(VarType.FieldMap.ItemsI[i].FieldType));
  end;

  procedure FFIUnion(VarType: TLapeType_Union);
  var
    i, m: Integer;
  begin
    if (VarType = nil) or (VarType.FieldMap.Count < 1) then
      LapeException(lpeInvalidCast);

    with VarType.FieldMap do
    begin
      m := 0;
      for i := 1 to Count - 1 do
        if (ItemsI[i].FieldType.Size > ItemsI[m].FieldType.Size) then
          m := i;

      Result.addElem(LapeTypeToFFIType(ItemsI[m].FieldType));
    end;
  end;

begin
  Result := TFFITypeManager.Create(ffi_type_void);
  if (VarType = nil) or (VarType.Size <= 0) then
    Exit;

  try
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
        ltLargeEnum,
        ltSmallSet:    Result.Typ := ConvertBaseIntType(DetermineIntType(VarType.Size, False));
        ltShortString,
        ltVariant,
        ltLargeSet:    FFIArray(VarType.Size, TFFITypeManager.Create(ffi_type_uint8));
        ltStaticArray: FFIStaticArray(VarType as TLapeType_StaticArray);
        ltRecord:      FFIRecord(VarType as TLapeType_Record);
        ltUnion:       FFIUnion(VarType as TLapeType_Union);
        else Assert(False);
      end;
  except
    FreeAndNil(Result);
  end;
end;

function LapeFFIPointerParam(const Param: TLapeParameter; ABI: TFFIABI): Boolean;
begin
  //http://docwiki.embarcadero.com/RADStudio/en/Program_Control#Register_Convention
  if (Param.VarType = nil) or (Param.ParType in Lape_RefParams) then
    Exit(True);

  {$IF DEFINED(CPU86) AND DECLARED(FFI_CDECL) AND DECLARED(FFI_MS_CDECL)}
    if (ABI in [FFI_CDECL, FFI_MS_CDECL]) then
      Exit(Param.VarType.BaseType in [ltShortString, ltStaticArray]);
  {$IFEND}

  Result := (Param.VarType.BaseType in [ltShortString, ltLargeSet])
      or   ((Param.ParType in Lape_ConstParams) and (Param.VarType.BaseType = ltRecord) and (Param.VarType.Size > SizeOf(Pointer)))

  {$IF DECLARED(FFI_REGISTER) AND DECLARED(FFI_PASCAL) AND DECLARED(FFI_FASTCALL)}
      or ((ABI  =  FFI_REGISTER)                            and (Param.VarType.BaseType in [ltVariant]))
      or ((ABI in [FFI_REGISTER, FFI_PASCAL, FFI_FASTCALL]) and (Param.VarType.BaseType in [ltRecord, ltStaticArray]) and (Param.VarType.Size > SizeOf(Pointer)))
  {$IFEND}

  {$IF DECLARED(FFI_STDCALL)}
      or ((ABI = FFI_STDCALL) and (Param.VarType.BaseType in [ltVariant]) and (Param.ParType in Lape_ConstParams))
  {$IFEND}

  {$IFDEF CPUX86_64}
      or (Param.VarType.BaseType in [ltStaticArray, ltVariant])
  {$ENDIF}

  {$IF DECLARED(FFI_WIN64)}
      or ((ABI = FFI_WIN64) and (Param.VarType.BaseType = ltRecord) and (not (UInt8(Param.VarType.Size) in PowerTwoRegs)))
      or ((ABI = FFI_WIN64) and (Param.VarType.BaseType = ltRecord) and (Param.ParType in Lape_ConstParams) and (Param.VarType.Size = SizeOf(UInt8)))
  {$IFEND}
  ;
end;

function LapeParamToFFIType(const Param: TLapeParameter; ABI: TFFIABI): TFFITypeManager;
begin
  if LapeFFIPointerParam(Param, ABI) then
    Result := TFFITypeManager.Create(ffi_type_pointer)
{$IFDEF CPU86}
  else if (Param.VarType <> nil) and (Param.VarType.BaseType = ltDynArray) then
    Result := TFFITypeManager.Create(ffi_type_float) // Force on stack
{$ENDIF}
  else
    Result := LapeTypeToFFIType(Param.VarType);
end;

function LapeFFIComplexReturn(const VarType: TLapeType; ABI: TFFIABI): Boolean;
const
  ComplexTypes = LapeStringTypes + [ltStaticArray, ltLargeSet, ltVariant];
begin
  Result := False;

  if (VarType = nil) or (VarType.BaseType in ComplexTypes) {$IF FPC_VERSION >= 3}or VarType.NeedFinalization{$IFEND} then
    Exit(True);

  {$IFDEF CPU86}
    Result := (VarType.BaseType = ltRecord)
      {$IF DECLARED(FFI_CDECL) AND DECLARED(FFI_MS_CDECL)}
        and ((not (ABI in [FFI_CDECL, FFI_MS_CDECL])) or (not (UInt8(VarType.Size) in PowerTwoRegs)))
      {$IFEND}
    ;
  {$ENDIF}

  {$IF DECLARED(FFI_UNIX64)}
    Result := (ABI = FFI_UNIX64) and (VarType.BaseType = ltRecord) and (VarType.Size > SizeOf(Pointer));
  {$IFEND}

  {$IF DECLARED(FFI_WIN64)}
    Result := (ABI = FFI_WIN64)  and (VarType.BaseType = ltRecord) and (not (UInt8(VarType.Size) in PowerTwoRegs));
  {$IFEND}
end;

function LapeResultToFFIType(const Res: TLapeType; ABI: TFFIABI): TFFITypeManager;
begin
  {$IFDEF CurrencyImportExport}
  if (Res <> nil) and (Res.BaseType = ltCurrency) then
    Result := TFFITypeManager.Create(ffi_type_void)
  else
  {$ENDIF}
    Result := LapeTypeToFFIType(Res);
end;

function LapeResultEvalProc(const Res: TLapeType; ABI: TFFIABI): TLapeEvalProc;
begin
  if (Res = nil) then
    Exit(nil);
  if (Res.BaseType in LapeIntegerTypes) and (Res.Size < SizeOf(NativeInt)) then
  begin
    Result := LapeEval_GetProc(op_Assign, DetermineIntType(Res.size, False), ltNativeUInt);
    if (not ValidEvalFunction(Result)) then
      Result := nil;
  end
  {$IFDEF CurrencyImportExport}
  else if (Res.BaseType = ltCurrency) then
    Result := @_CurrencyImport
  {$ENDIF}
  else
    Result := nil;
end;

function LapeHeaderToFFICif(Header: TLapeType_Method; ABI: TFFIABI = FFI_LAPE_ABI): TFFICifManager;
var
  i: Integer;
  SelfParam: TLapeParameter;
begin
  if (Header = nil) then
    Exit(nil);

  Result := TFFICifManager.Create(ABI);
  if (Header.Res <> nil) then
    if LapeFFIComplexReturn(Header.Res, ABI) then
      Result.setComplexRes(True, MethodOfObject(Header))
    else
      Result.setRes(LapeResultToFFIType(Header.Res, ABI), True, LapeResultEvalProc(Header.Res, ABI));

  try
    if MethodOfObject(Header) then
    begin
      SelfParam := NullParameter;
      if (Header is TLapeType_MethodOfType) then
      begin
        SelfParam.ParType := TLapeType_MethodOfType(Header).SelfParam;
        SelfParam.VarType := TLapeType_MethodOfType(Header).ObjectType;
      end
      else
        SelfParam.ParType := Lape_SelfParam;
      Result.addArg(LapeParamToFFIType(SelfParam, ABI), True, LapeFFIPointerParam(SelfParam, ABI))
    end;

    for i := 0 to Header.Params.Count - 1 do
      Result.addArg(LapeParamToFFIType(Header.Params[i], ABI), True, LapeFFIPointerParam(Header.Params[i], ABI));
  except
    FreeAndNil(Result);
  end;
end;

type
  __LapeCompiler = class(TLapeCompiler);
function LapeHeaderToFFICif(Compiler: TLapeCompiler; Header: lpString; ABI: TFFIABI = FFI_LAPE_ABI): TFFICifManager;
var
  c: __LapeCompiler absolute Compiler;
  s: lpString;
  Method: TLapeType_Method;
  OldState: Pointer;
begin
  if (c = nil) or (Header = '') then
    Exit(nil);

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

procedure FreeImportClosureData(const AData: TImportClosureData);
begin
  if (AData.NativeCif <> nil) and AData.FreeCif then
    AData.NativeCif.Free();
end;

procedure LapeImportBinder(var Cif: TFFICif; Res: Pointer; Args: PPointerArray; UserData: Pointer); cdecl;
begin
  with PImportClosureData(UserData)^ do
    if (NativeCif.Res <> nil) or NativeCif.ComplexRes then
      NativeCif.Call(NativeFunc, Pointer(Args^[1]^), PPointerArray(Args^[0]^))
    else
      NativeCif.Call(NativeFunc, nil, PPointerArray(Args^[0]^));
end;

function LapeImportWrapper(Func: Pointer; NativeCif: TFFICifManager): TImportClosure;
begin
  Assert(NativeCif <> nil);

  Result := TImportClosure.Create(TFFICifManager.Create(FFI_LAPE_ABI), @LapeImportBinder);
  Result.FreeData := @FreeImportClosureData;

  try
    Result.UserData.NativeFunc := Func;
    Result.UserData.NativeCif := NativeCif;
    Result.UserData.FreeCif := True;

    Result.Cif.addArg(ffi_type_pointer);
    if (Result.UserData.NativeCif.Res <> nil) or Result.UserData.NativeCif.ComplexRes then
      Result.Cif.addArg(ffi_type_pointer);
  except
    FreeAndNil(Result);
  end;
end;

function LapeImportWrapper(Func: Pointer; Header: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI): TImportClosure;
begin
  Result := LapeImportWrapper(Func, LapeHeaderToFFICif(Header, ABI));
end;

function LapeImportWrapper(Func: Pointer; Compiler: TLapeCompiler; Header: lpString; ABI: TFFIABI = FFI_DEFAULT_ABI): TImportClosure;
begin
  Result := LapeImportWrapper(Func, LapeHeaderToFFICif(Compiler, Header, ABI));
end;

procedure LapeExportBinder(var Cif: TFFICif; Res: Pointer; Args: PPointerArray; UserData: Pointer); cdecl;
var
  i, b: Integer;
  r: UInt64;
  VarStack: TByteArray;
  Pointers: TArrayOfPointer;
begin
  with PExportClosureData(UserData)^ do
  begin
    SetLength(VarStack, TotalParamSize);

    Pointers := NativeCif.TakePointers(Args, Res, False);
    Assert(Length(ParamInfo) - Length(Pointers) <= 1);

    b := 0;
    for i := 0 to High(Pointers) do
      if (ParamInfo[i].Size < 0) then
      begin
        PPointer(@VarStack[b])^ := Pointers[i];
        Inc(b, SizeOf(Pointer));
      end
      else
      begin
        if ({$IFNDEF FPC}@{$ENDIF}ParamInfo[i].Eval <> nil) then
        begin
          FillChar(VarStack[b], ParamInfo[i].Size, 0);
          ParamInfo[i].Eval(@VarStack[b], Pointers[i], nil)
        end
        else
          Move(Pointers[i]^, VarStack[b], ParamInfo[i].Size);
        Inc(b, ParamInfo[i].Size);
      end;

    if (NativeCif.Res <> nil) then
      if ({$IFNDEF FPC}@{$ENDIF}ParamInfo[High(ParamInfo)].Eval <> nil) then
      begin
        r := 0;
        PPointer(@VarStack[b])^ := @r;
      end
      else
      begin
        FillChar(Res^, NativeCif.Res.Typ.size, 0);
        PPointer(@VarStack[b])^ := Res;
      end;

    RunCode(CodeBase, VarStack, CodePos);

    if (NativeCif.Res <> nil) and ({$IFNDEF FPC}@{$ENDIF}ParamInfo[High(ParamInfo)].Eval <> nil) then
      ParamInfo[High(ParamInfo)].Eval(Res, @r, nil);
  end;
end;

function LapeExportWrapper(Code: PByte; Func: TCodePos; NativeCif: TFFICifManager; ParamInfo: array of TExportClosureParamInfo): TExportClosure;
var
  i: Integer;
begin
  Assert(NativeCif <> nil);

  Result := TExportClosure.Create(NativeCif, @LapeExportBinder);
  try
    Result.UserData.CodeBase := Code;
    Result.UserData.CodePos := Func;
    Result.UserData.NativeCif := NativeCif;
    Result.UserData.TotalParamSize := 0;

    SetLength(Result.UserData.ParamInfo, Length(ParamInfo));
    for i := 0 to High(ParamInfo) do
    begin
      Result.UserData.ParamInfo[i] := ParamInfo[i];
      if (ParamInfo[i].Size < 0) then
        Inc(Result.UserData.TotalParamSize, SizeOf(Pointer))
      else
        Inc(Result.UserData.TotalParamSize, ParamInfo[i].Size);
    end;
  except
    FreeAndNil(Result);
  end;
end;

function LapeExportWrapper(Code: PByte; Func: TCodePos; Header: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI): TExportClosure;
const
  RefPar: TExportClosureParamInfo = (Size: -1; Eval: nil);
var
  i, c: Integer;
  ParInfo: array of TExportClosureParamInfo;
begin
  if (Header = nil) or (Header.BaseType <> ltScriptMethod) then
    Exit(nil);

  c := 0;
  SetLength(ParInfo, Header.Params.Count);

  if MethodOfObject(Header) then
  begin
    SetLength(ParInfo, Length(ParInfo) + 1);
    ParInfo[0] := RefPar;
    Inc(c);
  end;

  for i := 0 to Header.Params.Count - 1 do
  begin
    if (Header.Params[i].ParType in Lape_RefParams) or (Header.Params[i].VarType = nil) then
      ParInfo[c] := RefPar
    else
    begin
      ParInfo[c].Size := Header.Params[i].VarType.Size;

      if (ParInfo[c].Size = LapeTypeSize[Header.Params[i].VarType.BaseType]) then
      begin
        ParInfo[c].Eval := getEvalProc(op_Assign, Header.Params[i].VarType.BaseType, Header.Params[i].VarType.BaseType);
        if (not ValidEvalFunction({$IFNDEF FPC}@{$ENDIF}ParInfo[c].Eval)) then
          ParInfo[c].Eval := nil;
      end
      else
        ParInfo[c].Eval := nil;

      if ({$IFNDEF FPC}@{$ENDIF}ParInfo[c].Eval = nil) and Header.Params[i].VarType.NeedFinalization then
        LapeExceptionFmt(lpeUnsupportedType, [Header.Params[i].VarType.AsString]);
    end;
    Inc(c);
  end;

  if (Header.Res <> nil) then
  begin
    SetLength(ParInfo, c + 1);
    ParInfo[c] := RefPar;

    {$IFDEF CurrencyImportExport}
    if (Header.Res.BaseType = ltCurrency) then
      ParInfo[c].Eval := @_CurrencyExport;
    {$ENDIF}
  end;

  Result := LapeExportWrapper(Code, Func, LapeHeaderToFFICif(Header, ABI), ParInfo);
end;

function LapeExportWrapper(Func: TLapeGlobalVar; ABI: TFFIABI = FFI_DEFAULT_ABI): TExportClosure;
begin
  if (Func = nil) or (not Func.HasType()) or (Func.VarType.Size < SizeOf(Pointer)) or
     (Func.VarType.Compiler = nil) or (Func.VarType.Compiler.Emitter = nil) or
     (PCodePos(Func.Ptr)^ = 0) or (PCodePos(Func.Ptr)^ = EndJump)
  then
    Exit(nil);

  Result := LapeExportWrapper(
    Func.VarType.Compiler.Emitter.Code,
    TCodePos(Func.Ptr^),
    Func.VarType as TLapeType_Method,
    ABI);
end;

end.

