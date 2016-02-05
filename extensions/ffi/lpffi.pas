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
      Eval: TLapeEvalProc;
    end;

    procedure setABI(AABI: TFFIABI);
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
    procedure setRes(ARes: TFFITypeManager; DoManage: Boolean = True); overload;
    procedure setRes(ARes: TFFIType); overload;

    function TakePointers(Args: PPointerArray; TakeAddr: Boolean = True): TArrayOfPointer;
    procedure Call(Func, Res: Pointer; Args: PPointerArray; ATakePointers: Boolean = True); overload;
    procedure Call(Func, Res: Pointer); overload;
    procedure Call(Func, Res: Pointer; Args: array of Pointer; ATakePointers: Boolean = True); overload;
    procedure Call(Func: Pointer; Args: array of Pointer; ATakePointers: Boolean = True); overload;

    property ABI: TFFIABI read FABI write setABI;
    property Res: TFFITypeManager read FRes.Typ;
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

  FFI_LAPE_ABI = {$IF DEFINED(Lape_CDECL) AND DECLARED(FFI_SYSV)}FFI_SYSV{$ELSE}FFI_DEFAULT_ABI{$IFEND};

var
  ffi_type_complex: TFFIType;

function LapeTypeToFFIType(const VarType: TLapeType): TFFITypeManager;
function LapeFFIPointerParam(const Param: TLapeParameter; ABI: TFFIABI): Boolean;
function LapeParamToFFIType(const Param: TLapeParameter; ABI: TFFIABI): TFFITypeManager;
function LapeFFIComplexReturn(const VarType: TLapeType; ABI: TFFIABI): Boolean;
function LapeResultToFFIType(const Res: TLapeType; ABI: TFFIABI): TFFITypeManager;
function LapeResultEvalProc(const Res: TFFIType): TLapeEvalProc;

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

  if (FRes.Typ <> nil) then
    r := FRes.Typ.PTyp
  else
    r := @ffi_type_void;

  if (Length(FArgs) > 0) then
    a := @PArgs[0]
  else
    a := @ffi_type_void;

  Assert(FFILoaded());

  if (ffi_prep_cif(FCif, FABI, Length(FArgs), r, a) <> FFI_OK) then
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

procedure TFFICifManager.setRes(ARes: TFFITypeManager; DoManage: Boolean = True);
begin
  TryAlter();

  with FRes do
  begin
    if (Typ <> nil) and (Typ <> ARes) and DoFree then
      Typ.Free();

    Typ := ARes;
    DoFree := DoManage;

    if (ARes <> nil) then
      Eval := LapeResultEvalProc(ARes.Typ)
    else
      Eval := nil;
  end;
end;

procedure TFFICifManager.setRes(ARes: TFFIType);
begin
  setRes(TFFITypeManager.Create(ARes), True);
end;

function TFFICifManager.TakePointers(Args: PPointerArray; TakeAddr: Boolean = True): TArrayOfPointer;
var
  i: Integer;
begin
  Result := nil;
  if (Args = nil) or (Length(FArgs) <= 0) then
    Exit;

  SetLength(Result, Length(FArgs));
  if TakeAddr then
    for i := 0 to High(Result) do
      if FArgs[i].TakePointer then
        Result[i] := @Args^[i]
      else
        Result[i] := Args^[i]
  else
    for i := 0 to High(Result) do
      if FArgs[i].TakePointer then
        Result[i] := PPointer(Args^[i])^
      else
        Result[i] := Args^[i];
end;

procedure TFFICifManager.Call(Func, Res: Pointer; Args: PPointerArray; ATakePointers: Boolean = True);
var
  p: TArrayOfPointer;
  r: NativeUInt;
begin
  PrepareCif();
  if ATakePointers then
  begin
    p := TakePointers(Args);
    if (Length(p) > 0) then
      Args := @p[0]
    else
      Args := nil;
  end;

  if ({$IFNDEF FPC}@{$ENDIF}FRes.Eval = nil) then
    ffi_call(FCif, Func, Res, Args)
  else
  begin
    //Workaround for libffi quirk that integers are always returned in full size
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
  var
    i: Integer;
  begin
    if (VarType = nil) or (VarType.FieldMap.Count < 1) then
      LapeException(lpeInvalidCast);

    for i := 0 to VarType.FieldMap.Count - 1 do
      Result.addElem(LapeTypeToFFIType(VarType.FieldMap.ItemsI[i].FieldType));
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

      Result := LapeTypeToFFIType(ItemsI[m].FieldType);
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
const
  ConstPointerParams = LapeStructTypes;
begin
  //http://docwiki.embarcadero.com/RADStudio/en/Program_Control#Register_Convention
  Result :=
    (Param.ParType in Lape_RefParams) or
    (Param.VarType = nil) or
    ((Param.ParType in Lape_ConstParams) and (Param.VarType.BaseType in ConstPointerParams) and (Param.VarType.Size > SizeOf(Pointer)))

    {$IFDEF CPU86}
        or (Param.VarType.BaseType in [ltVariant, ltShortString, ltLargeSet])
        or ((Param.VarType.BaseType in [ltRecord, ltStaticArray]) and (Param.VarType.Size > SizeOf(Pointer)))
    {$ENDIF}

    {$IFDEF CPUX86_64}
        or (Param.VarType.BaseType in [ltVariant, ltShortString, ltStaticArray, ltLargeSet])
    {$ENDIF}
  ;
end;

function LapeParamToFFIType(const Param: TLapeParameter; ABI: TFFIABI): TFFITypeManager;
begin
  if LapeFFIPointerParam(Param, ABI) then
    Result := TFFITypeManager.Create(ffi_type_pointer)
{$IFDEF CPU86}
  else if (ABI in [FFI_REGISTER, FFI_PASCAL]) and (Param.VarType <> nil) and (Param.VarType.BaseType = ltDynArray) then
    Result := TFFITypeManager.Create(ffi_type_float) // Force on stack
{$ENDIF}
  else
    Result := LapeTypeToFFIType(Param.VarType);
end;

function LapeFFIComplexReturn(const VarType: TLapeType; ABI: TFFIABI): Boolean;
begin
  Result := (VarType <> nil) and ((VarType.BaseType in LapeArrayTypes) or VarType.NeedFinalization);
end;

function LapeResultToFFIType(const Res: TLapeType; ABI: TFFIABI): TFFITypeManager;
begin
  if LapeFFIComplexReturn(Res, ABI) then
    Result := TFFITypeManager.Create(ffi_type_complex)
  else
    Result := LapeTypeToFFIType(Res);
end;

function LapeResultEvalProc(const Res: TFFIType): TLapeEvalProc;
const
  FFI_IntegerTypes = [FFI_CTYPE_INT, FFI_CTYPE_UINT8..FFI_CTYPE_SINT64];
begin
  if (Res.size < SizeOf(NativeInt)) and (TFFI_CTYPE(Res._type) in FFI_IntegerTypes) then
  begin
    Result := LapeEval_GetProc(op_Assign, DetermineIntType(Res.size, False), ltNativeUInt);
    if (not ValidEvalFunction(Result)) then
      Result := nil;
  end
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
    Result.setRes(LapeResultToFFIType(Header.Res, ABI));

  try
    if MethodOfObject(Header) then
    begin
      {$IF DEFINED(FFI_PASCAL)}
      if (ABI = FFI_PASCAL) then
        LapeException(lpeImpossible);
      {$IFEND}

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
    if (NativeCif.Res <> nil) then
      NativeCif.Call(NativeFunc, Pointer(args^[1]^), PPointerArray(Args^[0]^))
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
    if (Result.UserData.NativeCif.Res <> nil) then
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
  VarStack: TByteArray;
  Pointers: TArrayOfPointer;
begin
  with PExportClosureData(UserData)^ do
  begin
    SetLength(VarStack, TotalParamSize);

    Pointers := NativeCif.TakePointers(Args, False);
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
        if ValidEvalFunction({$IFNDEF FPC}@{$ENDIF}ParamInfo[i].Eval) then
        begin
          FillChar(VarStack[b], ParamInfo[i].Size, 0);
          ParamInfo[i].Eval(@VarStack[b], Pointers[i], nil)
        end
        else
          Move(Pointers[i]^, VarStack[b], ParamInfo[i].Size);
        Inc(b, ParamInfo[i].Size);
      end;

    if (NativeCif.Res <> nil) then
      PPointer(@VarStack[b])^ := Res;

    RunCode(CodeBase, VarStack, CodePos);
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
    {$IF DECLARED(FFI_PASCAL)}
    if (ABI = FFI_PASCAL) then
      LapeException(lpeImpossible);
    {$IFEND}

    SetLength(ParInfo, Length(ParInfo) + 1);
    ParInfo[0] := RefPar;
    Inc(c);
  end;

  for i := 0 to High(ParInfo) do
  begin
    if (Header.Params[i].ParType in Lape_RefParams) or (Header.Params[i].VarType = nil) then
      ParInfo[c] := RefPar
    else
    begin
      ParInfo[c].Size := Header.Params[i].VarType.Size;

      if (ParInfo[c].Size = LapeTypeSize[Header.Params[i].VarType.BaseType]) then
        ParInfo[c].Eval := getEvalProc(op_Assign, Header.Params[i].VarType.BaseType, Header.Params[i].VarType.BaseType)
      else
        ParInfo[c].Eval := nil;

      if (not ValidEvalFunction(ParInfo[c].Eval)) and Header.Params[i].VarType.NeedFinalization then
        LapeExceptionFmt(lpeUnsupportedType, [Header.Params[i].VarType.AsString]);
    end;
    Inc(c);
  end;

  if (Header.Res <> nil) then
  begin
    SetLength(ParInfo, c + 1);
    ParInfo[c] := RefPar;
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

var
  ffi_type_complex_manager: TFFITypeManager;
initialization
  ffi_type_complex_manager := TFFITypeManager.Create(ffi_type_void);
  ffi_type_complex_manager.addElem(ffi_type_pointer);
  ffi_type_complex_manager.addElem(ffi_type_pointer);
  ffi_type_complex_manager.addElem(ffi_type_pointer);

  ffi_type_complex := ffi_type_complex_manager.Typ;

finalization
  ffi_type_complex_manager.Free();
end.

