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
  Classes, SysUtils,
  lptypes, lpvartypes, lpvartypes_array, lpcompiler, lptree,
  ffi, lpffiwrappers;

type
  TLapeType_NativeMethod = class(TLapeType)
  protected
    FHeader: TLapeType_Method;
    FABI: TFFIABI;

    function getAsString: lpString; override;
  public
    constructor Create(AHeader: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    constructor Create(ACompiler: TLapeCompiler; AHeader: lpString; ABI: TFFIABI = FFI_DEFAULT_ABI; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; overload; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;

    function NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property Header: TLapeType_Method read FHeader;
    property ABI: TFFIABI read FABI;
  end;

  TLapeType_ClosureDisposer = class(TLapeType)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    procedure Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil); override;
  end;

  TLapeTree_InternalMethod_Native = class(TLapeTree_InternalMethod)
  protected
    FABIType: TLapeType;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    procedure ClearCache; override;
    function isConstant: Boolean; override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Natify = class(TLapeTree_InternalMethod_Native)
  protected
    FClosures: TLapeGlobalVar;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  TLapeTree_InternalMethod_Lapify = class(TLapeTree_InternalMethod)
  protected
    FClosures: TLapeGlobalVar;
  public
    constructor Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil); override;
    function resType: TLapeType; override;
    function Evaluate: TLapeGlobalVar; override;
    function Compile(var Offset: Integer): TResVar; override;
  end;

  FFIInit = (fsiNative, fsiNatify, fsiLapify, fsiDynLibs, fsiExternal);
  FFIInitSet = set of FFIInit;

  TFFINatifyClosures = array of TExportClosure;
  TFFILapifyClosures = array of TImportClosure;

procedure InitializeFFI(Compiler: TLapeCompiler; Initialize: FFIInitSet = [fsiNative, fsiNatify, fsiLapify, fsiDynLibs, fsiExternal]);

implementation

uses
  {$IFDEF FPC}dynlibs{$ELSE}Windows{$ENDIF},
  lpexceptions;

{$IFNDEF FPC}
type
  TLibHandle  = THandle;
const
  SharedSuffix = {$IFDEF MSWINDOWS}'dll'{$ELSE}'so'{$ENDIF};
{$ENDIF}

procedure _ClosureDisposer(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  FreeAndNil(Params^[0]^);
end;

function AddNatifyClosure(var l: TFFINatifyClosures; c: TExportClosure): SizeInt;
begin
  Result := Length(l);
  SetLength(l, Result + 1);
  l[Result] := c;
end;

function AddLapifyClosure(var l: TFFILapifyClosures; c: TImportClosure): SizeInt;
begin
  Result := Length(l);
  SetLength(l, Result + 1);
  l[Result] := c;
end;

procedure _Natify(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Natify(var p: TCodePos; var c: TFFINatifyClosures; i: SizeInt; out Result: Pointer);
  var
    b, r: TExportClosure;
  begin
    Assert(i >= 0);
    Assert(i <= Length(c));
    b := c[i];
    Assert(b.UserData.CodeBase <> nil);
    Assert(b.UserData.CodePos = nil);

    r := TExportClosure.Create(b.Cif, b.Callback);
    r.ManageCif := False;

    r.UserData := b.UserData;
    r.UserData._CodePos := p;
    r.UserData.CodePos := @r.UserData._CodePos;

    AddNatifyClosure(c, r);
    Result := r.Func;
  end;

begin
  Natify(TCodePos(Params^[0]^), TFFINatifyClosures(Params^[1]^), SizeInt(Params^[2]^), PPointer(Result)^);
end;

procedure _Lapify(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

  procedure Lapify(p: Pointer; var c: TFFILapifyClosures; i: SizeInt; out Result: Pointer);
  var
    b, r: TImportClosure;
  begin
    Assert(i >= 0);
    Assert(i <= Length(c));
    b := c[i];
    Assert(b.UserData.FreeCif);
    Assert(b.UserData.NativeFunc = nil);

    r := TImportClosure.Create(b.Cif, b.Callback);
    r.ManageCif := False;

    r.UserData := b.UserData;
    r.UserData.FreeCif := False;
    r.UserData.NativeFunc := p;

    AddLapifyClosure(c, r);
    Result := r.Func;
  end;

begin
  Lapify(Pointer(Params^[0]^), TFFILapifyClosures(Params^[1]^), SizeInt(Params^[2]^), PPointer(Result)^);
end;

procedure _LapeLoadLibrary(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TLibHandle(Result^) := LoadLibrary(PlpString(Params^[0])^);
end;

procedure _LapeGetProcAddress(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := GetProcAddress(TLibHandle(Params^[0]^), PlpString(Params^[1])^);
end;

procedure _LapeFreeLibrary(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEvalBool(Result)^ := FreeLibrary(TLibHandle(Params^[0]^));
end;

type
  TExternalCompiler = class(TLapeCompiler) // TODO: class helper
    function HandleExternal(Sender: TLapeCompiler; Header: TLapeGlobalVar): Boolean;
  end;

function TExternalCompiler.HandleExternal(Sender: TLapeCompiler; Header: TLapeGlobalVar): Boolean;
var
  Arg: TLapeTree_ExprBase;
  ArgStr, LibName, FuncName: lpString;
  Closure: TImportClosure;
  Func: Pointer;
  Lib: TLibHandle;
  ABI: TFFIABI;
  p: SizeInt;
begin
  Arg := ParseExpression();
  if (Arg = nil) then
    ArgStr := ''
  else if (Arg.resType() = nil) or (not (Arg.resType().BaseType in LapeStringTypes)) then
    LapeExceptionFmt(lpeExpected, [LapeTypeToString(ltString)], DocPos)
  else
  begin
    ArgStr := Arg.Evaluate().AsString;
    Assert(Length(ArgStr) >= 2);
    ArgStr := Copy(ArgStr, 2, Length(ArgStr) - 2);
  end;

  if (Header = nil) or (not (Header.HasType())) or (not (Header.VarType is TLapeType_Method)) then
    Exit(False);

  p := LastDelimiter(' @', ArgStr);
  if (p > 0) and (ArgStr[p] = ' ') then
  begin
    ABI := StrToABI(Copy(ArgStr, p + 1, Length(ArgStr) - p));
    if (ABI = FFI_UNKNOWN_ABI) then
      LapeException(lpeInvalidCast, DocPos);

    Delete(ArgStr, p, Length(ArgStr) - p + 1);
    p := LastDelimiter('@', ArgStr);
  end
  else
    ABI := FFI_DEFAULT_ABI;

  if (p > 0) and (ArgStr[p] = '@') then
  begin
    LibName := Copy(ArgStr, p + 1, Length(ArgStr) - p);
    FuncName := Copy(ArgStr, 1, p - 1);
  end
  else
  begin
    LibName := ArgStr;
    FuncName := Header.Name;
  end;

  Lib := LoadLibrary(LibName);
  if (Lib <> NilHandle) then
    Func := GetProcAddress(Lib, FuncName)
  else
    Func := nil;

  if (Func = nil) then
    Exit(False);

  Closure := LapeImportWrapper(Func, TLapeType_Method(Header.VarType), ABI);

  if (Closure = nil) then
    Exit(False);

  Assert(Globals['!ffi_lapify_closures'] <> nil);
  AddLapifyClosure(TFFILapifyClosures(Globals['!ffi_lapify_closures'].Ptr^), Closure);

  TLapeType_Method(Header.VarType).setImported(Header, True);
  PPointer(Header.Ptr)^ := Closure.Func;

  Result := (Closure.Func <> nil);
end;

procedure InitializeFFI(Compiler: TLapeCompiler; Initialize: FFIInitSet = [fsiNative, fsiNatify, fsiLapify, fsiDynLibs, fsiExternal]);
var
  f: TFFIABI;
  s, a: string;
  t: TLapeType;
begin
  if (Compiler = nil) then
    Exit;

  if (fsiNative in Initialize) or (fsiNatify in Initialize) or (fsiLapify in Initialize) then
  begin
    s := '';
    t := nil;

    for f := Succ(FFI_UNKNOWN_ABI) to Pred(FFI_LAST_ABI) do
    begin
      a := ABIToStr(f);
      if (a = '') then
        Continue;

      if (s <> '') then
        s := s + ',';
      s := s + 'ffi_' + a + '=' + IntToStr(Ord(f));
    end;

    Compiler.addGlobalType('('+s+')', 'TFFI_ABI');
    Compiler.addGlobalVar('TFFI_ABI', 'TFFI_ABI('+IntToStr(Ord(FFI_DEFAULT_ABI))+')', 'FFI_DEFAULT_ABI').isConstant := True;

    Compiler.InternalMethodMap['native'] := TLapeTree_InternalMethod_Native;
  end;

  if (fsiNatify in Initialize) or (fsiLapify in Initialize) or (fsiExternal in Initialize) then
  begin
    t := Compiler.addManagedType(TLapeType_ClosureDisposer.Create(Compiler));
    t := Compiler.addManagedType(TLapeType_DynArray.Create(t, Compiler));
    Compiler.addGlobalFunc([TLapeType(nil)], [lptVar], [TLapeGlobalVar(nil)], @_ClosureDisposer, '!closuredisposer');
    Compiler.addGlobalVar(TLapeType_DynArray(t).NewGlobalVar(nil), '!ffi_natify_closures'); //TFFINatifyClosures
    Compiler.addGlobalVar(TLapeType_DynArray(t).NewGlobalVar(nil), '!ffi_lapify_closures'); //TFFILapifyClosures
  end;

  if (fsiLapify in Initialize) then
  begin
    Compiler.addGlobalFunc(
      [TLapeType(nil),      t,                   Compiler.getBaseType(ltSizeInt)],
      [lptConstRef,         lptVar,              lptNormal],
      [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)],
      Compiler.getBaseType(ltPointer),
      @_Natify, '!ffi_natify'
    );
    Compiler.InternalMethodMap['Natify'] := TLapeTree_InternalMethod_Natify;
  end;

  if (fsiLapify in Initialize) then
  begin
    Compiler.addGlobalFunc(
      [TLapeType(nil),      t,                   Compiler.getBaseType(ltSizeInt)],
      [lptConstRef,         lptVar,              lptNormal],
      [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)],
      Compiler.getBaseType(ltPointer),
      @_Lapify, '!ffi_lapify'
    );
    Compiler.InternalMethodMap['Lapify'] := TLapeTree_InternalMethod_Lapify;
  end;

  if (fsiDynLibs in Initialize) then
  begin
    Compiler.addGlobalType(Compiler.getBaseType(ltSizeInt).CreateCopy(True), 'TLibHandle');
    Compiler.addGlobalVar(SharedSuffix, 'SharedSuffix').isConstant := True;

    Compiler.addGlobalFunc('function LoadLibrary(const Name: string): TLibHandle', @_LapeLoadLibrary);
    Compiler.addGlobalFunc('function GetProcAddress(Lib: TlibHandle; const ProcName: string): ConstPointer', @_LapeGetProcAddress);
    Compiler.addGlobalFunc('function FreeLibrary(Lib: TLibHandle): EvalBool', @_LapeFreeLibrary);
  end;

  if (fsiExternal in Initialize) then
    Compiler.OnHandleExternal := @TExternalCompiler(Compiler).HandleExternal;
end;

function TLapeType_NativeMethod.getAsString: lpString;
begin
  Result := 'native[' + ABIToStr(FABI) + '] ' + FHeader.AsString;
end;

constructor TLapeType_NativeMethod.Create(AHeader: TLapeType_Method; ABI: TFFIABI = FFI_DEFAULT_ABI; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(AHeader <> nil);
  inherited Create(ltPointer, AHeader.Compiler, AName, ADocPos);

  FHeader := AHeader;
  FABI := ABI;
end;

constructor TLapeType_NativeMethod.Create(ACompiler: TLapeCompiler; AHeader: lpString; ABI: TFFIABI = FFI_DEFAULT_ABI; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ACompiler <> nil);
  Create(_ParseMethodHeader(ACompiler, AHeader), ABI, AName, ADocPos);
end;

function TLapeType_NativeMethod.CreateCopy(DeepCopy: Boolean = False): TLapeType;
begin
  Result := inherited;
  TLapeType_NativeMethod(Result).FHeader := FHeader;
  TLapeType_NativeMethod(Result).FABI := FABI;
end;

function TLapeType_NativeMethod.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
begin
  Result := 'begin Result := '#39 + AsString + ' ('#39' + System.ToString(Pointer(Param0)) + '#39')'#39'; end;';
end;

function TLapeType_NativeMethod.NewGlobalVar(Ptr: Pointer = nil; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PPointer(Result.Ptr)^ := Ptr;
end;

function TLapeType_NativeMethod.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
var
  m: TLapeGlobalVar;
begin
  if (Right <> nil) and (Right is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right).getMethod(FHeader);
    if (m <> nil) then
      Right := m.VarType;
  end;

  if (Op = op_Assign) and (Right <> nil) then
    if (Right is TLapeType_NativeMethod) then
      if Equals(Right) then
        Result := Self
      else
        Result := nil
    else if (Right is TLapeType_Method) then
      if FHeader.Equals(Right) and (Right.BaseType = ltScriptMethod) then
        Result := Self
      else
        Result := nil
    else if (Right is TLapeType_Pointer) then
      if (TLapeType_Pointer(Right).HasType()) then
        Result := nil
      else
        Result := Self
    else
      Result := inherited
  else
    Result := inherited;
end;

function TLapeType_NativeMethod.CanEvalConst(Op: EOperator; Left, Right: TLapeGlobalVar): Boolean;
begin
  if (op = op_Assign) then
    Result := (Right <> nil) and Right.isConstant and Right.HasType() and (not (Right.VarType is TLapeType_Method))
  else
    Result := inherited;
end;

function TLapeType_NativeMethod.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  m: TLapeGlobalVar;
begin
  if Right.HasType() and (Right.VarType is TLapeType_OverloadedMethod) then
  begin
    m := TLapeType_OverloadedMethod(Right.VarType).getMethod(FHeader);
    if (m <> nil) then
      Right := _ResVar.New(m);
  end;

  if (op = op_Assign) and Right.HasType() and (Right.VarType is TLapeType_Method) then
    with TLapeTree_InternalMethod_Natify.Create(FCompiler, Pos) do
    try
      addParam(TLapeTree_ResVar.Create(Right, FCompiler, Pos));
      Right := Compile(Offset);
    finally
      Free();
    end;

  Result := inherited;
end;

constructor TLapeType_ClosureDisposer.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltPointer, ACompiler, AName, ADocPos);
  FInit := bTrue;
end;

function TLapeType_ClosureDisposer.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
end;

function TLapeType_ClosureDisposer.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  Result := nil;
  LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), '!ClosureDisposer']);
end;

function TLapeType_ClosureDisposer.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  Result := NullResVar;
  LapeExceptionFmt(lpeIncompatibleOperator1, [LapeOperatorToString(op), '!ClosureDisposer']);
end;

procedure TLapeType_ClosureDisposer.Finalize(AVar: TResVar; var Offset: Integer; UseCompiler: Boolean = True; Pos: PDocPos = nil);

begin
  Assert(AVar.VarType = Self);
  if (AVar.VarPos.MemPos = NullResVar.VarPos.MemPos) then
    Exit;

  if UseCompiler and (FCompiler <> nil) then
  with TLapeTree_Invoke.Create('!closuredisposer', FCompiler, Pos) do
  try
    addParam(TLapeTree_ResVar.Create(AVar, FCompiler, Pos));
    Compile(Offset).Spill(1);
  finally
    Free();
  end
  else if (AVar.VarPos.MemPos <> mpMem) then
    LapeException(lpeImpossible)
  else
    FreeAndNil(AVar.VarPos.GlobalVar.Ptr^);
end;

constructor TLapeTree_InternalMethod_Native.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;

  FForceParam := True;
  FABIType  := ACompiler.getGlobalType('TFFI_ABI'); Assert(FABIType <> nil);
end;

procedure TLapeTree_InternalMethod_Native.ClearCache;
begin
  FConstant := bUnknown;
  inherited;
end;

function TLapeTree_InternalMethod_Native.isConstant: Boolean;
var
  ParamType: TLapeType;
begin
  if (FConstant = bUnknown) then
  begin
    FConstant := bFalse;
    if (FParams.Count >= 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) and
         (TLapeType_Type(ParamType).TType is TLapeType_Method)
      then
        FConstant := bTrue;
    end;
  end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Native.resType: TLapeType;
var
  ParamType: TLapeType;
  ABI: TFFIABI;
begin
  if (FResType = nil) then
    if ((FParams.Count = 1) or (FParams.Count = 2)) and (not isEmpty(FParams[0])) then
    begin
      if (FParams.Count = 1) or isEmpty(FParams[1]) then
        ABI := FFI_DEFAULT_ABI
      else if FParams[1].isConstant and FABIType.Equals(FParams[1].resType()) then
        ABI := TFFIABI(FParams[1].Evaluate().AsInteger)
      else
        ABI := FFI_UNKNOWN_ABI;

      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_Type) then
        ParamType := TLapeType_Type(ParamType).TType;

      if (ParamType <> nil) and (ParamType is TLapeType_Method) then
        FResType := FCompiler.addManagedType(TLapeType_NativeMethod.Create(TLapeType_Method(ParamType), ABI, '', @_DocPos));
    end;
  Result := inherited;
end;

function TLapeTree_InternalMethod_Native.Evaluate: TLapeGlobalVar;
var
  Method: TLapeType_NativeMethod;
begin
  if (FRes = nil) then
    if (FParams.Count < 1) or (FParams.Count > 2) or isEmpty(FParams[0]) then
      LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos)
    else if isConstant() then
    begin
      Method := resType() as TLapeType_NativeMethod;
      if (Method <> nil) and (Method.ABI = FFI_UNKNOWN_ABI) then
        if (FParams.Count = 2) and (not (FParams[1].isConstant)) then
          LapeException(lpeConstantExpected, [FParams[1], Self])
        else
          LapeExceptionFmt(lpeExpected, [FABIType.AsString], [FParams[1], Self]);

      if (Method <> nil) then
        FRes := FCompiler.getTypeVar(Method);
    end
    else
      LapeException(lpeInvalidCast, [FParams[0], Self]);

  Result := inherited;
end;

function TLapeTree_InternalMethod_Native.Compile(var Offset: Integer): TResVar;
begin
  Result := _ResVar.New(Evaluate());
end;

constructor TLapeTree_InternalMethod_Natify.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;

  FForceParam := False;
  FClosures := ACompiler.getGlobalVar('!ffi_natify_closures'); Assert(FClosures <> nil);
end;

function TLapeTree_InternalMethod_Natify.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_InternalMethod_Natify.Compile(var Offset: Integer): TResVar;
var
  Method: TLapeType_NativeMethod;
  Closure: TExportClosure;
  Index: Integer;
begin
  Method := resType() as TLapeType_NativeMethod;
  if (FParams.Count < 1) or (FParams.Count > 2) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [2], DocPos)
  else if isConstant() or (Method = nil) or (Method.Header.BaseType <> ltScriptMethod) then
    LapeException(lpeInvalidCast, [FParams[0], Self])
  else if (Method.ABI = FFI_UNKNOWN_ABI) then
    if (FParams.Count = 2) and (not (FParams[1].isConstant)) then
      LapeException(lpeConstantExpected, [FParams[1], Self])
    else
      LapeExceptionFmt(lpeExpected, [FABIType.AsString], [FParams[1], Self]);

  Closure := LapeExportWrapper(FCompiler.Emitter.PCode, nil, Method.Header, Method.ABI);
  if (Closure = nil) then
    LapeException(lpeImpossible, [FParams[0], Self])
  else
    Closure.Func; //Enforce closure to be prepared

  FDest := NullResVar;
  Index := AddNatifyClosure(TFFINatifyClosures(FClosures.Ptr^), Closure);

  with TLapeTree_Invoke.Create('!ffi_natify', Self) do
  try
    addParam(TLapeTree_ResVar.Create(Self.Params[0].Compile(Offset), Self));
    addParam(TLapeTree_GlobalVar.Create(FClosures, Self));
    addParam(TLapeTree_Integer.Create(Index, Self));
    Result := Compile(Offset);
    Result.VarType := Method;
  finally
    Free();
  end;
end;

constructor TLapeTree_InternalMethod_Lapify.Create(ACompiler: TLapeCompilerBase; ADocPos: PDocPos = nil);
begin
  inherited;

  FClosures := ACompiler.getGlobalVar('!ffi_lapify_closures'); Assert(FClosures <> nil);
end;

function TLapeTree_InternalMethod_Lapify.resType: TLapeType;
var
  ParamType: TLapeType;
begin
  if (FResType = nil) then
    if (FParams.Count = 1) and (not isEmpty(FParams[0])) then
    begin
      ParamType := FParams[0].resType();
      if (ParamType <> nil) and (ParamType is TLapeType_NativeMethod) then
      begin
        FResType := TLapeType_NativeMethod(ParamType).Header;
        if (TLapeType_Method(FResType).BaseType <> ltImportedMethod) then
        begin
          FResType := FResType.CreateCopy();
          FResType.BaseType := ltImportedMethod;
          FResType := FCompiler.addManagedType(FResType);
        end;
      end;
    end;

  Result := inherited;
end;

function TLapeTree_InternalMethod_Lapify.Evaluate: TLapeGlobalVar;
begin
  Result := nil;
  LapeException(lpeCannotEvalConst, DocPos);
end;

function TLapeTree_InternalMethod_Lapify.Compile(var Offset: Integer): TResVar;
var
  Method: TLapeType_NativeMethod;
  Closure: TImportClosure;
  Index: Integer;
begin
  if (FParams.Count <> 1) or isEmpty(FParams[0]) then
    LapeExceptionFmt(lpeWrongNumberParams, [1], DocPos)
  else if (resType() = nil) then
    LapeException(lpeInvalidCast, [FParams[0], Self]);

  Method := FParams[0].resType() as TLapeType_NativeMethod;
  Assert(Method <> nil);

  Closure := LapeImportWrapper(nil, Method.Header, Method.ABI);
  if (Closure = nil) then
    LapeException(lpeImpossible, [FParams[0], Self])
  else
    Closure.Func; //Enforce closure to be prepared

  FDest := NullResVar;
  Index := AddLapifyClosure(TFFILapifyClosures(FClosures.Ptr^), Closure);

  with TLapeTree_Invoke.Create('!ffi_lapify', Self) do
  try
    addParam(TLapeTree_ResVar.Create(Self.Params[0].Compile(Offset), Self));
    addParam(TLapeTree_GlobalVar.Create(FClosures, Self));
    addParam(TLapeTree_Integer.Create(Index, Self));
    Result := Compile(Offset);
    Result.VarType := Self.resType();
  finally
    Free();
  end;
end;

end.

