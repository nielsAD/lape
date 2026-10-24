{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  All (script)type and (script)variable classes, including corresponding evaluation functions (runtime/compile time).
}
unit lpvartypes_ord;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes;

type
  {$IFDEF FPC}generic{$ENDIF} TLapeType_Integer<_Type> = class(TLapeType)
  protected type
    PType = ^_Type;
  var public
    function NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeType_Float<_Type> = class(TLapeType)
  protected type
    PType = ^_Type;
  var public
    function NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  {$IFDEF FPC}generic{$ENDIF} TLapeType_Char<_Type> = class(TLapeType)
  protected type
    PType = ^_Type;
  var public
    function NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  TLapeType_UInt8 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt8>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int8 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int8>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UInt16 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt16>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int16 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int16>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UInt32 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt32>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int32 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int32>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_UInt64 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<UInt64>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Int64 = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Integer<Int64>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_Single = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Single>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Double = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Double>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_Currency = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Currency>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  {$IFNDEF Lape_NoExtended}
  TLapeType_Extended = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Extended>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  {$ENDIF}

  TLapeType_AnsiChar = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Char<AnsiChar>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;
  TLapeType_WideChar = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Char<WideChar>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_Variant = class(TLapeType)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function NewGlobalVar(Val: Variant; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
  end;

  TLapeType_SubRange = class(TLapeType)
  protected
    FRange: TLapeRange;
    FVarType: TLapeType;
    FAsArray: TLapeGlobalVar;

    function getAsString: lpString; override;
    function getAsArray: TLapeGlobalVar; virtual;
  public
    constructor Create(ARange: TLapeRange; ACompiler: TLapeCompilerBase; AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    procedure ClearCache; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property Range: TLapeRange read FRange;
    property VarType: TLapeType read FVarType;
    property AsArray: TLapeGlobalVar read getAsArray;
  end;

  TLapeEnumElement = record
    Name: lpString;
    Value: Int64;
  end;
  TLapeEnumElements = array of TLapeEnumElement;

  // An enum: names for numbers, kept in 4 bytes (as FPC). Each element has a higher value than the one before it.
  TLapeType_Enum = class(TLapeType_SubRange)
  protected
    FElements: TLapeEnumElements;
    FScoped: Boolean;

    function getAsString: lpString; override;
    function getAsArray: TLapeGlobalVar; override;
  public
    constructor Create(ACompiler: TLapeCompilerBase; AScoped: Boolean; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function canSet: Boolean;
    function ElementCount: Integer;
    // The name of the element that has the value ('' when none has)
    function ElementName(Value: Int64): lpString; virtual;
    // False: it has no element of that name
    function ElementValue(AName: lpString; out Value: Int64): Boolean; virtual;
    function HasElement(AName: lpString): Boolean; virtual;
    function addElement(Value: Int64; AName: lpString): Int64; overload; virtual;
    function addElement(AName: lpString): Int64; overload; virtual;

    function Equals(Other: TLapeType; ContextOnly: Boolean = True): Boolean; reintroduce; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    // (in the order that they were declared, which is that of their values)
    property Elements: TLapeEnumElements read FElements;
    property Scoped: Boolean read FScoped;
  end;

  TLapeType_Boolean = class(TLapeType_Enum)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean; override;
  end;

  TLapeType_Bool = class(TLapeType_SubRange)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;
    destructor Destroy; override;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;
  end;

  TLapeType_ByteBool = class(TLapeType_Bool)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_WordBool = class(TLapeType_Bool)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  TLapeType_LongBool = class(TLapeType_Bool)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

  {$IFDEF Lape_SmallCode}
  TLapeType_EvalBool = TLapeType_Boolean;
  {$ELSE}
  TLapeType_EvalBool = TLapeType_LongBool;
  {$ENDIF}

  TLapeType_Set = class(TLapeType)
  protected
    FRange: TLapeType_SubRange;
    FSmall: Boolean;
    function getAsString: lpString; override;
    procedure setName(const AName: lpString); override;
  public
    constructor Create(ARange: TLapeType_SubRange; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy: TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Values: array of UInt8; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    // Of its range, of the enum that the range is of, or of another range of that enum
    function TakesElement(AType: TLapeType): Boolean; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;

    property Range: TLapeType_SubRange read FRange;
    property Small: Boolean read FSmall;
  end;

implementation

uses
  Variants,
  lptokenizer, lpeval, lpmessages, lpvartypes_array;

function TLapeType_Integer{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PType(Result.Ptr)^ := Val;
end;

function TLapeType_Integer{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
{$IFNDEF FPC}
var
  i64: Int64;
  ui64: UInt64;
  t: PType;
{$ENDIF}
begin
  if {(Length(Str) > 1) and (Str[1] <> '-')} (BaseType in LapeUnsignedIntegerTypes) then
  begin
    {$IFDEF FPC}
    Result := NewGlobalVar(StrToUInt64(string(Str)), AName, ADocPos);
    {$ELSE}
    ui64 := StrToUInt64(string(Str)); t := @ui64;
    Result := NewGlobalVar(t^ , AName, ADocPos);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF FPC}
    Result := NewGlobalVar(StrToInt64(string(Str)), AName, ADocPos);
    {$ELSE}
    i64 := StrToInt64(string(Str)); t := @i64;
    Result := NewGlobalVar(t^ , AName, ADocPos);
    {$ENDIF}
  end;
end;

function TLapeType_Float{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PType(Result.Ptr)^ := Val;
end;

function TLapeType_Float{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
{$IFNDEF FPC}var a: Extended; b: PType;{$ENDIF}
begin
  {$IFDEF FPC}
  Result := NewGlobalVar(StrToFloatDot(string(Str)), AName, ADocPos);
  {$ELSE}
  a := StrToFloatDot(string(Str)); b := @a;
  Result := NewGlobalVar(b^ , AName, ADocPos);
  {$ENDIF}
end;

function TLapeType_Char{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVar(Val: _Type; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PType(Result.Ptr)^ := Val;
end;

function TLapeType_Char{$IFNDEF FPC}<_Type>{$ENDIF}.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
{$IFNDEF FPC}var a: WideChar; b: PType;{$ENDIF}
var
  c: Integer;
begin
  if (Length(Str) <> 1) then
    try
      if (Str[1] = '#') then
        Delete(Str, 1, 1);
      c := StrToInt(string(Str));
      Result := NewGlobalVarP(nil, AName, ADocPos);
      case Size of
        SizeOf(UInt8):  PUInt8(Result.Ptr)^ := c;
        SizeOf(UInt16): PUInt16(Result.Ptr)^ := c;
        else LapeException(lpeImpossible);
      end;
      Exit;
    except
      LapeExceptionFmt(lpeInvalidValueForType, [AsString]);
    end;

  {$IFDEF FPC}
  Result := NewGlobalVar(_Type(Str[1]), AName, ADocPos);
  {$ELSE}
  a := Str[1]; b := @a;
  Result := NewGlobalVar(b^, AName, ADocPos);
  {$ENDIF}
end;

constructor TLapeType_UInt8.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt8, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int8.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt8, ACompiler, AName, ADocPos);
end;

constructor TLapeType_UInt16.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt16, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int16.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt16, ACompiler, AName, ADocPos);
end;

constructor TLapeType_UInt32.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt32, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int32.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt32, ACompiler, AName, ADocPos);
end;

constructor TLapeType_UInt64.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUInt64, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Int64.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltInt64, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Single.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltSingle, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Double.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltDouble, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Currency.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltCurrency, ACompiler, AName, ADocPos);
end;

{$IFNDEF Lape_NoExtended}
constructor TLapeType_Extended.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltExtended, ACompiler, AName, ADocPos);
end;
{$ENDIF}

constructor TLapeType_AnsiChar.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltAnsiChar, ACompiler, AName, ADocPos);
end;

constructor TLapeType_WideChar.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltWideChar, ACompiler, AName, ADocPos);
end;

constructor TLapeType_Variant.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltVariant, ACompiler, AName, ADocPos);
end;

function TLapeType_Variant.NewGlobalVar(Val: Variant; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  PVariant(Result.Ptr)^ := Val;
end;

function TLapeType_Variant.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  Result := NewGlobalVar(Str, AName, ADocPos);
end;

procedure TLapeType_SubRange.ClearCache;
begin
  inherited ClearCache();

  if (FAsArray <> nil) then
    FreeAndNil(FAsArray);
end;

function TLapeType_SubRange.getAsString: lpString;
begin
  if (FAsString = '') then
    if (FVarType <> nil) then
      FAsString := FVarType.VarToString(@FRange.Lo) + '..' + FVarType.VarToString(@FRange.Hi)
    else
      FAsString := lpString(IntToStr(FRange.Lo)) + '..' + lpString(IntToStr(FRange.Hi));
  Result := inherited;
end;

function TLapeType_SubRange.getAsArray: TLapeGlobalVar;
var
  CounterVar, ValueVar: Integer;
  Counter, Value: TLapeGlobalVar;
  Element: TLapeGlobalVar;
begin
  if (FAsArray = nil) then
  begin
    with TLapeType_DynArray(FCompiler.addManagedType(TLapeType_DynArray.Create(FVarType, FCompiler))) do
    begin
      FAsArray := TLapeGlobalVar(FCompiler.addManagedDecl(NewGlobalVarP()));

      VarSetLength(FAsArray, (FRange.Hi - FRange.Lo) + 1);
    end;

    CounterVar := 0;
    Counter := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@CounterVar);
    // (an element of an enum is not given a number: the value is read as one of the enum)
    if (FVarType <> nil) and (FVarType.ClassType = TLapeType_Enum) then
    begin
      Assert(FVarType.Size = SizeOf(ValueVar));
      Value := FVarType.NewGlobalVarP(@ValueVar);
    end
    else
      Value := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@ValueVar);

    try
      for ValueVar := FRange.Lo to FRange.Hi do
      try
        Element := FAsArray.VarType.EvalConst(op_Index, FAsArray, Counter, []);
        Element.VarType.EvalConst(op_Assign, Element, Value, [lefAssigning]);

        Inc(CounterVar);
      finally
        if (Element <> nil) then
          FreeAndNil(Element);
      end;
    finally
      Counter.Free();
      Value.Free();
    end;
  end;

  Result := FAsArray;
end;

constructor TLapeType_SubRange.Create(ARange: TLapeRange; ACompiler: TLapeCompilerBase; AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltUnknown, ACompiler, AName, ADocPos);
  if (AVarType = nil) and (ACompiler <> nil) then
    AVarType := ACompiler.getBaseType(DetermineIntType(ARange.Lo, ARange.Hi));
  if (AVarType <> nil) then
    FBaseType := AVarType.BaseType;
  FRange := ARange;
  FVarType := AVarType;
end;

function TLapeType_SubRange.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (FCompiler = nil) or (FVarType = nil) then
    Exit;

  Index := ToStr.getMethodIndex(getTypeArray([FVarType]), FCompiler.getBaseType(ltString));
  if (Index < 0) then
    Exit;

  Result := 'begin Result := System.ToString['+lpString(IntToStr(Index))+'](Param0); end;';
end;

function TLapeType_SubRange.VarToString(AVar: Pointer): lpString;
begin
  if (FVarType <> nil) then
    Result := FVarType.VarToString(AVar)
  else
    Result := lpString(IntToStr(VarToInt(AVar)));
end;

function TLapeType_SubRange.VarLo(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FLo = nil) and (FCompiler <> nil) and (BaseType in LapeOrdinalTypes) then
    FLo := FCompiler.addManagedDecl(NewGlobalVar(FRange.Lo)) as TLapeGlobalVar;
  Result := FLo;
end;

function TLapeType_SubRange.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FHi = nil) and (FCompiler <> nil) and (BaseType in LapeOrdinalTypes) then
    FHi := FCompiler.addManagedDecl(NewGlobalVar(FRange.Hi)) as TLapeGlobalVar;
  Result := FHi;
end;

function TLapeType_SubRange.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_SubRange;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FCompiler, FVarType, '', @_DocPos);
  with TLapeType_SubRange(Result) do
  begin
    FBaseType := Self.BaseType;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_SubRange.NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Value < FRange.Lo) or (Value > FRange.Hi) then
    LapeExceptionFmt(lpeOutOfTypeRange1, [Value, FRange.Lo, FRange.Hi]);

  Result := NewGlobalVarP(nil, AName, ADocPos);
  case DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes) of
    ltInt8:   PInt8(Result.Ptr)^   := Value;
    ltUInt8:  PUInt8(Result.Ptr)^  := Value;
    ltInt16:  PInt16(Result.Ptr)^  := Value;
    ltUInt16: PUInt16(Result.Ptr)^ := Value;
    ltInt32:  PInt32(Result.Ptr)^  := Value;
    ltUInt32: PUInt32(Result.Ptr)^ := Value;
    ltInt64:  PInt64(Result.Ptr)^  := Value;
    ltUInt64: PUInt64(Result.Ptr)^ := Value;
  end;
end;

function TLapeType_SubRange.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  with FVarType.NewGlobalVarStr(Str) do
  try
    Result := NewGlobalVar(AsInteger, AName, ADocPos);
  finally
    Free();
  end;
end;

function TLapeType_SubRange.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  if (FVarType <> nil) and (FVarType.ClassType = TLapeType_Enum) and (Right <> nil) then
  begin
    Result := FVarType.EvalRes(Op, Right, Flags);
    if (Result = FVarType) and (Op in AssignOperators) then
      Result := Self;
  end
  else
    Result := inherited;
end;

function TLapeType_SubRange.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
begin
  if (FVarType <> nil) and (FVarType.ClassType = TLapeType_Enum) and (Right <> nil) and Right.HasType() and (EvalRes(Op, Right.VarType, Flags) <> nil) then
  begin
    // (a constant outside of the range is an error)
    if (Op = op_Assign) and (Right.VarType <> Self) and ((Right.AsInteger < FRange.Lo) or (Right.AsInteger > FRange.Hi)) then
      LapeExceptionFmt(lpeOutOfTypeRange1, [Right.AsInteger, FRange.Lo, FRange.Hi]);

    Left.VarType := FVarType;
    try
      Result := FVarType.EvalConst(Op, Left, Right, Flags);
    finally
      Left.VarType := Self;
    end;
    if (Op in AssignOperators) and (Result <> nil) and (Result.VarType = FVarType) then
      Result.VarType := Self;
  end
  else
    Result := inherited;
end;

function TLapeType_SubRange.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
begin
  if (FVarType <> nil) and (FVarType.ClassType = TLapeType_Enum) and Right.HasType() and (EvalRes(Op, Right.VarType, Flags) <> nil) then
  begin
    // (a constant outside of the range is an error; what a variable holds is not checked, as FPC)
    if (Op = op_Assign) and (Right.VarType <> Self) and (Right.VarPos.MemPos = mpMem) and
       (Right.VarPos.GlobalVar <> nil) and Right.VarPos.GlobalVar.isConstant and
       ((Right.VarPos.GlobalVar.AsInteger < FRange.Lo) or (Right.VarPos.GlobalVar.AsInteger > FRange.Hi))
    then
      if (Pos <> nil) then
        LapeExceptionFmt(lpeOutOfTypeRange1, [Right.VarPos.GlobalVar.AsInteger, FRange.Lo, FRange.Hi], Pos^)
      else
        LapeExceptionFmt(lpeOutOfTypeRange1, [Right.VarPos.GlobalVar.AsInteger, FRange.Lo, FRange.Hi]);

    Left.VarType := FVarType;
    if Dest.HasType() and Equals(Dest.VarType) then
      Dest.VarType := FVarType;
    Result := FVarType.Eval(Op, Dest, Left, Right, Flags, Offset, Pos);
    if (Op in AssignOperators) then
      Result.VarType := Self;
  end
  else
    Result := inherited;
end;

function TLapeType_Enum.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := '(';
    for i := 0 to High(FElements) do
    begin
      if (i > 0) then
        FAsString := FAsString + ', ';
      FAsString := FAsString + FElements[i].Name + '=' + lpString(IntToStr(FElements[i].Value));
    end;
    FAsString := FAsString + ')';
  end;
  Result := inherited;
end;

function TLapeType_Enum.getAsArray: TLapeGlobalVar;
var
  CounterVar: Integer;
  Counter, Value: TLapeGlobalVar;
  Element: TLapeGlobalVar;
  I: Integer;
begin
  if (FAsArray = nil) then
  begin
    if (not (BaseType in LapeOrdinalTypes)) then
      LapeException(lpeImpossible, DocPos);

    with TLapeType_DynArray(FCompiler.addManagedType(TLapeType_DynArray.Create(Self, FCompiler))) do
    begin
      FAsArray := TLapeGlobalVar(FCompiler.addManagedDecl(NewGlobalVarP()));

      VarSetLength(FAsArray, Length(FElements));
    end;

    CounterVar := 0;
    Counter := FCompiler.getBaseType(ltInt32).NewGlobalVarP(@CounterVar);
    Value := NewGlobalVarP(nil);

    try
      for i := 0 to High(FElements) do
        try
          case DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes) of
            ltInt8:   PInt8(Value.Ptr)^   := FElements[i].Value;
            ltUInt8:  PUInt8(Value.Ptr)^  := FElements[i].Value;
            ltInt16:  PInt16(Value.Ptr)^  := FElements[i].Value;
            ltUInt16: PUInt16(Value.Ptr)^ := FElements[i].Value;
            ltInt32:  PInt32(Value.Ptr)^  := FElements[i].Value;
            ltUInt32: PUInt32(Value.Ptr)^ := FElements[i].Value;
            ltInt64:  PInt64(Value.Ptr)^  := FElements[i].Value;
            ltUInt64: PUInt64(Value.Ptr)^ := FElements[i].Value;
          end;

          Element := FAsArray.VarType.EvalConst(op_Index, FAsArray, Counter, []);
          Element.VarType.EvalConst(op_Assign, Element, Value, [lefAssigning]);

          Inc(CounterVar);
        finally
          if (Element <> nil) then
            FreeAndNil(Element);
        end;
    finally
      Counter.Free();
      Value.Free();
    end;
  end;

  Result := FAsArray;
end;

constructor TLapeType_Enum.Create(ACompiler: TLapeCompilerBase; AScoped: Boolean; AName: lpString; ADocPos: PDocPos);
begin
  inherited Create(NullRange, ACompiler, nil, AName, ADocPos);
  FBaseType := ltSmallEnum;
  FVarType := nil;
  FScoped := AScoped;
  FRange.Hi := -1;
end;

function TLapeType_Enum.canSet: Boolean;
begin
  // (a set holds the values 0 to 255)
  Result := (FRange.Lo >= Low(TLapeLargeSetItem)) and (FRange.Hi <= High(TLapeLargeSetItem));
end;

function TLapeType_Enum.ElementCount: Integer;
begin
  Result := Length(FElements);
end;

function TLapeType_Enum.ElementName(Value: Int64): lpString;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := 0;
  Hi := High(FElements);
  while (Lo <= Hi) do
  begin
    Mid := (Lo + Hi) div 2;
    if (FElements[Mid].Value = Value) then
      Exit(FElements[Mid].Name)
    else if (FElements[Mid].Value < Value) then
      Lo := Mid + 1
    else
      Hi := Mid - 1;
  end;
  Result := '';
end;

function TLapeType_Enum.ElementValue(AName: lpString; out Value: Int64): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(FElements) do
    if LapeSameString(FElements[i].Name, AName) then
    begin
      Value := FElements[i].Value;
      Exit(True);
    end;

  Value := 0;
  Result := False;
end;

function TLapeType_Enum.HasElement(AName: lpString): Boolean;
var
  Value: Int64;
begin
  Result := ElementValue(AName, Value);
end;

function TLapeType_Enum.addElement(Value: Int64; AName: lpString): Int64;
begin
  // (higher than the one before; it has to fit in the 4 bytes)
  if (Value < Low(Int32)) or (Value > High(Int32)) or ((Length(FElements) > 0) and (Value <= FRange.Hi)) then
    LapeException(lpeInvalidRange)
  else if (AName = '') or HasElement(AName) then
    LapeException(lpeDuplicateDeclaration);

  Result := Value;
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)].Name := AName;
  FElements[High(FElements)].Value := Value;

  if (Length(FElements) = 1) then
    FRange.Lo := Value;
  FRange.Hi := Value;

  // (small: every element has a place in a set of 4 bytes; once large it stays so, the values go up)
  if (FBaseType = ltSmallEnum) and ((FRange.Lo < Low(TLapeSmallSetItem)) or (FRange.Hi > High(TLapeSmallSetItem))) then
    FBaseType := ltLargeEnum;

  ClearCache();
end;

function TLapeType_Enum.addElement(AName: lpString): Int64;
begin
  if (Length(FElements) = 0) then
    Result := addElement(0, AName)
  else
    Result := addElement(FRange.Hi + 1, AName);
end;

function TLapeType_Enum.Equals(Other: TLapeType; ContextOnly: Boolean): Boolean;
var
  OtherEnum: TLapeType_Enum absolute Other;
  i: Integer;
begin
  Result := (Other is TLapeType_Enum) and
            (FScoped = OtherEnum.FScoped) and
            (Length(FElements) = Length(OtherEnum.FElements)) and inherited;
  if Result then
    for i := 0 to High(FElements) do
      if (FElements[i].Value <> OtherEnum.FElements[i].Value) or (not LapeSameString(FElements[i].Name, OtherEnum.FElements[i].Name)) then
        Exit(False);
end;

function TLapeType_Enum.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  i: Integer;
  Shown: lpString;
begin
  // (a case of the elements only: not of every number in a gap)
  Result := 'begin' + LineEnding + '  case System.Ord(Param0) of' + LineEnding;
  for i := 0 to High(FElements) do
  begin
    Shown := FElements[i].Name;
    if FScoped then
      Shown := FName + '.' + Shown;
    Result := Result + '    ' + lpString(IntToStr(FElements[i].Value)) + ': Result := ' + #39 + Shown + #39 + ';' + LineEnding;
  end;
  Result := Result +
    '  else' + LineEnding +
    '    Result := ' + #39 + 'InvalidEnum(' + #39 + ' + System.ToString(System.Ord(Param0)) + ' + #39 + ')' + #39 + ';' + LineEnding +
    '  end;' + LineEnding +
    'end;';
end;

function TLapeType_Enum.VarToString(AVar: Pointer): lpString;
var
  i: Int64;
begin
  try
    Result := '';
    i := VarToInt(AVar);

    if (FBaseType in LapeBoolTypes) then
      if (i = 0) then
        Result := 'False'
      else if (Abs(i) > 1) then
        Result := 'True(' + lpString(IntToStr(i)) + ')'
      else
        Result := 'True'
    else
    begin
      Result := ElementName(i);

      if (Result = '') then
        if (Name <> '') then
          Result := Name + '(' + lpString(IntToStr(i)) + ')'
        else
          Result := 'InvalidEnum';
    end;
  except
    Result := 'EnumException';
  end;
end;

function TLapeType_Enum.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Enum;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, Scoped, '', @_DocPos);

  with TLapeType_Enum(Result) do
  begin
    FElements := Copy(Self.FElements);
    FBaseType := Self.BaseType;
    FRange := Self.Range;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_Enum.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
var
  Value: Int64;
begin
  if (Str <> '') and (AnsiChar(Str[1]) in ['-', '0'..'9']) then
    Value := StrToInt64(string(Str))
  else if (not ElementValue(lpString(Str), Value)) then
    LapeExceptionFmt(lpeUnknownDeclaration, [Str]);
  Result := NewGlobalVar(Value, AName, ADocPos);
end;

function TLapeType_Enum.EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean;
begin
  if (Right = nil) then
    Result := False
  else if (op in CompareOperators + [op_Assign]) then
    // (itself, or a range of its elements)
    Result := Equals(Right) or ((Right is TLapeType_SubRange) and (TLapeType_SubRange(Right).VarType <> nil) and Equals(TLapeType_SubRange(Right).VarType))
  else if (op in [op_Plus, op_Minus]) then
    Result := Right.BaseType in LapeIntegerTypes
  else
    Result := False;
end;

function TLapeType_Enum.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Assert(FCompiler <> nil);

  if EvalAsSubType(Op, Right) then
  begin
    Result := FCompiler.getBaseType(DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes)).EvalRes(Op, FCompiler.getBaseType(DetermineIntType(Right.Size, Right.BaseType in LapeSignedOrdinalTypes)), Flags);
    if (Result <> nil) and (not (op in CompareOperators)) then
      Result := Self;
  end
  else
    Result := inherited;
end;

function TLapeType_Enum.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  tmpType: TLapeType;
  tmpRes: TLapeGlobalVar;
begin
  Assert(FCompiler <> nil);
  Assert((Left = nil) or (Left.VarType = Self));

  if (Right <> nil) and Right.HasType() and EvalAsSubType(Op, Right.VarType) then
  try
    tmpType := Right.VarType;
    if (not (BaseType in LapeOrdinalTypes)) or (not Right.HasType()) or (not (Right.VarType.BaseType in LapeOrdinalTypes)) then
      LapeException(lpeInvalidEvaluation);

    Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes));
    Right.VarType := FCompiler.getBaseType(DetermineIntType(Right.VarType.Size, Right.VarType.BaseType in LapeSignedOrdinalTypes));

    Result := Left.VarType.EvalConst(Op, Left, Right, Flags);
    if Result.HasType() and (not (op in CompareOperators)) then
    begin
      if (DetermineIntType(Result.VarType.Size, Result.VarType.BaseType in LapeSignedOrdinalTypes) <> DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes)) then
      try
        tmpRes := Result;
        Result := Left.VarType.EvalConst(op_Assign, Left.VarType.NewGlobalVarP(), tmpRes, []);
      finally
        FreeAndNil(tmpRes);
      end;
      Result.VarType := Self
    end;
  finally
    Left.VarType := Self;
    Right.VarType := tmpType;
  end
  else
    Result := inherited;
end;

function TLapeType_Enum.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  tmpRes: TResVar;
begin
  Assert(FCompiler <> nil);
  Assert(Left.VarType = Self);

  if Right.HasType() and EvalAsSubType(Op, Right.VarType) then
  begin
    if (not (BaseType in LapeOrdinalTypes)) or (not Right.HasType()) or (not (Right.VarType.BaseType in LapeOrdinalTypes)) then
      LapeException(lpeInvalidEvaluation);

    Left.VarType := FCompiler.getBaseType(DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes));
    Right.VarType := FCompiler.getBaseType(DetermineIntType(Right.VarType.Size, Right.VarType.BaseType in LapeSignedOrdinalTypes));

    if Dest.HasType() and Equals(Dest.VarType) then
      Dest.VarType := Left.VarType;
    Result := Left.VarType.Eval(Op, Dest, Left, Right, Flags, Offset, Pos);
    if Result.HasType() and (not (op in CompareOperators)) then
    begin
      if (DetermineIntType(Result.VarType.Size, Result.VarType.BaseType in LapeSignedOrdinalTypes) <> DetermineIntType(Size, BaseType in LapeSignedOrdinalTypes)) then
      begin
        tmpRes := Result;

        Result := NullResVar;
        Result.VarType := Left.VarType;
        FCompiler.getDestVar(Dest, Result, Op);

        Result := Left.VarType.Eval(op_Assign, Dest, Result, tmpRes, [], Offset, Pos);
      end;
      Result.VarType := Self;
    end;
  end
  else
    Result := inherited;
end;

constructor TLapeType_Boolean.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, False, AName, ADocPos);
  addElement('False');
  addElement('True');
  FBaseType := ltBoolean;
end;

function TLapeType_Boolean.NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Value <> 0) then
    Value := FRange.Hi;
  Result := inherited;
end;

function TLapeType_Boolean.EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean;
begin
  if (op in [op_Plus, op_Minus]) then
    Result := inherited
  else
    Result := False;
end;

constructor TLapeType_Bool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(NullRange, ACompiler, ACompiler.getBaseType(ltBoolean).CreateCopy(), AName, ADocPos);
end;

destructor TLapeType_Bool.Destroy;
begin
  FVarType.Free();
  inherited;
end;

function TLapeType_Bool.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Bool;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, '', @_DocPos);

  with TLapeType_Bool(Result) do
  begin
    FBaseType := Self.BaseType;

    FRange := Self.Range;
    TLapeType_Boolean(FVarType).FSize := Self.VarType.Size;
    CopyHints(Self);
  end;
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_Bool.NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Value <> 0) then
    Value := FRange.Hi;
  Result := inherited;
end;

function TLapeType_Bool.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := inherited;
  if (Result = nil) then
  begin
    if Equals(Right, False) then
      Right := FVarType;

    Result := FVarType.EvalRes(Op, Right, Flags);
    if (Result = FVarType) then
      Result := Self;
  end;
end;

function TLapeType_Bool.EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar;
var
  Res, tmpType: TLapeType;
begin
  Assert(Left.VarType = Self);
  if (Right = nil) or (not Right.HasType()) then
    Res := inherited EvalRes(Op, TLapeType(nil), Flags)
  else
    Res := inherited EvalRes(Op, Right.VarType, Flags);

  if (Res <> nil) then
    Result := inherited
  else
  begin
    Left.VarType := FVarType;
    if (Right <> nil) and Right.HasType() and Equals(Right.VarType) then
    begin
      tmpType := Right.VarType;
      Right.VarType := FVarType;
    end
    else
      tmpType := nil;

    try
      Result := FVarType.EvalConst(Op, Left, Right, Flags);
      if (Result.VarType = FVarType) then
        Result.VarType := Self;
    finally
      Left.VarType := Self;
      if (tmpType <> nil) then
        Right.VarType := tmpType;
    end;
  end;
end;

function TLapeType_Bool.Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar;
var
  Res: TLapeType;
begin
  Assert(Left.VarType = Self);
  Res := inherited EvalRes(Op, Right.VarType, Flags);

  if (Res <> nil) then
    Result := inherited
  else
  begin
    Left.VarType := FVarType;
    if Right.HasType() and Equals(Right.VarType) then
      Right.VarType := FVarType;

    Result := FVarType.Eval(Op, Dest, Left, Right, Flags, Offset, Pos);
    if (Result.VarType = FVarType) then
      Result.VarType := Self;
  end;
end;

constructor TLapeType_ByteBool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  BoolRange: TLapeRange = (Lo: Ord(Low(UInt8{ByteBool})); Hi: Ord(High(UInt8{ByteBool})));
begin
  inherited Create(ACompiler, AName, ADocPos);
  FRange := BoolRange;
  FBaseType := ltByteBool;
  TLapeType_Boolean(FVarType).FSize := LapeTypeSize[ltByteBool];
end;

constructor TLapeType_WordBool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  BoolRange: TLapeRange = (Lo: Ord(Low(UInt16{WordBool})); Hi: Ord(High(UInt16{WordBool})));
begin
  inherited Create(ACompiler, AName, ADocPos);
  FRange := BoolRange;
  FBaseType := ltWordBool;
  TLapeType_Boolean(FVarType).FSize := LapeTypeSize[ltWordBool];
end;

constructor TLapeType_LongBool.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
const
  BoolRange: TLapeRange = (Lo: Ord(Low(UInt32{LongBool})); Hi: Ord(High(UInt32{LongBool})));
begin
  inherited Create(ACompiler, AName, ADocPos);
  FRange := BoolRange;
  FBaseType := ltLongBool;
  TLapeType_Boolean(FVarType).FSize := LapeTypeSize[ltLongBool];
end;

function TLapeType_Set.getAsString: lpString;
begin
  if (FAsString = '') then
    FAsString := 'set of ' + FRange.AsString;
  Result := inherited;
end;

procedure TLapeType_Set.setName(const AName: lpString);
var
  Enum: TLapeType;
begin
  inherited setName(AName);

  if (AName <> '') and (FRange is TLapeType_Enum) and TLapeType_Enum(FRange).Scoped and (FRange.Name = '') then
  begin
    Enum := FRange.CreateCopy();
    Enum.Name := AName;
    if (FCompiler <> nil) then
      Enum := FCompiler.addManagedType(Enum);
    FRange := Enum as TLapeType_SubRange;
  end;
end;

constructor TLapeType_Set.Create(ARange: TLapeType_SubRange; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ARange <> nil);
  inherited Create(ltLargeSet, ACompiler, AName, ADocPos);

  FRange := ARange;
  FSmall := (FRange.Range.Hi <= High(TLapeSmallSetItem));
  if FSmall then
    FBaseType := ltSmallSet;

  if (FRange.Range.Lo < Low(TLapeLargeSetItem)) or (FRange.Range.Hi > High(TLapeLargeSetItem)) then
    if (FRange.Range.Lo < Low(TLapeLargeSetItem)) then
      LapeExceptionFmt(lpeOutOfTypeRange1, [FRange.Range.Lo, Low(TLapeLargeSetItem), High(TLapeLargeSetItem)])
    else
      LapeExceptionFmt(lpeOutOfTypeRange1, [FRange.Range.Hi, Low(TLapeLargeSetItem), High(TLapeLargeSetItem)])
end;

function TLapeType_Set.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (FCompiler = nil) or (FRange = nil) or (not (FRange.BaseType in LapeOrdinalTypes)) then
    Exit;

  Index := ToStr.getMethodIndex(getTypeArray([FRange]), FCompiler.getBaseType(ltString));
  if (Index < 0) then
    Exit;

  if FSmall then
    Result := '_SmallSetToString'
  else
    Result := '_LargeSetToString';

  Result := Format(lpString(
    '  function _ElementToString(p: System.ConstPointer): System.string; static;'           + LineEnding +
    '  type'                                                                                + LineEnding +
    '    TEnum = (se0, se1 = %d);'                                                          + LineEnding +
    '    TRangeToString = function(constref r: System.%s): string;'                         + LineEnding +
    '  begin'                                                                               + LineEnding +
    '    Result := TRangeToString(System.ToString[%d])(Ord(TEnum(p^)));'                    + LineEnding +
    '  end;'                                                                                + LineEnding +
    'type'                                                                                  + LineEnding +
    '  TSetToString = private function(constref ASet; AToString: System.ConstPointer; Lo, Hi: System.SizeInt): System.string;' + LineEnding +
    'begin'                                                                                 + LineEnding +
    '  Result := TSetToString(System.%s)(Param0, _ElementToString, %d, %d);'                + LineEnding +
    'end;'
  ), [FRange.Range.Hi, LapeTypeToString(DetermineIntType(FRange.Size, FRange.BaseType in LapeSignedOrdinalTypes)), Index, Result, FRange.Range.Lo, FRange.Range.Hi]);
end;

function TLapeType_Set.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
begin
  Result := '[';
  for i := FRange.Range.Lo to FRange.Range.Hi do
    if (FSmall and (TLapeSmallSetItem(i) in PLapeSmallSet(AVar)^)) or ((not FSmall) and (TLapeLargeSetItem(i) in PLapeLargeSet(AVar)^)) then
    begin
      if (Result <> '[') then
        Result := Result + ', ';
      Result := Result + FRange.VarToString(@i);
    end;
  Result := Result + ']';
end;

function TLapeType_Set.CreateCopy: TLapeType;
type
  TLapeClassType = class of TLapeType_Set;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FCompiler, '', @_DocPos);
  Result.CopyHints(Self);
  Result.ParentType := Self;
  Result.TypeID := TypeID;
end;

function TLapeType_Set.NewGlobalVar(Values: array of UInt8; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := NewGlobalVarP(nil, AName, ADocPos);
  for i := 0 to High(Values) do
    if FSmall then
      PLapeSmallSet(Result.Ptr)^ := PLapeSmallSet(Result.Ptr)^ + [TLapeSmallSetItem(Values[i])]
    else
      PLapeLargeSet(Result.Ptr)^ := PLapeLargeSet(Result.Ptr)^ + [TLapeLargeSetItem(Values[i])]
end;

function TLapeType_Set.TakesElement(AType: TLapeType): Boolean;
var
  Enum: TLapeType;
begin
  Enum := FRange;
  if (FRange.VarType <> nil) and (FRange.VarType.ClassType = TLapeType_Enum) then
    Enum := FRange.VarType;
  Result := AType.Equals(FRange) or AType.Equals(Enum) or
    ((AType is TLapeType_SubRange) and (TLapeType_SubRange(AType).VarType <> nil) and
     (TLapeType_SubRange(AType).VarType.ClassType = TLapeType_Enum) and TLapeType_SubRange(AType).VarType.Equals(Enum));
end;

function TLapeType_Set.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
  if (Right = nil) then
    Result := inherited
  else
    case getEvalRes(Op, FBaseType, Right.BaseType) of
      ltSmallEnum, ltLargeEnum:
        if (not (Right.BaseType in LapeEnumTypes)) or TakesElement(Right) then
          Result := FRange.VarType;
      ltSmallSet, ltLargeSet:
        if (not (Right.BaseType in LapeEnumTypes + LapeSetTypes)) or TakesElement(Right) or Equals(Right) or
           ((Right is TLapeType_Set) and TakesElement(TLapeType_Set(Right).Range))
        then
          if (not (Op in AssignOperators)) and (Right is TLapeType_Set) and (Right.BaseType = ltLargeSet) and (FBaseType = ltSmallSet) then
            Result := Right
          else
            Result := Self;
      else
        Result := inherited;
    end;
end;

end.

