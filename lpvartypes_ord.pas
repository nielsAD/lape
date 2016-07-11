{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
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
  TLapeType_Extended = class({$IFDEF FPC}specialize{$ENDIF} TLapeType_Float<Extended>)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual; end;

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
    function getAsString: lpString; override;
  public
    constructor Create(ARange: TLapeRange; ACompiler: TLapeCompilerBase; AVarType: TLapeType; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function VarLo(AVar: Pointer = nil): TLapeGlobalVar; override;
    function VarHi(AVar: Pointer = nil): TLapeGlobalVar; override;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    property Range: TLapeRange read FRange;
    property VarType: TLapeType read FVarType;
  end;

  TEnumMap = TStringList;
  TLapeType_Enum = class(TLapeType_SubRange)
  protected
    FMemberMap: TEnumMap;
    FSmall: Boolean;
    function getAsString: lpString; override;
  public
    FreeMemberMap: Boolean;
    constructor Create(ACompiler: TLapeCompilerBase; AMemberMap: TEnumMap; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;
    destructor Destroy; override;

    function hasMember(AName: lpString): Boolean; virtual;
    function addMember(Value: Int64; AName: lpString): Int16; overload; virtual;
    function addMember(AName: lpString): Int64; overload; virtual;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;

    function EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;
    function EvalConst(Op: EOperator; Left, Right: TLapeGlobalVar; Flags: ELapeEvalFlags): TLapeGlobalVar; override;
    function Eval(Op: EOperator; var Dest: TResVar; Left, Right: TResVar; Flags: ELapeEvalFlags; var Offset: Integer; Pos: PDocPos = nil): TResVar; override;

    property MemberMap: TEnumMap read FMemberMap;
    property Small: Boolean read FSmall;
  end;

  TLapeType_Boolean = class(TLapeType_Enum)
    public constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;

    function NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; override;
    function EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean; override;
  end;

  TLapeType_Bool = class(TLapeType_SubRange)
  public
    constructor Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;
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
  public
    constructor Create(ARange: TLapeType_SubRange; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil); reintroduce; virtual;
    function CreateCopy(DeepCopy: Boolean = False): TLapeType; override;

    function VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString; override;
    function VarToString(AVar: Pointer): lpString; override;

    function NewGlobalVar(Values: array of UInt8; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar; virtual;
    function EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType; override;

    property Range: TLapeType_SubRange read FRange;
    property Small: Boolean read FSmall;
  end;

implementation

uses
  Variants,
  {$IFDEF Lape_NeedAnsiStringsUnit}AnsiStrings,{$ENDIF}
  lpparser, lpeval, lpexceptions;

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

constructor TLapeType_Extended.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ltExtended, ACompiler, AName, ADocPos);
end;

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

function TLapeType_SubRange.getAsString: lpString;
begin
  if (FAsString = '') then
    if (FVarType <> nil) then
      FAsString := FVarType.VarToString(@FRange.Lo) + '..' + FVarType.VarToString(@FRange.Hi)
    else
      FAsString := lpString(IntToStr(FRange.Lo)) + '..' + lpString(IntToStr(FRange.Hi));
  Result := inherited;
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
  if (FLo = nil) and (FCompiler <> nil) and IsOrdinal() then
    FLo := FCompiler.addManagedVar(NewGlobalVarStr(lpString(IntToStr(Range.Lo)))) as TLapeGlobalVar;
  Result := FLo;
end;

function TLapeType_SubRange.VarHi(AVar: Pointer = nil): TLapeGlobalVar;
begin
  if (FHi = nil) and (FCompiler <> nil) and IsOrdinal() then
    FHi := FCompiler.addManagedVar(NewGlobalVarStr(lpString(IntToStr(Range.Hi)))) as TLapeGlobalVar;
  Result := FHi;
end;

function TLapeType_SubRange.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type TLapeClassType = class of TLapeType_SubRange;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FCompiler, FVarType, Name, @_DocPos);
  with TLapeType_SubRange(Result) do
  begin
    inheritManagedDecls(Self, not DeepCopy);
    TypeID := Self.TypeID;
    FBaseType := Self.BaseType;
  end;
end;

function TLapeType_SubRange.NewGlobalVar(Value: Int64 = 0; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Value < FRange.Lo) or (Value > FRange.Hi) then
    LapeExceptionFmt(lpeOutOfTypeRange1, [Value, FRange.Lo, FRange.Hi]);

  Result := NewGlobalVarP(nil, AName);
  case BaseIntType of
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

function TLapeType_Enum.getAsString: lpString;
var
  i: Integer;
begin
  if (FAsString = '') then
  begin
    FAsString := '(';
    for i := 0 to FMemberMap.Count - 1 do
    begin
      if (FMemberMap[i] = '') then
        Continue;
      if (FAsString <> '(') then
        FAsString := FAsString + ', ';
      FAsString := FAsString + lpString(FMemberMap[i]) + '=' + lpString(IntToStr(i+FRange.Lo));
    end;
    FAsString := FAsString + ')';
  end;
  Result := inherited;
end;

constructor TLapeType_Enum.Create(ACompiler: TLapeCompilerBase; AMemberMap: TEnumMap; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(NullRange, ACompiler, nil, AName, ADocPos);
  FBaseType := ltLargeEnum;
  FVarType := nil;

  FreeMemberMap := (AMemberMap = nil);
  if (AMemberMap = nil) then
  begin
    AMemberMap := TEnumMap.Create();
    AMemberMap.CaseSensitive := LapeCaseSensitive;
  end;
  FMemberMap := AMemberMap;

  FRange.Hi := Int64(FMemberMap.Count) - 1;
  FSmall := (FRange.Hi <= Ord(High(ELapeSmallEnum)));

  if FSmall then
    FBaseType := ltSmallEnum;
end;

destructor TLapeType_Enum.Destroy;
begin
  if FreeMemberMap then
    FMemberMap.Free();
  inherited;
end;

function TLapeType_Enum.hasMember(AName: lpString): Boolean;
begin
  Result := FMemberMap.IndexOf(string(AName)) > -1;
end;

function TLapeType_Enum.addMember(Value: Int64; AName: lpString): Int16;
var
  i: Integer;
begin
  if (Value < FMemberMap.Count) then
    LapeException(lpeInvalidRange)
  else if (AName = '') or hasMember(AName) then
    LapeException(lpeDuplicateDeclaration);

  Result:= Value;
  FRange.Hi := Value;
  if (FMemberMap.Count = 0) then
    FRange.Lo := Value
  else for i := FMemberMap.Count to Value - FRange.Lo - 1 do
    FMemberMap.Add('');
  FMemberMap.Add(string(AName));

  FSmall := (FRange.Hi <= Ord(High(ELapeSmallEnum)));
  if (not FSmall) then
    FBaseType := ltLargeEnum;

  ClearCache();
end;

function TLapeType_Enum.addMember(AName: lpString): Int64;
begin
  Result := addMember(FRange.Hi + 1, AName);
end;

function TLapeType_Enum.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to FMemberMap.Count - 1 do
  begin
    if (Result <> '') then
      Result := Result + ', ';
    Result := Result + #39 + lpString(FMemberMap[i]) + #39;
  end;
  Result := Format(lpString(
    'type TEnumToString = private function(constref Arr; Index, Lo, Hi: System.SizeInt): System.string;' + LineEnding +
    'begin Result := TEnumToString(System._EnumToString)([%s], System.Ord(Param0), %d, %d); end;'),
    [Result, FRange.Lo, FRange.Hi]
  );
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
      if (i > -1) and (i < FMemberMap.Count) then
        Result := lpString(FMemberMap[i]);

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

function TLapeType_Enum.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_Enum;
begin
  if DeepCopy then
  begin
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, nil, Name, @_DocPos);
    TLapeType_Enum(Result).MemberMap.Assign(FMemberMap);
  end
  else
    Result := TLapeClassType(Self.ClassType).Create(FCompiler, FMemberMap, Name, @_DocPos);

  with TLapeType_Enum(Result) do
  begin
    inheritManagedDecls(Self, not DeepCopy);
    TypeID := Self.TypeID;
    FBaseType := Self.BaseType;
    FRange := Self.Range;
    FSmall := Self.Small;
  end;
end;

function TLapeType_Enum.NewGlobalVarStr(Str: UnicodeString; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
begin
  if (Str <> '') and (AnsiChar(Str[1]) in ['-', '0'..'9']) then
    Result := NewGlobalVar(StrToInt64(string(Str)), AName, ADocPos)
  else
    Result := NewGlobalVar(FMemberMap.IndexOf(Str), AName, ADocPos);
end;

function TLapeType_Enum.EvalAsSubType(Op: EOperator; Right: TLapeType): Boolean;
begin
  if (Right = nil) then
    Result := False
  else if (op in CompareOperators + [op_Assign]) then
    Result := Equals(Right)
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
    Result := FCompiler.getBaseType(BaseIntType).EvalRes(Op, FCompiler.getBaseType(Right.BaseIntType), Flags);
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
    if (not IsOrdinal()) or (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
      LapeException(lpeInvalidEvaluation);

    Left.VarType := FCompiler.getBaseType(BaseIntType);
    Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    Result := Left.VarType.EvalConst(Op, Left, Right, Flags);
    if Result.HasType() and (not (op in CompareOperators)) then
    begin
      if (Result.VarType.BaseIntType <> BaseIntType) then
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
    if (not IsOrdinal()) or (not Right.HasType()) or (not Right.VarType.IsOrdinal()) then
      LapeException(lpeInvalidEvaluation);

    Left.VarType := FCompiler.getBaseType(BaseIntType);
    Right.VarType := FCompiler.getBaseType(Right.VarType.BaseIntType);

    if Dest.HasType() and Equals(Dest.VarType) then
      Dest.VarType := Left.VarType;
    Result := Left.VarType.Eval(Op, Dest, Left, Right, Flags, Offset, Pos);
    if Result.HasType() and (not (op in CompareOperators)) then
    begin
      if (Result.VarType.BaseIntType <> BaseIntType) then
      begin
        tmpRes := Result;

        Result := NullResVar;
        Result.VarType := Left.VarType;
        FCompiler.getDestVar(Dest, Result, Op);

        Result := Left.VarType.Eval(op_Assign, Dest, Result, tmpRes, [], Offset, Pos);
        tmpRes.Spill(1);
      end;
      Result.VarType := Self;
    end;
  end
  else
    Result := inherited;
end;

constructor TLapeType_Boolean.Create(ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  inherited Create(ACompiler, nil, AName, ADocPos);
  addMember('False');
  addMember('True');
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

function TLapeType_Bool.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_Bool;
begin
  Result := TLapeClassType(Self.ClassType).Create(FCompiler, Name, @_DocPos);

  with TLapeType_Bool(Result) do
  begin
    inheritManagedDecls(Self, not DeepCopy);
    TypeID := Self.TypeID;
    FBaseType := Self.BaseType;

    FRange := Self.Range;
    TLapeType_Boolean(FVarType).FSize := Self.VarType.Size;
  end;
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

constructor TLapeType_Set.Create(ARange: TLapeType_SubRange; ACompiler: TLapeCompilerBase; AName: lpString = ''; ADocPos: PDocPos = nil);
begin
  Assert(ARange <> nil);
  inherited Create(ltLargeSet, ACompiler, AName, ADocPos);

  FRange := ARange;
  FSmall := (FRange.Range.Hi <= Ord(High(ELapeSmallEnum)));
  if FSmall then
    FBaseType := ltSmallSet;

  if (FRange.Range.Lo < Ord(Low(ELapeSmallEnum))) or (FRange.Range.Hi > Ord(High(ELapeLargeEnum))) then
    if (FRange.Range.Lo < Ord(Low(ELapeSmallEnum))) then
      LapeExceptionFmt(lpeOutOfTypeRange1, [FRange.Range.Lo, Ord(Low(ELapeSmallEnum)), Ord(High(ELapeLargeEnum))])
    else
      LapeExceptionFmt(lpeOutOfTypeRange1, [FRange.Range.Hi, Ord(Low(ELapeSmallEnum)), Ord(High(ELapeLargeEnum))])
end;

function TLapeType_Set.VarToStringBody(ToStr: TLapeType_OverloadedMethod = nil): lpString;
var
  Index: Integer;
begin
  Result := '';
  if (ToStr = nil) or (FCompiler = nil) or (FRange = nil) or (FRange.BaseIntType = ltUnknown) then
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
  ), [FRange.Range.Hi, LapeTypeToString(FRange.BaseIntType), Index, Result, FRange.Range.Lo, FRange.Range.Hi]);
end;

function TLapeType_Set.VarToString(AVar: Pointer): lpString;
var
  i: Integer;
begin
  Result := '[';
  for i := FRange.Range.Lo to FRange.Range.Hi do
    if (FSmall and (ELapeSmallEnum(i) in PLapeSmallSet(AVar)^)) or ((not FSmall) and (ELapeLargeEnum(i) in PLapeLargeSet(AVar)^)) then
    begin
      if (Result <> '[') then
        Result := Result + ', ';
      Result := Result + FRange.VarToString(@i);
    end;
  Result := Result + ']';
end;

function TLapeType_Set.CreateCopy(DeepCopy: Boolean = False): TLapeType;
type
  TLapeClassType = class of TLapeType_Set;
begin
  Result := TLapeClassType(Self.ClassType).Create(FRange, FCompiler, Name, @_DocPos);
  Result.inheritManagedDecls(Self, not DeepCopy);
  Result.TypeID := TypeID;
end;

function TLapeType_Set.NewGlobalVar(Values: array of UInt8; AName: lpString = ''; ADocPos: PDocPos = nil): TLapeGlobalVar;
var
  i: Integer;
begin
  Result := NewGlobalVarP(nil, AName);
  for i := 0 to High(Values) do
    if FSmall then
      PLapeSmallSet(Result.Ptr)^ := PLapeSmallSet(Result.Ptr)^ + [ELapeSmallEnum(Values[i])]
    else
      PLapeLargeSet(Result.Ptr)^ := PLapeLargeSet(Result.Ptr)^ + [ELapeLargeEnum(Values[i])]
end;

function TLapeType_Set.EvalRes(Op: EOperator; Right: TLapeType = nil; Flags: ELapeEvalFlags = []): TLapeType;
begin
  Result := nil;
  if (Right = nil) then
    Result := inherited
  else
    case getEvalRes(Op, FBaseType, Right.BaseType) of
      ltSmallEnum, ltLargeEnum:
        if (not (Right.BaseType in LapeEnumTypes)) or Right.Equals(FRange) then
          Result := FRange.VarType;
      ltSmallSet, ltLargeSet:
        if (not (Right.BaseType in LapeEnumTypes + LapeSetTypes)) or Right.Equals(FRange) or Equals(Right) then
          Result := Self;
      else
        Result := inherited;
    end;
end;

end.

