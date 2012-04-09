{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Lape utilities/extra's.
}
unit lputils;

{$I lape.inc}

interface

uses
  lpcompiler;

type
  PSInit = (psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions, psiUselessTypes);
  PSInitSet = set of PSInit;

procedure InitializePascalScriptBasics(Compiler: TLapeCompiler; Initialize: PSInitSet = [psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions]);
procedure ExposeGlobals(Compiler: TLapeCompiler; HeaderOnly: Boolean = False; DoOverride: Boolean = False);

implementation

uses
  lptypes, lpvartypes, lpparser, lpeval,
  SysUtils;

procedure InitializePascalScriptBasics(Compiler: TLapeCompiler; Initialize: PSInitSet = [psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions]);
begin
  if (Compiler = nil) then
    Exit;

  with Compiler do
  begin
    if (psiSettings in Initialize) then
      Compiler.Options := Compiler.Options + [lcoLooseSemicolon, lcoAutoInvoke];

    if (psiMagicMethod in Initialize) then
    begin
      InternalMethodMap['GetArrayLength'] := InternalMethodMap['Length'];
      InternalMethodMap['SetArrayLength'] := InternalMethodMap['SetLength'];
    end;

    if (psiTypeAlias in Initialize) then
    begin
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).createCopy(), 'Byte');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(ShortInt), True)).createCopy(), 'ShortInt');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Word), False)).createCopy(), 'Word');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(SmallInt), True)).createCopy(), 'SmallInt');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(LongWord), False)).createCopy(), 'LongWord');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(LongInt), True)).createCopy(), 'LongInt');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Cardinal), False)).createCopy(), 'Cardinal');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).createCopy(), 'Integer');
      addGlobalType(getPointerType(ltChar).createCopy(), 'PChar');
    end;

    if (psiUselessTypes in Initialize) then
    begin
      addGlobalType(getPointerType({$IFDEF Delphi}ltUnicodeString{$ELSE}ltAnsiString{$ENDIF}).createCopy(), 'NativeString');
      addGlobalType(getPointerType(ltString).createCopy(), 'AnyString');
      addGlobalType(getPointerType(ltString).createCopy(), 'tbtString');
      addGlobalType(getPointerType(ltPointer).createCopy(), '___Pointer');
      addGlobalType('array of Variant', 'TVariantArray');
    end;

    if (psiFunctionWrappers in Initialize) then
      addDelayedCode(
        'function Chr(IntValue: UInt8):  AnsiChar; overload; begin Result := AnsiChar(IntValue); end;' + LineEnding +
        'function Chr(IntValue: UInt16): WideChar; overload; begin Result := WideChar(IntValue); end;' + LineEnding +
        'function StrGet(var s: string; Index: SizeInt): Char; begin Result := s[Index]; end;' + LineEnding +
        'function StrGet2(s: string; Index: SizeInt): Char; begin Result := s[Index]; end;' + LineEnding +
        'procedure StrSet(c: Char; Index: SizeInt; var s: string); begin s[Index] := c; end;' + LineEnding +
        'function WStrGet(var s: WideString; Index: SizeInt): WideChar; begin Result := s[Index]; end;' + LineEnding +
        'function VarArrayGet(var s: Variant; Index: Int32): Variant; overload; begin Result := VarArrayGet(s, [Index]); end;' + LineEnding +
        'procedure VarArraySet(c: Variant; Index: Int32; var s: Variant); overload; begin VarArraySet(s, c, [Index]); end;' + LineEnding +
        'function PadZ(s: string; Len: SizeInt): string; begin Result := PadL(s, Len, ''0''); end;' + LineEnding +
        'function Replicate(c: Char; l: SizeInt): string; begin Result := StringOfChar(c, l); end;' + LineEnding +
        'function Int64ToStr(i: Int64): string; begin Result := IntToStr(i); end;' + LineEnding +
        'function UInt64ToStr(i: UInt64): string; begin Result := IntToStr(i); end;'
      );

    if (psiExceptions in Initialize) then
    begin
      addGlobalType('('+
        'erNoError, erCannotImport, erInvalidType, erInternalError,' +
        'erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter,' +
        'erNoMainProc, erOutOfGlobalVarsRange, erOutOfProcRange, erOutOfRange,' +
        'erOutOfStackRange, erTypeMismatch, erUnexpectedEof, erVersionError,' +
        'erDivideByZero, erMathError, erCouldNotCallProc, erOutofRecordRange,' +
        'erOutOfMemory, erException, erNullPointerException, erNullVariantError,' +
        'erInterfaceNotSupported, erCustomError)',
        'TIFException');
      addDelayedCode(
        'function ExceptionToString(Ex: TIFException; Param: string): string; begin ' +
          'Result := ToString(Ex);'                                                   +
          'if (Param <> '''') then Result := Result + ''('' + Param + '')'';'         +
        'end;'                                                                        + LineEnding +
        'procedure RaiseException(Ex: TIFException; Param: string); overload; begin RaiseException(ExceptionToString(Ex, Param)); end;'
      );
    end;
  end;
end;

function ExposeGlobals__GetPtr(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompiler): lpString;
begin
  Result := '';
  if (not v.HasType()) or MethodOfObject(v.VarType) then
    Exit;

  AName := LapeCase(AName);
  if v.Writeable then
    Result := '''' + AName + ''': Result := @' + AName + ';' + LineEnding
  else if (v.VarType is TLapeType_Method) then
    Result := '''' + AName + ''': Result := ' + AIA + AName + ';' + LineEnding;
end;

function ExposeGlobals__GetName(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompiler): lpString;
begin
  Result := '';
  if (not v.HasType()) or MethodOfObject(v.VarType) then
    Exit;

  if v.Writeable then
    Result := '@' + AName + ': Result := ''' + AName + ''';' + LineEnding
  else if (v.VarType is TLapeType_Method) then
    Result := AIA + AName + ': Result := ''' + AName + ''';' + LineEnding;
end;

function ExposeGlobals__GetVal(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompiler): lpString;
begin
  Result := '';
  if (not v.HasType()) then
    Exit;

  AName := LapeCase(AName);
  if Compiler.getBaseType(ltVariant).CompatibleWith(v.VarType) then
    Result := '''' + AName + ''': Result := ' + AName + ';' + LineEnding;
end;

function ExposeGlobals__Invoke(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompiler): lpString;
var
  VariantType: TLapeType;

  function ParamsCompatible(Params: TLapeParameterList): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to Params.Count - 1 do
      if (not (Params[i].ParType in Lape_ValParams)) then
        Exit(False)
      else if (not VariantType.CompatibleWith(Params[i].VarType)) then
        Exit(False);
  end;

var
  i: Integer;
begin
  Result := '';
  if (not v.HasType()) or (not  (v.VarType is TLapeType_Method)) or MethodOfObject(v.VarType) then
    Exit;

  AName := LapeCase(AName);
  VariantType := Compiler.getBaseType(ltVariant);

  with TLapeType_Method(v.VarType) do
  begin
    if (not ParamsCompatible(Params)) then
      Exit;

    Result := '''' + AName + ''': begin ' +
      'Assert(Length(Params) = ' + IntToStr(Params.Count) + ');' +
      'Result := ';
    if (Res = nil) or (not VariantType.CompatibleWith(Res)) then
      Result := Result + 'Null; ';
    Result := Result + AName + '(';

    for i := 0 to Params.Count - 1 do
    begin
      if (i > 0) then
        Result := Result + ', ';
      Result := Result + 'Params[' + IntToStr(i) + ']';
    end;

    Result := Result + '); end;' + LineEnding;
  end;
end;

procedure ExposeGlobals(Compiler: TLapeCompiler; HeaderOnly: Boolean = False; DoOverride: Boolean = False);
type
  TTraverseCallback = function(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompiler): lpString;

  function TraverseGlobals(Decls: TLapeDeclarationList; Callback: TTraverseCallback; BaseName: lpString = ''): lpString;
  var
    i: Integer;
    n: lpString;
  begin
    for i := 0 to Decls.Items.Count - 1 do
      with Decls.Items[i] do
      begin
        if (Name = '') then
          if (BaseName = '') then
            Continue
          else
            n := BaseName + '[' + IntToStr(i) + ']'
        else if (BaseName <> '') then
          n := BaseName + '.' + Name
        else
          n := Name;

        if (n = '') or (n[1] = '!') then
          Continue;

        if (Decls.Items[i] is TLapeType) then
          Result := Result + TraverseGlobals(TLapeType(Decls.Items[i]).ManagedDecls, Callback, n)
        else if (Decls.Items[i] is TLapeGlobalvar) then
          with TLapeGlobalVar(Decls.Items[i]) do
          begin
            Result := Result + Callback(TLapeGlobalVar(Decls.Items[i]), n, Compiler);
            if (VarType is TLapeType_Type) or (VarType is TLapeType_OverloadedMethod) then
              Result := Result + TraverseGlobals(VarType.ManagedDecls, Callback, n);
          end;
      end;
  end;

  function GetGlobalPtr: lpString;
  begin
    Result := 'function GetGlobalPtr(Name: string): Pointer;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'begin Result := nil;';
    if (not HeaderOnly) then
    begin
      {$IFDEF Lape_CaseSensitive}
      Result := Result + 'case Name of ';
      {$ELSE}
      Result := Result + 'case LowerCase(Name) of ';
      {$ENDIF}

      Result := Result + LineEnding +
        TraverseGlobals(Compiler.GlobalDeclarations, @ExposeGlobals__GetPtr) +
        'end;';
    end;
    Result := Result + 'end;';
  end;

  function GetGlobalName: lpString;
  begin
    Result := 'function GetGlobalName(Ptr: Pointer): string;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'begin Result := '''';';
    if (not HeaderOnly) then
    begin
      Result := Result + 'case Ptr of ' + LineEnding +
        TraverseGlobals(Compiler.GlobalDeclarations, @ExposeGlobals__GetName) +
        'end;';
    end;
    Result := Result + 'end;';
  end;

  function GetGlobalVal: lpString;
  begin
    Result := 'function GetGlobal(Name: string): Variant;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'begin Result := Unassigned;';
    if (not HeaderOnly) then
    begin
      {$IFDEF Lape_CaseSensitive}
      Result := Result + 'case Name of ';
      {$ELSE}
      Result := Result + 'case LowerCase(Name) of ';
      {$ENDIF}

      Result := Result + LineEnding +
        TraverseGlobals(Compiler.GlobalDeclarations, @ExposeGlobals__GetVal) +
        'end;';
    end;
    Result := Result + 'end;';
  end;

  function VariantInvoke: lpString;
  begin
    Result := 'function VariantInvoke(Name: string; Params: array of Variant): Variant;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'begin Result := Unassigned;';
    if (not HeaderOnly) then
    begin
      {$IFDEF Lape_CaseSensitive}
      Result := Result + 'case Name of ';
      {$ELSE}
      Result := Result + 'case LowerCase(Name) of ';
      {$ENDIF}

      Result := Result + LineEnding +
        TraverseGlobals(Compiler.GlobalDeclarations, @ExposeGlobals__Invoke) +
        'end;';
    end;
    Result := Result + 'end;';
  end;

  function ToString: lpString;
  begin
    Result := '';
    if DoOverride then
      Exit;

    Result :=
      'function ToString(const v): string; overload; begin' +
      '  Result := GetGlobalName(Pointer(v));' +
      '  if (Result = '''') then Result := ToString(Pointer(v));'+
      'end;';
  end;

begin
  if (Compiler = nil) then
    Exit;

  Compiler.addDelayedCode(
    GetGlobalPtr()  + LineEnding +
    GetGlobalName() + LineEnding +
    GetGlobalVal()  + LineEnding +
    VariantInvoke() {+ LineEnding +
    ToString()      }
  );
end;

end.

