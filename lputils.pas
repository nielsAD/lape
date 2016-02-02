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
  lptypes, lpvartypes, lpcompiler;

type
  PSInit = (psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions, psiUselessTypes);
  PSInitSet = set of PSInit;
  TTraverseCallback = procedure(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);

procedure InitializePascalScriptBasics(Compiler: TLapeCompiler; Initialize: PSInitSet = [psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions]);
procedure TraverseGlobals(Compiler: TLapeCompilerBase; Callback: TTraverseCallback; var Arg; BaseName: lpString = ''; Decls: TLapeDeclarationList = nil);
procedure ExposeGlobals(Compiler: TLapeCompiler; HeaderOnly, DoOverride: Boolean); overload;
procedure ExposeGlobals(Compiler: TLapeCompiler); overload;

implementation

uses
  lpparser, lpeval,
  SysUtils, Classes;

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
      addGlobalType(getPointerType(ltChar, False).createCopy(), 'PChar');
    end;

    if (psiUselessTypes in Initialize) then
    begin
      addGlobalType(getBaseType({$IFDEF Lape_Unicode}ltUnicodeString{$ELSE}ltAnsiString{$ENDIF}).createCopy(), 'NativeString');
      addGlobalType(getBaseType(ltString).createCopy(), 'AnyString');
      addGlobalType(getBaseType(ltString).createCopy(), 'tbtString');
      addGlobalType(getBaseType(ltPointer).createCopy(), '___Pointer');
      addGlobalType('array of Variant', 'TVariantArray');
    end;

    if (psiFunctionWrappers in Initialize) then
      addDelayedCode(
        LapeDelayedFlags +
        'function StrGet(var s: string; Index: SizeInt): Char; begin Result := s[Index]; end;' + LineEnding +
        'function StrGet2(s: string; Index: SizeInt): Char; begin Result := s[Index]; end;' + LineEnding +
        'procedure StrSet(c: Char; Index: SizeInt; var s: string); begin s[Index] := c; end;' + LineEnding +
        'function WStrGet(var s: WideString; Index: SizeInt): WideChar; begin Result := s[Index]; end;' + LineEnding +
        'function VarArrayGet(var s: Variant; Index: Int32): Variant; overload; begin Result := VarArrayGet(s, [Index]); end;' + LineEnding +
        'procedure VarArraySet(c: Variant; Index: Int32; var s: Variant); overload; begin VarArraySet(s, c, [Index]); end;' + LineEnding +
        'function PadZ(s: string; Len: SizeInt): string; begin Result := PadL(s, Len, '#39'0'#39'); end;' + LineEnding +
        'function Replicate(c: Char; l: SizeInt): string; begin Result := StringOfChar(c, l); end;' + LineEnding +
        'function Int64ToStr(i: Int64): string; begin Result := IntToStr(i); end;' + LineEnding +
        'function UInt64ToStr(i: UInt64): string; begin Result := IntToStr(i); end;'
      , '!addDelayedPSUseless');

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
        LapeDelayedFlags +
        'function ExceptionToString(Ex: TIFException; Param: string): string; begin '         +
          'Result := ToString(Ex);'                                                           +
          'if (Param <> '#39#39') then Result := Result + '#39'('#39' + Param + '#39')'#39';' +
        'end;'                                                                                + LineEnding +
        'procedure RaiseException(Ex: TIFException; Param: string); overload; begin RaiseException(ExceptionToString(Ex, Param)); end;'
      , '!addDelayedPSExceptions');
    end;
  end;
end;

procedure ExposeGlobals__GetPtr(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
var
  Temp: lpString;
begin
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) then
    Exit
  else if (v.VarType is TLapeType_Method) then
    Temp := ''
  else if (v.VarType.EvalRes(op_Addr) <> nil) then
    Temp := '@'
  else
    Exit;

  AName := LapeCase(AName);
  TLapeStringList(Arg).Add(#39 + AName + #39': Result := ' + Temp + AName);
end;

procedure ExposeGlobals__GetName(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
var
  Temp: lpString;
begin
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) then
    Exit
  else if (v.VarType is TLapeType_Method) then
    Temp := 'ConstPointer(' + AName + ')'
  else if (v.VarType.EvalRes(op_Addr) <> nil) then
    Temp := '@' + AName
  else
    Exit;

  TLapeStringList(Arg).Add(Temp + ': Result := '#39 + AName + #39);
end;

procedure ExposeGlobals__GetVal(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
begin
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) or
     (not v.Readable) or (not Compiler.getBaseType(ltVariant).CompatibleWith(v.VarType))
  then
    Exit;

  AName := LapeCase(AName);
  TLapeStringList(Arg).Add(#39 + AName + #39': Result := ' + AName);
end;

procedure ExposeGlobals__Invoke(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
var
  VariantType: TLapeType;

  function ParamsCompatible(Params: TLapeParameterList): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 0 to Params.Count - 1 do
      if (Params[i].VarType = nil) or (not (Params[i].ParType in Lape_ValParams)) then
        Exit(False)
      else if (not Params[i].VarType.CompatibleWith(VariantType)) then
        Exit(False);
  end;

var
  i: Integer;
  Temp: lpString;
begin
  Temp := '';
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) or
      (not (v.VarType is TLapeType_Method)) or MethodOfObject(v.VarType)
  then
    Exit;

  AName := LapeCase(AName);
  VariantType := Compiler.getBaseType(ltVariant);

  with TLapeType_Method(v.VarType) do
  begin
    if (not ParamsCompatible(Params)) then
      Exit;

    Temp := #39 + AName + #39': begin ' +
      'Assert(ParamsLen = ' + lpString(IntToStr(Params.Count)) + '); ';
    if (Res <> nil) and VariantType.CompatibleWith(Res) then
      Temp := Temp + 'Result := ';
    Temp := Temp + AName + '(';

    for i := 0 to Params.Count - 1 do
    begin
      if (i > 0) then
        Temp := Temp + ', ';
      Temp := Temp + 'Params[' + lpString(IntToStr(i)) + ']';
    end;

    Temp := Temp + '); end';
  end;

  TLapeStringList(Arg).Add(Temp);
end;

procedure TraverseGlobals(Compiler: TLapeCompilerBase; Callback: TTraverseCallback; var Arg; BaseName: lpString = ''; Decls: TLapeDeclarationList = nil);

  procedure TraverseBaseTypes;
  var
    BaseType: ELapeBaseType;
    Typ: TLapeType;
  begin
    for BaseType := Succ(ltUnknown) to High(ELapeBaseType) do
    begin
      Typ := Compiler.getBaseType(BaseType);
      if (Typ <> nil) then
        TraverseGlobals(Compiler, Callback, Arg, Typ.Name, Typ.ManagedDeclarations);
    end;
  end;

var
  i: Integer;
  n: lpString;
  Decl: TLapeDeclaration;
begin
  if (BaseName = 'System') then
    Exit;

  if (Decls = nil) then
    if (Compiler = nil) then
      Exit
    else
    begin
      TraverseBaseTypes();
      Decls := Compiler.GlobalDeclarations;
    end;

  for i := 0 to Decls.Count - 1 do
  begin
    Decl := Decls[i];
    try
      if (Decl.Name = '') then
        if (Decl is TLapeGlobalVar) and (TLapeGlobalVar(Decl).VarType is TLapeType_Method) then
          n := BaseName + '[' + lpString(IntToStr(i)) + ']'
        else
          Continue
      else if (BaseName <> '') then
        n := BaseName + '.' + Decl.Name
      else
        n := Decl.Name;

      if (Decl is TLapeType) then
        TraverseGlobals(Compiler, Callback, Arg, n, TLapeType(Decl).ManagedDeclarations)
      else if (Decl is TLapeGlobalvar) then
        with TLapeGlobalVar(Decl) do
        begin
          Callback(TLapeGlobalVar(Decl), n, Compiler, Arg);
          if (VarType is TLapeType_Type) or (VarType is TLapeType_OverloadedMethod) then
            TraverseGlobals(Compiler, Callback, Arg, n, VarType.ManagedDeclarations);
        end;
    except
      {catch exception}
    end;
  end;
end;

procedure ExposeGlobals(Compiler: TLapeCompiler; HeaderOnly, DoOverride: Boolean);

  function TraverseGlobals_String(Callback: TTraverseCallback): lpString;
  var
    Builder: TLapeStringList;
  begin
    Builder := TLapeStringList.Create('', dupIgnore, False, False);
    try
      TraverseGlobals(Compiler, Callback, Builder);
      Builder.Add('');
      Result := Builder.Implode(';' + LineEnding);
    finally
      Builder.Free();
    end;
  end;

  function GetGlobalPtr: lpString;
  begin
    Result := 'function GetGlobalPtr(Name: string): ConstPointer;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'begin Result := nil;';
    if (not HeaderOnly) then
    begin
      {$IFDEF Lape_CaseSensitive}
      Result := Result + 'case Name of ' + LineEnding;
      {$ELSE}
      Result := Result + 'case LowerCase(Name) of ' + LineEnding;
      {$ENDIF}

      Result := Result + TraverseGlobals_String(@ExposeGlobals__GetPtr);
      Result := Result + 'end;';
    end;
    Result := Result + 'end;';
  end;

  function GetGlobalName: lpString;
  begin
    Result := 'function GetGlobalName(Ptr: ConstPointer): string;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'begin Result := '#39#39';';
    if (not HeaderOnly) then
    begin
      Result := Result + 'case Ptr of ' + LineEnding;
      Result := Result + TraverseGlobals_String(@ExposeGlobals__GetName);
      Result := Result + 'end;';
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
      Result := Result + 'case Name of ' + LineEnding;
      {$ELSE}
      Result := Result + 'case LowerCase(Name) of ' + LineEnding;
      {$ENDIF}

      Result := Result + TraverseGlobals_String(@ExposeGlobals__GetVal);
      Result := Result + 'end;';
    end;
    Result := Result + 'end;';
  end;

  function VariantInvoke: lpString;
  begin
    Result := 'function VariantInvoke(Name: string; Params: array of Variant = []): Variant;';
    if DoOverride then
      Result := Result + 'override;';
    Result := Result + 'var ParamsLen: SizeInt := Length(Params);';
    Result := Result + 'begin Result := Unassigned;';
    if (not HeaderOnly) then
    begin
      {$IFDEF Lape_CaseSensitive}
      Result := Result + 'case Name of ' + LineEnding;
      {$ELSE}
      Result := Result + 'case LowerCase(Name) of ' + LineEnding;
      {$ENDIF}

      Result := Result + TraverseGlobals_String(@ExposeGlobals__Invoke);
      Result := Result + 'end;';
    end;
    Result := Result + 'end;';
  end;

  function ToString: lpString;
  begin
    Result := '';
    if DoOverride then
      Exit;

    Result :=
      'function ToString(constref p: Pointer): string; override;' +
      'var n: string; begin' +
      '  Result := inherited();' +
      '  if (p <> nil) then' +
      '  begin' +
      '    n := GetGlobalName(p);' +
      '    if (n <> '#39#39') then Result := '#39'"'#39' + n + '#39'"::'#39' + Result;' +
      '  end;' +
      'end;';
  end;

begin
  if (Compiler = nil) then
    Exit;

  Compiler.addDelayedCode(
    LapeDelayedFlags +
    GetGlobalPtr()  + LineEnding +
    GetGlobalName() + LineEnding +
    GetGlobalVal()  + LineEnding +
    VariantInvoke() + LineEnding +
    ToString()
  , '!addDelayedExpose');
end;

procedure _ExposeGlobals_FillProcs(Compiler: TLapeCompiler);
begin
  ExposeGlobals(Compiler, False, True);
end;

procedure ExposeGlobals(Compiler: TLapeCompiler);
begin
  if (Compiler = nil) then
    Exit;

  ExposeGlobals(Compiler, True, False);
  Compiler.addBaseDefine('Lape_ExposeGlobals');
  Compiler.AfterParsing.AddProc(@_ExposeGlobals_FillProcs);
end;

end.

