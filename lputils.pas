{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
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

  EExposeGlobalsMethod = (egmName, egmPtr, egmValue, egmInvoke);
  EExposeGlobalsMethods = set of EExposeGlobalsMethod;

  TTraverseCallback = procedure(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);

procedure InitializePascalScriptBasics(Compiler: TLapeCompiler; Initialize: PSInitSet = [psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions]);
procedure TraverseGlobals(Compiler: TLapeCompilerBase; Callback: TTraverseCallback; var Arg; BaseName: lpString = ''; Decls: TLapeDeclarationList = nil);
procedure ExposeGlobals(Compiler: TLapeCompiler; Methods: EExposeGlobalsMethods = [egmName, egmPtr, egmValue, egmInvoke]); overload;

const
  LapePascalScriptCompilerOptions = [lcoLooseSemicolon, lcoAutoInvoke];

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
      Compiler.Options := Compiler.Options + LapePascalScriptCompilerOptions;

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
      addGlobalType('(' +
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

  for Decl in Decls.ExportToArray() do
  begin
    try
      if (Decl.Name = '') then
        if (Decl is TLapeGlobalVar) and (TLapeGlobalVar(Decl).VarType is TLapeType_Method) then
          n := BaseName + '[' + lpString(IntToStr(Decls.IndexOf(Decl))) + ']'
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

procedure _ExposeGlobals_Invoke_Callback(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
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
      'Assert(ParamsLen = ' + lpString(IntToStr(Params.Count)) + ', ErrParamCount); ';
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

procedure _ExposeGlobals_GetPtr_Callback(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
var
  Temp: lpString;
begin
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) then
    Exit
  else if (v.VarType is TLapeType_Method) and (TLapeType_Method(v.VarType).MethodDef = mdProperty) then
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

procedure _ExposeGlobals_GetName_Callback(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
var
  Temp: lpString;
begin
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) then
    Exit
  else if (v.VarType is TLapeType_Method) and (TLapeType_Method(v.VarType).MethodDef = mdProperty) then
    Exit
  else if (v.VarType is TLapeType_Method) then
    Temp := 'ConstPointer(' + AName + ')'
  else if (v.VarType.EvalRes(op_Addr) <> nil) then
    Temp := '@' + AName
  else
    Exit;

  TLapeStringList(Arg).Add(Temp + ': Result := '#39 + AName + #39);
end;

procedure _ExposeGlobals_GetVal_Callback(v: TLapeGlobalVar; AName: lpString; Compiler: TLapeCompilerBase; var Arg);
begin
  if (AName = '') or (AName[1] = '!') or (not v.HasType()) or
     (not v.Readable) or (not Compiler.getBaseType(ltVariant).CompatibleWith(v.VarType))
  then
    Exit;

  AName := LapeCase(AName);
  TLapeStringList(Arg).Add(#39 + AName + #39': Result := ' + AName);
end;

function TraverseGlobals_String(Compiler: TLapeCompiler; Callback: TTraverseCallback): lpString;
var
  Builder: TLapeStringList;
begin
  Builder := TLapeStringList.Create('', dupIgnore, True, False);

  try
    TraverseGlobals(Compiler, Callback, Builder);
    Builder.Add('');
    Result := Builder.Implode(';' + LineEnding);
  finally
    Builder.Free();
  end;
end;

procedure _ExposeGlobals_AddInvoke(Compiler: TLapeCompiler);
begin
  Compiler.addDelayedCode(
     LapeDelayedFlags                                                                         + LineEnding +
    'function VariantInvoke(Name: string; Params: array of Variant = []): Variant; override;' + LineEnding +
    'const ErrParamCount = '#39'Parameter count mismatch'#39';'                               + LineEnding +
    'var ParamsLen: SizeInt := Length(Params);'                                               + LineEnding +
    'begin'                                                                                   + LineEnding +
    '  Result := Unassigned;'                                                                 + LineEnding +
    {$IFDEF Lape_CaseSensitive}
    '  case Name of '                                                                         + LineEnding +
    {$ELSE}
    '  case LowerCase(Name) of '                                                              + LineEnding +
    {$ENDIF}
    '    ' + TraverseGlobals_String(Compiler, @_ExposeGlobals_Invoke_Callback)                + LineEnding +
    '    else raise '#39'Cannot invoke "'#39' + Name + '#39'" using VariantInvoke'#39';'      + LineEnding +
    '  end;'                                                                                  + LineEnding +
    'end;',
    '!VariantInvoke');
end;

procedure _ExposeGlobals_AddPtr(Compiler: TLapeCompiler);
begin
  Compiler.addDelayedCode(
    LapeDelayedFlags                                                                          + LineEnding +
    'function GetGlobalPtr(Name: string): ConstPointer; override;'                            + LineEnding +
    'begin'                                                                                   + LineEnding +
    '  Result := nil;'                                                                        + LineEnding +
    {$IFDEF Lape_CaseSensitive}
    '  case Name of '                                                                         + LineEnding +
    {$ELSE}
    '  case LowerCase(Name) of '                                                              + LineEnding +
    {$ENDIF}
    '    ' + TraverseGlobals_String(Compiler, @_ExposeGlobals_GetPtr_Callback)                 + LineEnding +
    '  end;'                                                                                  + LineEnding +
    'end;',
    '!GetGlobalPtr'
  );
end;

procedure _ExposeGlobals_AddGlobalVal(Compiler: TLapeCompiler);
begin
  Compiler.addDelayedCode(
    LapeDelayedFlags                                                                          + LineEnding +
    'function GetGlobal(Name: string): Variant; override;'                                    + LineEnding +
    'begin'                                                                                   + LineEnding +
    '  Result := Unassigned;'                                                                 + LineEnding +
    {$IFDEF Lape_CaseSensitive}
    '  case Name of '                                                                         + LineEnding +
    {$ELSE}
    '  case LowerCase(Name) of '                                                              + LineEnding +
    {$ENDIF}
    '    ' + TraverseGlobals_String(Compiler, @_ExposeGlobals_GetVal_Callback)                + LineEnding +
    '    else raise '#39'Cannot get value of "'#39' + Name + '#39'" using GetGlobal'#39';'    + LineEnding +
    '  end;'                                                                                  + LineEnding +
    'end;',
    '!GetGlobalVal'
  );
end;

procedure _ExposeGlobals_AddGlobalName(Compiler: TLapeCompiler);
begin
  Compiler.addDelayedCode(
    LapeDelayedFlags                                                                          + LineEnding +
    'function GetGlobalName(Ptr: ConstPointer): string; override;'                            + LineEnding +
    'begin'                                                                                   + LineEnding +
    '  Result := '#39#39';'                                                                   + LineEnding +
    '  case Ptr of '                                                                          + LineEnding +
    '    ' + TraverseGlobals_String(Compiler, @_ExposeGlobals_GetName_Callback)               + LineEnding +
    '  end;'                                                                                  + LineEnding +
    'end;',
    '!GetGlobalName'
  );
end;

procedure ExposeGlobals(Compiler: TLapeCompiler; Methods: EExposeGlobalsMethods);
var
  Method: EExposeGlobalsMethod;
begin
  if (Compiler = nil) then
    Exit;

  for Method in Methods do
    case Method of
      egmInvoke:
        begin
          Compiler.addDelayedCode('{$H-} function VariantInvoke(Name: string; Params: array of Variant = []): Variant; begin end;', '!VariantInvoke');
          Compiler.AfterParsing.AddProc(@_ExposeGlobals_AddInvoke);
        end;

      egmPtr:
        begin
          Compiler.addDelayedCode('{$H-} function GetGlobalPtr(Name: string): ConstPointer; begin end;', '!GetGlobalPtr');
          Compiler.AfterParsing.AddProc(@_ExposeGlobals_AddPtr);
        end;

      egmValue:
        begin
          Compiler.addDelayedCode('{$H-} function GetGlobal(Name: string): Variant; begin end;', '!GetGlobalVal');
          Compiler.AfterParsing.AddProc(@_ExposeGlobals_AddGlobalVal);
        end;

      egmName:
        begin
          Compiler.addDelayedCode('{$H-} function GetGlobalName(Ptr: ConstPointer): string; begin end;', '!GetGlobalName');
          Compiler.AfterParsing.AddProc(@_ExposeGlobals_AddGlobalName);
        end;
    end;

  Compiler.addBaseDefine('Lape_ExposeGlobals');
end;

end.

