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

procedure InitializePascalScriptBasics(Compiler: TLapeCompiler; Initialize: PSInitSet = [psiSettings, psiTypeAlias, psiMagicMethod, psiFunctionWrappers, psiExceptions]);

const
  LapePascalScriptCompilerOptions = [lcoLooseSemicolon, lcoAutoInvoke];

implementation

uses
  lptokenizer, lpeval,
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
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).CreateAlias(), 'Byte');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(ShortInt), True)).CreateAlias(), 'ShortInt');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Word), False)).CreateAlias(), 'Word');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(SmallInt), True)).CreateAlias(), 'SmallInt');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(LongWord), False)).CreateAlias(), 'LongWord');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(LongInt), True)).CreateAlias(), 'LongInt');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Cardinal), False)).CreateAlias(), 'Cardinal');
      addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).CreateAlias(), 'Integer');
      addGlobalType(getPointerType(ltChar, False).CreateAlias(), 'PChar');
    end;

    if (psiUselessTypes in Initialize) then
    begin
      addGlobalType(getBaseType({$IFDEF Lape_Unicode}ltUnicodeString{$ELSE}ltAnsiString{$ENDIF}).CreateAlias(), 'NativeString');
      addGlobalType(getBaseType(ltString).CreateAlias(), 'AnyString');
      addGlobalType(getBaseType(ltString).CreateAlias(), 'tbtString');
      addGlobalType(getBaseType(ltPointer).CreateAlias(), '___Pointer');
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

end.

