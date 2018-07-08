{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for file related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeExtractFilePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFilePath(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileDrive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileDrive(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileName(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileExt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileExt(PlpString(Params^[0])^);
end;

procedure _LapeExtractFileDir(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractFileDir(PlpString(Params^[0])^);
end;

procedure _LapeExpandFileName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExpandFileName(PlpString(Params^[0])^);
end;

procedure _LapeExtractRelativePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExtractRelativePath(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure _LapeIncludeTrailingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := IncludeTrailingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeExcludeTrailingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExcludeTrailingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeIncludeTrailingBackslash(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := IncludeTrailingBackslash(PlpString(Params^[0])^);
end;

procedure _LapeExcludeTrailingBackslash(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExcludeTrailingBackslash(PlpString(Params^[0])^);
end;

procedure _LapeIncludeLeadingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := IncludeLeadingPathDelimiter(PlpString(Params^[0])^);
end;

procedure _LapeExcludeLeadingPathDelimiter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := ExcludeLeadingPathDelimiter(PlpString(Params^[0])^);
end;

