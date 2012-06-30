{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the headers for the DateTime related wrappers.
}
{%MainUnit lpeval.pas}

procedure _LapeEncodeDate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeEncodeTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeDecodeDate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeDecodeDateFully(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeDecodeTime(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeDate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeNow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeGetTickCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeSleep(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeReplaceTime(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeReplaceDate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}

procedure _LapeFormatDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeStrToDate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeStrToDateDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeStrToTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeStrToTimeDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeStrToDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
procedure _LapeStrToDateTimeDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
