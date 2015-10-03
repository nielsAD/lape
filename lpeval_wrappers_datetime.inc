{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for DateTime related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeEncodeDate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := EncodeDate(PUInt16(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^);
end;

procedure _LapeEncodeTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := EncodeTime(PUInt16(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^);
end;

procedure _LapeDecodeDate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DecodeDate(PDateTime(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^);
end;

procedure _LapeDecodeDateFully(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := DecodeDateFully(PDateTime(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^, PUInt16(Params^[4])^);
end;

procedure _LapeDecodeTime(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  DecodeTime(PDateTime(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^, PUInt16(Params^[4])^);
end;

procedure _LapeDateTimeToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(DateTimeToStr(PDateTime(Params^[0])^));
end;

procedure _LapeDateToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(DateToStr(PDateTime(Params^[0])^));
end;

procedure _LapeTimeToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(TimeToStr(PDateTime(Params^[0])^));
end;

procedure _LapeDate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := Date();
end;

procedure _LapeTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := Time();
end;

procedure _LapeNow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := Now();
end;

procedure _LapeGetTickCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IF DECLARED(GetTickCount)}
  PUInt32(Result)^ := GetTickCount();
  {$ELSE}
  PUInt32(Result)^ := Trunc(Now() * 24 * 60 * 60 * 1000);
  {$IFEND}
end;

procedure _LapeSleep(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure _LapeReplaceTime(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ReplaceTime(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeReplaceDate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ReplaceDate(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeFormatDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := lpString(FormatDateTime(string(PlpString(Params^[0])^), PDateTime(Params^[1])^));
end;

procedure _LapeStrToDate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := StrToDate(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToDateDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := StrToDateDef(string(PlpString(Params^[0])^), PDateTime(Params^[1])^);
end;

procedure _LapeStrToTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := StrToTime(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToTimeDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := StrToTimeDef(string(PlpString(Params^[0])^), PDateTime(Params^[1])^);
end;

procedure _LapeStrToDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := StrToDateTime(string(PlpString(Params^[0])^));
end;

procedure _LapeStrToDateTimeDef(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := StrToDateTimeDef(string(PlpString(Params^[0])^), PDateTime(Params^[1])^);
end;
