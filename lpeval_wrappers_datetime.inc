{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
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
  {$IF DECLARED(GetTickCount64)}
  PUInt64(Result)^ := GetTickCount64();
  {$ELSE}
    {$IF DECLARED(GetTickCount)}
    PUInt64(Result)^ := GetTickCount();
    {$ELSE}
    PUInt64(Result)^ := Trunc(Now() * 24 * 60 * 60 * 1000);
    {$IFEND}
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

procedure _LapeDateTimeToUnix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := DateTimeToUnix(PDateTime(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeUnixToDateTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := UnixToDateTime(PInt64(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeUnixTime(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := DateTimeToUnix(Now(), False);
end;

procedure _LapeNowUTC(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := UnixToDateTime(DateTimeToUnix(Now(), False));
end;

procedure _LapeYearsBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := YearsBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeMonthsBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := MonthsBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeWeeksBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := WeeksBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeDaysBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := DaysBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeHoursBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := HoursBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeMinutesBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := MinutesBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeSecondsBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := SecondsBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeMilliSecondsBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := MilliSecondsBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeIncYear(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncYear(PDateTime(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeIncWeek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncWeek(PDateTime(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeIncDay(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncDay(PDateTime(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeIncHour(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncHour(PDateTime(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeIncMinute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncMinute(PDateTime(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeIncSecond(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncSecond(PDateTime(Params^[0])^, PInt64(Params^[1])^);
end;

procedure _LapeIncMilliSecond(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDateTime(Result)^ := IncMilliSecond(PDateTime(Params^[0])^, PInt64(Params^[1])^);
end;
