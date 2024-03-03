{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include constains all the wrappers for DateTime related functions.
}
{%MainUnit lpeval.pas}

addGlobalVar(HoursPerDay, 'HoursPerDay').isConstant := True;
addGlobalVar(MinsPerHour, 'MinsPerHour').isConstant := True;
addGlobalVar(SecsPerMin, 'SecsPerMin').isConstant := True;
addGlobalVar(MSecsPerSec, 'MSecsPerSec').isConstant := True;
addGlobalVar(MinsPerDay, 'MinsPerDay').isConstant := True;
addGlobalVar(SecsPerDay, 'SecsPerDay').isConstant := True;
addGlobalVar(MSecsPerDay, 'MSecsPerDay').isConstant := True;
addGlobalVar(DateDelta, 'DateDelta').isConstant := True;

addGlobalType(getBaseType(ltDouble).createCopy(True), 'TDateTime', False);

addGlobalFunc('function EncodeDate(Year, Month, Day: UInt16): TDateTime;', @_LapeEncodeDate);
addGlobalFunc('function EncodeTime(Hour, Min, Sec, MSec: UInt16): TDateTime;', @_LapeEncodeTime);
addGlobalFunc('procedure DecodeDate(DateTime: TDateTime; var Year, Month, Day: UInt16);', @_LapeDecodeDate);
addGlobalFunc('function DecodeDateFully(DateTime: TDateTime; var Year, Month, Day, DOW: UInt16): Boolean;', @_LapeDecodeDateFully);
addGlobalFunc('procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: UInt16);', @_LapeDecodeTime);

addGlobalFunc('function DateTimeToStr(const DateTime: TDateTime): string;', @_LapeDateTimeToStr);
addGlobalFunc('function DateToStr(const DateTime: TDateTime): string;', @_LapeDateToStr);
addGlobalFunc('function TimeToStr(const DateTime: TDateTime): string;', @_LapeTimeToStr);

TLapeType_OverloadedMethod(Globals['ToString'].VarType).addMethod(
  TLapeType_Method(addManagedType(
    TLapeType_Method.Create(
      Self,
      [getGlobalType('TDateTime')],
      [lptConstRef],
      [TLapeGlobalVar(nil)],
      getBaseType(ltString)
    )
  )).NewGlobalVar(@_LapeDateTimeToStr)
);

addGlobalFunc('function Date: TDateTime;', @_LapeDate);
addGlobalFunc('function Time: TDateTime;', @_LapeTime);
addGlobalFunc('function Now: TDateTime;', @_LapeNow);
addGlobalFunc('function NowUTC: TDateTime;', @_LapeNowUTC);

addGlobalFunc('function GetTickCount: UInt64;', @_LapeGetTickCount);
addGlobalFunc('procedure Sleep(MilliSeconds: UInt32);', @_LapeSleep);

addGlobalFunc('procedure ReplaceTime(var DateTime: TDateTime; NewTime: TDateTime);', @_LapeReplaceTime);
addGlobalFunc('procedure ReplaceDate(var DateTime: TDateTime; NewDate: TDateTime);', @_LapeReplaceDate);

addGlobalFunc('function FormatDateTime(Format: string; DateTime: TDateTime): string;', @_LapeFormatDateTime);
addGlobalFunc('function StrToDate(s: string): TDateTime; overload;', @_LapeStrToDate);
addGlobalFunc('function StrToDate(s: string; Default: TDateTime): TDateTime; overload;', @_LapeStrToDateDef);
addGlobalFunc('function StrToTime(s: string): TDateTime; overload;', @_LapeStrToTime);
addGlobalFunc('function StrToTime(s: string; Default: TDateTime): TDateTime; overload;', @_LapeStrToTimeDef);
addGlobalFunc('function StrToDateTime(s: string): TDateTime; overload;', @_LapeStrToDateTime);
addGlobalFunc('function StrToDateTime(s: string; Default: TDateTime): TDateTime; overload;', @_LapeStrToDateTimeDef);

addGlobalFunc('function DateTimeToUnix(const Value: TDateTime; InputIsUTC: Boolean = True): Int64;', @_LapeDateTimeToUnix);
addGlobalFunc('function UnixToDateTime(const Value: Int64; ReturnUTC: Boolean = True): TDateTime;', @_LapeUnixToDateTime);
addGlobalFunc('function UnixTime: Int64;', @_LapeUnixTime);

addGlobalFunc('function YearsBetween(const ANow, AThen: TDateTime): Int32;', @_LapeYearsBetween);
addGlobalFunc('function MonthsBetween(const ANow, AThen: TDateTime): Int32;', @_LapeMonthsBetween);
addGlobalFunc('function WeeksBetween(const ANow, AThen: TDateTime): Int32;', @_LapeWeeksBetween);
addGlobalFunc('function DaysBetween(const ANow, AThen: TDateTime): Int32;', @_LapeDaysBetween);
addGlobalFunc('function HoursBetween(const ANow, AThen: TDateTime): Int64;', @_LapeHoursBetween);
addGlobalFunc('function MinutesBetween(const ANow, AThen: TDateTime): Int64;', @_LapeMinutesBetween);
addGlobalFunc('function SecondsBetween(const ANow, AThen: TDateTime): Int64;', @_LapeSecondsBetween);
addGlobalFunc('function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;', @_LapeMilliSecondsBetween);

addGlobalFunc('function IncYear(const Value: TDateTime; const NumberOfYears: Int32 = 1): TDateTime;', @_LapeIncYear);
addGlobalFunc('function IncWeek(const Value: TDateTime; const NumberOfWeeks: Int32 = 1): TDateTime;', @_LapeIncWeek);
addGlobalFunc('function IncDay(const Value: TDateTime; const NumberOfDays: Int32 = 1): TDateTime;', @_LapeIncDay);
addGlobalFunc('function IncHour(const Value: TDateTime; const NumberOfHours: Int64 = 1): TDateTime;', @_LapeIncHour);
addGlobalFunc('function IncMinute(const Value: TDateTime; const NumberOfMinutes: Int64 = 1): TDateTime;', @_LapeIncMinute);
addGlobalFunc('function IncSecond(const Value: TDateTime; const NumberOfSeconds: Int64 = 1): TDateTime;', @_LapeIncSecond);
addGlobalFunc('function IncMilliSecond(const Value: TDateTime; const NumberOfMilliSeconds: Int64 = 1): TDateTime;', @_LapeIncMilliSecond);
