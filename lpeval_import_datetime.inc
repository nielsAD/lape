{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the wrappers for DateTime related functions.
}
{%MainUnit lpcompiler.pas}

addGlobalVar(HoursPerDay, 'HoursPerDay').isConstant := True;
addGlobalVar(MinsPerHour, 'MinsPerHour').isConstant := True;
addGlobalVar(SecsPerMin, 'SecsPerMin').isConstant := True;
addGlobalVar(MSecsPerSec, 'MSecsPerSec').isConstant := True;
addGlobalVar(MinsPerDay, 'MinsPerDay').isConstant := True;
addGlobalVar(SecsPerDay, 'SecsPerDay').isConstant := True;
addGlobalVar(MSecsPerDay, 'MSecsPerDay').isConstant := True;
addGlobalVar(DateDelta, 'DateDelta').isConstant := True;

addGlobalType(getBaseType(ltDouble).createCopy(), 'TDateTime');
addGlobalType(getBaseType(ltDouble).createCopy(), 'TTime');

addGlobalFunc('function EncodeDate(Year, Month, Day: UInt16): TDateTime;', @_LapeEncodeDate);
addGlobalFunc('function EncodeTime(Hour, Min, Sec, MSec: UInt16): TDateTime;', @_LapeEncodeTime);
addGlobalFunc('procedure DecodeDate(DateTime: TDateTime; var Year, Month, Day: UInt16);', @_LapeDecodeDate);
addGlobalFunc('function DecodeDateFully(DateTime: TDateTime; var Year, Month, Day, DOW: UInt16): Boolean;', @_LapeDecodeDateFully);
addGlobalFunc('procedure DecodeTime(DateTime: TDateTime; var Hour, Min, Sec, MSec: UInt16);', @_LapeDecodeTime);

addGlobalFunc('function Date: TDateTime;', @_LapeDate);
addGlobalFunc('function Time: TDateTime;', @_LapeTime);
addGlobalFunc('function Now: TDateTime;', @_LapeNow);

addGlobalFunc('function GetTickCount: UInt32;', @_LapeGetTickCount);
addGlobalFunc('procedure Sleep(MilliSeconds: UInt32);', @_LapeSleep);

addGlobalFunc('procedure ReplaceTime(var DateTime: TDateTime; NewTime: TDateTime);', @_LapeReplaceTime);
addGlobalFunc('procedure ReplaceDate(var DateTime: TDateTime; NewDate: TDateTime);', @_LapeReplaceDate);

addGlobalFunc('function FormatDateTime(Format: string; DateTime: TDateTime): string;', @_LapeFormatDateTime);
addGlobalFunc('function StrToDate(s: string): TDateTime;', @_LapeStrToDate);
addGlobalFunc('function StrToDateDef(s: string; Default: TDateTime): TDateTime;', @_LapeStrToDateDef);
addGlobalFunc('function StrToTime(s: string): TDateTime;', @_LapeStrToTime);
addGlobalFunc('function StrToTimeDef(s: string; Default: TDateTime): TDateTime;', @_LapeStrToTimeDef);
addGlobalFunc('function StrToDateTime(s: string): TDateTime;', @_LapeStrToDateTime);
addGlobalFunc('function StrToDateTimeDef(s: string; Default: TDateTime): TDateTime;', @_LapeStrToDateTimeDef);
