{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	This include constains all the wrappers for DateTime related functions.
}
{%MainUnit lpeval.pas}

procedure _LapeEncodeDate(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := EncodeDate(PUInt16(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^);
end;

procedure _LapeEncodeTime(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := EncodeTime(PUInt16(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^);
end;

procedure _LapeDecodeDate(const Params: PParamArray);
begin
  DecodeDate(PDateTime(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^);
end;

procedure _LapeDecodeDateFully(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := DecodeDateFully(PDateTime(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^, PUInt16(Params^[4])^);
end;

procedure _LapeDecodeTime(const Params: PParamArray);
begin
  DecodeTime(PDateTime(Params^[0])^, PUInt16(Params^[1])^, PUInt16(Params^[2])^, PUInt16(Params^[3])^, PUInt16(Params^[4])^);
end;

procedure _LapeDate(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := Date();
end;

procedure _LapeTime(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := Time();
end;

procedure _LapeNow(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := Now();
end;

procedure _LapeGetTickCount(const Params: PParamArray; const Result: Pointer);
begin
  {$IF DECLARED(GetTickCount)}
  PUInt32(Result)^ := GetTickCount();
  {$ELSE}
  PUInt32(Result)^ := Trunc(Now() * 24 * 60 * 60 * 1000);
  {$IFEND}
end;

procedure _LapeSleep(const Params: PParamArray);
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure _LapeReplaceTime(const Params: PParamArray);
begin
  ReplaceTime(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeReplaceDate(const Params: PParamArray);
begin
  ReplaceDate(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeFormatDateTime(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := FormatDateTime(PlpString(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeStrToDate(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := StrToDate(PlpString(Params^[0])^);
end;

procedure _LapeStrToDateDef(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := StrToDateDef(PlpString(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeStrToTime(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := StrToTime(PlpString(Params^[0])^);
end;

procedure _LapeStrToTimeDef(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := StrToTimeDef(PlpString(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure _LapeStrToDateTime(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := StrToDateTime(PlpString(Params^[0])^);
end;

procedure _LapeStrToDateTimeDef(const Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := StrToDateTimeDef(PlpString(Params^[0])^, PDateTime(Params^[1])^);
end;
