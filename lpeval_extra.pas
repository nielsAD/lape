{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Extra evalulation methods that do more than basic ops.
}
unit lpeval_extra;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpmessages;

const
  LapePointerIndexEvalSizes = [4, 8, 16, 24, 32];

function getEvalProc_StringLength: TLapeEvalProc;
function getEvalProc_StringHigh: TLapeEvalProc;
function getEvalProc_DynArrayLength: TLapeEvalProc;
function getEvalProc_DynArrayHigh: TLapeEvalProc;

function getEvalProc_DynArrayRangeCheck(IndexType: ELapeBaseType): TLapeEvalProc;
function getEvalProc_PointerIndex(Size: Integer; IndexType: ELapeBaseType): TLapeEvalProc;

implementation

// Note: in dynarray headers FPC stores high and Delphi stores length

procedure lpeStringLength(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) then
    PSizeInt(Dest)^ := 0
  else
    PSizeInt(Dest)^ := PSizeInt(PPointer(Left)^)[-1];
end;

procedure lpeDynArrayLength(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) then
    PSizeInt(Dest)^ := 0
  else
    PSizeInt(Dest)^ := PSizeInt(PPointer(Left)^)[-1] {$IFDEF FPC}+1{$ENDIF};
end;

procedure lpeDynArrayHigh(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) then
    PSizeInt(Dest)^ := -1
  else
    PSizeInt(Dest)^ := PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF};
end;

procedure lpeDynArrayRangeCheck_WithInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or ((PInt8(Right)^ < 0) or (PInt8(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1])) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt8(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt8(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithUInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or (PUInt8(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1]) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt8(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt8(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or ((PInt16(Right)^ < 0) or (PInt16(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1])) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt16(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt16(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithUInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or (PUInt16(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1]) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt16(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt16(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or ((PInt32(Right)^ < 0) or (PInt32(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1])) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt32(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt32(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithUInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or (PUInt32(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1]) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt32(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt32(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or ((PInt64(Right)^ < 0) or (PInt64(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1])) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt64(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PInt64(Right)^, 0, -1]);
end;

procedure lpeDynArrayRangeCheck_WithUInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (PPointer(Left)^ = nil) or (PUInt64(Right)^ {$IFDEF DELPHI}>={$ELSE}>{$ENDIF} PSizeInt(PPointer(Left)^)[-1]) then
    if (PPointer(Left)^ <> nil) then
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt64(Right)^, 0, PSizeInt(PPointer(Left)^)[-1] {$IFDEF DELPHI}-1{$ENDIF}])
    else
      LapeExceptionFmt(lpeIndexOutOfRange, [PUInt64(Right)^, 0, -1]);
end;

// index by 4
procedure lpePointerIndexBy4_WithInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt8(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithUInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt8(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt16(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithUInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt16(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt32(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithUInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt32(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt64(Right)^ * 4);
end;

procedure lpePointerIndexBy4_WithUInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt64(Right)^ * 4);
end;

// index by 8
procedure lpePointerIndexBy8_WithInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt8(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithUInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt8(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt16(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithUInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt16(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt32(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithUInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt32(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt64(Right)^ * 8);
end;

procedure lpePointerIndexBy8_WithUInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt64(Right)^ * 8);
end;

// index by 16
procedure lpePointerIndexBy16_WithInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt8(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithUInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt8(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt16(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithUInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt16(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt32(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithUInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt32(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt64(Right)^ * 16);
end;

procedure lpePointerIndexBy16_WithUInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt64(Right)^ * 16);
end;

// index by 24
procedure lpePointerIndexBy24_WithInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt8(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithUInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt8(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt16(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithUInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt16(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt32(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithUInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt32(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt64(Right)^ * 24);
end;

procedure lpePointerIndexBy24_WithUInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt64(Right)^ * 24);
end;

// index by 32
procedure lpePointerIndexBy32_WithInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt8(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithUInt8(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt8(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt16(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithUInt16(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt16(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt32(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithUInt32(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt32(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PInt64(Right)^ * 32);
end;

procedure lpePointerIndexBy32_WithUInt64(const Dest, Left, Right: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Dest)^ := PByte(PPointer(Left)^) + (PUInt64(Right)^ * 32);
end;

function getEvalProc_StringLength: TLapeEvalProc;
begin
  Result := @lpeStringLength;
end;

function getEvalProc_StringHigh: TLapeEvalProc; // Same as Length for a string
begin
  Result := @lpeStringLength;
end;

function getEvalProc_DynArrayLength: TLapeEvalProc;
begin
  Result := @lpeDynArrayLength;
end;

function getEvalProc_DynArrayHigh: TLapeEvalProc;
begin
  Result := @lpeDynArrayHigh;
end;

function getEvalProc_DynArrayRangeCheck(IndexType: ELapeBaseType): TLapeEvalProc;
begin
  case IndexType of
    ltInt8:   Result := @lpeDynArrayRangeCheck_WithInt8;
    ltUInt8:  Result := @lpeDynArrayRangeCheck_WithUInt8;
    ltInt16:  Result := @lpeDynArrayRangeCheck_WithInt16;
    ltUInt16: Result := @lpeDynArrayRangeCheck_WithUInt16;
    ltInt32:  Result := @lpeDynArrayRangeCheck_WithInt32;
    ltUInt32: Result := @lpeDynArrayRangeCheck_WithUInt32;
    ltInt64:  Result := @lpeDynArrayRangeCheck_WithInt64;
    ltUInt64: Result := @lpeDynArrayRangeCheck_WithUInt64;
    else
      LapeException(lpeImpossible);
  end;
end;

function getEvalProc_PointerIndex(Size: Integer; IndexType: ELapeBaseType): TLapeEvalProc;
begin
  Result := nil;

  case Size of
    4:
      case IndexType of
        ltInt8:   Result   := @lpePointerIndexBy4_WithInt8;
        ltUInt8:  Result  := @lpePointerIndexBy4_WithUInt8;
        ltInt16:  Result  := @lpePointerIndexBy4_WithInt16;
        ltUInt16: Result := @lpePointerIndexBy4_WithUInt16;
        ltInt32:  Result  := @lpePointerIndexBy4_WithInt32;
        ltUInt32: Result := @lpePointerIndexBy4_WithUInt32;
        ltInt64:  Result  := @lpePointerIndexBy4_WithInt64;
        ltUInt64: Result := @lpePointerIndexBy4_WithUInt64;
      end;

    8:
      case IndexType of
        ltInt8:   Result   := @lpePointerIndexBy8_WithInt8;
        ltUInt8:  Result  := @lpePointerIndexBy8_WithUInt8;
        ltInt16:  Result  := @lpePointerIndexBy8_WithInt16;
        ltUInt16: Result := @lpePointerIndexBy8_WithUInt16;
        ltInt32:  Result  := @lpePointerIndexBy8_WithInt32;
        ltUInt32: Result := @lpePointerIndexBy8_WithUInt32;
        ltInt64:  Result  := @lpePointerIndexBy8_WithInt64;
        ltUInt64: Result := @lpePointerIndexBy8_WithUInt64;
      end;

    16:
      case IndexType of
        ltInt8:   Result   := @lpePointerIndexBy16_WithInt8;
        ltUInt8:  Result  := @lpePointerIndexBy16_WithUInt8;
        ltInt16:  Result  := @lpePointerIndexBy16_WithInt16;
        ltUInt16: Result := @lpePointerIndexBy16_WithUInt16;
        ltInt32:  Result  := @lpePointerIndexBy16_WithInt32;
        ltUInt32: Result := @lpePointerIndexBy16_WithUInt32;
        ltInt64:  Result  := @lpePointerIndexBy16_WithInt64;
        ltUInt64: Result := @lpePointerIndexBy16_WithUInt64;
      end;

    24:
      case IndexType of
        ltInt8:   Result   := @lpePointerIndexBy24_WithInt8;
        ltUInt8:  Result  := @lpePointerIndexBy24_WithUInt8;
        ltInt16:  Result  := @lpePointerIndexBy24_WithInt16;
        ltUInt16: Result := @lpePointerIndexBy24_WithUInt16;
        ltInt32:  Result  := @lpePointerIndexBy24_WithInt32;
        ltUInt32: Result := @lpePointerIndexBy24_WithUInt32;
        ltInt64:  Result  := @lpePointerIndexBy24_WithInt64;
        ltUInt64: Result := @lpePointerIndexBy24_WithUInt64;
      end;

    32:
      case IndexType of
        ltInt8:   Result   := @lpePointerIndexBy32_WithInt8;
        ltUInt8:  Result  := @lpePointerIndexBy32_WithUInt8;
        ltInt16:  Result  := @lpePointerIndexBy32_WithInt16;
        ltUInt16: Result := @lpePointerIndexBy32_WithUInt16;
        ltInt32:  Result  := @lpePointerIndexBy32_WithInt32;
        ltUInt32: Result := @lpePointerIndexBy32_WithUInt32;
        ltInt64:  Result  := @lpePointerIndexBy32_WithInt64;
        ltUInt64: Result := @lpePointerIndexBy32_WithUInt64;
      end;
  end;

  if ({$IFNDEF FPC}@{$ENDIF}Result = nil) then
    LapeException(lpeImpossible);
end;

end.

