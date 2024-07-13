{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
unit lpeval_pointerindex;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpmessages;

const
  LapePointerIndexEvalSizes = [4, 8, 16, 24, 32];

function getPointerIndexEvalProc(Size: Integer; IndexType: ELapeBaseType): TLapeEvalProc;

implementation

var
  LapePointerIndexEvals: array[4..32] of array [LapeIntegerTypeRange] of TLapeEvalProc;

function getPointerIndexEvalProc(Size: Integer; IndexType: ELapeBaseType): TLapeEvalProc;
begin
  if (not (IndexType in LapeIntegerTypes)) then
    LapeException(lpeImpossible);
  Result := LapePointerIndexEvals[Size, IndexType];
  if (Result = nil) then
    LapeException(lpeImpossible);
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

procedure LapeInitPointerIndexEvals;
begin
  LapePointerIndexEvals[4, ltInt8]   := @lpePointerIndexBy4_WithInt8;
  LapePointerIndexEvals[4, ltUInt8]  := @lpePointerIndexBy4_WithUInt8;
  LapePointerIndexEvals[4, ltInt16]  := @lpePointerIndexBy4_WithInt16;
  LapePointerIndexEvals[4, ltUInt16] := @lpePointerIndexBy4_WithUInt16;
  LapePointerIndexEvals[4, ltInt32]  := @lpePointerIndexBy4_WithInt32;
  LapePointerIndexEvals[4, ltUInt32] := @lpePointerIndexBy4_WithUInt32;
  LapePointerIndexEvals[4, ltInt64]  := @lpePointerIndexBy4_WithInt64;
  LapePointerIndexEvals[4, ltUInt64] := @lpePointerIndexBy4_WithUInt64;

  LapePointerIndexEvals[8, ltInt8]   := @lpePointerIndexBy8_WithInt8;
  LapePointerIndexEvals[8, ltUInt8]  := @lpePointerIndexBy8_WithUInt8;
  LapePointerIndexEvals[8, ltInt16]  := @lpePointerIndexBy8_WithInt16;
  LapePointerIndexEvals[8, ltUInt16] := @lpePointerIndexBy8_WithUInt16;
  LapePointerIndexEvals[8, ltInt32]  := @lpePointerIndexBy8_WithInt32;
  LapePointerIndexEvals[8, ltUInt32] := @lpePointerIndexBy8_WithUInt32;
  LapePointerIndexEvals[8, ltInt64]  := @lpePointerIndexBy8_WithInt64;
  LapePointerIndexEvals[8, ltUInt64] := @lpePointerIndexBy8_WithUInt64;

  LapePointerIndexEvals[16, ltInt8]   := @lpePointerIndexBy16_WithInt8;
  LapePointerIndexEvals[16, ltUInt8]  := @lpePointerIndexBy16_WithUInt8;
  LapePointerIndexEvals[16, ltInt16]  := @lpePointerIndexBy16_WithInt16;
  LapePointerIndexEvals[16, ltUInt16] := @lpePointerIndexBy16_WithUInt16;
  LapePointerIndexEvals[16, ltInt32]  := @lpePointerIndexBy16_WithInt32;
  LapePointerIndexEvals[16, ltUInt32] := @lpePointerIndexBy16_WithUInt32;
  LapePointerIndexEvals[16, ltInt64]  := @lpePointerIndexBy16_WithInt64;
  LapePointerIndexEvals[16, ltUInt64] := @lpePointerIndexBy16_WithUInt64;

  LapePointerIndexEvals[24, ltInt8]   := @lpePointerIndexBy24_WithInt8;
  LapePointerIndexEvals[24, ltUInt8]  := @lpePointerIndexBy24_WithUInt8;
  LapePointerIndexEvals[24, ltInt16]  := @lpePointerIndexBy24_WithInt16;
  LapePointerIndexEvals[24, ltUInt16] := @lpePointerIndexBy24_WithUInt16;
  LapePointerIndexEvals[24, ltInt32]  := @lpePointerIndexBy24_WithInt32;
  LapePointerIndexEvals[24, ltUInt32] := @lpePointerIndexBy24_WithUInt32;
  LapePointerIndexEvals[24, ltInt64]  := @lpePointerIndexBy24_WithInt64;
  LapePointerIndexEvals[24, ltUInt64] := @lpePointerIndexBy24_WithUInt64;

  LapePointerIndexEvals[32, ltInt8]   := @lpePointerIndexBy32_WithInt8;
  LapePointerIndexEvals[32, ltUInt8]  := @lpePointerIndexBy32_WithUInt8;
  LapePointerIndexEvals[32, ltInt16]  := @lpePointerIndexBy32_WithInt16;
  LapePointerIndexEvals[32, ltUInt16] := @lpePointerIndexBy32_WithUInt16;
  LapePointerIndexEvals[32, ltInt32]  := @lpePointerIndexBy32_WithInt32;
  LapePointerIndexEvals[32, ltUInt32] := @lpePointerIndexBy32_WithUInt32;
  LapePointerIndexEvals[32, ltInt64]  := @lpePointerIndexBy32_WithInt64;
  LapePointerIndexEvals[32, ltUInt64] := @lpePointerIndexBy32_WithUInt64;
end;

initialization
  LapeInitPointerIndexEvals();

end.

