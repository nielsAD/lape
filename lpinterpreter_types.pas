{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Interpreter types and opcodes.
}
unit lpinterpreter_types;

{$I lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes;

{$MINENUMSIZE 4} //Better alignment

type
  opCodeP = ^opCode;
  opCode = (
    ocNone,
    ocUnused,                                                  //Unused TStackOffset (in a frame's entry, what it does not need: never run)
    ocIsScriptMethod,                                          //IsScriptMethod
    ocGetExceptionMessage,                                     //GetExceptionMessage
    ocGetExceptionLocation,
    ocGetCallerAddress,
    ocGetScriptMethodName,
    ocDumpCallStack,
    ocInitStackLen,                                            //InitStackLen TStackOffset
    ocInitStack,                                               //InitStack TStackOffset
    ocGrowStack,                                               //GrowStack TStackOffset
    ocExpandVar,                                               //ExpandVar TStackOffset
    ocExpandVarAndInit,                                        //ExpandVarAndInit TStackOffset
    ocGrowVar,                                                 //GrowVar TStackOffset
    ocGrowVarAndInit,                                          //GrowVarAndInit TStackOffset
    ocPopStackToVar,                                           //PopStackToVar TStackOffset TVarStackOffset
    ocPopVarToStack,                                           //PopVarToStack TStackOffset TVarStackOffset
    ocPopVar,                                                  //PopVar
    ocJmpVar,                                                  //JmpVar
    ocJmpSafe,                                                 //JmpSafe TCodePos
    ocJmpSafeR,                                                //JmpSafeR TCodeOffset

    ocIncTry,                                                  //IncTry TCodeOffset UInt32
    ocDecTry,                                                  //DecTry
    ocEndTry,                                                  //EndTry
    ocCatchException,                                          //CatchException
    ocReRaiseException,                                        //ReRaiseException

    ocDecCall,                                                 //DecCall
    ocDecCall_EndTry,                                          //DecCall_EndTry

    {$I lpinterpreter_invokeopcodes.inc}
    {$I lpinterpreter_jumpopcodes.inc}
    {$I lpinterpreter_evalopcodes.inc}
  );

  POC_PopStackToVar = ^TOC_PopStackToVar;
  TOC_PopStackToVar = record
    Size: TStackOffset;
    VOffset: TVarStackOffset;
  end;

  POC_IncTry = ^TOC_IncTry;
  TOC_IncTry = record
    Jmp: TCodeOffset;
    JmpFinally: UInt32;
  end;

  {$I lpinterpreter_invokerecords.inc}
  {$I lpinterpreter_jumprecords.inc}
  {$I lpinterpreter_evalrecords.inc}

const
  ocSize = SizeOf(opCode);

  // The version of TCodeTrailer and what it describes
  LapeCodeVersion = 1;

  Try_NoFinally: UInt32 = UInt32(-1);
  Try_NoExcept: UInt32 = UInt32(-2);
  EndJump: TCodePos = TCodePos(-1);

type
  // An entry of the code's DocPos table (after the program): from CodeOffset on
  TCodeDocPosEntry = packed record
    CodeOffset, Line, Col: UInt32;
    FileName: UInt32; // in the string table
  end;

  // An entry of the code's method table (after the program): a script method that starts at CodeOffset
  TCodeMethodEntry = packed record
    CodeOffset: UInt32;
    Name: UInt32; // in the string table
  end;

  // The last bytes of the code: the program starts at 0, the string, DocPos and method tables follow it;
  // Version is the very last (the rest is what that version says it is)
  TCodeTrailer = packed record
    ProgramSize: UInt32;
    StringTableStart, StringTableCount, StringTableSize: UInt32;
    DocPosTableStart, DocPosTableCount, DocPosTableSize: UInt32;
    MethodTableStart, MethodTableCount, MethodTableSize: UInt32;
    Version: UInt32;
  end;

// What the code says of itself (Code, CodeLen: all of it, tables and trailer included)
// Its trailer (False: none, or not of this version)
function CodeTrailer(Code: PByte; CodeLen: Integer; out Trailer: TCodeTrailer): Boolean;
// The string at Index in the string table
function CodeString(Code: PByte; CodeLen: Integer; Index: Integer): lpString;
// Where the instruction at CodeOffset is from
function CodeDocPos(Code: PByte; CodeLen: Integer; CodeOffset: TCodePos): TDocPos;
// The name of the method that starts at CodeOffset ('': none does)
function CodeMethodName(Code: PByte; CodeLen: Integer; CodeOffset: TCodePos): lpString;

implementation

function CodeTrailer(Code: PByte; CodeLen: Integer; out Trailer: TCodeTrailer): Boolean;
var
  Version: UInt32;
begin
  FillChar(Trailer, SizeOf(Trailer), 0);
  Result := False;
  if (CodeLen < SizeOf(Version)) then
    Exit;

  // (the version first: the last 4 bytes)
  Move(PByte(PtrUInt(Code) + TCodePos(CodeLen - SizeOf(Version)))^, Version, SizeOf(Version));
  Result := (Version = LapeCodeVersion) and (CodeLen >= SizeOf(Trailer));
  if (not Result) then
    Exit;
  Move(PByte(PtrUInt(Code) + TCodePos(CodeLen - SizeOf(Trailer)))^, Trailer, SizeOf(Trailer));

  // (the tables follow each other up to the trailer: else it is not one)
  with Trailer do
    Result := (StringTableStart = ProgramSize) and
              (DocPosTableStart = StringTableStart + StringTableSize) and
              (MethodTableStart = DocPosTableStart + DocPosTableSize) and
              (UInt64(MethodTableStart) + MethodTableSize + SizeOf(Trailer) = UInt64(CodeLen));
  if (not Result) then
    FillChar(Trailer, SizeOf(Trailer), 0);
end;

function CodeString(Code: PByte; CodeLen: Integer; Index: Integer): lpString;
var
  Trailer: TCodeTrailer;
  Raw: RawByteString;
begin
  Result := '';
  if (not CodeTrailer(Code, CodeLen, Trailer)) or (Index < 0) or (UInt32(Index) >= Trailer.StringTableSize) then
    Exit;
  Raw := PAnsiChar(PtrUInt(Code) + Trailer.StringTableStart + TCodePos(Index));
  {$IFDEF Lape_Unicode}
  Result := UTF8Decode(Raw);
  {$ELSE}
  Result := Raw;
  {$ENDIF}
end;

function CodeDocPos(Code: PByte; CodeLen: Integer; CodeOffset: TCodePos): TDocPos;
var
  Trailer: TCodeTrailer;
  Entry, Found: TCodeDocPosEntry;
  i: Integer;
begin
  Result := NullDocPos;
  if (not CodeTrailer(Code, CodeLen, Trailer)) or (Trailer.DocPosTableCount = 0) then
    Exit;

  // (the entries are in code order: the last one at or before CodeOffset)
  Move(PByte(PtrUInt(Code) + Trailer.DocPosTableStart)^, Found, SizeOf(Found));
  if (Found.CodeOffset > CodeOffset) then
    Exit;
  for i := 1 to Integer(Trailer.DocPosTableCount) - 1 do
  begin
    Move(PByte(PtrUInt(Code) + Trailer.DocPosTableStart + TCodePos(i * SizeOf(Entry)))^, Entry, SizeOf(Entry));
    if (Entry.CodeOffset > CodeOffset) then
      Break;
    Found := Entry;
  end;
  Result := TDocPos.Create(Found.Line, Found.Col, CodeString(Code, CodeLen, Found.FileName));
end;

function CodeMethodName(Code: PByte; CodeLen: Integer; CodeOffset: TCodePos): lpString;
var
  Trailer: TCodeTrailer;
  Entry: TCodeMethodEntry;
  i: Integer;
begin
  Result := '';
  if (not CodeTrailer(Code, CodeLen, Trailer)) then
    Exit;

  // (the method that starts at CodeOffset)
  for i := 0 to Integer(Trailer.MethodTableCount) - 1 do
  begin
    Move(PByte(PtrUInt(Code) + Trailer.MethodTableStart + TCodePos(i * SizeOf(Entry)))^, Entry, SizeOf(Entry));
    if (Entry.CodeOffset = CodeOffset) then
      Exit(CodeString(Code, CodeLen, Entry.Name));
  end;
end;

end.

