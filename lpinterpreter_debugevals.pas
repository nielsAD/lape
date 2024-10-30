{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
unit lpinterpreter_debugevals;

{$i lape.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpinterpreter_types;

type
  TLapeDebugEvals = class
  protected
    FHits: array of array[opCode] of Int64;
    FNames: TStringArray;
    FLo: PtrUInt;
    FHi: PtrUInt;
    FTotalHits: Int64;

    function getName(Proc: TLapeEvalProc): lpString;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Proc: TLapeEvalProc; op: opCode);

    property Name[Proc: TLapeEvalProc]: lpString read getName;
  end;

implementation

uses
  TypInfo, StrUtils,
  lpeval, lpeval_extra;

constructor TLapeDebugEvals.Create;
var
  op: EOperator;
  t1, t2: ELapeBaseType;
  proc: TLapeEvalProc;
  i: Integer;
  TheName: string;
begin
  FLo := High(PtrUInt);
  FHi := 0;
  FNames := nil;

  for op := Low(EOperator) to High(EOperator) do
  begin
    if (op_name[op] = '') then
      Continue;

    for t1 := High(ELapeBaseType) downto Low(ELapeBaseType) do
      for t2 := High(ELapeBaseType) downto Low(ELapeBaseType) do
      begin
        proc := getEvalProc(op, t1, t2);
        if ValidEvalFunction(proc) then
        begin
          if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) < FLo) then FLo := PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc);
          if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) > FHi) then FHi := PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc);
        end;
      end;
  end;

  // TODO
  //for i := Low(LapePointerIndexEvals) to High(LapePointerIndexEvals) do
  //  for t1 := Low(LapeIntegerTypeRange) to High(LapeIntegerTypeRange) do
  //  begin
  //    proc := LapePointerIndexEvals[i][t1];
  //    if ValidEvalFunction(proc) then
  //    begin
  //      if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) < FLo) then FLo := PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc);
  //      if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) > FHi) then FHi := PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc);
  //    end;
  //  end;
  //
  //  for t1 := Low(LapeIntegerTypeRange) to High(LapeIntegerTypeRange) do
  //  begin
  //    proc := LapeDynArrayRangeCheckEvals[t1];
  //    if ValidEvalFunction(proc) then
  //    begin
  //      if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) < FLo) then FLo := PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc);
  //      if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) > FHi) then FHi := PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc);
  //    end;
  //  end;

  SetLength(FHits, (FHi - FLo) + 1);
  SetLength(FNames, (FHi - FLo) + 1);

  for op := Low(EOperator) to High(EOperator) do
  begin
    if (op_name[op] = '') then
      Continue;

    for t1 := High(ELapeBaseType) downto Low(ELapeBaseType)  do
      for t2 := High(ELapeBaseType) downto Low(ELapeBaseType) do
      begin
        proc := getEvalProc(op, t1, t2);
        if ValidEvalFunction(proc) then
        begin
          if (t1 = ltUnknown) then
            TheName := 'lpe'+string(op_name[op])
          else if (t2 = ltUnknown) then
            TheName := 'lpe'+string(LapeTypeToString(t1))+'_'+string(op_name[op])
          else
            TheName := 'lpe'+string(LapeTypeToString(t1))+'_'+string(op_name[op]+'_'+LapeTypeToString(t2));

          FNames[PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) - FLo] := TheName;
        end;
      end;
  end;

  // TODO:
  //for i := Low(LapePointerIndexEvals) to High(LapePointerIndexEvals) do
  //  for t1 := Low(LapeIntegerTypeRange) to High(LapeIntegerTypeRange) do
  //  begin
  //    proc := LapePointerIndexEvals[i][t1];
  //    if ValidEvalFunction(proc) then
  //      FNames[PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) - FLo] := 'lpePointerIndexBy' + IntToStr(i) + '_With' + LapeTypeToString(t1);
  //  end;
  //
  //  for t1 := Low(LapeIntegerTypeRange) to High(LapeIntegerTypeRange) do
  //  begin
  //    proc := LapeDynArrayRangeCheckEvals[t1];
  //    if ValidEvalFunction(proc) then
  //      FNames[PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) - FLo] := 'lpeDynArrayRangeCheck_With' + LapeTypeToString(t1);
  //  end;
end;

destructor TLapeDebugEvals.Destroy;
var
  i: Integer;
  op: opCode;
  Weights: TInt64Array;
  Lines: TStringArray;
begin
  Weights := nil;
  Lines := nil;

  for i := 0 to High(FHits) do
    for op := Low(opCode) to High(opCode) do
    begin
      if FHits[i][op] > 0 then
      begin
        SetLength(Weights, Length(Weights) + 1);
        SetLength(Lines, Length(Lines) + 1);

        Weights[High(Weights)] := FHits[I][op];
        Lines[High(Lines)] := Format('%s %s %d - %f%%', [
          {$IFDEF FPC}PadRight(FNames[i], 35){$ELSE}FNames[i]{$ENDIF},
          {$IFDEF FPC}PadRight(GetEnumName(TypeInfo(opCode), Ord(op)), 25){$ELSE}GetEnumName(TypeInfo(opCode), Ord(op)){$ENDIF},
          FHits[i][op],
          FHits[i][op] / FTotalHits * 100
        ]);
      end;
    end;

  if Length(Lines) > 0 then
  begin
    {$IFDEF FPC}specialize{$ENDIF} TLapeSorter<Int64>.QuickSort(@Lines[0], SizeOf(String), Length(Lines), {$IFDEF FPC}specialize{$ENDIF}TLapeSorter<Int64>.TWeightArr(Weights), False);
    for i := 0 to High(Lines) do
      WriteLn(Lines[i]);
  end;

  inherited Destroy();
end;

function TLapeDebugEvals.getName(Proc: TLapeEvalProc): lpString;
begin
  if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) >= FLo) and (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) <= FHi) then
    Result := FNames[PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) - FLo]
  else
    Result := 'EvalProc(' + IntToHex(PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc)) + ')';
end;

procedure TLapeDebugEvals.Add(Proc: TLapeEvalProc; op: opCode);
begin
  if (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) >= FLo) and (PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) <= FHi) then
  begin
    Inc(FHits[PtrUInt({$IFNDEF FPC}@{$ENDIF}Proc) - FLo][op]);
    Inc(FTotalHits);
  end;
end;

end.

