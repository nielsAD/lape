{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  These are the jump related records for the bytecode.
}
{%MainUnit lpinterpreter.pas}

POC_Jmp_Var = ^TOC_Jmp_Var;
TOC_Jmp_Var = record
  Target: TCodePos;
  VarStackOffset: TVarStackOffset;
end;

POC_Jmp_Ptr = ^TOC_Jmp_Ptr;
TOC_Jmp_Ptr = record
  Target: TCodePos;
  Ptr: Pointer;
end;

POC_Jmp_PVar = ^TOC_Jmp_PVar;
TOC_Jmp_PVar = record
  Target: TCodePos;
  VarStackOffset: TVarStackOffset;
  PointerOffset: TPointerOffset;
end;

POC_JmpR_Var = ^TOC_JmpR_Var;
TOC_JmpR_Var = record
  Offset: TCodeOffset;
  VarStackOffset: TVarStackOffset;
end;

POC_JmpR_Ptr = ^TOC_JmpR_Ptr;
TOC_JmpR_Ptr = record
  Offset: TCodeOffset;
  Ptr: Pointer;
end;

POC_JmpR_PVar = ^TOC_JmpR_PVar;
TOC_JmpR_PVar = record
  Offset: TCodeOffset;
  VarStackOffset: TVarStackOffset;
  PointerOffset: TPointerOffset;
end;
