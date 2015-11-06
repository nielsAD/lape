{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  These are the invoke related records for the bytecode.
}
{%MainUnit lpinterpreter.pas}

POC_Invoke_Stk = ^TOC_Invoke_Stk;
TOC_Invoke_Stk = record
  SOffset: TStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Var = ^TOC_Invoke_Var;
TOC_Invoke_Var = record
  VOffset: TVarStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_PVar = ^TOC_Invoke_PVar;
TOC_Invoke_PVar = record
  VOffset: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Ptr = ^TOC_Invoke_Ptr;
TOC_Invoke_Ptr = record
  Ptr: Pointer;
  ParamSize: TParamSize;
end;

POC_Invoke_Stk_Stk = ^TOC_Invoke_Stk_Stk;
TOC_Invoke_Stk_Stk = record
  SOffset: TStackOffset;
  ParamSize: TParamSize;
  StackInc: TStackInc;
end;

POC_Invoke_Stk_PStk = ^TOC_Invoke_Stk_PStk;
TOC_Invoke_Stk_PStk = record
  SOffset: TStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Stk_Var = ^TOC_Invoke_Stk_Var;
TOC_Invoke_Stk_Var = record
  SOffset: TStackOffset;
  VOffsetD: TVarStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Stk_Ptr = ^TOC_Invoke_Stk_Ptr;
TOC_Invoke_Stk_Ptr = record
  SOffset: TStackOffset;
  PtrD: Pointer;
  ParamSize: TParamSize;
end;

POC_Invoke_Stk_PVar = ^TOC_Invoke_Stk_PVar;
TOC_Invoke_Stk_PVar = record
  SOffset: TStackOffset;
  VOffsetD: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Var_Stk = ^TOC_Invoke_Var_Stk;
TOC_Invoke_Var_Stk = record
  VOffsetS: TVarStackOffset;
  ParamSize: TParamSize;
  StackInc: TStackInc;
end;

POC_Invoke_Var_PStk = ^TOC_Invoke_Var_PStk;
TOC_Invoke_Var_PStk = record
  VOffsetS: TVarStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Var_Var = ^TOC_Invoke_Var_Var;
TOC_Invoke_Var_Var = record
  VOffsetS, VOffsetD: TVarStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Var_PVar = ^TOC_Invoke_Var_PVar;
TOC_Invoke_Var_PVar = record
  VOffsetS, VOffsetD: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Var_Ptr = ^TOC_Invoke_Var_Ptr;
TOC_Invoke_Var_Ptr = record
  VOffsetS: TVarStackOffset;
  PtrD: Pointer;
  ParamSize: TParamSize;
end;

POC_Invoke_PVar_Stk = ^TOC_Invoke_PVar_Stk;
TOC_Invoke_PVar_Stk = record
  VOffsetS: TVarStackOffset;
  POffsetS: TPointerOffset;
  ParamSize: TParamSize;
  StackInc: TStackInc;
end;

POC_Invoke_PVar_PStk = ^TOC_Invoke_PVar_PStk;
TOC_Invoke_PVar_PStk = record
  VOffsetS: TVarStackOffset;
  POffsetS: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_PVar_Var = ^TOC_Invoke_PVar_Var;
TOC_Invoke_PVar_Var = record
  VOffsetS, VOffsetD: TVarStackOffset;
  POffsetS: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_PVar_PVar = ^TOC_Invoke_PVar_PVar;
TOC_Invoke_PVar_PVar = record
  VOffsetS, VOffsetD: TVarStackOffset;
  POffsetS, POffsetD: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_PVar_Ptr = ^TOC_Invoke_PVar_Ptr;
TOC_Invoke_PVar_Ptr = record
  VOffsetS: TVarStackOffset;
  POffsetS: TPointerOffset;
  PtrD: Pointer;
  ParamSize: TParamSize;
end;

POC_Invoke_Ptr_Stk = ^TOC_Invoke_Ptr_Stk;
TOC_Invoke_Ptr_Stk = record
  PtrS: Pointer;
  ParamSize: TParamSize;
  StackInc: TStackInc;
end;

POC_Invoke_Ptr_PStk = ^TOC_Invoke_Ptr_PStk;
TOC_Invoke_Ptr_PStk = record
  PtrS: Pointer;
  ParamSize: TParamSize;
end;

POC_Invoke_Ptr_Var = ^TOC_Invoke_Ptr_Var;
TOC_Invoke_Ptr_Var = record
  PtrS: Pointer;
  VOffsetD: TVarStackOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Ptr_PVar = ^TOC_Invoke_Ptr_PVar;
TOC_Invoke_Ptr_PVar = record
  PtrS: Pointer;
  VOffsetD: TVarStackOffset;
  POffset: TPointerOffset;
  ParamSize: TParamSize;
end;

POC_Invoke_Ptr_Ptr = ^TOC_Invoke_Ptr_Ptr;
TOC_Invoke_Ptr_Ptr = record
  PtrS, PtrD: Pointer;
  ParamSize: TParamSize;
end;
