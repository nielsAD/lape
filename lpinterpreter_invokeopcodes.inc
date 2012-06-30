{
  Author: Niels A.D
  Project: Lape (http://code.google.com/p/la-pe/)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  These are the invoke related opcodes for the bytecode.
}
{%MainUnit lpinterpreter.pas}

ocIncCall_Var,                                             //IncCall_Var TVarStackOffset TParamSize
ocIncCall_PVar,                                            //IncCall_PVar TVarStackOffset TPointerOffset TParamSize
ocIncCall_Ptr,                                             //IncCall_Ptr Pointer TParamSize

ocInvokeImported_Var,                                      //InvokeImported_Var TVarStackOffset TParamSize
ocInvokeImported_PVar,                                     //InvokeImported_PVar TVarStackOffset TPointerOffset TParamSize
ocInvokeImported_Ptr,                                      //InvokeImported_Ptr Pointer TParamSize

ocInvokeImported_Var_Stk,                                  //InvokeImported_Var_Stk TVarStackOffset TParamSize TStackInc
ocInvokeImported_Var_PStk,                                 //InvokeImported_Var_PStk TVarStackOffset TParamSize
ocInvokeImported_Var_Var,                                  //InvokeImported_Var_Var TVarStackOffset TVarStackOffset TParamSize
ocInvokeImported_Var_PVar,                                 //InvokeImported_Var_PVar TVarStackOffset TVarStackOffset TPointerOffset TParamSize
ocInvokeImported_Var_Ptr,                                  //InvokeImported_Var_Ptr TVarStackOffset Pointer TParamSize

ocInvokeImported_PVar_Stk,                                 //InvokeImported_PVar_Stk TVarStackOffset TPointerOffset TParamSize TStackInc
ocInvokeImported_PVar_PStk,                                //InvokeImported_PVar_PStk TVarStackOffset TPointerOffset TParamSize
ocInvokeImported_PVar_Var,                                 //InvokeImported_PVar_Var TVarStackOffset TVarStackOffset TPointerOffset TParamSize
ocInvokeImported_PVar_PVar,                                //InvokeImported_PVar_Var TVarStackOffset TVarStackOffset TPointerOffset TPointerOffset TParamSize
ocInvokeImported_PVar_Ptr,                                 //InvokeImported_PVar_Ptr TVarStackOffset Pointer TParamSize

ocInvokeImported_Ptr_Stk,                                  //InvokeImported_Ptr_Stk Pointer TParamSize TStackInc
ocInvokeImported_Ptr_PStk,                                 //InvokeImported_Ptr_PStk Pointer TParamSize
ocInvokeImported_Ptr_Var,                                  //InvokeImported_Ptr_Var Pointer TVarStackOffset TParamSize
ocInvokeImported_Ptr_PVar,                                 //InvokeImported_Ptr_PVar Pointer TVarStackOffset TPointerOffset TParamSize
ocInvokeImported_Ptr_Ptr,                                  //InvokeImported_Ptr_Var Pointer Pointer TParamSize
