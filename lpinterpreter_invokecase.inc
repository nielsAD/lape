{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This include directs the invoke opcodes to the related functions.
}
{%MainUnit lpinterpreter.pas}

ocIncCall_Var: DoIncCall_Var();
ocIncCall_PVar: DoIncCall_PVar();
ocIncCall_Ptr: DoIncCall_Ptr();

ocInvokeImported_Var: DoInvokeImported_Var();
ocInvokeImported_PVar: DoInvokeImported_PVar();
ocInvokeImported_Ptr: DoInvokeImported_Ptr();

ocInvokeImported_Var_Stk: DoInvokeImported_Var_Stk();
ocInvokeImported_Var_PStk: DoInvokeImported_Var_PStk();
ocInvokeImported_Var_Var: DoInvokeImported_Var_Var();
ocInvokeImported_Var_PVar: DoInvokeImported_Var_PVar();
ocInvokeImported_Var_Ptr: DoInvokeImported_Var_Ptr();

ocInvokeImported_PVar_Stk: DoInvokeImported_PVar_Stk();
ocInvokeImported_PVar_PStk: DoInvokeImported_PVar_PStk();
ocInvokeImported_PVar_Var: DoInvokeImported_PVar_Var();
ocInvokeImported_PVar_PVar: DoInvokeImported_PVar_PVar();
ocInvokeImported_PVar_Ptr: DoInvokeImported_PVar_Ptr();

ocInvokeImported_Ptr_Stk: DoInvokeImported_Ptr_Stk();
ocInvokeImported_Ptr_PStk: DoInvokeImported_Ptr_PStk();
ocInvokeImported_Ptr_Var: DoInvokeImported_Ptr_Var();
ocInvokeImported_Ptr_PVar: DoInvokeImported_Ptr_PVar();
ocInvokeImported_Ptr_Ptr: DoInvokeImported_Ptr_Ptr();
