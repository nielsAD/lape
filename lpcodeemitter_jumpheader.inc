{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: Mozilla Public License (http://mozilla.org/MPL/2.0) 

  Jump headers for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function _Jmp(Target: TCodePos): Integer;
function _JmpR(Jmp: TCodeOffset): Integer;

function _JmpR_Var(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset): Integer;
function _JmpR_Ptr(Jmp: TCodeOffset; APtr: Pointer): Integer;
function _JmpR_PVar(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset): Integer;

//jump relative if
function _JmpRIf8_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIf16_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIf32_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIf64_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIf8_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIf16_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIf32_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIf64_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
function _JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
function _JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
function _JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
//jump relative if not
function _JmpRIfNot8_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot16_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot32_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot64_Stk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset): Integer;
function _JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer): Integer;
function _JmpRIfNot8_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot16_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot32_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot64_PStk(Jmp: TCodeOffset): Integer;
function _JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
function _JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
function _JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;
function _JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset): Integer;



