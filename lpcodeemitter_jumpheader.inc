{
	Author: Niels A.D
	Project: Lape (http://code.google.com/p/la-pe/)
	License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

	Jump headers for emitter class.
}
{%MainUnit lpcodeemitter.pas}

function _Jmp(Target: TCodePos; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _Jmp(Target: TCodePos; Pos: PDocPos = nil): Integer; overload;
function _JmpR(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpR(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;

function _JmpR_Var(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; var Offset: Integer): Integer;
function _JmpR_Ptr(Jmp: TCodeOffset; APtr: Pointer; var Offset: Integer): Integer;
function _JmpR_PVar(Jmp: TCodeOffset; AVarStackOffset: TVarStackOffset; APointerOffset: TPointerOffset; var Offset: Integer): Integer;

//jump relative if
function _JmpRIf8_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
//jump relative if not
function _JmpRIfNot8_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Stk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PStk(Jmp: TCodeOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; var Offset: Integer; Pos: PDocPos = nil): Integer; overload;

function _JmpRIf8_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIf64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;

function _JmpRIfNot8_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Stk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Var(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_Ptr(Jmp: TCodeOffset; Ptr: Pointer; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PStk(Jmp: TCodeOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot8_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot16_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot32_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;
function _JmpRIfNot64_PVar(Jmp: TCodeOffset; VarStackOffset: TVarStackOffset; PointerOffset: TPointerOffset; Pos: PDocPos = nil): Integer; overload;

