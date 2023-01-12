{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This is include directs the opcode to the correct function.
}
{%MainUnit lpinterpreter.pas}

case opCodeP(Code)^ of
  ocNone: Break;
  ocIsInternal: DoCheckInternal();
  ocGetExceptionMessage: DoGetExceptionMessage();
  ocGetExceptionLocation: DoGetExceptionLocation();
  ocGetCallerLocation: DoGetCallerLocation();
  ocGetScriptMethodName: DoGetScriptMethodName();
  ocDumpCallStack: DoDumpCallStack();
  ocInitStackLen: DoInitStackLen();
  ocInitStack: DoInitStack();
  ocGrowStack: DoGrowStack();
  ocExpandVar: DoExpandVar();
  ocExpandVarAndInit: DoExpandVarAndInit();
  ocGrowVar: DoGrowVar();
  ocGrowVarAndInit: DoGrowVarAndInit();
  ocPopVar: DoPopVar();
  ocPopStackToVar: DoPopStackToVar();
  ocPopVarToStack: DoPopVarToStack();
  ocJmpVar: DoJmpVar();
  ocJmpSafe: DoJmpSafe();
  ocJmpSafeR: DoJmpSafeR();

  ocIncTry: DoIncTry();
  ocDecTry: DoDecTry();
  ocEndTry: DoEndTry();
  ocCatchException: DoCatchException();
  ocReRaiseException: DoReRaiseException();

  ocDecCall: DoDecCall();
  ocDecCall_EndTry: DoDecCall_EndTry();

  {$I lpinterpreter_invokecase.inc}
  {$I lpinterpreter_jumpcase.inc}
  {$I lpinterpreter_evalcase.inc}

  else
    LapeException(lpeUnknownOC);
end;
