{
  Author: Niels A.D
  Project: Lape (https://github.com/nielsAD/lape)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Defines for the Lape project.
}

{.$DEFINE Lape_CaseSensitive}            //Case sentive in script
{.$DEFINE Lape_CDECL}                    //CDECL function headers for callbacks/evaluations (useful for plugins/ffi)
{$DEFINE Lape_EmitPos}                  //Emit source position when compiling (more memory)
{$DEFINE Lape_Inline}                   //Compile with function inlining where it could be benefitial (faster, harder debugging)
{$DEFINE Lape_PascalLabels}             //Label sections and declarations with colon, instead of label type and label() method
{.$DEFINE Lape_SmallCode}                //Try to keep compiled code as small as possible (packed records, no aligning -- less memory, slower)
{.$DEFINE Lape_TrackObjects}             //Track all created classes with a list to check for leaks
{.$DEFINE Lape_Unicode}                  //Use unicodestrings
{.$DEFINE Lape_NoExtended}               //No extended type
{$DEFINE Lape_UseFPCTrunk_FillChar}     //In interpreter, use FPC trunk FillChar/Move. Faster.

{.$DEFINE Lape_InterpreterDebug_StackResize}
{.$DEFINE Lape_InterpreterDebug_ScriptMethodInvokes}
{.$DEFINE Lape_InterpreterDebug_StackUsage}
{.$DEFINE Lape_InterpreterDebug_EvalUsage}

{$IFDEF FPC}
  {$MODE objfpc}{$H+}
  {$MODESWITCH advancedrecords}
{$ELSE}
  {$DEFINE Delphi}
  {$DEFINE Lape_Unicode} // Delphi: The default string type is UnicodeString.
  {$POINTERMATH ON}

  {$IFDEF WIN64}
    {$DEFINE CPUX86_64}
  {$ELSE}
    {$DEFINE CPU86}
  {$ENDIF}
{$ENDIF}

// https://bugs.freepascal.org/view.php?id=37305
{$IF (DEFINED(FPC) and ((FPC_FULLVERSION=30200) or (FPC_FULLVERSION=30201)))}
  {$OPTIMIZATION NOLOOPUNROLL}
{$ENDIF}
