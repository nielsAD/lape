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

{$IFDEF FPC}
  {$MODE objfpc}{$H+}
{$ELSE}
  {$DEFINE Delphi}
  {$POINTERMATH ON}

  {$IFDEF WIN64}
    {$DEFINE CPUX86_64}
  {$ELSE}
    {$DEFINE CPU86}
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(UNICODE) and not DEFINED(Lape_Unicode)}
  {$DEFINE Lape_NeedAnsiStringsUnit}
{$IFEND}


// https://bugs.freepascal.org/view.php?id=37305
{$IF (DEFINED(FPC) and ((FPC_FULLVERSION=30200) or (FPC_FULLVERSION=30201)))}
  {$OPTIMIZATION NOLOOPUNROLL}
{$ENDIF}
