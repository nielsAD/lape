Lape
====

[![tests](https://github.com/nielsAD/lape/actions/workflows/tests.yml/badge.svg)](https://github.com/nielsAD/lape/actions/workflows/tests.yml)

Lape is a scripting engine with a Pascal derived syntax for Free Pascal and Delphi. It's written with speed and a broad platform compatibility in mind. The syntax is backwards compatible with Pascal Script (to a certain degree).

Lape is:
- A scripting engine
- A Pascal derived language
    - Basetypes: `Integer`, `Float`, `Char`, `String`, `Boolean`, `Variant`, `Array`, `Record`, `Union`, `Enum`, `Set`, `Pointer`, `Function pointer`
    - Operations: `:=` `=` `<>` `>` `>=` `<` `<=` `@` `^` `+` `-` `*` `/` `**` `AND` `OR` `DIV` `XOR` `NOT` `IN` `SHL` `SHR` `MOD`
    - Constructs: `If` `For` `Case` `Repeat` `While` `Try` `Label`
    - Optional C style operators: `+=` `/=` `*=` `-=`
    - Internal and external functions (with support for default params & type methods)
    - Internal/external variables and constants (every variable is represented exactly as how it would be by FPC/Delphi)
    - Function directives: `override`, `overload`, `static`, `deprecated`, `experimental`, `unimplemented`
    - Support for local (nested) declarations. So function can have their own constants/types/variables/functions.
    - Operator overloading
    - Multiline strings using double quotes `"`
    - Optional array "helpers" such as `array.SetLength(5)`, `array.Pop()` and many more
- Portable
    - Fully written in FPC/Delphi. No need for external libraries.
    - Tested with Linux(x64/aarch64), Windows(x86/x64) and Mac OS(x64/aarch64), but supports virtually every platform FPC supports.
- Extensible
    - New types can easily be added with specified behaviour for operations.
    - Optional foreign function interface which allows native calling of Lape functions and importing of functions without creating wrappers.
- Fast

---

### Simple example of running a script:

- Remember to add unit & include search paths to the lape directory

```pascal
uses
  Classes, Sysutils, lpparser, lpcompiler, lpinterpreter;   

const
  MyScript = 'function DiceRoll: Int32;'           + LineEnding +
             'begin'                               + LineEnding +
             '  Result := Random(1, 6);'           + LineEnding +
             'end;'                                + LineEnding +
             ''                                    + LineEnding +
             'begin'                               + LineEnding +
             '  WriteLn("Rolled a ", DiceRoll());' + LineEnding +
             'end.';

var
  Compiler: TLapeCompiler;
begin
  Randomize();

  Compiler := TLapeCompiler.Create(TLapeTokenizerString.Create(MyScript));
  try
    if Compiler.Compile() then
      RunCode(Compiler.Emitter);
  except
    on E: Exception do
      WriteLn(E.ToString());
  end;
  Compiler.Free();

  ReadLn;
end.
```
