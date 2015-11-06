Lape
====

Lape is a scripting engine with a Pascal derived syntax for Free Pascal and Delphi. It's written with speed and a broad platform compatibility in mind. The syntax is backwards compatible with Pascal Script (to a certain degree).

Lape is:
- A scripting engine
- A Pascal derived language
    - Basetypes: `Integer`, `Float`, `Char`, `String`, `Boolean`, `Variant`, `Array`, `Record`, `Union`, `Enum`, `Set`, `Pointer`, `Function pointer`
    - Operations: `:=` `=` `<>` `>` `>=` `<` `<=` `@` `^` `+` `-` `*` `/` `**` `AND` `OR` `DIV` `XOR` `NOT` `IN` `SHL` `SHR`
    - Constructs: `If`, `For`, `Case`, `Repeat`, `While`, `Try`, `Label`
    - Internal and external (overloaded) functions (with support for default params)
    - Internal/external variables and constants (every variable is represented exactly as how it would be by FPC/Delphi)
    - Support for local (nested) declarations. So function can have their own constants/types/variables/functions.
- Portable
    - Fully written in FPC/Delphi. No need for external libraries.
    - Tested with Linux(x86/x64), Windows(x86/x64) and Mac OS, but supports virtually every platform FPC supports.
- Extensible
    - New types can easily be added with specified behaviour for operations.
    - Optional foreign function interface which allows native calling of Lape functions and importing of functions without creating wrappers.
- Fast
