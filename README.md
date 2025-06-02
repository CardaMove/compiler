# CardaMove

Simple POC for a transpiler from [Move](https://move-language.github.io/move/) to [Aiken](https://aiken-lang.org/).

## Notes

### Move function bodies

Function bodies in Move are a concatenation of "void expressions" (separated by ";") and a last expression which
has type of the function's return type.

Void expressions are:
* Let bindings 
* Normal expression with void type
