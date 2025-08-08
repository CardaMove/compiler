# CardaMove

Simple POC for a transpiler from [Move](https://move-language.github.io/move/) to [Aiken](https://aiken-lang.org/).

## Stack commands

```shell
$ stack build # Also builds Alex and Happy
$ stack test
$ stack run

```

To manually run Happy:
`ghci -ilib lib/Move/Parser.hs`

## Notes

### Move function bodies

Function bodies in Move are a concatenation of "void expressions" (separated by ";") and a last expression which
has type of the function's return type.

Void expressions are:
* Let bindings 
* Normal expression with void type
