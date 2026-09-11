# CardaMove

Simple POC for a transpiler from [Move](https://move-language.github.io/move/) to [Aiken](https://aiken-lang.org/).

## Stack commands

```shell
$ stack build # Also builds Alex and Happy
$ stack test
$ stack run

```

## Running the compiler
From the repository root, invoke the CLI with `stack exec` and provide the path to the Move project's `Move.toml` file:

```shell
$ cd C:\path\to\compiler
$ stack exec cardamove-cli -- "test\Move\Loader\files\Move.toml"
```

Use `-h` or `--help` to display the available CLI options:

```shell
$ stack exec cardamove-cli -- --help
```

## Lexer commands
By default, Stack generates the lexer by itself on the build step. Alex can still be manually invoked with the following:

```shell
$ alex lib/Move/Lexer.x
```

## Parser commands
By default, Stack generates the parser by itself on the build step. Happy can still be manually invoked with the following:

```shell
$ happy lib/Move/Parser.y -i -p -a -d
```

Where:
- `-i` generates the `Parser.info` files
- `-p` generates the `Parser.grammar` file
- `-a` and `-d` are useful for debugging and will make Happy print the state transitions and shifts/reductions when invoked

The generated parser can also be loaded in **GHCI**:

```shell
$ ghci -ilib lib/Move/Parser.hs
```

## Haskell debugger
For VSCode, install [Haskell GHCi Debug Adapter Phoityne](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)

Run with configuration **haskell(stack)**

## Notes

### Move function bodies

Function bodies in Move are a concatenation of "void expressions" (separated by ";") and a last expression which
has type of the function's return type.

Void expressions are:
* Let bindings 
* Normal expression with void type
