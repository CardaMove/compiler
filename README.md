# CardaMove

Simple POC for a transpiler from [Move](https://move-language.github.io/move/) to [Aiken](https://aiken-lang.org/).

Off-chain code generation to TypeScript + [MeshJS](https://meshjs.dev) is in progress (currently a skeleton: imports, structs and function signatures). See [doc/offchain.md](doc/offchain.md).

## Stack commands

```shell
$ stack build # Also builds Alex and Happy
$ stack test

```

## Running the compiler
From the repository root, invoke the CLI with `stack exec` and provide the path to the Move project's `Move.toml` file:

```shell
$ cd C:\path\to\compiler
$ stack exec cardamove-cli -- "test\Move\Loader\files\Move.toml"
```

By default the compiler writes two projects in the current directory:

- `aiken_out/`: the on-chain Aiken project (template from `resources/aiken_template/`);
- `ts_out/`: the off-chain TypeScript project (template from `resources/typescript_template/`).

Use `--onchain-only` or `--offchain-only` to generate only one of them. Use `-h` or `--help` to display the available CLI options:

```shell
$ stack exec cardamove-cli -- --help
```

## Off-chain TypeScript project
Requires Node.js and npm. The generated project and its template share the same commands:

```shell
$ cd ts_out                # or resources/typescript_template
$ npm install
$ npm run typecheck        # type-check the runtime library, its tests and the generated code
$ npm test                 # runtime library tests, including the Aiken parity fixtures
```

## Runtime libraries
The Aiken runtime library (`resources/aiken_template/lib/utils/`) and its TypeScript port (`resources/typescript_template/src/lib/`) are vendored from the [rosetta-redo](https://github.com/roveroniandrea/rosetta-redo) repository, which is their source of truth. Do not edit them here: change them in rosetta-redo, then re-vendor them:

```shell
$ make sync-libs           # copies from ../rosetta-redo (override with ROSETTA_REDO) and updates resources/runtime-libs.lock.json
$ make check-libs          # fails if the vendored files differ from the lock or from rosetta-redo
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


### CLI versions
As of this commit, the project compioles with the following CLI versions:
```shell
$ aptos -V
aptos 9.6.0

$ aptos move -V
aptos-move 9.6.0

$ aiken --version
aiken v1.1.21+42babe5
```