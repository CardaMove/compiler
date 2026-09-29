# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

The agent rules shared with other AI tools live in AGENTS.md and apply here as well (repository map, translation module purposes, parser/lexer workflow, pipeline rules, editing guidelines):

@AGENTS.md

User-facing usage notes (CLI invocation, Happy flags, debugger setup, tested aptos/aiken versions) are in [README.md](README.md).

## Commands

```shell
stack build                                   # also runs alex/happy on Lexer.x / Parser.y
stack test                                    # full Hspec suite (hspec-discover via test/Spec.hs)
stack test --ta '--match "alphaConvert"'      # run only tests whose description matches
stack exec cardamove-cli -- "test\Bet\Move.toml"   # transpile a Move project
```

- Toolchain: Stack resolver `lts-22.44` (GHC 9.6.x), `GHC2021`, `-Wall` plus `-Wmissing-export-lists` — every module needs an explicit export list.
- `make setup` installs `alex` and `happy` via Stack.
- Tests read fixtures with paths relative to the repo root (e.g. `test/Move/Translations/files/...`), so run them from the repo root.

## End-to-end flow

1. **Load** — [lib/Move/Loader/Loader.hs](lib/Move/Loader/Loader.hs): `loadToml` takes a `Move.toml`, lexes+parses every `.move` file under the sibling `scripts/` and `sources/` folders into `Root`s (`RModule` / `RScript`), and reads the `[addresses]` table into `AddrAssociations`.
2. **Transpile** — [lib/Move/Transpiler.hs](lib/Move/Transpiler.hs), in this order:
   `translateWhilesToFunctionsInRoot` → `annotateBindingsWithUUID` → `translateTParamsInRoot` → `markVariablesForLocalScope` + `addLocalScopeInRoot` → `translatePolymorphismInRoot` → `postProcessRoot`.
   The `State Int` UUID counter is threaded from one pass into the next; each step is wrapped in `tryIO "<step name>" $ evaluate ...` so failures report which pass crashed.
3. **Generate** — [lib/Aiken/AikenGenerator.hs](lib/Aiken/AikenGenerator.hs) (`generateRoot`) prints each Root as Aiken text; [lib/Aiken/ValidatorGenerator.hs](lib/Aiken/ValidatorGenerator.hs) collects the single entry function of each Move script (`ScriptMainMetadata`) and emits one project validator.
4. **Write** — [src/Main.hs](src/Main.hs) wipes and recreates `aiken_out/` (relative to the CWD), copies [resources/template_project/](resources/template_project/) into it, then writes modules to `aiken_out/lib/module_<address>/<Module>.ak`, scripts to `aiken_out/lib/scripts/<name>.ak`, and the validator to `aiken_out/validators/main.ak` (Aiken forbids the name `validator`).

The generated code relies on the Aiken helper libraries in [resources/template_project/lib/utils/](resources/template_project/lib/utils/) (scope/CPS, references, global storage, signer, coin, timestamp, etc.). `PostProcessing` injects the imports for them; changes to what translations emit often require matching changes there.

## Key internals

- [lib/Move/AST.hs](lib/Move/AST.hs) is the single AST shared by all stages. Besides the Move constructs it contains `IntermediateExpr` / `IntermediateTypeWitnessExpr` nodes that only translations produce (CPS/local-scope ops, upcasts/downcasts, type witnesses) and that only the Aiken generator consumes. AST types derive `Show`/`Read`/`Eq`.
- [lib/Move/Translations/TraversalUtils.hs](lib/Move/Translations/TraversalUtils.hs) (`traverseRootPostOrder` and `TraversalMapper`) is the standard way to write a new pass: supply an expression mapper and a bindings mapper that receive the current `[Scope]` and a user state.
- [lib/Move/Translations/Utils.hs](lib/Move/Translations/Utils.hs) holds `inferExprType`, scope helpers, `iterateWithOthers` (process each root with access to all the other roots, needed for cross-module lookups), well-known addresses/identifiers of the utils library, and `tryIO`.
- `AlphaConversion.hs` (`alphaConvert`) renames same-scope rebinding collisions. It is tested but **not yet wired into `transpiler`**; it is intended for targets that can't rebind variables (e.g. off-chain JS).
- `*.__hs` modules (`Mutation`, `Shadowing`) and their `*Spec.__hs` tests are parked and not compiled.

## Tests

- Specs mirror the library layout under [test/](test/); `hspec-discover` picks up any `*Spec.hs` exporting `spec`.
- Translation tests usually parse a `.move` fixture, run the pass, and compare against a `.txt` fixture containing the expected AST printed with `show` (loaded via `read`). To create or update an expected fixture, `show` the new result and review the diff carefully rather than accepting it blindly.
- Aiken generator tests compare against `.ak` fixtures in [test/Aiken/files/](test/Aiken/files/).
- [test/Bet/](test/Bet/) is a full sample Move project (modules + scripts) useful for manual end-to-end runs of the CLI.
