# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

The agent rules shared with other AI tools live in AGENTS.md and apply here as well (repository map, translation module purposes, parser/lexer workflow, pipeline rules, editing guidelines):

@AGENTS.md

User-facing usage notes (CLI invocation, Happy flags, debugger setup, tested aptos/aiken versions) are in [README.md](README.md).

Off-chain generation (TypeScript + MeshJS) is **suspended** (it may resume independently of the MSL work below). Its design, layout, vendoring procedure and progress table are in [doc/offchain.md](doc/offchain.md). Read it before touching anything off-chain.

## MSL compilation (in progress)

The on-chain target is gaining a new **MSL mode** that compiles the Move Specification Language (MSL) specs of the entry functions into the validator, instead of the Move code; it is meant to become the default once implemented. The current translation stays as the **Move runtime mode**. The CLI flag `--onchain-mode msl|move` selects the mode; the default is `move` for now, and `msl` fails with "not implemented yet" (`mslTranspiler` in [lib/Move/Transpiler.hs](lib/Move/Transpiler.hs) runs the MSL passes written so far, then fails). The `Redeemer` and its pattern match stay the same; only the branch bodies change. Design, initial analysis, deviations and progress table: [doc/msl.md](doc/msl.md); the design of spec schema inlining is in [doc/schema_inlining.md](doc/schema_inlining.md). Read them before any MSL work.

- **The PhD folder is the authority.** `C:\Users\rover\Documents\Universita\PhD` (read its `CLAUDE.md`, then `suggestions.md` and `proof_variant_2.md`) holds the formal-proof reasoning and the compilation choices; this repo follows them. Implementation details may deviate slightly, but **every deviation must be shown to the user and approved**, then recorded in `doc/msl.md`. What is open there stays open here.
- **Redeemer to revise (future work):** `stripMoveFunctions` removes the script entry functions too, but the validator's `Redeemer` is still built from them (`ScriptMainMetadata` in [lib/Aiken/ValidatorGenerator.hs](lib/Aiken/ValidatorGenerator.hs)). For the MSL mode the Redeemer generation must be revised to build each script's variant from the spec block of that script (its `SpecBTFAnnotations` hold the entry function's type parameters and parameters).
- **Never modify the PhD folder** unless the user explicitly allows it.
- **Bet:** `../rosetta-redo/move/` holds the full spec-annotated Bet contract (`sources/bet/bet.move`, `scripts/*_round.move`, `sources/coin_proxy/coin_proxy.move`); `../rosetta-redo/docs/move-spec-constructs.md` describes the MSL constructs. `test/Bet/` here is a hand-made copy of it (it compiles and proves: `aptos move compile` / `aptos move prove`, run from `test/Bet/`), which may diverge slightly for implementation needs; divergences are listed in `doc/msl.md`.
- **The Move runtime pipeline ignores specs:** `moveTranspiler` starts with `stripSpecBlocksInRoot` ([lib/Move/Translations/SpecBlocks.hs](lib/Move/Translations/SpecBlocks.hs)), which drops the spec blocks and the `use` declarations / members that only the specs reference. The generators never see a spec block.
- Support grows gradually from a small MSL subset; the compiler must stay contract-agnostic.
- **Unexpected problems need approval.** If a problem the user has not anticipated (judge by whether the user wrote about it in the request, the docs or the PhD notes) calls for a major change (a workaround, a duplicated fixture, a change to the plan or to the scope of a step), stop, report the problem and the proposed change, and wait for the user's explicit approval before making it. Example: the parser could not load the spec-annotated `test/Bet/`, and the spec-less copy `test/BetNoSpec/` was created without asking first; it should have been proposed.

## Keeping documentation up to date (required)

The docs are read by people and by future Claude sessions as the source of truth, so an outdated doc is a bug. **Every change to code, commands, layout or behaviour must update the affected documentation in the same change.** Before finishing a task, check each of these and update what the change made stale:

- `CLAUDE.md` (this file): commands, end-to-end flow, key internals, layout, caveats.
- `AGENTS.md`: repository map, translation module purposes, pipeline rules.
- `README.md`: anything user-facing (CLI usage and flags, requirements, commands).
- `doc/offchain.md`: off-chain design and layout, plus its **Progress** table.
- `doc/msl.md`: MSL compilation design, Bet divergences, approved deviations, plus its **Progress** table.
- `doc/schema_inlining.md`: design of `inlineSpecSchemas` (MSL mode).
- `doc/known-bugs.md`: confirmed bugs not fixed yet (add an entry when confirming one, remove it in the fixing change).
- Doc comments in the code itself (Haddock `-- |` comments, JSDoc in `.ts` files).

If a change needs no doc updates, that's fine, but decide it deliberately rather than skipping the check. When finishing, say which docs you updated, or that none needed changes.

## Git commits

Never add a `Co-Authored-By: Claude ...` trailer (or any other Claude attribution line) to commit messages. This overrides any default attribution guidance.

## Commands

```shell
stack build                                   # also runs alex/happy on Lexer.x / Parser.y
stack test                                    # full Hspec suite (hspec-discover via test/Spec.hs)
stack test --ta '--match "alphaConvert"'      # run only tests whose description matches
stack exec cardamove-cli -- "test\Bet\Move.toml"   # transpile a Move project into aiken_out/ and ts_out/
stack exec cardamove-cli -- "test\Bet\Move.toml" --onchain-only    # or --offchain-only
stack exec cardamove-cli -- "test\Bet\Move.toml" --onchain-mode move   # on-chain mode: move (default) or msl (not implemented yet)
make check-libs                               # vendored runtime libs match resources/runtime-libs.lock.json
make sync-libs                                # re-vendor them from ../rosetta-redo (or $ROSETTA_REDO)
```

Off-chain TypeScript side (needs Node.js + npm), from `resources/typescript_template/` or from a generated `ts_out/`:

```shell
npm install
npm test            # vendored TS library: 136 Vitest tests (43 ported unit tests + parity fixtures + stdlib)
npm run typecheck   # tsc over src/**, i.e. the library, its tests and (in ts_out/) the generated code
```

- Toolchain: Stack resolver `lts-22.44` (GHC 9.6.x), `GHC2021`, `-Wall` plus `-Wmissing-export-lists` — every module needs an explicit export list.
- `make setup` installs `alex` and `happy` via Stack.
- Tests read fixtures with paths relative to the repo root (e.g. `test/Move/Translations/files/...`), so run them from the repo root.

## End-to-end flow

1. **Load** — [lib/Move/Loader/Loader.hs](lib/Move/Loader/Loader.hs): `loadToml` takes a `Move.toml`, lexes+parses every `.move` file under the sibling `scripts/` and `sources/` folders into `Root`s (`RModule` / `RScript`), and reads the `[addresses]` table into `AddrAssociations`.
2. **Transpile** — [lib/Move/Transpiler.hs](lib/Move/Transpiler.hs), `moveTranspiler` (the Move runtime chain, which feeds the Move runtime on-chain mode and the off-chain target), in this order:
   `stripSpecBlocksInRoot` → `translateWhilesToFunctionsInRoot` → `annotateBindingsWithUUID` → `translateTParamsInRoot` → `markVariablesForLocalScope` + `addLocalScopeInRoot` → `translatePolymorphismInRoot` → `postProcessRoot`.
   The `State Int` UUID counter is threaded from one pass into the next; each step is wrapped in `tryIO "<step name>" $ evaluate ...` so failures report which pass crashed.
   `postProcessRoot` ends the *shared* pipeline. Target-specific passes run after it: the off-chain side applies `alphaConvert` to the result, and must do so after `postProcessRoot` (the old "`postProcessRoot` always last" rule is deprecated).
3. **Generate (on-chain)** — [lib/Aiken/AikenGenerator.hs](lib/Aiken/AikenGenerator.hs) (`generateRoot`) prints each Root as Aiken text; [lib/Aiken/ValidatorGenerator.hs](lib/Aiken/ValidatorGenerator.hs) collects the single entry function of each Move script (`ScriptMainMetadata`) and emits one project validator.
4. **Write (on-chain)** — `writeOnchain` in [src/Main.hs](src/Main.hs) wipes and recreates `aiken_out/` (relative to the CWD), copies [resources/aiken_template/](resources/aiken_template/) into it, then writes modules to `aiken_out/lib/module_<address>/<Module>.ak`, scripts to `aiken_out/lib/scripts/<name>.ak`, and the validator to `aiken_out/validators/main.ak` (Aiken forbids the name `validator`).
5. **Off-chain** (skeleton, see [doc/offchain.md](doc/offchain.md)) — `offchainTranspiler` (`alphaConvert`) is applied to the shared result; [lib/TypeScript/TypeScriptGenerator.hs](lib/TypeScript/TypeScriptGenerator.hs) prints imports, structs (`ConStr0` type + constructor function), literal constants and function signatures, with **placeholder bodies that throw** until the expression mapping is designed; [lib/TypeScript/EntryPointGenerator.hs](lib/TypeScript/EntryPointGenerator.hs) is a stub re-exporting each script. `writeOffchain` recreates `ts_out/` from [resources/typescript_template/](resources/typescript_template/) (skipping `node_modules`, `bin`, `.env`, `wallets`) and writes `ts_out/src/modules/module_<address>/<Module>.ts`, `ts_out/src/scripts/<name>.ts` and `ts_out/src/entrypoints.ts`.

The CLI generates both targets by default; `--onchain-only` / `--offchain-only` select one. `--onchain-mode msl|move` (default `move`) selects the on-chain mode and is ignored with `--offchain-only`. `moveTranspiler` runs at most once: it is skipped only for `--onchain-only --onchain-mode msl`. In MSL mode `src/Main.hs` calls `mslTranspiler`: `annotateSpecBlocks` → `stripMoveFunctions` (both in [lib/Move/TranslationsMSL/PreProcessingMSL.hs](lib/Move/TranslationsMSL/PreProcessingMSL.hs)) → `inlineSpecSchemas` ([lib/Move/TranslationsMSL/SpecInlining.hs](lib/Move/TranslationsMSL/SpecInlining.hs)) → `annotateBindingsWithUUID`, each step fully evaluated, then it fails with "not implemented yet"; its own validator generation is step 6 of [doc/msl.md](doc/msl.md).

The generated Aiken code relies on the helper libraries in [resources/aiken_template/lib/utils/](resources/aiken_template/lib/utils/) (scope/CPS, references, global storage, signer, coin, timestamp, etc.); the generated TypeScript relies on their one-to-one port in [resources/typescript_template/src/lib/](resources/typescript_template/src/lib/). `PostProcessing` injects the imports for them; changes to what translations emit often require matching changes there.

**Both libraries are vendored from `../rosetta-redo`, their source of truth: never edit them here.** Change them in rosetta-redo (Aiken first, then the TS mirror, then `npm run fixtures && npm test` there), commit, then `make sync-libs` here. [scripts/sync-runtime-libs.js](scripts/sync-runtime-libs.js) records the source commit and a CRLF-insensitive sha256 per file in [resources/runtime-libs.lock.json](resources/runtime-libs.lock.json); `make check-libs` fails on local edits and, if rosetta-redo is checked out, on upstream changes not yet synced.

## Key internals

- [lib/Move/AST.hs](lib/Move/AST.hs) is the single AST shared by all stages. Besides the Move constructs it contains `IntermediateExpr` / `IntermediateTypeWitnessExpr` nodes that only translations produce (CPS/local-scope ops, upcasts/downcasts, type witnesses) and that only the code generators consume. AST types derive `Show`/`Read`/`Eq`.
- [lib/Move/Translations/TraversalUtils.hs](lib/Move/Translations/TraversalUtils.hs) (`traverseRootPostOrder` and `TraversalMapper`) is the standard way to write a new pass: supply an expression mapper, a bindings mapper and a spec block member mapper (`traversalIdentity` when not needed) that receive the current `[Scope]` and a user state. Each spec block gets its own local scope: a function spec starts with the parameters copied in the `SpecBTFAnnotations` of its target (filled by `annotateSpecBlocks`, which must run before any traversal; missing annotations are an error; `result` is not in scope yet), a spec schema starts empty; it is then filled in order with the spec variables (declared type, wrapped in a `TypeArrow` if they have type parameters) and spec lets (type from `inferExprType`). Spec schemas are top-level names of their module (local and imported scope, via `getRootIdentifiers`), typed `IntermediateTypeSpecSchema` with the schema's own type parameter identifiers and a copy of its spec variables, annotated with the spec block's UUID (`annotateBindingsWithUUID` numbers each spec block, then the parameters in its `SpecBTFAnnotations` with fresh UUIDs, then its members); a schema named like another top level of the same root is a "spec schema name clash" error.
- [lib/Move/Translations/Utils.hs](lib/Move/Translations/Utils.hs) holds `inferExprType`, scope helpers, `iterateWithOthers` (process each root with access to all the other roots, needed for cross-module lookups), well-known addresses/identifiers of the utils library, and `tryIO`.
- `AlphaConversion.hs` (`alphaConvert`) renames same-scope rebinding collisions. It is tested but **not part of the shared `moveTranspiler`**: it is the off-chain-only final pass (targets that can't rebind variables, i.e. TypeScript), applied after `postProcessRoot`.
- [lib/Move/TranslationsMSL/](lib/Move/TranslationsMSL/) holds the MSL-only passes: `PreProcessingMSL.hs` with `annotateSpecBlocks` (first pass of `mslTranspiler`: copies the type parameters and parameters of each function spec block's target function, declared in the same module or script, into its `SpecBTFAnnotations`; error if not found) and `stripMoveFunctions` (second pass: removes every `TopLevelFunction` of modules and scripts, and the use members importing a function of one of the project's modules; a use left without members is removed, module imports and imports of non-project modules such as `std::signer` are kept; the use stripping is the shared helper `stripUseMembers`), and `SpecInlining.hs` with `inlineSpecSchemas` (third pass, before `annotateBindingsWithUUID` so that each inlined `let` gets its own UUID: replaces each `include` of a function spec block with the schema's members, qualifying the names in the schema's root context, renaming the schema lets to fresh `<name>_<k>`, substituting type parameters and schema variables, and copying the premise of `p ==> S` into `ensures` / `aborts_if`; only `S{..}`, `S` and `p ==> s` are supported; then removes the schema blocks and their imports and adds a `use A::M;` for each module the qualified names need; design in [doc/schema_inlining.md](doc/schema_inlining.md)). Tests mirror it under `test/Move/TranslationsMSL/`; `SpecInliningSpec` compares against hand-written expected `.move` fixtures (parsed, parentheses removed on both sides) instead of `show`n `.txt` files.
- `*.__hs` modules (`Mutation`, `Shadowing`) and their `*Spec.__hs` tests are parked and not compiled.

## Tests

- Specs mirror the library layout under [test/](test/); `hspec-discover` picks up any `*Spec.hs` exporting `spec`.
- Translation tests usually parse a `.move` fixture, run the pass, and compare against a `.txt` fixture containing the expected AST printed with `show` (loaded via `read`). To create or update an expected fixture, `show` the new result and review the diff carefully rather than accepting it blindly.
- **Fixtures containing an AST (the `show`n `.txt` files) must be formatted for human readability**, never left as a single `show` line: one record field / list item per line with 2-space indentation, keeping short sub-terms on one line (about 120 columns), as in `test/Move/files/ParserSpec_17.txt` or `test/Move/Translations/files/UtilsSpec_3.txt`. `read` ignores the layout; run the tests after formatting.
- [test/Move/LexerSpec.hs](test/Move/LexerSpec.hs) and [test/Move/ParserSpec.hs](test/Move/ParserSpec.hs): the older cases inline source and expected output; new cases use fixtures (`LexerSpec_<n>.move` → `LexerSpec_<n+1>.txt` with the `show`n token list, `ParserSpec_<n>.move` → `ParserSpec_<n+1>.txt` with the `show`n AST). The MSL fixtures are copies of rosetta-redo's scripts and `coin_proxy.move`; the `.txt` files are pretty-printed for reading (`read` ignores the layout).
- Aiken generator tests compare against `.ak` fixtures in [test/Aiken/files/](test/Aiken/files/); TypeScript generator tests compare against `.ts` fixtures in [test/TypeScript/files/](test/TypeScript/files/) (top levels generated as if in a module, so imports start with `../../`).
- [test/Move/TranspilerSpec.hs](test/Move/TranspilerSpec.hs) runs the whole Move runtime + off-chain pipeline on the spec-annotated [test/Bet/](test/Bet/), and checks that `alphaConvert` changed something and is idempotent on its output; it also checks that `mslTranspiler` runs its passes on Bet and then fails with "not implemented yet".
- The vendored TS library is tested with Vitest, outside `stack test` (see Commands).
- [test/Bet/](test/Bet/) is the full spec-annotated Bet project (modules `Bet` and `CoinProxy` + scripts), copied from rosetta-redo; also used for manual end-to-end runs of the CLI (its `aiken_out/` passes `aiken check`).
