# AGENTS.md

This file gives AI coding agents project-specific instructions for working in this repository.

## Project Purpose

- CardaMove is a Haskell proof-of-concept transpiler from Move to Aiken (on-chain) and, work in progress, to TypeScript + MeshJS (off-chain).
- The core flow is: parse Move source -> apply AST translations -> generate Aiken output (and, off-chain, apply `alphaConvert` -> generate TypeScript output).
- The off-chain design, layout and progress live in `doc/offchain.md`. Off-chain work is currently suspended.
- In progress: a new default on-chain mode that compiles MSL (Move Specification Language) specs into the validator; the current code translation stays as the "Move runtime" mode behind a flag. Design and progress live in `doc/msl.md`. The semantics and compilation choices are decided in the PhD folder (`C:\Users\rover\Documents\Universita\PhD`), which is the authority; deviations need the user's approval.

## Repository Map

- `lib/Move/`: Move language frontend and transformation pipeline.
- `lib/Move/Lexer.x`: Alex lexer source.
- `lib/Move/Parser.y`: Happy parser grammar source.
- `lib/Move/Lexer.hs` and `lib/Move/Parser.hs`: generated files.
- `lib/Move/Transpiler.hs`: ordered translation pipeline: the shared `transpiler` and the off-chain-only tail `offchainTranspiler` (`alphaConvert`).
- `lib/Move/Translations/`: AST-to-AST transformation passes and shared traversal/type helpers.
- `lib/Aiken/`: on-chain code generation (`AikenGenerator.hs`, `ValidatorGenerator.hs`).
- `lib/TypeScript/`: off-chain code generation (`TypeScriptGenerator.hs`, `EntryPointGenerator.hs`), currently a skeleton (see `doc/offchain.md`).
- `resources/aiken_template/`: Aiken project template copied into `aiken_out/`; `lib/utils/` is the vendored Aiken runtime library.
- `resources/typescript_template/`: npm project template copied into `ts_out/`; `src/lib/` is the vendored TypeScript port of the runtime library (with its Vitest tests and parity fixtures).
- `resources/runtime-libs.lock.json` and `scripts/sync-runtime-libs.js`: provenance and sync/drift check of the vendored libraries (source of truth: the rosetta-redo repository).
- `src/Main.hs`: CLI entrypoint (`--onchain-only` / `--offchain-only`, default both).
- `test/`: Hspec test suite and fixture files.
- `doc/offchain.md`: off-chain design, layout and progress.
- `doc/msl.md`: MSL compilation design, Bet divergences from rosetta-redo, approved deviations from the PhD notes, and progress.

## Translation Modules Purpose

Inside `lib/Move/Translations/`, the active Haskell modules have the following responsibilities:

- `AlphaConversion.hs`: renames colliding local bindings within the same scope (appending `_i` with a traversal-wide counter) and updates their references; bindings shadowed in nested scopes and local-scope entries are left untouched. Not part of the shared `transpiler` pipeline: it is the off-chain-only final pass (TypeScript cannot rebind variables in the same scope) and must run after `postProcessRoot`.
- `Loops.hs`: rewrites `loop` into `while(true)` and lifts while loops into recursive helper functions, including break/continue flag handling and free-variable reference threading.
- `Polymorphism.hs`: removes parametric polymorphism for code generation by inserting upcasts/downcasts and replacing type parameters with an intermediate `Data`-like representation.
- `PostProcessing.hs`: final AST cleanup before generation; injects required utils imports and resolves named addresses to numerical ones (except the utils address).
- `Scopes.hs`: rewrites mutation/reference semantics into explicit CPS/local-scope operations, including assignment lowering, borrow/deref rewrites, and local-state insertion.
- `TraversalUtils.hs`: provides post-order traversals with scope threading for expressions, bindings, sequences, and full roots; used by translation passes to apply scoped rewrites.
- `TypeWitness.hs`: rewrites generic type parameters into explicit runtime type-witness parameters/arguments and normalizes type-parameter identifiers for Aiken compatibility.
- `Utils.hs`: shared translation utilities for scopes, UUID annotation, type inference, helper iterators across roots, and common primitive/intermediate types.

Notes:

- Files ending with `.__hs` in this folder are inactive/parked and are not part of the normal build.

## Build And Test

- Primary commands:
  - `stack build`
  - `stack test`
- Running the compiler: the CLI requires the path to a Move project's `Move.toml`, so use `stack exec` (as in `README.md`) instead of `stack run`:
  - `stack exec cardamove-cli -- "path\to\Move.toml"`
  - `stack exec cardamove-cli -- --help`
- VS Code tasks available:
  - `haskell build`
  - `haskell clean & build`
  - `haskell test`
  - `haskell watch`
- Off-chain (needs Node.js + npm), from `resources/typescript_template/` or a generated `ts_out/`: `npm install`, `npm test`, `npm run typecheck`.
- Vendored runtime libraries: `make check-libs` (drift check), `make sync-libs` (re-vendor from `../rosetta-redo` or `$ROSETTA_REDO`).
- CI expectation: repository should pass both `stack build` and `stack test`.

## Parser/Lexer Workflow

- Treat `Parser.y` and `Lexer.x` as source of truth.
- Do not hand-edit generated parser/lexer outputs unless explicitly requested.
- If grammar or tokenization changes are needed:
  - Update `lib/Move/Token.hs` consistently with lexer/parser tokens.
  - Update `lib/Move/Lexer.x` and/or `lib/Move/Parser.y`.
  - Regenerate via `stack build` (preferred), or manually:
    - `alex lib/Move/Lexer.x`
    - `happy lib/Move/Parser.y -i -p -a -d`
  - Run tests after regeneration.

## Transpilation Pipeline Rules

- Preserve semantic ordering in `lib/Move/Transpiler.hs` unless a task explicitly requires reordering.
- The UUID/state threading across translation passes is intentional; avoid resetting or skipping state values.
- ~~Keep post-processing (`postProcessRoot`) at the end of the pipeline.~~ **Deprecated.** `postProcessRoot` ends the *shared* pipeline (`transpiler`); target-specific passes run after it. In particular, the off-chain `alphaConvert` must run after `postProcessRoot` for transpilation to be correct.

## Test Conventions

- Framework: Hspec.
- Many tests compare exact AST structures or golden-style fixture outputs.
- Fixtures live under `test/**/files/`; preserve naming conventions like `*Spec_N.move` and `*Spec_N.txt`.
- When changing parser, lexer, or translations, add/update focused tests in matching spec modules.

## Editing Guidelines For Agents

- Prefer minimal, localized patches over broad refactors.
- Keep module exports explicit and consistent with current style.
- Follow existing formatting conventions in touched files.
- Avoid introducing new dependencies unless required by the task.
- Do not alter unrelated files in the same change.
- Always add a brief 2-3 line description comment to each function created by the agent.
- Keep documentation in sync: every change to code, commands, layout or behaviour must update the affected docs in the same change (`CLAUDE.md`, `AGENTS.md`, `README.md` when user-facing, `doc/offchain.md` and `doc/msl.md` including their Progress tables, and doc comments in code). When finishing, state which docs were updated, or that none needed changes.

## Common Pitfalls

- Never edit the vendored runtime libraries (`resources/aiken_template/lib/utils/`, `resources/typescript_template/src/lib/`): change them in rosetta-redo, then run `make sync-libs`.
- Never modify the PhD folder (`C:\Users\rover\Documents\Universita\PhD`) unless the user explicitly allows it. Its decisions bind the MSL compilation; any deviation must be shown to the user, approved, and recorded in `doc/msl.md`.
- **MSL work: unexpected problems need approval.** If a problem the user has not anticipated (judge by whether the user wrote about it in the request, the docs or the PhD notes) calls for a major change (a workaround, a duplicated fixture, a change to the plan or to the scope of a step), stop, report the problem and the proposed change, and wait for the user's explicit approval before making it. Example: the parser could not load the spec-annotated `test/Bet/`, and the spec-less copy `test/BetNoSpec/` was created without asking first; it should have been proposed.
- The full spec-annotated Bet contract lives in `../rosetta-redo/move/`; `test/Bet/` is a copy that may diverge slightly, and divergences are listed in `doc/msl.md`. `test/Bet/` parses, but the Move runtime pipeline does not ignore spec blocks yet, so the tests use the temporary spec-less copy `test/BetNoSpec/` (delete it once the Move runtime mode ignores spec blocks).

- `lib/Move/Parser.hs` is generated and very large; edit `Parser.y` instead.
- Whitespace-sensitive `<` token behavior is intentional (`TokenOperatorWhiteLt`).
- `>>` handling in type arguments has special parser constraints; preserve existing approach unless intentionally redesigned.
- The MSL words (`spec`, `schema`, `include`, `pragma`, `ensures`, `aborts_if`, `modifies`, `post`) are reserved keywords of this lexer, unlike the Move compiler where only `spec` is reserved: they cannot be used as identifiers.
