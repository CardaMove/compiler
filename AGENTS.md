# AGENTS.md

This file gives AI coding agents project-specific instructions for working in this repository.

## Project Purpose

- CardaMove is a Haskell proof-of-concept transpiler from Move to Aiken.
- The core flow is: parse Move source -> apply AST translations -> generate Aiken output.

## Repository Map

- `lib/Move/`: Move language frontend and transformation pipeline.
- `lib/Move/Lexer.x`: Alex lexer source.
- `lib/Move/Parser.y`: Happy parser grammar source.
- `lib/Move/Lexer.hs` and `lib/Move/Parser.hs`: generated files.
- `lib/Move/Transpiler.hs`: ordered translation pipeline.
- `lib/Move/Translations/`: AST-to-AST transformation passes and shared traversal/type helpers.
- `lib/Aiken/AikenGenerator.hs`: Aiken code generation.
- `src/Main.hs`: CLI entrypoint.
- `test/`: Hspec test suite and fixture files.

## Translation Modules Purpose

Inside `lib/Move/Translations/`, the active Haskell modules have the following responsibilities:

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
  - `stack run`
- VS Code tasks available:
  - `haskell build`
  - `haskell clean & build`
  - `haskell test`
  - `haskell watch`
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
- Keep post-processing (`postProcessRoot`) at the end of the pipeline.

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

## Common Pitfalls

- `lib/Move/Parser.hs` is generated and very large; edit `Parser.y` instead.
- Whitespace-sensitive `<` token behavior is intentional (`TokenOperatorWhiteLt`).
- `>>` handling in type arguments has special parser constraints; preserve existing approach unless intentionally redesigned.
