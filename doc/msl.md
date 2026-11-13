# Compiling MSL specs into the validator

Status: **in progress.** The spec-annotated Bet contract is in `test/Bet/`, the parser reads the MSL subset, and the Move runtime mode ignores the spec blocks. See [Progress](#progress).

## Goal

Until now the compiler translates Move **code** into an Aiken validator. The validator re-executes the selected entry function on the input state (CPS emulation of Move, via `resources/aiken_template/lib/utils/`) and compares the result with the transaction outputs.

The new goal is to compile the **Move Specification Language (MSL)** specs of the entry functions into the validator instead. In this mode the validator is a boolean predicate over the encodings of the environment and of the initial and final states, `(enc ρ, enc S_i, enc S_f)`, and its correctness is the subject of a formal proof (see [Authority](#authority-the-phd-folder)).

There will be two on-chain compilation modes:

| Mode | What is compiled | Status |
|---|---|---|
| **MSL** (new default) | the `spec` blocks of the script entry functions | to be implemented, gradually |
| **Move runtime** (legacy) | the Move code, as today | implemented; must keep working |

A CLI flag will select the mode (planned: `--onchain-mode msl|move`, default `msl`; the name is not final). The off-chain target is not affected by it.

**What stays the same:** the validator prelude (input/output assets, input/output state, timestamp), the `Redeemer` type built from the script entry functions' parameters, and the `when redeemer is` pattern match (`lib/Aiken/ValidatorGenerator.hs`). **What changes** is what each branch does after the match: in MSL mode it evaluates the compiled spec instead of running the transpiled function and comparing states.

MSL support is added **gradually**: first a very small subset (the user will specify it in a dedicated step), then more constructs. Work is centred on the Bet example, but the compiler must stay agnostic of the contract.

## Authority: the PhD folder

`C:\Users\rover\Documents\Universita\PhD` holds the reasoning about the formal proof and the compilation choices. Read its `CLAUDE.md` first; it points to `suggestions.md` (the assessment) and `proof_variant_2.md` (**the proof**; `proof_variant_1.md` is outdated).

- **It is the authority.** This repository follows the semantics and compilation choices decided there.
- **Implementation details may deviate slightly, but every deviation must be shown to the user and approved** before it is implemented. Approved deviations are recorded in [Deviations](#deviations-approved).
- **It is read-only.** Never modify anything in the PhD folder unless the user explicitly allows it.
- What the PhD notes mark as open or "under analysis" is open here too. Do not decide it in this repository. At the time of writing that includes: the MSL fragment, the semantics of each predicate kind and how clauses combine (`assemble`), `Bool` vs `Prop`, the target types, and how generics / type witnesses enter the proof.

Main points decided there that the implementation must respect (summary; the PhD notes are authoritative):

- Theorem (TH_1): `∀Γ φ ρ S_i S_f. Γ ⊢ φ : pred ∧ ρ ⊨ Γ ⇒ (⟦φ⟧_S(ρ,S_i,S_f) ⇔ ⟦compile φ⟧_T(enc ρ, enc S_i, enc S_f))`, with the lemma (L_exp) for expressions. Predicates (`ensures`, `aborts_if`, ...) are distinct from expressions.
- `compile` keeps variable names; ρ is the source environment (here: the redeemer arguments), the target environment is `enc ρ`.
- `⟦old(e)⟧_S(ρ,S_i,S_f) := ⟦e⟧_S(ρ,S_i,S_i)`.
- Junk: the validator is guarded by `wf(T_i) ∧ wf(T_f)`, where `wf` is exactly the image of `enc`.
- Frames: **only `modifies` is used.** The compiler keeps `perm(T)` (permitted addresses per resource type, initially ∅ for every type, address expressions evaluated in the pre-state) and checks `∀T ∀a ∉ perm(T). S_f[T][a] = S_i[T][a]` (a missing resource counts as a value). This is a compiler rule, stricter than MSL.
- Generics are handled with **type witnesses** (an extra Redeemer parameter), as the compiler already does.
- Partial expressions (`x / 0`, out-of-range index) are ignored for now.

## The Bet example: source of truth

`../rosetta-redo/move/` (`C:\Users\rover\Documents\Universita\move-project\rosetta-redo\move\`) holds the **full spec-annotated Bet contract**:

- `sources/bet/bet.move` (`BetAddr::Bet`, with `spec` blocks holding the `modifies` frames);
- `scripts/init_round.move`, `join_round.move`, `win_round.move`, `timeout_round.move` (the specs of the entry points);
- `sources/coin_proxy/coin_proxy.move` (`CoinProxy`, with the `WithdrawSchema` / `DepositSchema` schemas).

`../rosetta-redo/docs/move-spec-constructs.md` describes the MSL constructs used and their behaviour as verified on the Move Prover (frames, `include`, undefined expressions). Read it before relying on a construct's semantics.

`test/Bet/` in this repository is a **copy** of that contract (copied 2026-11-12 from rosetta-redo commit `54f4968`; no automation, re-copy by hand if it changes there). It **may diverge slightly** for implementation needs. Every divergence is listed here.

Current divergences:

- **Flat layout:** `sources/bet.move` and `sources/coin_proxy.move` instead of `sources/bet/bet.move` and `sources/coin_proxy/coin_proxy.move`, because the loader (`lib/Move/Loader/Loader.hs`) reads only the files directly inside `sources/` and `scripts/`. The four scripts are identical.
- **Explicit type arguments:** `CoinProxy::withdraw<CoinType>(..)` in `Bet::join`, and `coin::merge<CoinType>(..)` / `coin::value<CoinType>(..)` in `CoinProxy`, where rosetta-redo lets Move infer them. The compiler does not infer type arguments (`inferExprType` fails with "Inferring type arguments is currently not supported").
- **`Move.toml`:** package `Bet`, only `BetAddr = "0xBE7"` and `CoinProxyAddr = "0xC0"` (rosetta-redo: package `rosetta-aptos-redo`, `BetAddr = 0x2`, `CoinProxyAddr = 0x4`, plus the other contracts' addresses).

`test/Bet/` compiles (`aptos move compile`) and proves (`aptos move prove`: 8 verification roots, about a minute). The warnings that the opaque `CoinProxy` calls have an incomplete `modifies` frame are expected: rosetta-redo prints them too. Run both commands from `test/Bet/`.

`test/Bet/` also transpiles in the Move runtime mode (the spec blocks are dropped, see [Parser and AST](#parser-and-ast)) and the generated `aiken_out/` passes `aiken check`; `test/Move/TranspilerSpec.hs` and the manual CLI runs use it. (Until 2026-11-13 a spec-less copy, `test/BetNoSpec/`, stood in for it; it was deleted when the Move runtime mode started ignoring specs.)

## Initial analysis

These are first considerations, **not decisions**. Each point will be discussed in a dedicated step.

### Parser and AST

**Parsed subset (done, 2026-11-12).** The user defined it as a grammar comment in `lib/Move/AST.hs` (section "Below is the grammar for the MSL"); that comment is the reference. In short:

- `spec <function> { ... }` and `spec schema <Name><T..> { ... }` blocks, as top levels of scripts and modules (`TopLevelSpecBlock`).
- Members: conditions `ensures` / `aborts_if` / `modifies <Exp>;`, schema variables `<Identifier> <TypeParams>? : <Type>;`, `include <Exp>;`, `let [post] <Identifier> = <Exp>;`, and `pragma ...;`, which is parsed and **dropped** (no AST node).
- Spec expressions are ordinary expressions: `old(..)`, `global<T>(a)`, `exists<T>(a)`, `publish` / `remove` are parsed as function calls, and schemas in `include` as named struct literals. The only new operator is `==>` (`Implies` in `BinaryOpExpr`), with the lowest binary precedence and left-associative like every binary operator of the Move compiler (`a ==> b ==> c` is `(a ==> b) ==> c`).
- Conditions (`SpecCondition`) are kept distinct from expressions, as `proof_variant_2.md` does.

Decisions taken with the user while implementing it:

- `modifies` was not in the first draft of the subset but every rosetta script uses it; added as a fourth condition kind (approved 2026-11-12).
- Schema variables (`SpecVariable`) were added to the grammar comment by the user (2026-11-12), since every schema declares its parameters.
- The MSL words are **reserved keywords** of the lexer (in the Move compiler only `spec` is; the others are contextual). Simpler, and no Move code in this repository or in rosetta-redo uses them as identifiers.
- Not parsed: `assume` (dropped from the subset by the user, 2026-11-12), `requires`, quantifiers, `invariant`, spec blocks inside function bodies or loops, condition properties (`aborts_if [abort_code] ...`), `aborts_with`, `emits`, `apply`.

**Move runtime mode ignores specs (done, 2026-11-13).** The first pass of `transpiler` is `stripSpecBlocksInRoot` (`lib/Move/Translations/SpecBlocks.hs`). It drops every spec block and, per root, the `use` declarations and use members referenced by some spec block but by no other top level. Removing the spec blocks alone is not enough: imports such as `aptos_framework::timestamp` (only `timestamp::spec_now_seconds` in the specs) or the `CoinStore` member of `aptos_framework::coin` name modules and structs the compiler's built-in library stubs do not model, and import resolution fails on them. Imports that nothing references, and those the Move code uses, are kept, so spec-less sources are unaffected. On `test/Bet/` the result equals the former spec-less copy, except `use BetAddr::Bet::{Self}` in `win_round.move` where the copy had the equivalent `use BetAddr::Bet;`. The generators still fail with an explicit error on a spec block (now unreachable) or a `==>` (only valid in specs).

### Transpilation passes

- MSL bindings are immutable and MSL has no references, so the CPS / local-scope passes (`Scopes`, probably `Loops`) are likely unnecessary in MSL mode.
- But specs reason about an initial and a final state (`S_i`, `S_f`, `old`) and an environment ρ, which needs its own representation. To be analysed.
- Likely approach: a **dedicated MSL pass chain** (e.g. `mslTranspiler` in `lib/Move/Transpiler.hs`), keeping the current one as the Move runtime chain. Candidates for reuse: `TypeWitness` (generics via witnesses), the address resolution of `PostProcessing`, `TraversalUtils`, the helpers in `Translations/Utils.hs`.
- Type inference (`inferExprType`) will need extending to the spec constructs.

### Validator

- Unchanged: prelude, `Redeemer`, dispatch on the redeemer, signer extraction.
- In MSL mode each branch would evaluate something like `wf(S_i) ∧ wf(S_f) ∧ compiled spec ∧ frame(perm)` instead of running the function and calling `compare_tx_states` / `compare_assets`. How the clauses combine is open in the PhD notes.
- **State decoding (user, 2026-11-10):** the encoded states are what the current validator prelude already builds. `S_i` comes from the input UTxOs (`state_utils.get_tx_input_state(datum)`, plus the input assets via `coin_utils.add_assets_to_state`), and `S_f` from the output UTxOs (`state_utils.get_tx_output_state`). To check when implementing: today the output assets are compared separately (`compare_assets`) rather than merged into the output state, so `S_f` may need the same asset mirroring as `S_i`.
- **Coins (user, 2026-11-10):** contracts reach `aptos_framework::coin` only through the `CoinProxy` module from rosetta-redo, extended there so that it completely hides `CoinStore`. The compiler then **intercepts `CoinProxy`** (its functions and schemas) and applies its own rules. Coin balances stay in the library's existing mirror (the `__COIN__Coin<...>` keys of the state, from the real tx assets), hidden behind the `CoinProxy` translation. Fee handling follows the current prelude (`deduce_transaction_fees_from_assets`) unless decided otherwise. Since this shapes `enc`, check it against the PhD notes (e.g. the (Vis) side condition on projections) before implementing.
- **`aborts_if` and clause combination:** to be decided in the PhD folder, not here.
- New runtime helpers (e.g. the frame check over the key-value pairs of two states) go **into rosetta-redo first**, then `make sync-libs`; the vendored libraries are never edited here.

### Off-chain

In MSL mode a transaction builder still has to compute `S_f`. This is out of scope for now. The off-chain work ([offchain.md](offchain.md)) is **suspended**; the two tracks may proceed independently.

## Deviations (approved)

Deviations of this implementation from the PhD notes, each approved by the user.

| Date | Deviation | Reason |
|---|---|---|
| — | none yet | — |

## Progress

Each step is discussed with the user before coding. A major change caused by a problem the user had not anticipated (one not mentioned in the request, these notes or the PhD notes) is reported to the user and needs their explicit approval before it is made.

| Step | Description | State |
|---|---|---|
| 0 | Notes: this file, `CLAUDE.md` / `AGENTS.md` pointers, off-chain marked suspended | Done (2026-11-10) |
| 0.1 | Copy the spec-annotated Bet from rosetta-redo into `test/Bet/` (compiles and proves); temporary spec-less `test/BetNoSpec/` for the tests and the CLI (deleted in step 2) | Done (2026-11-12) |
| 1 | Agree the first MSL subset with the user, checked against the PhD notes | Partly done: the parsed subset is fixed (grammar comment in `lib/Move/AST.hs`, 2026-11-12); its semantics are still open in the PhD notes |
| 2 | Lexer / Token / Parser + AST for spec blocks, schemas, predicates and spec expressions (subset only); Move runtime mode ignores spec blocks; `TranspilerSpec` back on `test/Bet/`, delete `test/BetNoSpec/` | Done (2026-11-13): lexer, parser and AST, tested on the 7 rosetta scripts and `coin_proxy.move` (`LexerSpec`, `ParserSpec`, 2026-11-12); `stripSpecBlocksInRoot` drops specs and spec-only imports in the Move runtime mode (`SpecBlocksSpec`); `TranspilerSpec` on `test/Bet/`; `test/BetNoSpec/` deleted |
| 3 | CLI flag for the on-chain mode (default MSL), dispatch in `src/Main.hs` / `lib/Move/Transpiler.hs` | To do |
| 4 | Design the MSL pipeline: AST and typing changes, reused passes, representation of `S_i` / `S_f` / `old` / ρ | To do |
| 5 | MSL passes (`mslTranspiler`) with tests | To do |
| 6 | Validator generation for MSL mode: new branch bodies, `wf` guard, `perm`-based frame check (helpers via rosetta-redo) | To do |
| 7 | Bet end-to-end: golden `.ak` fixtures from `test/Bet/`, `aiken check` of `aiken_out/` | To do |
| 8 | Extend the subset gradually (schemas / `include`, quantifiers, ...) | To do |
