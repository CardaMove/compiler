# Off-chain code generation (TypeScript + Mesh)

Status: **suspended** (since 2026-11-10, while the MSL compilation in [msl.md](msl.md) starts; it may resume independently of it). See [Progress](#progress).

## Why off-chain code is needed

The generated Aiken validator doesn't compute the result of a Move call and write it on-chain. It *checks* a result that the transaction already contains:

1. it rebuilds the `ProgramState` from the input datum and the input assets;
2. it runs the transpiled Move entry function;
3. it compares the resulting state and assets with the transaction's actual outputs and output datum (`compare_tx_states`, `compare_assets`).

Whoever builds the transaction must therefore compute **exactly** the same `ProgramState`, assets and datum CBOR before submitting it. If a single byte differs, the validator rejects the transaction. That computation is the off-chain code, and the same Move source must produce it.

## Target

- Language: **TypeScript**.
- Cardano library: [MeshJS](https://meshjs.dev) (`@meshsdk/core` from npm). It is used for Plutus data, CBOR encoding, addresses and transaction building.
- Output: an npm project, `ts_out/`, next to the Aiken project `aiken_out/`.

## The TypeScript runtime library

Generated Aiken code calls the helper library in `resources/aiken_template/lib/utils/` (CPS/scopes, references, global storage, signer, coin, assets, state, timestamp). Generated TypeScript calls a **one-to-one port** of that library.

The port was written and tested in the sibling repository **rosetta-redo** (`../rosetta-redo`, https://github.com/roveroniandrea/rosetta-redo), in `aiken-redo/mesh/src/lib/`:

| Folder | Contents |
|---|---|
| `stdlib/` | Emulation of the Aiken stdlib parts the library uses (`data`, `dict`, `assets`, `bytearray`, `list`, `interval`, `transaction`). Mesh provides the data *types*, constructors and CBOR, but not Aiken's *operations* on them, e.g. sorted dicts or byte-level slicing. |
| `utils/<module>.ts` | One file per `lib/utils/<module>.ak`. Same snake_case function names, same `fail` messages, same quirks. |
| `tests/` | The 43 Aiken unit tests ported to Vitest, plus the **parity fixtures**: CBOR produced by the real Aiken library (`fixtures.json`), which the TS code must reproduce byte for byte. |

Rules inherited from the port (details in rosetta-redo's `docs/typescript-port-plan.md`):

- **Parity rule.** Every function is ported faithfully, bugs included. Behaviour is never "fixed" in TS alone: it is fixed in Aiken first, then mirrored, and the fixtures are regenerated.
- Values are Mesh JSON Plutus data. Aiken constructors are TS functions of the same name (`ProgramState(...)`, `Coin(...)`, `TypeWithnessC(...)`). Ints are built with `int(n)` and string literals with `str("...")`.
- CBOR is always decoded with `fromCbor`, never with raw `deserializeDatum`.
- Known bugs, reproduced on purpose, are listed in rosetta-redo's `docs/known-bugs.md`:
  - local references (`make_ref` / `deref` / `put_deref`) use inconsistent scope numbering;
  - asset ids containing certain bytes are corrupted in `__COIN__Coin<...>` keys;
  - Mesh `addrBech32ToPlutusDataObj` omits the `Inline` wrapper. Use `address_from_bech32` instead.

### Vendoring

**rosetta-redo is the source of truth** for both runtime libraries: the Aiken utils and their TypeScript port. Only there are the parity fixtures regenerated, because only there do the Aiken fixture tests (`lib/tests/fixtures_tests.ak`) live. This repository holds vendored copies:

| Source (rosetta-redo) | Vendored copy (this repo) |
|---|---|
| `aiken-redo/lib/utils/*.ak` | `resources/aiken_template/lib/utils/` |
| `aiken-redo/mesh/src/lib/**` | `resources/typescript_template/src/lib/` |

Vendored files are **never edited here**. To change a library:

1. change it in rosetta-redo (Aiken first, then the TS mirror);
2. regenerate the fixtures there with `npm run fixtures && npm test`, and commit;
3. run `make sync-libs` here (or `node scripts/sync-runtime-libs.js`).

The sync script reads rosetta-redo from `$ROSETTA_REDO`, defaulting to `../rosetta-redo`. It refuses to copy uncommitted changes. It records the source commit and a sha256 of every vendored file in `resources/runtime-libs.lock.json`.

`make check-libs` (`node scripts/sync-runtime-libs.js --check`) fails if a vendored file no longer matches the lock. When rosetta-redo is available, it also fails if the libraries there differ from the vendored ones, i.e. an upstream change has not been synced yet. Hashes ignore CRLF vs LF, so Windows and Unix checkouts agree.

## Pipeline

The off-chain code is produced by the same transformations as the on-chain code, followed by off-chain-only passes:

```
loadToml
  -> transpiler (shared):  translateWhilesToFunctionsInRoot -> annotateBindingsWithUUID -> translateTParamsInRoot
                           -> markVariablesForLocalScope + addLocalScopeInRoot -> translatePolymorphismInRoot -> postProcessRoot
       |-> Aiken.AikenGenerator / Aiken.ValidatorGenerator                         -> aiken_out/
       |-> offchainTranspiler:  alphaConvert
             -> TypeScript.TypeScriptGenerator / TypeScript.EntryPointGenerator   -> ts_out/
```

- `postProcessRoot` ends the **shared** pipeline. Target-specific passes run after it. The old rule "`postProcessRoot` is always the last pass" is deprecated.
- `alphaConvert` (`lib/Move/Translations/AlphaConversion.hs`) renames bindings that are rebound in the same scope, because JavaScript `const`/`let` can't be redeclared there. It **must run after `postProcessRoot`** for transpilation to be correct.
- The shared passes run once. When both targets are generated, the off-chain tail is applied to the on-chain result.
- The exact mapping of each AST node onto TypeScript, and any further off-chain passes, are still to be designed (see [Progress](#progress), step 5).

### What the skeleton generates today

| Move | TypeScript |
|---|---|
| `use 0x..::M` / `use utils::m.{A, b as c} as x` | `import * as M from "../../modules/module_0x../M"` / `import * as x from "../../lib/utils/m"` + `import { A, b as c } from ...` (paths start with `../` in scripts) |
| named / positional struct `S` | `export type S = ConStr0<[...field types]>` + `export function S(...fields): S`, as the runtime library does for `Coin` / `Signer`. Type arguments are dropped, since every struct is Plutus data. Positional fields are named `field_<i>`. |
| literal constant | `export const C: T = int(42)` / `true` / `str("0x1")` |
| function | the signature, exported if it has a visibility modifier, with a placeholder body: `throw new Error("CardaMove: the body of ... is not generated yet")` |
| types | `u8`…`u256` → Mesh `Integer`; `bool` → `boolean` (`Bool` inside struct fields); `address` → `Addr`; `signer` → `Signer`; `&T` / `&mut T` → `Reference<T>`; `()` → `Void`; tuples → TS tuples; type witnesses → `TWitness`; scopes → `CPS`; `Data` |

Each generated file starts with the same header imports (Mesh `Bool`, `ConStr0`, `Integer`, and `Data`, `Void`, `VOID`, `int`, `str` from `lib/stdlib/data`). Unused imports are allowed. `src/entrypoints.ts` only re-exports each script (`export * as <script> from "./scripts/<script>"`).

## Repository layout

The on-chain and off-chain parts are kept apart. Each side is named after its target language:

| Path | Side | Contents |
|---|---|---|
| `lib/Move/` | shared | frontend (lexer, parser, loader) and the AST translation passes |
| `lib/Aiken/` | on-chain | `AikenGenerator`, `ValidatorGenerator` |
| `lib/TypeScript/` | off-chain | `TypeScriptGenerator`, `EntryPointGenerator` |
| `resources/aiken_template/` | on-chain | Aiken project template, including the vendored Aiken utils |
| `resources/typescript_template/` | off-chain | npm project template, including the vendored TS library and its tests |
| `resources/runtime-libs.lock.json` | both | provenance and hashes of the vendored libraries |
| `scripts/sync-runtime-libs.js` | both | sync / drift check of the vendored libraries |
| `test/Aiken/`, `test/TypeScript/` | per side | generator specs and golden files |

### Output

The CLI generates both targets by default. `--onchain-only` / `--offchain-only` restrict it to one.

- `aiken_out/`: copy of `resources/aiken_template/` plus the generated `.ak` files (unchanged).
- `ts_out/`: copy of `resources/typescript_template/` (without `node_modules`, `bin`, `.env`, `wallets`) plus:
  - `src/modules/module_<address>/<Module>.ts`, one per Move module;
  - `src/scripts/<name>.ts`, one per Move script;
  - `src/entrypoints.ts`, the off-chain counterpart of `validators/main.ak`.

## Testing

| What | Command | Where |
|---|---|---|
| TS generator and off-chain pipeline (golden `.ts` files, Bet end-to-end) | `stack test` | repo root |
| Vendored TS library: ported unit tests + parity fixtures | `npm install && npm test`, `npm run typecheck` | `resources/typescript_template/` |
| Generated project type-checks against the library | `npm install && npm run typecheck` | `ts_out/`, after running the CLI |
| Vendored copies unchanged | `make check-libs` | repo root |

## Notes for the Mesh integration (step 6)

These notes are inherited from rosetta-redo:

- Off-chain datums must contain the coin entries that the validator flow adds (`__COIN__Coin<...>` keys from `add_assets_to_state`), not only the Move resources.
- Compute the timestamp from the same validity range the transaction uses, e.g. with `tx_view_from_mesh_body`. The range must stay under 120 s.
- Redeemers must be encoded exactly as `aiken_out/validators/main.ak` decodes them.
- rosetta-redo's `aiken-redo/mesh/src/{bet.ts,common.ts}` are a hand-written end-to-end Bet run on a local Yaci DevKit. They are the starting point for the generated runner, but they use old helpers, not the ported library.

## Progress

| Step | Description | State |
|---|---|---|
| 1 | Docs and instructions (this file, doc-sync rule, pipeline rule update) | Done |
| 2 | Layout: rename the Aiken template, vendor the TS library, sync/check script | Done. Vendored from rosetta-redo `833e791`. The only change to the Aiken utils is that `coin_utils.ak` is now `aiken fmt`-formatted, so `aiken_out/` is otherwise byte-identical. |
| 3 | `offchainTranspiler`, `TypeScriptGenerator` / `EntryPointGenerator` skeletons, CLI flags, `ts_out/` writer | Done (skeleton, see [What the skeleton generates today](#what-the-skeleton-generates-today)) |
| 4 | Tests: generator golden files, Bet end-to-end through `alphaConvert`, TS library tests | Done: `test/TypeScript/TypeScriptGeneratorSpec.hs` (6 cases), `test/Move/TranspilerSpec.hs` (2 cases, Bet). The TS library passes 136/136 in `resources/typescript_template/`. The `ts_out/` generated from Bet type-checks. |
| 5 | Full AST → TypeScript mapping, plus Aiken ↔ TS parity tests on generated code | To be designed |
| 6 | Mesh transaction building: entry points, datum/redeemer, Yaci runner | To be designed |
