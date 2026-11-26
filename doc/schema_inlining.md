# Spec schema inlining (MSL mode)

Design of `inlineSpecSchemas` (`lib/Move/TranslationsMSL/SpecInlining.hs`), step 5.1 of [msl.md](msl.md#progress). Plan approved by the user on 2026-11-26, with these decisions:

- only the schema expressions `S{…}` (and the bare name `S`) and `p ==> s` are supported for now; `s1 && s2`, `if (p) s1 else s2` and every other expression are an error ("not supported yet");
- the imports needed by the qualified names are added to the including root, and the schema blocks are removed after inlining (see [Changes to the root](#changes-to-the-root));
- omitted type arguments of a generic schema are an error, they are not inferred;
- tests compare against hand-written expected `.move` fixtures (parsed and compared as ASTs) instead of `show`n `.txt` files.

## Semantics

Source: `../rosetta-redo/move/sources/prover_playground/playground_include.move` (lines 1-31 summarise the experiments, Aptos CLI 9.6.0) and the PhD notes (`proof_variant_2.md`, "Considerations on ρ": `include` is expanded by substitution before compilation, so schema variables never reach ρ).

- **Binding by substitution.** Each argument is pasted into every member of the schema and takes the state of that member (`aborts_if` and `let` in the pre-state, `ensures` and `let post` in the post-state).
- **Implicit arguments.** A schema variable without an argument binds by name to a parameter or `let` of the including spec: it is left as a plain name. An argument naming a variable the schema does not declare is an error.
- **Hygiene of the schema's `let`s.** A schema `let` does not shadow a caller binding of the same name for the caller's later clauses (playground c4: `let w = 100; include OwnLet { z: 1 }; ensures w == 100;` proves, although `OwnLet` binds its own `w`). With one ordered spec scope, pasting the `let` unchanged would shadow the caller's `w`, so every inlined schema `let` is **renamed to a fresh name**. Bet needs this: `timeout_round` includes `DepositSchema` twice, and both copies bind `pre_bal` and `post_bal`. Not a deviation: it keeps the MSL meaning.
- **Premise `p ==> S`.** `p` is copied into every member of `S`:
  - `ensures e` becomes `ensures p ==> e`;
  - `aborts_if c` becomes `aborts_if p && c`;
  - `modifies` is **unchanged** (MSL ignores the premise there, it is always active);
  - `let` / `let post` are unchanged (they only define names);
  - nested includes receive the premise through the recursion: `ensures p ==> (q ==> e)`, `aborts_if p && (q && c)`.
- **`==>` is left-associative**: `include a ==> b ==> S` parses as `Implies (a ==> b) S`, whose premise is `a ==> b`, as MSL does.
- **`result`** is passed through unchanged: correct for module function specs, and the Move compiler already rejects it in script specs (the open point "an `include` bringing in `result`" of the PhD notes needs no special case).

**Consequence (approved):** an `include p ==> S` where `S` holds a `modifies` adds its address to `perm(T)` even when `p` is false. This matches MSL, but makes the validator's frame check looser than the premise suggests.

## Schema expressions

| Parsed form | Expansion |
|---|---|
| `NamedStructExprExpr S<τ…>{f: e, …}` | apply the schema |
| `NameAccessChainExpr S` | apply the schema with no explicit arguments and no type arguments |
| `BinaryOpExprExpr (Implies p s)` | add the premise `p`, then expand `s` |
| `CommaExpr [s]` (parentheses) | expand `s` |
| `BinaryOpExprExpr (And s1 s2)`, `IfThenElseTerm …` | error: not supported yet (MSL supports them) |
| anything else | error: not a schema expression |

## Where it runs

`mslTranspiler`: `annotateSpecBlocks` → `stripMoveFunctions` → **`inlineSpecSchemas`** → `annotateBindingsWithUUID` → failure ("not implemented yet").

- Before `annotateBindingsWithUUID`, so that every inlined copy of a `let` gets its own UUID (inlining after it would duplicate the schema's UUIDs).
- It is not written with `traverseRootPostOrder`: spec member mappers are one-to-one while an include becomes several members, and the schema scope entry (`IntermediateTypeSpecSchema`) holds the schema's variables, not its members.
- It runs on every function spec block of every root (modules and scripts).

## Algorithm

For an include of schema `S` with type arguments `TA`, arguments `A` and premises `P`, in the context of the including root `C`:

1. **Resolve `S`** in `C` with a syntactic resolver that only reads the `use` declarations (no module lookup):
   - `x`: a member imported by `use A::M::{x}` (or `{y as x}`), otherwise a top level of `C`;
   - `m::x`: `m` is a module alias from `use A::M`, `use A::M as m` or `use A::M::{Self}`; `Self::x` is `C` itself;
   - `A::M::x`: itself.

   Then find the `SpecBlockTargetSchema` block among the roots: an error if not found. A stack of the schemas being expanded detects cycles (error).
2. **Expand the nested includes first**, recursively, in the schema's own root context. The result has no includes.
3. **Qualify the names** in the schema's root context, so that the members keep their meaning once pasted into `C` (Bet: `coin::CoinStore` becomes `aptos_framework::coin::CoinStore`, `address_of` becomes `std::signer::address_of`). Positions: types (`TypeConstructor`), call names, struct literal names, variable names. Untouched: the schema's locals (its variables and every `let` of the members after step 2), its type parameters, names that do not resolve (built-ins `global`, `exists`, `old`, `publish`, `remove`, `result`, primitive types), field labels and `.field`. A name of `C`'s own module stays local, since the traversals resolve the root's own top levels only by their local name.
4. **Rename the schema's `let`s** to fresh names `<name>_<k>`, `k` being a counter shared by the whole pass; a candidate that collides with any identifier of the project is skipped. References are renamed in order (only the members after the `let`), including the `Struct{x}` field shorthand.
5. **Substitute the type parameters with `TA`**, simultaneously. The number of type arguments must match (an error otherwise, including `include S;` for a generic `S`).
6. **Substitute the schema variables with `A`**, simultaneously. The argument shorthand `{x}` means `x: x`; a field shorthand inside the members naming a substituted variable is expanded. Steps 3-5 run before this one because the arguments and `TA` belong to the caller's context: they must not be qualified, renamed or retyped.
7. **Drop the `SpecBlockMemberVariable`s and apply the premises** `P` as described in [Semantics](#semantics).

The include member is replaced, in place, by the resulting members.

Nodes outside the pure fragment inside an inlined member (`SequenceExpr`, `AssignmentExpr`, `WhileTerm`, `Loop`, `Return`, `Abort`, `Break`, `Continue`, intermediate nodes) are an error: substitution does not cross local binders.

## Changes to the root

- **Imports.** For every module referenced by a name qualified in step 3 that `C` does not import yet (and that is not `C` itself), a module import `use A::M;` is added to `C`, so that the later traversals resolve the qualified names (`buildImportedScope` only knows the modules a root imports). Known limitation: a module neither in the project nor in `moveStdLibModules` makes `buildImportedScope` fail, as it already does for any import of such a module.
- **Schemas removed.** After inlining, every spec schema block is removed, together with the use members importing a schema of a project module (a use left without members is removed, as in `stripMoveFunctions`). Later passes never see `SpecBlockTargetSchema`. The schema scope support of `TraversalUtils` stays, for traversals running before inlining.

## Tests

`test/Move/TranslationsMSL/SpecInliningSpec.hs`. Each project is a set of `.move` files (one root per file, `SpecInliningSpec_<n>.move`); the expected result is a hand-written `.move` file per root, parsed and compared with the pass output after removing the parentheses (single element `CommaExpr`) on both sides, since the expected file needs them to express the shape of the tree. The error cases use small inline sources. Cases: plain include with arguments (substitution, qualification, added `use`, renamed `let`s); implicit arguments and shorthand; caller `let` with the same name as a schema `let` (playground c4) and the same schema included twice; premises on every member kind, nested premises, left-associative `==>`; nested include across modules and forwarded type arguments; schemas and their imports removed; errors (unknown schema, unknown argument, type argument count, cycle, impure node, unsupported or invalid schema expression). On Bet, `SpecInliningSpec` checks that no include and no schema are left, and `TranspilerSpec` that `mslTranspiler` (which now runs the inlining) still fails only with "not implemented yet".

## Not handled

- `include S<T>;` without braces does not parse (confirmed, see [known-bugs.md](known-bugs.md)); `include S<T> {};` works.
- Calls to user-defined functions inside schemas: their imports are removed by `stripMoveFunctions`, and they are out of the subset anyway.
- `s1 && s2` and `if (p) s1 else s2` (not supported yet).
