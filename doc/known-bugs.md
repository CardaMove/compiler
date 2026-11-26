# Known bugs

Confirmed bugs not fixed yet. Remove an entry in the same change that fixes it.

## `include S<T>;` without braces does not parse

- **Where:** `lib/Move/Parser.y`, rule `Term`: the variable alternative `NameAccessChain %prec LOWER` (producing `NameAccessChainExpr`) takes no type arguments, so a bare generic schema name is read as `S < T >` and fails on what follows.
- **Reproduce:** parse `module A::M { spec schema S<T> {} spec f { include S<u64>; } }` → `Parse error on tokens: [TokenSeparatorSemiColon, …]`. Same for `include S<u64> && …`.
- **Impact:** valid MSL; the brace form `include S<u64> {};` works, and Bet always uses it. A non-generic `include S;` parses.
- **Suggested fix:** accept `NameAccessChain TypeArgs` as a term in spec contexts (e.g. parse it as a `NamedStructExpr` with no fields, which `inlineSpecSchemas` already handles), taking care of the `<` / `>` ambiguity with comparisons that `TokenOperatorWhiteLt` resolves today.
