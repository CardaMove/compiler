use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/collection/pairs
use aiken/primitive/bytearray
use aiken/primitive/string
use utils/common_utils.{CPS, Ident, Scope, ScopePtr}

/// GET an identifier from the scopes.
/// Throws an error if the identifier is not found
/// Used for translating `IntermediateGetLocalState`
/// A manual casting must be performed after this call
pub fn get_scope(ident: Ident, (scopes, gs): CPS) -> Data {
  when scopes is {
    [] -> fail common_utils.fail_msg("Identifier not in scope: ", ident)
    [scope, ..scopes] ->
      when dict.get(scope, ident) is {
        Some([]) ->
          fail common_utils.fail_msg(
            "Accessing uninitialized variable: ",
            ident,
          )
        Some([x, ..]) -> x
        None -> get_scope(ident, (scopes, gs))
      }
  }
}

/// POST of a new identifier on the scope.
/// Translation for `IntermediatePostLocalState`
/// If the same identifier already exists in an outer scope, it shadows it
/// If the identifier already exists in the same scope, it is rebinded
///
/// Note how the expression can not be Nothing. In the case of a binding without expression, pass Void.
/// The compiler has already ensured that no access to uninitialized variable is performed
pub fn post_scope(ident: Ident, expr: Data, (scopes, gs): CPS) -> CPS {
  (
    when scopes is {
      [] -> fail @"Empty scopes"
      [scope, ..scopes] ->
        [
          dict.insert_with(
            scope,
            ident,
            [expr],
            with: fn(_, existing, _) { Some([expr, ..existing]) },
          ),
          ..scopes
        ]
    },
    gs,
  )
}

/// PUT of an existing identifier on the scope
/// Translation for `IntermediatePutLocalState`
pub fn put_scope(
  ident: Ident,
  additional_fields: List<Int>,
  expr: Data,
  (scopes, gs): CPS,
) -> CPS {
  (
    when scopes is {
      [] -> fail @"Empty scopes"
      [scope, ..scopes] ->
        [
          dict.insert_with(
            scope,
            ident,
            [expr],
            with: fn(_, bindings, _) {
              when bindings is {
                [] ->
                  fail common_utils.fail_msg(
                    "PUT-ting uninitialized variable: ",
                    ident,
                  )
                [bind, ..binds] ->
                  Some(
                    [
                      common_utils.internal_put_data(
                        bind,
                        additional_fields,
                        expr,
                      ),
                      ..binds
                    ],
                  )
              }
            },
          ),
          ..scopes
        ]
    },
    gs,
  )
}

/// PUSH-es a new local scope on top of the existing
pub fn push_scope((scopes, gs): CPS) -> CPS {
  ([dict.empty, ..scopes], gs)
}

///POP-s the topmost scope
pub fn pop_scope((scopes, gs): CPS) -> CPS {
  (
    when scopes is {
      [] -> fail @"Cannot POP empty scopes"
      [_, ..scopes] -> scopes
    },
    gs,
  )
}
