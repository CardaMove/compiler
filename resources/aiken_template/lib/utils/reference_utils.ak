use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/collection/pairs
use aiken/primitive/bytearray
use aiken/primitive/string
use utils/common_utils.{
  CPS, GlobalStorageRef, Ident, ProgramState, RefKind, Reference, Scope,
  ScopePtr, ScopeRef, TypeWithnessC, as_data,
}
use utils/global_storage_utils

/// Searches for an identifer in the scope and returns a pointer to it
fn internal_scope_ptr(ident: Ident, scopes: List<Scope>) -> ScopePtr {
  when scopes is {
    [] ->
      fail common_utils.fail_msg(
        "Identifier not in scope when creating ptr: ",
        ident,
      )
    [scope, ..scopes] ->
      when dict.get(scope, ident) is {
        Some([]) ->
          fail common_utils.fail_msg(
            "Accessing uninitialized variable when creating ptr: ",
            ident,
          )
        Some(bindings) ->
          ScopePtr(list.length(scopes) - 1, ident, list.length(bindings) - 1)
        None -> internal_scope_ptr(ident, scopes)
      }
  }
}

/// Extends a reference with additional field indices, reinterpreting it as a reference to a nested field of type `b`
pub fn extend_ref(ref: Reference<a>, extra_fields: List<Int>) -> Reference<b> {
  let (variant, fields) = ref
  (variant, list.concat(fields, extra_fields))
}

/// Creates a reference to any identifier in the scope, tracking also the indices of any nested field to access
/// Translation for `IntermediateReferenceLocalState`
pub fn make_ref(
  ident: Ident,
  fields: List<Int>,
  (scopes, gs): CPS,
) -> Reference<a> {
  (ScopeRef { resource: internal_scope_ptr(ident, scopes) }, fields)
}

/// Dereferences a reference of the scope.
/// Used for translating `IntermediateGetDereferenceLocalState`
/// A manual casting must be performed after this call
pub fn deref(ref: Reference<a>, (scopes, gs): CPS) -> Data {
  let (variant, fields) = ref
  when variant is {
    ScopeRef { .. } -> common_utils.internal_deref_scope(ref, (scopes, gs))
    GlobalStorageRef { addr, resource_type } -> {
      let base =
        global_storage_utils.internal_get_global_storage(
          addr,
          resource_type,
          gs,
        )
      common_utils.internal_get_data(base, fields)
    }
  }
}

/// Givena reference to the scope, updates its value
/// Translation for `IntermediatePutDereferenceLocalState`
pub fn put_deref(
  ref: Reference<a>,
  additional_fields: List<Int>,
  val: Data,
  (scopes, gs): CPS,
) -> CPS {
  let (variant, ref_fields) = ref
  when variant is {
    ScopeRef{resource: ScopePtr(height, ident, binding_idx)} -> {
      let fields: List<Int> = list.concat(ref_fields, additional_fields)

      let scopes_height = list.length(scopes)

      (
        list.indexed_map(
          scopes,
          fn(scope_idx, scope_el) {
            if scopes_height - scope_idx - 1 == height {
              dict.map(
                scope_el,
                fn(binded_name, bindings_el) {
                  if binded_name == ident {
                    let bindings_height = list.length(bindings_el)
                    list.indexed_map(
                      bindings_el,
                      fn(bind_idx, bind_el) {
                        if bindings_height - bind_idx - 1 == binding_idx {
                          common_utils.internal_put_data(bind_el, fields, val)
                        } else {
                          bind_el
                        }
                      },
                    )
                  } else {
                    bindings_el
                  }
                },
              )
            } else {
              scope_el
            }
          },
        ),
        gs,
      )
    }
    GlobalStorageRef { addr, resource_type } -> {
      let fields: List<Int> = list.concat(ref_fields, additional_fields)
      let base =
        global_storage_utils.internal_get_global_storage(
          addr,
          resource_type,
          gs,
        )
      let updated = common_utils.internal_put_data(base, fields, val)
      global_storage_utils.internal_move_to(
        addr,
        resource_type,
        updated,
        (scopes, gs),
      )
    }
  }
}
