/** Port of `lib/utils/reference_utils.ak` */
import { Integer, List } from "@meshsdk/core";
import { Data, constrIndex, data_equals, fail, int, str, toBigInt } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import {
  CPS,
  GlobalStorageRef,
  Ident,
  Reference,
  Scope,
  ScopePtr,
  ScopeRef,
  fail_msg,
  internal_deref_scope,
  internal_get_data,
  internal_put_data,
  reference,
} from "./common_utils";
import { internal_get_global_storage, internal_move_to } from "./global_storage_utils";

/** Searches for an identifer in the scope and returns a pointer to it */
function internal_scope_ptr(ident: Ident, scopes: Scope[]): ScopePtr {
  if (scopes.length === 0) {
    return fail(fail_msg(str("Identifier not in scope when creating ptr: "), ident));
  }
  // Like in Aiken, `scopes` below is the *tail* of the list
  const [scope, ...rest] = scopes;
  const bindings = dict.get(scope, ident);
  if (bindings === undefined) {
    return internal_scope_ptr(ident, rest);
  }
  if (bindings.list.length === 0) {
    return fail(fail_msg(str("Accessing uninitialized variable when creating ptr: "), ident));
  }
  return ScopePtr(int(rest.length - 1), ident, int(bindings.list.length - 1));
}

/** Extends a reference with additional field indices, reinterpreting it as a reference to a nested field of type `B` */
export function extend_ref<B = Data>(ref: Reference, extra_fields: Integer[]): Reference<B> {
  const [variant, fields] = ref.list;
  return reference<B>(variant, [...fields.list, ...extra_fields]);
}

/**
 * Creates a reference to any identifier in the scope, tracking also the indices of any nested field to access
 * Translation for `IntermediateReferenceLocalState`
 */
export function make_ref<A = Data>(ident: Ident, fields: Integer[], [scopes, _gs]: CPS): Reference<A> {
  return reference<A>(ScopeRef(internal_scope_ptr(ident, scopes)), fields);
}

/**
 * Dereferences a reference of the scope.
 * Used for translating `IntermediateGetDereferenceLocalState`
 * A manual casting must be performed after this call
 */
export function deref(ref: Reference, [scopes, gs]: CPS): Data {
  const [variant, fields] = ref.list;
  if (constrIndex(variant) === 0) {
    return internal_deref_scope(ref, [scopes, gs]);
  }
  const [addr, resource_type] = (variant as GlobalStorageRef).fields;
  const base = internal_get_global_storage(addr, resource_type, gs);
  return internal_get_data(base, fields.list);
}

/**
 * Given a reference to the scope, updates its value
 * Translation for `IntermediatePutDereferenceLocalState`
 */
export function put_deref(ref: Reference, additional_fields: Integer[], val: Data, [scopes, gs]: CPS): CPS {
  const [variant, ref_fields] = ref.list;
  const fields: Integer[] = [...ref_fields.list, ...additional_fields];

  if (constrIndex(variant) === 0) {
    const [height, ident, binding_idx] = (variant as ScopeRef).fields[0].fields;
    const scopes_height = BigInt(scopes.length);

    return [
      scopes.map((scope_el, scope_idx) => {
        if (scopes_height - BigInt(scope_idx) - 1n !== toBigInt(height)) {
          return scope_el;
        }
        return dict.map(scope_el, (binded_name, bindings_el): List<Data> => {
          if (!data_equals(binded_name, ident)) {
            return bindings_el;
          }
          const bindings_height = BigInt(bindings_el.list.length);
          return {
            list: bindings_el.list.map((bind_el, bind_idx) =>
              bindings_height - BigInt(bind_idx) - 1n === toBigInt(binding_idx)
                ? internal_put_data(bind_el, fields, val)
                : bind_el,
            ),
          };
        });
      }),
      gs,
    ];
  }

  const [addr, resource_type] = (variant as GlobalStorageRef).fields;
  const base = internal_get_global_storage(addr, resource_type, gs);
  const updated = internal_put_data(base, fields, val);
  return internal_move_to(addr, resource_type, updated, [scopes, gs]);
}
