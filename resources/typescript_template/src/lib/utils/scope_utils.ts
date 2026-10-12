/** Port of `lib/utils/scope_utils.ak` */
import { Integer, List } from "@meshsdk/core";
import { Data, fail, str } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { CPS, Ident, fail_msg, internal_put_data } from "./common_utils";

/**
 * GET an identifier from the scopes.
 * Throws an error if the identifier is not found
 * Used for translating `IntermediateGetLocalState`
 * A manual casting must be performed after this call
 */
export function get_scope(ident: Ident, [scopes, gs]: CPS): Data {
  for (const scope of scopes) {
    const bindings = dict.get(scope, ident);
    if (bindings !== undefined) {
      if (bindings.list.length === 0) {
        return fail(fail_msg(str("Accessing uninitialized variable: "), ident));
      }
      return bindings.list[0];
    }
  }
  return fail(fail_msg(str("Identifier not in scope: "), ident));
}

/**
 * POST of a new identifier on the scope.
 * Translation for `IntermediatePostLocalState`
 * If the same identifier already exists in an outer scope, it shadows it
 * If the identifier already exists in the same scope, it is rebinded
 *
 * Note how the expression can not be Nothing. In the case of a binding without expression, pass Void.
 * The compiler has already ensured that no access to uninitialized variable is performed
 */
export function post_scope(ident: Ident, expr: Data, [scopes, gs]: CPS): CPS {
  if (scopes.length === 0) {
    return fail("Empty scopes");
  }
  const [scope, ...rest] = scopes;
  return [
    [
      dict.insert_with<List<Data>>(scope, ident, { list: [expr] }, (_, existing) => ({
        list: [expr, ...existing.list],
      })),
      ...rest,
    ],
    gs,
  ];
}

/**
 * PUT of an existing identifier on the scope
 * Translation for `IntermediatePutLocalState`
 */
export function put_scope(ident: Ident, additional_fields: Integer[], expr: Data, [scopes, gs]: CPS): CPS {
  if (scopes.length === 0) {
    return fail("Empty scopes");
  }
  const [scope, ...rest] = scopes;
  return [
    [
      dict.insert_with<List<Data>>(scope, ident, { list: [expr] }, (_, bindings) => {
        if (bindings.list.length === 0) {
          return fail(fail_msg(str("PUT-ting uninitialized variable: "), ident));
        }
        const [bind, ...binds] = bindings.list;
        return { list: [internal_put_data(bind, additional_fields, expr), ...binds] };
      }),
      ...rest,
    ],
    gs,
  ];
}

/** PUSH-es a new local scope on top of the existing */
export function push_scope([scopes, gs]: CPS): CPS {
  return [[dict.empty<List<Data>>(), ...scopes], gs];
}

/** POP-s the topmost scope */
export function pop_scope([scopes, gs]: CPS): CPS {
  if (scopes.length === 0) {
    return fail("Cannot POP empty scopes");
  }
  return [scopes.slice(1), gs];
}

