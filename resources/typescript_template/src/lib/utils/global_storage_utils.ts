/** Port of `lib/utils/global_storage_utils.ak` */
import { Dict } from "@meshsdk/core";
import { Data, VOID, Void, expect_fail, fail } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import {
  Addr,
  CPS,
  GlobalStorageRef,
  ProgramState,
  Reference,
  TWitness,
  assert,
  internal_map_type_to_str,
  reference,
} from "./common_utils";
import { Signer, address_of } from "./signer_utils";

/**
 * Same behaviour of `move_to`, but used internally to translate mutable references to the storage.
 * If the value in state[addr][resource_type] already exists, it is overwritten
 */
export function internal_move_to(addr: Addr, resource_type: TWitness, resource_value: Data, [scopes, state]: CPS): CPS {
  const type_str = internal_map_type_to_str(resource_type);

  const [state_dict] = state.fields;

  return [
    scopes,
    ProgramState(
      dict.insert_with(
        state_dict,
        addr,
        dict.from_pairs<Data>({ map: [{ k: type_str, v: resource_value }] }),
        // This function merges the `state[addr]` so to prevent overriding already existing keys in the inner dict
        // If a key in the sub dicts already exists, its value is overriden since dict.union gives priority to the first argument
        (_key, existing_value, new_value) => dict.union(new_value, existing_value),
      ),
    ),
  ];
}

/**
 * Publishes `resource_value` under `state[signer_address][resource_type]`
 * Aborts if the signer already holds the resource
 */
export function move_to(
  signer: Reference<Signer>,
  resource_type: TWitness,
  resource_value: Data,
  [scopes, state]: CPS,
): [Void, CPS] {
  const [signer_addr, cps] = address_of(signer, [scopes, state]);
  assert(() => !exists(signer_addr, resource_type, cps));

  return [VOID, internal_move_to(signer_addr, resource_type, resource_value, cps)];
}

/**
 * Removes the resource under `state[addr][resource_type]` and returns it
 * Aborts if the address does not hold the resource
 */
export function move_from(addr: Addr, resource_type: TWitness, [scopes, state]: CPS): [Data, CPS] {
  const [state_dict] = state.fields;

  const addr_state = dict.get(state_dict, addr);
  if (addr_state === undefined) {
    return expect_fail("Some(addr_state) = state_dict |> dict.get(addr)");
  }

  const type_str = internal_map_type_to_str(resource_type);

  const resource = dict.get(addr_state, type_str);
  if (resource === undefined) {
    return expect_fail("Some(resource) = addr_state |> dict.get(type_str)");
  }

  return [resource, [scopes, ProgramState(dict.insert(state_dict, addr, dict.delete(addr_state, type_str)))]];
}

/**
 * Returns the corresponding resource under `state[addr][resource_type]`
 * Aborts if the address does not hold the resource
 */
export function borrow_global<A = Data>(addr: Addr, resource_type: TWitness, [scopes, state]: CPS): Reference<A> {
  if (exists(addr, resource_type, [scopes, state])) {
    return reference<A>(GlobalStorageRef(addr, resource_type), []);
  }
  return fail("borrow_global invoked for non existing resource");
}

/** Returns True if the address holds the resource, meaning if `state[addr][resource_type]` exists */
export function exists(addr: Addr, resource_type: TWitness, [_scopes, state]: CPS): boolean {
  const [state_dict] = state.fields;

  const addr_state = dict.get(state_dict, addr);
  if (addr_state === undefined) {
    return false;
  }
  return dict.get(addr_state, internal_map_type_to_str(resource_type)) !== undefined;
}

/** Returns the resource without removin it */
export function internal_get_global_storage(addr: Addr, resource_type: TWitness, state: ProgramState): Data {
  const [state_dict]: [Dict<Dict<Data>>] = state.fields;

  const addr_state = dict.get(state_dict, addr);
  if (addr_state === undefined) {
    return fail("internal_get_global_storage: Missing resource addr");
  }
  const resource = dict.get(addr_state, internal_map_type_to_str(resource_type));
  if (resource === undefined) {
    return fail("internal_get_global_storage: Missing resource");
  }
  return resource;
}
