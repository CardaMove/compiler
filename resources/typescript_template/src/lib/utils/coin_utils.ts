/** Port of `lib/utils/coin_utils.ak` */
import { ConStr0, Dict, Integer, Value } from "@meshsdk/core";
import * as assets from "../stdlib/assets";
import * as bytearray from "../stdlib/bytearray";
import { Data, VOID, Void, data_equals, expect_fail, fail, int, isConstr, isInt, str, toBigInt, trace } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import {
  Addr,
  CPS,
  ProgramState,
  Reference,
  Str,
  TWitness,
  TypeWithnessC,
  expect_some,
  internal_concat_str,
  internal_map_str_to_type,
  internal_map_type_to_str,
} from "./common_utils";
import { deref, put_deref } from "./reference_utils";
import { Signer, address_of } from "./signer_utils";

/** Defined internally in Move: `Coin<tw_coin_type> { value: Int }` */
export type Coin = ConStr0<[Integer]>;

export function Coin(value: Integer): Coin {
  return { constructor: 0, fields: [value] };
}

/**
 * `if data is Coin<Data>`: constructor 0 with exactly one `Int` field.
 * Any other single-`Int` record passes too (same as in Aiken), which is why the callers
 * also check the `__COIN__` key prefix
 */
export function is_coin(data: Data): data is Coin {
  return isConstr(data) && Number(data.constructor) === 0 && data.fields.length === 1 && isInt(data.fields[0]);
}

/** `expect coin: Coin<Data> = data` */
function expect_coin(data: Data, pattern: string): Coin {
  if (!is_coin(data)) {
    return expect_fail(pattern);
  }
  return data;
}

function coin_value(coin: Coin): bigint {
  return toBigInt(coin.fields[0]);
}

const coin_address_prefix: Str = str("__COIN__");

const lovelace_resource_type: TWitness = TypeWithnessC(str("lovelace"), []);

/**
 * Defined internally in Move, creates a coin from two coins of the same type
 * Does never abort since Move already ensures that the CoinType(s) correspond
 */
export function merge(dst_coin: Reference<Coin>, source_coin: Coin, _tw_coin_type: TWitness, cps: CPS): [Void, CPS] {
  const value1 = coin_value(expect_coin(deref(dst_coin, cps), "Coin { value: value1 }: Coin<Data> = reference_utils.deref(dst_coin, cps)"));

  const updated = put_deref(dst_coin, [], Coin(int(value1 + coin_value(source_coin))), cps);

  return [VOID, updated];
}

/** Defined internally in Move, uses CoinStore */
export function deposit(account_addr: Addr, coin: Coin, coin_type: TWitness, cps: CPS): [Void, CPS] {
  const type_addr = internal_concat_str([
    coin_address_prefix,
    internal_map_type_to_str(TypeWithnessC(str("Coin"), [coin_type])),
  ]);

  const [scopes, state] = cps;
  const [state_dict] = state.fields;
  const coin_data: Data = coin;

  const updated_state_dict = dict.insert_with(
    state_dict,
    account_addr,
    dict.from_pairs<Data>({ map: [{ k: type_addr, v: coin_data }] }),
    (_key, existing_value, new_value) =>
      dict.union_with(existing_value, new_value,
        // If the address already has some balance, add the coin value to it
        (_key, ex_coin_value, new_coin_value) => {
          const value1 = coin_value(expect_coin(ex_coin_value, "Coin { value: value1 }: Coin<Data> = ex_coin_value"));
          const value2 = coin_value(expect_coin(new_coin_value, "Coin { value: value2 }: Coin<Data> = new_coin_value"));

          const sum: Data = Coin(int(value1 + value2));
          return sum;
        },
      ),
  );

  return [VOID, [scopes, ProgramState(updated_state_dict)]];
}

/**
 * Updates the program state so to include all assets presents in the outputs
 * The ouputs parameter should not have zero-valued assets
 */
export function add_assets_to_state(state: ProgramState, outputs: Dict<Value>): ProgramState {
  return dict.foldr(outputs, state, (addr, value, state) => {
    const flattened = assets.flatten(value).filter(([_policy_id, _asset_name, qt]) => toBigInt(qt) > 0n);

    return flattened.reduceRight((state, [policy_id, asset_name, qt]) => {
      const is_ada = data_equals(policy_id, assets.ada_policy_id) && data_equals(asset_name, assets.ada_asset_name);

      // Depending if ADA or not, set the rource type Coin<coin_type>
      const coin_type: TWitness = is_ada
        ? lovelace_resource_type
        : TypeWithnessC(policy_id, [TypeWithnessC(asset_name, [])]);

      const [_scopes, new_state] = deposit(addr, Coin(qt), coin_type, [[], state])[1];

      return new_state;
    }, state);
  });
}

/**
 * Given the state, returns all the assets in it grouped by the target address
 * Similar to `assets_utils.map_outputs_to_assets`
 */
export function get_assets_from_state(state: ProgramState): Dict<Value> {
  const [state_dict] = state.fields;

  const res_dict: Dict<Value> = dict.filter(
    dict.map(state_dict, (_addr, sub_dict) =>
      dict.foldr(sub_dict, assets.zero, (resource_type, val, acc_value) => {
        if (!is_coin(val)) {
          return acc_value;
        }
        const coin_value_int = val.fields[0];

        // The second check is needed because should still be allowed to insert Coin structures in the state without considering them as assets
        if (!(bytearray.starts_with(resource_type, coin_address_prefix) && toBigInt(coin_value_int) > 0n)) {
          return acc_value;
        }

        // Retrieve the Coin<coin_type>
        const full_coin_type: TWitness = internal_map_str_to_type(
          bytearray.drop(resource_type, bytearray.length(coin_address_prefix)),
        );

        const [coin_constr, full_args] = full_coin_type.fields;
        if (full_args.list.length !== 1) {
          trace("full_coin_type", full_coin_type);
          return fail("Malformed coin type");
        }
        if (!data_equals(coin_constr, str("Coin"))) {
          trace("coin_constr", coin_constr);
          return fail("Coin constructor is not Coin");
        }

        // Retrieve the policy id and asset name from the single type argument
        const coin_type = full_args.list[0];
        const [policy_id, coin_type_args] = coin_type.fields;
        if (coin_type_args.list.length === 0) {
          if (data_equals(coin_type, lovelace_resource_type)) {
            return assets.add(acc_value, assets.ada_policy_id, assets.ada_asset_name, coin_value_int);
          }
          // The only asset that is identified by just a name is lovelace
          trace("coin_type", coin_type);
          return fail("Unexpected type name for coin_type");
        }
        if (coin_type_args.list.length === 1 && coin_type_args.list[0].fields[1].list.length === 0) {
          const [asset_name] = coin_type_args.list[0].fields;
          return assets.add(acc_value, policy_id, asset_name, coin_value_int);
        }
        // A coin cannot have more than one asset_name
        trace("resource_type", bytearray.drop(resource_type, bytearray.length(coin_address_prefix)));
        trace("coin_type", coin_type);
        return fail("Unexpected coin_type");
      }),
    ),
    (_addr, value) => !assets.is_zero(value),
  );

  return res_dict;
}

/** Given the state, removes all assets in it and returns the updated state */
export function remove_assets_from_state(state: ProgramState): ProgramState {
  const [state_dict] = state.fields;

  return ProgramState(
    dict.map(state_dict, (_addr, sub_dict) =>
      dict.filter(sub_dict, (resource_type, val) =>
        // See `get_assets_from_state`
        !(is_coin(val) && bytearray.starts_with(resource_type, coin_address_prefix)),
      ),
    ),
  );
}

/**
 * Deducts some amount from a coin, returning the new one as first place and the updated one as second place
 * Aborts if the input coin has not enough value
 */
export function extract(coin: Reference<Coin>, amount: Integer, _tw_coin_type: TWitness, cps: CPS): [Coin, CPS] {
  const value = coin_value(expect_coin(deref(coin, cps), "Coin { value }: Coin<Data> = reference_utils.deref(coin, cps)"));

  if (value < toBigInt(amount)) {
    return fail("Not enought Coin value to extract");
  }
  const updated_coin = Coin(int(value - toBigInt(amount)));
  const new_coin = Coin(amount);

  return [new_coin, put_deref(coin, [], updated_coin, cps)];
}

/**
 * Retrieves a certain amount of Coin<coin_type> from the signer's storage
 * Returns a pair with `(retrieved_coin, cps)`
 * Aborts if the signer has not enought amount of the Coin
 */
export function withdraw(signer: Reference<Signer>, amount: Integer, tw_coin_type: TWitness, cps: CPS): [Coin, CPS] {
  const type_addr = internal_concat_str([
    coin_address_prefix,
    internal_map_type_to_str(TypeWithnessC(str("Coin"), [tw_coin_type])),
  ]);

  const [signer_addr, cps_after] = address_of(signer, cps);

  const [scopes, state] = cps_after;
  const [state_dict] = state.fields;
  const existing_coin = expect_coin(
    expect_some(
      dict.get(expect_some(dict.get(state_dict, signer_addr), str("No address to withdraw from")), type_addr),
      str("No Coin to withdraw"),
    ),
    "existing_coin: Coin<Data> = ...",
  );

  if (coin_value(existing_coin) < toBigInt(amount)) {
    return fail("Not enought Coin value to withdraw");
  }
  const updated_coin = Coin(int(coin_value(existing_coin) - toBigInt(amount)));
  const new_coin = Coin(amount);

  const updated_state = ProgramState(
    dict.map(state_dict, (addr, sub_dict) =>
      data_equals(addr, signer_addr)
        ? // Override the existing key for the coin
          dict.insert<Data>(sub_dict, type_addr, updated_coin)
        : sub_dict,
    ),
  );

  return [new_coin, [scopes, updated_state]];
}

export function value(coin: Coin, _tw_coin_type: TWitness, cps: CPS): [Integer, CPS] {
  return [coin.fields[0], cps];
}

