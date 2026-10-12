/**
 * Port of the parts of `cardano/assets` used by the library, on Mesh's `Value` data type
 * (`{ map: [{ k: policy_id, v: { map: [{ k: asset_name, v: quantity }] } }] }`), which is also
 * exactly how Aiken encodes a `Value` as data.
 *
 * `MeshValue` is not used, because:
 * - it keys assets by the concatenated unit `policy_id + asset_name` and splits it back assuming a
 *   28-byte policy id, so it cannot hold the arbitrary policy ids used by the Aiken tests (`"my_policy"`);
 * - `toJSON()` goes through `Number(...)`, losing precision above 2^53;
 * - `eq()` is not symmetric, `merge()` mutates in place and nothing is sorted.
 *
 * Like in Aiken, a `Value` never holds zero quantities nor empty policies, and is sorted.
 */
import { Asset, ByteString, Integer, Value, byteString, parseAssetUnit } from "@meshsdk/core";
import { int, toBigInt } from "./data";
import * as dict from "./dict";

export const ada_policy_id: ByteString = byteString("");
export const ada_asset_name: ByteString = byteString("");

export const zero: Value = { map: [] };

export function from_asset(policy_id: ByteString, asset_name: ByteString, quantity: Integer): Value {
  if (toBigInt(quantity) === 0n) {
    return zero;
  }
  return dict.insert(dict.empty(), policy_id, dict.insert(dict.empty<Integer>(), asset_name, quantity));
}

export function from_lovelace(quantity: Integer): Value {
  return from_asset(ada_policy_id, ada_asset_name, quantity);
}

export function is_zero(self: Value): boolean {
  return self.map.length === 0;
}

function sum_non_zero(_k: ByteString, q1: Integer, q2: Integer): Integer | undefined {
  const q = toBigInt(q1) + toBigInt(q2);
  return q === 0n ? undefined : int(q);
}

export function add(self: Value, policy_id: ByteString, asset_name: ByteString, quantity: Integer): Value {
  if (toBigInt(quantity) === 0n) {
    return self;
  }
  return dict.insert_with(self, policy_id, dict.insert(dict.empty<Integer>(), asset_name, quantity), (_k, left) => {
    const inner = dict.insert_with(left, asset_name, quantity, sum_non_zero);
    return dict.is_empty(inner) ? undefined : inner;
  });
}

export function merge(left: Value, right: Value): Value {
  return dict.union_with(left, right, (_k, a0, a1) => {
    const result = dict.union_with(a0, a1, sum_non_zero);
    return dict.is_empty(result) ? undefined : result;
  });
}

export function negate(self: Value): Value {
  return dict.map(self, (_p, assets) => dict.map(assets, (_a, q) => int(-toBigInt(q))));
}

export function quantity_of(self: Value, policy_id: ByteString, asset_name: ByteString): bigint {
  const assets = dict.get(self, policy_id);
  const q = assets ? dict.get(assets, asset_name) : undefined;
  return q ? toBigInt(q) : 0n;
}

/** Flattens into `[policy_id, asset_name, quantity]` triples, in order */
export function flatten(self: Value): [ByteString, ByteString, Integer][] {
  return dict.foldr(self, [] as [ByteString, ByteString, Integer][], (policy_id, assets, acc) =>
    dict.foldr(assets, acc, (asset_name, quantity, xs) => [[policy_id, asset_name, quantity], ...xs]),
  );
}

export function reduce<R>(
  self: Value,
  start: R,
  with_: (policy_id: ByteString, asset_name: ByteString, quantity: Integer, acc: R) => R,
): R {
  return dict.foldr(self, start, (policy_id, assets, acc) =>
    dict.foldr(assets, acc, (asset_name, quantity, r) => with_(policy_id, asset_name, quantity, r)),
  );
}

// ---------------------------------------------------------------------------
// Conversions to and from Mesh `Asset[]` (not in Aiken)
// ---------------------------------------------------------------------------

/** Builds a `Value` from Mesh assets (units are `"lovelace"` or `policy_id + asset_name` hex) */
export function from_mesh_assets(assets: Asset[]): Value {
  return assets.reduce((value, { unit, quantity }) => {
    if (unit === "lovelace" || unit === "") {
      return add(value, ada_policy_id, ada_asset_name, int(BigInt(quantity)));
    }
    const { policyId, assetName } = parseAssetUnit(unit);
    return add(value, byteString(policyId.toLowerCase()), byteString(assetName.toLowerCase()), int(BigInt(quantity)));
  }, zero);
}

/** Converts a `Value` into Mesh assets, keeping `bigint` precision in the quantity strings */
export function to_mesh_assets(self: Value): Asset[] {
  return flatten(self).map(([policy_id, asset_name, quantity]) => ({
    unit: policy_id.bytes === "" && asset_name.bytes === "" ? "lovelace" : policy_id.bytes + asset_name.bytes,
    quantity: toBigInt(quantity).toString(),
  }));
}
