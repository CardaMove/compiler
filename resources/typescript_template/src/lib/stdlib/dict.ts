/**
 * Port of `aiken/collection/dict` on Mesh's `Dict<V>` shape (`{ map: [{ k, v }] }`, `ByteString` keys).
 *
 * Mesh's `assocMap`/`pairs`/`dict` keep insertion order and never sort or de-duplicate keys,
 * while an Aiken `Dict` is always sorted by key bytes and equality depends on that.
 * Every function here keeps the entries sorted and follows the stdlib implementation step by step
 * (e.g. `from_pairs` keeps the *first* occurrence of a duplicated key).
 * Functions never mutate their input.
 */
import { ByteString, Dict } from "@meshsdk/core";
import { compare } from "./bytearray";

type Entry<V> = { k: ByteString; v: V };

export function empty<V>(): Dict<V> {
  return { map: [] };
}

export function is_empty<V>(self: Dict<V>): boolean {
  return self.map.length === 0;
}

export function get<V>(self: Dict<V>, key: ByteString): V | undefined {
  return self.map.find((e) => compare(e.k, key) === 0)?.v;
}

export function has_key<V>(self: Dict<V>, key: ByteString): boolean {
  return self.map.some((e) => compare(e.k, key) === 0);
}

function do_insert_with<V>(
  self: Entry<V>[],
  k: ByteString,
  v: V,
  with_: (k: ByteString, newValue: V, existing: V) => V | undefined,
): Entry<V>[] {
  const i = self.findIndex((e) => compare(k, e.k) <= 0);
  if (i === -1) {
    return [...self, { k, v }];
  }
  if (compare(k, self[i].k) < 0) {
    return [...self.slice(0, i), { k, v }, ...self.slice(i)];
  }
  const combined = with_(k, v, self[i].v);
  return combined === undefined
    ? [...self.slice(0, i), ...self.slice(i + 1)]
    : [...self.slice(0, i), { k, v: combined }, ...self.slice(i + 1)];
}

/** Inserts or replaces */
export function insert<V>(self: Dict<V>, key: ByteString, value: V): Dict<V> {
  return { map: do_insert_with(self.map, key, value, (_k, v) => v) };
}

/**
 * Inserts, or combines with the existing value using `with_(key, existing, new)`.
 * Returning `undefined` (Aiken `None`) removes the key
 */
export function insert_with<V>(
  self: Dict<V>,
  key: ByteString,
  value: V,
  with_: (key: ByteString, existing: V, newValue: V) => V | undefined,
): Dict<V> {
  return { map: do_insert_with(self.map, key, value, (k, v1, v2) => with_(k, v2, v1)) };
}

function do_delete<V>(self: Dict<V>, key: ByteString): Dict<V> {
  return { map: self.map.filter((e) => compare(e.k, key) !== 0) };
}
export { do_delete as delete };

/** Builds a dict from possibly unsorted pairs. On duplicated keys the first occurrence wins */
export function from_pairs<V>(pairs: { map: Entry<V>[] }): Dict<V> {
  return { map: pairs.map.reduceRight<Entry<V>[]>((acc, { k, v }) => do_insert_with(acc, k, v, (_k, n) => n), []) };
}

export function to_pairs<V>(self: Dict<V>): { map: Entry<V>[] } {
  return { map: [...self.map] };
}

function do_map<A, B>(self: Dict<A>, with_: (key: ByteString, value: A) => B): Dict<B> {
  return { map: self.map.map(({ k, v }) => ({ k, v: with_(k, v) })) };
}
export { do_map as map };

export function filter<V>(self: Dict<V>, with_: (key: ByteString, value: V) => boolean): Dict<V> {
  return { map: self.map.filter(({ k, v }) => with_(k, v)) };
}

/** Folds from the last key to the first one: `with_(k1, v1, with_(k2, v2, zero))` */
export function foldr<V, R>(self: Dict<V>, zero: R, with_: (key: ByteString, value: V, acc: R) => R): R {
  return self.map.reduceRight((acc, { k, v }) => with_(k, v, acc), zero);
}

/** Union where `left` wins on duplicated keys */
export function union<V>(left: Dict<V>, right: Dict<V>): Dict<V> {
  return {
    map: left.map.reduce((acc, { k, v }) => do_insert_with(acc, k, v, (_k, l) => l), right.map),
  };
}

/** Union combining duplicated keys with `with_(key, left, right)`. Returning `undefined` removes the key */
export function union_with<V>(
  left: Dict<V>,
  right: Dict<V>,
  with_: (key: ByteString, leftValue: V, rightValue: V) => V | undefined,
): Dict<V> {
  return { map: left.map.reduce((acc, { k, v }) => do_insert_with(acc, k, v, with_), right.map) };
}
