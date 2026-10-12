/** Shared helpers for the ported Aiken tests */
import { Dict, ByteString } from "@meshsdk/core";
import { expect } from "vitest";
import { Data, data_equals, int, normalize_data, str } from "../stdlib/data";
import * as dict from "../stdlib/dict";

/** Aiken `==` on data. On mismatch, Vitest prints a readable diff of the normalized values */
export function expectDataEqual(actual: Data, expected: Data): void {
  if (!data_equals(actual, expected)) {
    expect(normalize_data(actual)).toEqual(normalize_data(expected));
  }
  expect(data_equals(actual, expected)).toBe(true);
}

/** `dict.empty |> dict.insert(k1, v1) |> dict.insert(k2, v2) ...` */
export function insert_all<V>(d: Dict<V>, entries: [ByteString, V][]): Dict<V> {
  return entries.reduce((acc, [k, v]) => dict.insert(acc, k, v), d);
}

/** `as_data([Pair("a", 1), Pair("b", b)])` */
export function pairs_ab(b: number): Data {
  return {
    map: [
      { k: str("a"), v: int(1) },
      { k: str("b"), v: int(b) },
    ],
  };
}

/** The sub-state that most Aiken tests build: key1..key4 */
export function fixture_sub_state(): Dict<Data> {
  return insert_all<Data>(dict.empty(), [
    [str("key1"), int(0)],
    [str("key2"), str("Hello world")],
    [str("key3"), int(42)],
    [str("key4"), pairs_ab(2)],
  ]);
}
