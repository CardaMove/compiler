import { describe, expect, test } from "vitest";
import { Dict, Value } from "@meshsdk/core";
import * as assets from "../stdlib/assets";
import { Data, int, str } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import {
  Coin,
  add_assets_to_state,
  extract,
  get_assets_from_state,
  merge,
  remove_assets_from_state,
  withdraw,
} from "../utils/coin_utils";
import {
  GlobalStorageRef,
  ProgramState,
  Reference,
  Scope,
  ScopePtr,
  ScopeRef,
  TypeWithnessC,
  consume_data,
  reference,
} from "../utils/common_utils";
import { Signer } from "../utils/signer_utils";
import { expectDataEqual, fixture_sub_state, insert_all, pairs_ab } from "./helpers";

function state_of(entries: [string, Dict<Data>][]): ProgramState {
  return ProgramState(insert_all(dict.empty(), entries.map(([addr, sub]) => [str(addr), sub])));
}

function sub_state_of(entries: [string, Data][]): Dict<Data> {
  return insert_all(dict.empty(), entries.map(([k, v]) => [str(k), v]));
}

function withdraw_fixture_state(): ProgramState {
  return state_of([
    ["addr1", sub_state_of([["key1", int(0)], ["__COIN__Coin<lovelace>", Coin(int(1000000))]])],
    [
      "addr2",
      sub_state_of([
        ["key1", int(0)],
        ["key4", pairs_ab(2)],
        ["__COIN__Coin<my_policy<my_asset>>", Coin(int(66))],
      ]),
    ],
  ]);
}

function signer_scope(addr: string): Scope {
  return dict.insert(dict.empty(), str("signer"), { list: [Signer(str(addr))] });
}

describe("lib/tests/coin_utils_tests.ak", () => {
  test("merge_test", () => {
    const tw = TypeWithnessC(str("Coin"), []);

    const initial_state = state_of([["addr1", sub_state_of([["Coin", Coin(int(12))]])]]);

    const coin_ref: Reference<Coin> = reference(GlobalStorageRef(str("addr1"), tw), []);

    const [, [, updated_state]] = merge(coin_ref, Coin(int(30)), tw, [[], initial_state]);

    expectDataEqual(updated_state, state_of([["addr1", sub_state_of([["Coin", Coin(int(42))]])]]));
  });

  test("add_assets_to_state_test", () => {
    const deserialized_state_1 = state_of([
      ["addr1", fixture_sub_state()],
      ["addr2", fixture_sub_state()],
    ]);

    const outputs = insert_all<Value>(dict.empty(), [
      [str("addr1"), assets.from_lovelace(int(1000000))],
      [str("addr2"), assets.from_asset(str("my_policy"), str("my_asset"), int(66))],
      [str("addr3"), assets.from_lovelace(int(3000000 + 1000001))],
      [str("script_hash"), assets.from_lovelace(int(5000000))],
    ]);

    const deserialized_sub_state_1 = dict.insert<Data>(fixture_sub_state(), str("__COIN__Coin<lovelace>"), Coin(int(1000000)));
    const deserialized_sub_state_2 = dict.insert<Data>(
      fixture_sub_state(),
      str("__COIN__Coin<my_policy<my_asset>>"),
      Coin(int(66)),
    );
    const deserialized_sub_state_3 = sub_state_of([["__COIN__Coin<lovelace>", Coin(int(3000000 + 1000001))]]);
    const deserialized_sub_state_script = sub_state_of([["__COIN__Coin<lovelace>", Coin(int(5000000))]]);

    expectDataEqual(
      add_assets_to_state(deserialized_state_1, outputs),
      state_of([
        ["addr1", deserialized_sub_state_1],
        ["addr2", deserialized_sub_state_2],
        ["addr3", deserialized_sub_state_3],
        ["script_hash", deserialized_sub_state_script],
      ]),
    );
  });

  test("get_assets_from_state_test", () => {
    const deserialized_sub_state_1 = insert_all<Data>(fixture_sub_state(), [
      [str("__COIN__Coin<lovelace>"), Coin(int(1000000))],
      [str("__COIN__Coin<my_policy<my_asset>>"), Coin(int(12))],
    ]);
    const deserialized_sub_state_2 = insert_all<Data>(fixture_sub_state(), [
      [str("__COIN__Coin<my_policy<my_asset>>"), Coin(int(66))],
      [str("__COIN__Coin<lovelace>"), Coin(int(0))],
    ]);
    const deserialized_sub_state_3 = sub_state_of([
      ["__COIN__Coin<lovelace>", Coin(int(3000000 + 1000001))],
      ["key1", Coin(int(12))],
    ]);
    const deserialized_sub_state_script = sub_state_of([["__COIN__Coin<lovelace>", Coin(int(5000000))]]);

    const program_state = state_of([
      ["addr1", deserialized_sub_state_1],
      ["addr2", deserialized_sub_state_2],
      ["addr3", deserialized_sub_state_3],
      ["script_hash", deserialized_sub_state_script],
    ]);

    const expected_assets = insert_all<Value>(dict.empty(), [
      [
        str("addr1"),
        assets.merge(assets.from_lovelace(int(1000000)), assets.from_asset(str("my_policy"), str("my_asset"), int(12))),
      ],
      [str("addr2"), assets.from_asset(str("my_policy"), str("my_asset"), int(66))],
      [str("addr3"), assets.from_lovelace(int(3000000 + 1000001))],
      [str("script_hash"), assets.from_lovelace(int(5000000))],
    ]);

    expectDataEqual(get_assets_from_state(program_state), expected_assets);
  });

  test("remove_assets_from_state_test", () => {
    const program_state = state_of([
      ["addr1", dict.insert<Data>(fixture_sub_state(), str("__COIN__Coin<lovelace>"), Coin(int(1000000)))],
      ["addr2", dict.insert<Data>(fixture_sub_state(), str("__COIN__Coin<my_policy<my_asset>>"), Coin(int(66)))],
      [
        "addr3",
        sub_state_of([
          ["__COIN__Coin<lovelace>", Coin(int(3000000 + 1000001))],
          ["key1", Coin(int(12))],
        ]),
      ],
      ["script_hash", sub_state_of([["__COIN__Coin<lovelace>", Coin(int(5000000))]])],
    ]);

    expectDataEqual(
      remove_assets_from_state(program_state),
      state_of([
        ["addr1", fixture_sub_state()],
        ["addr2", fixture_sub_state()],
        ["addr3", sub_state_of([["key1", Coin(int(12))]])],
        ["script_hash", dict.empty()],
      ]),
    );
  });

  test("extract_test", () => {
    const tw = TypeWithnessC(str("Coin"), []);

    const initial_state = state_of([["addr1", sub_state_of([["Coin", Coin(int(100))]])]]);

    const coin_ref: Reference<Coin> = reference(GlobalStorageRef(str("addr1"), tw), []);

    const [new_coin, [, updated_state]] = extract(coin_ref, int(20), tw, [[], initial_state]);

    expectDataEqual(new_coin, Coin(int(20)));
    expectDataEqual(updated_state, state_of([["addr1", sub_state_of([["Coin", Coin(int(80))]])]]));
  });

  test("extract_test_fail", () => {
    const tw = TypeWithnessC(str("Coin"), []);

    const initial_state = state_of([["addr1", sub_state_of([["Coin", Coin(int(100))]])]]);

    const coin_ref: Reference<Coin> = reference(GlobalStorageRef(str("addr1"), tw), []);

    expect(() => consume_data(extract(coin_ref, int(200), tw, [[], initial_state]))).toThrow(
      "Not enought Coin value to extract",
    );
  });

  test("withdraw_test", () => {
    const scope = signer_scope("addr2");

    const signer_ref: Reference<Signer> = reference(ScopeRef(ScopePtr(int(1), str("signer"), int(1))), []);

    const [withdrawn_coin, [updated_scopes, updated_state]] = withdraw(
      signer_ref,
      int(65),
      TypeWithnessC(str("my_policy"), [TypeWithnessC(str("my_asset"), [])]),
      [[scope], withdraw_fixture_state()],
    );

    const updated_deserialized_sub_state_2 = sub_state_of([
      ["key1", int(0)],
      ["key4", pairs_ab(2)],
      ["__COIN__Coin<my_policy<my_asset>>", Coin(int(1))],
    ]);

    expectDataEqual(withdrawn_coin, Coin(int(65)));
    expect(updated_scopes.length).toBe(1);
    expectDataEqual(updated_scopes[0], scope);
    expectDataEqual(
      updated_state,
      state_of([
        ["addr1", sub_state_of([["key1", int(0)], ["__COIN__Coin<lovelace>", Coin(int(1000000))]])],
        ["addr2", updated_deserialized_sub_state_2],
      ]),
    );
  });

  test("withdraw_test_fail", () => {
    const signer_ref: Reference<Signer> = reference(ScopeRef(ScopePtr(int(1), str("signer"), int(1))), []);

    expect(() =>
      consume_data(
        withdraw(
          signer_ref,
          int(65),
          TypeWithnessC(str("my_policy"), [TypeWithnessC(str("my_UNEXISTING_asset"), [])]),
          [[signer_scope("addr2")], withdraw_fixture_state()],
        ),
      ),
    ).toThrow("No Coin to withdraw");
  });

  test("withdraw_test_fail_2", () => {
    const signer_ref: Reference<Signer> = reference(ScopeRef(ScopePtr(int(1), str("signer"), int(1))), []);

    expect(() =>
      consume_data(
        withdraw(signer_ref, int(9999999), TypeWithnessC(str("lovelace"), []), [
          [signer_scope("addr1")],
          withdraw_fixture_state(),
        ]),
      ),
    ).toThrow("Not enought Coin value to withdraw");
  });
});
