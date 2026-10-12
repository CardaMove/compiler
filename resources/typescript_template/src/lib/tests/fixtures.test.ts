/**
 * Parity tests against the real Aiken library.
 * Each scenario mirrors a trace in `lib/tests/fixtures_tests.ak`, and its CBOR must be byte-identical
 * to the one Aiken produced (`fixtures.json`, regenerated with `npm run fixtures`).
 */
import { describe, expect, test } from "vitest";
import { Dict, Value } from "@meshsdk/core";
import * as assets from "../stdlib/assets";
import { Data, bytes, fromCbor, int, str, toCbor } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import * as interval from "../stdlib/interval";
import {
  InlineDatum,
  NoDatum,
  Script,
  TxOutput,
  TxView,
  VerificationKey,
  address,
  output_reference,
  placeholder,
} from "../stdlib/transaction";
import * as assets_utils from "../utils/assets_utils";
import * as coin_utils from "../utils/coin_utils";
import { Coin } from "../utils/coin_utils";
import * as common_utils from "../utils/common_utils";
import {
  CPS,
  GlobalStorageRef,
  ProgramState,
  Reference,
  ScopePtr,
  ScopeRef,
  TypeWithnessC,
  reference,
} from "../utils/common_utils";
import * as global_storage_utils from "../utils/global_storage_utils";
import * as reference_utils from "../utils/reference_utils";
import * as scope_utils from "../utils/scope_utils";
import * as signer_utils from "../utils/signer_utils";
import { Signer } from "../utils/signer_utils";
import * as state_utils from "../utils/state_utils";
import { SerializedProgramState } from "../utils/state_utils";
import * as timestamp_utils from "../utils/timestamp_utils";
import fixtures from "./fixtures.json";
import { insert_all } from "./helpers";

/** `as_data(cps)`: a tuple is a list */
function cps_data([scopes, state]: CPS): Data {
  return { list: [{ list: scopes }, state] };
}

function sample_state(): ProgramState {
  return ProgramState(
    insert_all(dict.empty(), [
      [
        str("addr2"),
        insert_all<Data>(dict.empty(), [
          [str("key1"), int(0)],
          [str("key2"), str("Hello world")],
        ]),
      ],
      [
        str("addr1"),
        insert_all<Data>(dict.empty(), [
          [str("Oracle<lovelace>"), ScopePtr(int(1), str("x"), int(2))],
          [str("__COIN__Coin<lovelace>"), Coin(int(10))],
        ]),
      ],
    ]),
  );
}

function signer_scope(addr: string): CPS {
  return [[dict.insert(dict.empty(), str("signer"), { list: [Signer(str(addr))] })], sample_state()];
}

function signer_ref(): Reference<Signer> {
  return reference(ScopeRef(ScopePtr(int(1), str("signer"), int(1))), []);
}

const lovelace = TypeWithnessC(str("lovelace"), []);
const oracle_lovelace = TypeWithnessC(str("Oracle"), [lovelace]);

function common_scenarios(): Record<string, () => Data> {
  const nested = TypeWithnessC(str("Map"), [
    TypeWithnessC(str("List"), [TypeWithnessC(str("Int"), [])]),
    TypeWithnessC(str("Pair"), [TypeWithnessC(str("A"), []), TypeWithnessC(str("B"), [])]),
  ]);
  const gref = GlobalStorageRef(str("a"), TypeWithnessC(str("X"), []));
  const scope_ref = ScopeRef(ScopePtr(int(1), str("y"), int(2)));
  return {
    "common.type_to_str": () => common_utils.internal_map_type_to_str(nested),
    "common.str_to_type": () => common_utils.internal_map_str_to_type(str("Map<List<Int>,  Pair< A,B<C> >>")),
    "common.concat_str": () => common_utils.internal_concat_str([str("a"), str(""), str("bc")]),
    "common.put_data_retag": () => common_utils.internal_put_data(gref, [int(0)], str("b")),
    "common.put_data_out_of_range": () => common_utils.internal_put_data(gref, [int(5)], str("b")),
    "common.put_data_nested": () => common_utils.internal_put_data(scope_ref, [int(0), int(1)], str("z")),
    "common.get_data_nested": () => common_utils.internal_get_data(scope_ref, [int(0), int(1)]),
  };
}

function scope_scenarios(): Record<string, () => Data> {
  let cps: CPS = [[], ProgramState(dict.empty())];
  cps = scope_utils.push_scope(cps);
  cps = scope_utils.post_scope(str("x"), int(1), cps);
  cps = scope_utils.post_scope(str("x"), ScopePtr(int(1), str("a"), int(2)), cps);
  cps = scope_utils.push_scope(cps);
  cps = scope_utils.post_scope(str("y"), int(3), cps);
  const after_posts = cps;
  cps = scope_utils.put_scope(str("x"), [], int(4), cps);
  cps = scope_utils.pop_scope(cps);
  cps = scope_utils.put_scope(str("x"), [int(1)], str("b"), cps);
  const final_cps = cps;
  return {
    "scope.after_posts": () => cps_data(after_posts),
    "scope.get_x": () => scope_utils.get_scope(str("x"), after_posts),
    "scope.final": () => cps_data(final_cps),
  };
}

function reference_scenarios(): Record<string, () => Data> {
  const cps: CPS = [
    [
      dict.insert(dict.empty(), str("x"), { list: [int(1)] }),
      dict.insert(dict.empty(), str("y"), { list: [ScopePtr(int(7), str("q"), int(8))] }),
    ],
    sample_state(),
  ];
  const ref_y = reference_utils.make_ref(str("y"), [int(2)], cps);
  const gref: Reference = reference(GlobalStorageRef(str("addr1"), oracle_lovelace), []);
  return {
    "reference.make_ref_x": () => reference_utils.make_ref(str("x"), [], cps),
    "reference.make_ref_y": () => ref_y,
    "reference.extend_ref": () => reference_utils.extend_ref(ref_y, [int(3), int(4)]),
    "reference.deref_global": () => reference_utils.deref(reference(gref.list[0], [int(1)]), cps),
    "reference.put_deref_scope": () =>
      cps_data(
        reference_utils.put_deref(reference(ScopeRef(ScopePtr(int(0), str("y"), int(0))), [int(1)]), [], str("w"), cps),
      ),
    "reference.put_deref_global": () => cps_data(reference_utils.put_deref(gref, [int(2)], int(99), cps)),
  };
}

function global_storage_scenarios(): Record<string, () => Data> {
  const bet_lovelace = TypeWithnessC(str("Bet"), [lovelace]);
  return {
    "global.move_to_void": () =>
      global_storage_utils.move_to(signer_ref(), bet_lovelace, Coin(int(5)), signer_scope("addr3"))[0],
    "global.move_to": () =>
      cps_data(global_storage_utils.move_to(signer_ref(), bet_lovelace, Coin(int(5)), signer_scope("addr3"))[1]),
    "global.move_from": () => {
      const [resource, cps] = global_storage_utils.move_from(str("addr1"), oracle_lovelace, [[], sample_state()]);
      return { list: [resource, cps_data(cps)] };
    },
    "global.internal_move_to": () =>
      cps_data(
        global_storage_utils.internal_move_to(str("addr2"), TypeWithnessC(str("key1"), []), str("new"), [
          [],
          sample_state(),
        ]),
      ),
    "global.borrow_global": () =>
      global_storage_utils.borrow_global(str("addr2"), TypeWithnessC(str("key2"), []), [[], sample_state()]),
  };
}

function signer_scenarios(): Record<string, () => Data> {
  const tx: TxView = { ...placeholder(), extra_signatories: [str("first_signer"), str("second_signer")] };
  return {
    "signer.address_of": () => signer_utils.address_of(signer_ref(), signer_scope("addr9"))[0],
    "signer.get_tx_signer": () => signer_utils.get_tx_signer(tx),
  };
}

function state_scenarios(): Record<string, () => Data> {
  // Unordered keys, and a duplicated key: the first occurrence wins
  const serialized: SerializedProgramState = {
    map: [
      {
        k: str("addr_b"),
        v: {
          map: [
            { k: str("k2"), v: int(2) },
            { k: str("k1"), v: int(1) },
          ],
        },
      },
      { k: str("addr_a"), v: { map: [] } },
      { k: str("addr_b"), v: { map: [{ k: str("k3"), v: int(3) }] } },
    ],
  };
  const state = state_utils.deserialize_state(serialized);

  const script_address = address(Script(str("script_hash")));
  const own_ref = output_reference("0011", 1);
  const output_state: SerializedProgramState = {
    map: [
      {
        k: str("zz"),
        v: {
          map: [
            { k: str("b"), v: int(1) },
            { k: str("a"), v: int(2) },
          ],
        },
      },
      { k: str("aa"), v: { map: [] } },
    ],
  };
  const tx: TxView = {
    ...placeholder(),
    inputs: [
      {
        output_reference: own_ref,
        output: { address: script_address, value: assets.from_lovelace(int(5)), datum: NoDatum() },
      },
    ],
    outputs: [
      {
        address: address(VerificationKey(str("someone"))),
        value: assets.from_lovelace(int(1)),
        datum: InlineDatum({ map: [{ k: str("ignored"), v: { list: [] } }] }),
      },
      { address: script_address, value: assets.from_lovelace(int(5)), datum: InlineDatum(output_state) },
    ],
  };

  return {
    "state.deserialize": () => state,
    "state.purge": () => state_utils.purge_state(state),
    "state.stringify": () => state_utils.stringify_program_state(sample_state()),
    "state.tx_output_state": () => state_utils.get_tx_output_state(own_ref, tx),
    "state.tx_input_state": () => state_utils.get_tx_input_state(serialized),
  };
}

function coin_scenarios(): Record<string, () => Data> {
  const outputs: Dict<Value> = insert_all<Value>(dict.empty(), [
    [
      str("addr1"),
      assets.merge(
        assets.from_lovelace(int(7)),
        assets.merge(
          assets.from_asset(bytes("abcd"), str("tok"), int(3)),
          assets.from_asset(bytes("abcd"), str("a"), int(2)),
        ),
      ),
    ],
    [str("addr4"), assets.from_lovelace(int(1))],
  ]);
  const with_assets = coin_utils.add_assets_to_state(sample_state(), outputs);
  const coin_ref: Reference<Coin> = reference(
    GlobalStorageRef(str("addr1"), TypeWithnessC(str("__COIN__Coin"), [lovelace])),
    [],
  );
  const direct_state = ProgramState(
    dict.insert(dict.empty(), str("addr1"), dict.insert<Data>(dict.empty(), str("__COIN__Coin<lovelace>"), Coin(int(10)))),
  );
  const [withdrawn, withdraw_cps] = coin_utils.withdraw(signer_ref(), int(4), lovelace, signer_scope("addr1"));
  const [extracted, extract_cps] = coin_utils.extract(coin_ref, int(3), lovelace, [[], direct_state]);
  return {
    "coin.add_assets_to_state": () => with_assets,
    "coin.get_assets_from_state": () => coin_utils.get_assets_from_state(with_assets),
    "coin.remove_assets_from_state": () => coin_utils.remove_assets_from_state(with_assets),
    "coin.deposit": () => coin_utils.deposit(str("addr1"), Coin(int(32)), lovelace, [[], sample_state()])[1][1],
    "coin.withdraw_coin": () => withdrawn,
    "coin.withdraw_cps": () => cps_data(withdraw_cps),
    "coin.extract_coin": () => extracted,
    "coin.extract_cps": () => cps_data(extract_cps),
    "coin.merge_cps": () => cps_data(coin_utils.merge(coin_ref, Coin(int(5)), lovelace, [[], direct_state])[1]),
  };
}

function assets_scenarios(): Record<string, () => Data> {
  const vk = (hash: string) => address(VerificationKey(str(hash)));
  const outputs: TxOutput[] = [
    { address: vk("b"), value: assets.from_asset(str("p"), str("n"), int(4)), datum: NoDatum() },
    { address: vk("a"), value: assets.from_lovelace(int(10)), datum: NoDatum() },
    {
      address: vk("b"),
      value: assets.merge(assets.from_lovelace(int(2)), assets.from_asset(str("p"), str("n"), int(1))),
      datum: NoDatum(),
    },
    { address: vk("c"), value: assets.zero, datum: NoDatum() },
  ];
  const mapped = assets_utils.map_outputs_to_assets(outputs);
  return {
    "assets.map_outputs_to_assets": () => mapped,
    // Fees larger than the signer's lovelace make the quantity negative
    "assets.deduce_fees": () => assets_utils.deduce_transaction_fees_from_assets(mapped, Signer(str("b")), int(5)),
  };
}

function timestamp_scenarios(): Record<string, () => Data> {
  const cps: CPS = [[dict.empty()], ProgramState(dict.empty())];
  const with_range = (range: interval.Interval): TxView => ({ ...placeholder(), validity_range: range });
  const negative = timestamp_utils.add_timestamp_to_cps(with_range(interval.between(-1001, -1000)), cps);
  return {
    "timestamp.odd_range": () =>
      cps_data(timestamp_utils.add_timestamp_to_cps(with_range(interval.between(1000, 1101)), cps)),
    // Aiken's `/` rounds towards negative infinity
    "timestamp.negative_range": () => cps_data(negative),
    "timestamp.now_seconds": () => timestamp_utils.now_seconds(negative)[0],
  };
}

const scenarios: Record<string, () => Data> = {
  ...common_scenarios(),
  ...scope_scenarios(),
  ...reference_scenarios(),
  ...global_storage_scenarios(),
  ...signer_scenarios(),
  ...state_scenarios(),
  ...coin_scenarios(),
  ...assets_scenarios(),
  ...timestamp_scenarios(),
};

const golden: Record<string, string> = fixtures;

describe("lib/tests/fixtures_tests.ak", () => {
  test("every Aiken fixture has a TS scenario, and vice versa", () => {
    expect(Object.keys(scenarios).sort()).toEqual(Object.keys(golden).sort());
  });

  test.each(Object.keys(golden))("%s matches the Aiken CBOR", (name) => {
    const scenario = scenarios[name];
    expect(scenario, `missing TS scenario for ${name}`).toBeDefined();
    expect(toCbor(scenario()).toLowerCase()).toBe(golden[name]);
  });

  test.each(Object.keys(golden))("%s decodes and re-encodes to the same CBOR", (name) => {
    expect(toCbor(fromCbor(golden[name])).toLowerCase()).toBe(golden[name]);
  });
});
