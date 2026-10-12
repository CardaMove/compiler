import { describe, expect, test } from "vitest";
import { Data, int, str } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { ProgramState } from "../utils/common_utils";
import { SerializedProgramState, compare_tx_states, deserialize_state, purge_state } from "../utils/state_utils";
import { expectDataEqual, fixture_sub_state, insert_all, pairs_ab } from "./helpers";

function fixture_state(): ProgramState {
  return ProgramState(
    insert_all(dict.empty(), [
      [str("addr1"), fixture_sub_state()],
      [str("addr2"), fixture_sub_state()],
    ]),
  );
}

describe("lib/tests/state_utils_tests.ak", () => {
  test("compare_tx_states_test", () => {
    expect(compare_tx_states(fixture_state(), fixture_state())).toBe(true);
  });

  test("compare_tx_states_test_fail", () => {
    const deserialized_sub_state_2 = dict.insert<Data>(fixture_sub_state(), str("key4"), pairs_ab(3));

    // Different
    const deserialized_state_2 = ProgramState(
      insert_all(dict.empty(), [
        [str("addr1"), fixture_sub_state()],
        [str("addr2"), deserialized_sub_state_2],
      ]),
    );

    expect(() => compare_tx_states(fixture_state(), deserialized_state_2)).toThrow("States do not match");
  });

  test("deserialize_state_test", () => {
    const sub_pairs = {
      map: [
        { k: str("key1"), v: int(0) },
        { k: str("key2"), v: str("Hello world") },
        { k: str("key3"), v: int(42) },
        { k: str("key4"), v: pairs_ab(2) },
      ],
    };

    const serialized_state: SerializedProgramState = {
      map: [
        { k: str("addr1"), v: sub_pairs },
        { k: str("addr2"), v: sub_pairs },
      ],
    };

    expectDataEqual(deserialize_state(serialized_state), fixture_state());
  });

  test("deserialize_state_test_unordered", () => {
    // Keys can also be unordered
    const sub_pairs = {
      map: [
        { k: str("key1"), v: int(0) },
        { k: str("key3"), v: int(42) },
        { k: str("key4"), v: pairs_ab(2) },
        { k: str("key2"), v: str("Hello world") },
      ],
    };

    const serialized_state: SerializedProgramState = {
      map: [
        { k: str("addr1"), v: sub_pairs },
        { k: str("addr2"), v: sub_pairs },
      ],
    };

    expectDataEqual(deserialize_state(serialized_state), fixture_state());
  });

  test("purge_state_test", () => {
    const deserialized_state_1 = ProgramState(
      insert_all(dict.empty(), [
        [str("addr1"), fixture_sub_state()],
        [str("addr2"), fixture_sub_state()],
        [str("addr3"), dict.empty()],
        [str("addr4"), dict.delete(dict.insert<Data>(dict.empty(), str("sub_key"), int(12)), str("sub_key"))],
      ]),
    );

    expectDataEqual(purge_state(deserialized_state_1), fixture_state());
  });
});
