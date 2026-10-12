import { describe, expect, test } from "vitest";
import { AikenError, Data, int, str } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { CPS, ProgramState, TypeWithnessC } from "../utils/common_utils";
import { borrow_global, exists, move_from } from "../utils/global_storage_utils";
import { expectDataEqual, fixture_sub_state, insert_all } from "./helpers";

function fixture_state(): ProgramState {
  return ProgramState(
    insert_all(dict.empty(), [
      [str("addr1"), fixture_sub_state()],
      [str("addr2"), fixture_sub_state()],
    ]),
  );
}

describe("lib/tests/global_storage_utils_tests.ak", () => {
  test("move_from_test", () => {
    const deserialized_state = fixture_state();

    const [resource, [scopes, state]]: [Data, CPS] = move_from(
      str("addr1"),
      TypeWithnessC(str("key3"), []),
      [[], deserialized_state],
    );

    const deserialized_sub_state_1 = dict.delete(fixture_sub_state(), str("key3"));

    expectDataEqual(resource, int(42));
    expect(scopes).toEqual([]);
    expectDataEqual(
      state,
      ProgramState(
        insert_all(dict.empty(), [
          [str("addr1"), deserialized_sub_state_1],
          [str("addr2"), fixture_sub_state()],
        ]),
      ),
    );
  });

  test("move_from_test_fail", () => {
    // Not exists
    expect(() => move_from(str("addr1"), TypeWithnessC(str("key5"), []), [[], fixture_state()])).toThrow(AikenError);
  });

  test("borrow_global_test_fail", () => {
    // Not exists
    expect(() => borrow_global(str("addr2"), TypeWithnessC(str("key5"), []), [[], fixture_state()])).toThrow(
      "borrow_global invoked for non existing resource",
    );
  });

  test("exists_test", () => {
    expect(exists(str("addr1"), TypeWithnessC(str("key2"), []), [[], fixture_state()])).toBe(true);
  });

  test("exists_test_2", () => {
    // A `fail` test in Aiken because it returns False: the resource does not exist
    expect(exists(str("addr1"), TypeWithnessC(str("key5"), []), [[], fixture_state()])).toBe(false);
  });
});
