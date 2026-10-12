import { describe, expect, test } from "vitest";
import { as_data } from "../utils/common_utils";
import { int, isInt, str, toBigInt } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { GlobalStorageRef, ProgramState, Reference, ScopePtr, ScopeRef, TypeWithnessC, reference } from "../utils/common_utils";
import { deref, extend_ref, put_deref } from "../utils/reference_utils";
import { expectDataEqual, insert_all } from "./helpers";

describe("lib/tests/reference_utils_tests.ak", () => {
  test("extend_ref_appends_fields_test", () => {
    const ptr = ScopePtr(int(0), str("x"), int(0));
    const [, fields] = extend_ref(reference(ScopeRef(ptr), [int(1), int(2)]), [int(3), int(4)]).list;
    expectDataEqual(fields, { list: [int(1), int(2), int(3), int(4)] });
  });

  test("extend_ref_preserves_variant_test", () => {
    const tw = TypeWithnessC(str("Coin"), []);
    const ref: Reference = reference(GlobalStorageRef(str("addr1"), tw), [int(0)]);
    const [variant] = extend_ref(ref, [int(1)]).list;
    expectDataEqual(variant, GlobalStorageRef(str("addr1"), tw));
  });

  test("extend_ref_empty_extra_fields_test", () => {
    const ptr = ScopePtr(int(0), str("x"), int(0));
    const [, fields] = extend_ref(reference(ScopeRef(ptr), [int(1), int(2)]), []).list;
    expectDataEqual(fields, { list: [int(1), int(2)] });
  });

  test("deref_global_storage_ref_with_fields_test", () => {
    // Store ScopePtr(99, "k", 3) at state["addr1"]["Resource"]
    // A Reference with fields [2] should read binding_idx = 3
    const stored = as_data(ScopePtr(int(99), str("k"), int(3)));
    const tw = TypeWithnessC(str("Resource"), []);
    const addr = str("addr1");
    const state = ProgramState(insert_all(dict.empty(), [[addr, insert_all(dict.empty(), [[str("Resource"), stored]])]]));
    const ref: Reference = reference(GlobalStorageRef(addr, tw), [int(2)]);
    const val = deref(ref, [[], state]);
    expect(isInt(val) && toBigInt(val)).toBe(3n);
  });

  test("put_deref_global_storage_ref_with_fields_test", () => {
    // Same setup: overwrite binding_idx (field [2]) from 3 to 99
    const stored = as_data(ScopePtr(int(99), str("k"), int(3)));
    const tw = TypeWithnessC(str("Resource"), []);
    const addr = str("addr1");
    const state = ProgramState(insert_all(dict.empty(), [[addr, insert_all(dict.empty(), [[str("Resource"), stored]])]]));
    const ref: Reference = reference(GlobalStorageRef(addr, tw), []);
    const new_cps = put_deref(ref, [int(2)], as_data(int(99)), [[], state]);
    const check_ref: Reference = reference(GlobalStorageRef(addr, tw), [int(2)]);
    const new_val = deref(check_ref, new_cps);
    expect(isInt(new_val) && toBigInt(new_val)).toBe(99n);
  });
});
