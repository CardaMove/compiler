import { describe, expect, test } from "vitest";
import { AikenError, int, isInt, str, toBigInt } from "../stdlib/data";
import {
  ScopePtr,
  ScopeRef,
  TWitness,
  TypeWithnessC,
  as_data,
  assert,
  consume_data,
  expect_some,
  internal_concat_str,
  internal_get_data,
  internal_map_str_to_type,
  internal_map_type_to_str,
} from "../utils/common_utils";
import { expectDataEqual } from "./helpers";

describe("lib/tests/common_utils_tests.ak", () => {
  test("internal_concat_str_test", () => {
    const a = str("Hello");
    const b = str(" ");
    const c = str("world!");

    expectDataEqual(internal_concat_str([a, b, c]), str("Hello world!"));
  });

  test("internal_map_type_to_str_test", () => {
    const t: TWitness = TypeWithnessC(str("List"), [
      TypeWithnessC(str("List"), [
        TypeWithnessC(str("Pair"), [
          TypeWithnessC(str("List"), [TypeWithnessC(str("Double"), [])]),
          TypeWithnessC(str("String"), []),
        ]),
      ]),
    ]);

    expectDataEqual(internal_map_type_to_str(t), str("List<List<Pair<List<Double>, String>>>"));
  });

  test("internal_map_type_to_str_test_2", () => {
    const t = TypeWithnessC(str("Pair"), [TypeWithnessC(str("Boolean"), []), TypeWithnessC(str("Double"), [])]);

    expectDataEqual(internal_map_type_to_str(t), str("Pair<Boolean, Double>"));
  });

  test("internal_map_type_to_str_test_3", () => {
    const t = TypeWithnessC(str("Oracle"), [TypeWithnessC(str("my_coin"), [])]);

    expectDataEqual(internal_map_type_to_str(t), str("Oracle<my_coin>"));
  });

  test("internal_map_type_to_str_test_4", () => {
    const t = TypeWithnessC(str("Zerary"), []);

    expectDataEqual(internal_map_type_to_str(t), str("Zerary"));
  });

  test("internal_map_str_to_type_test", () => {
    expectDataEqual(
      internal_map_str_to_type(str("List<List<Pair<List<Double>, String>>>")),
      TypeWithnessC(str("List"), [
        TypeWithnessC(str("List"), [
          TypeWithnessC(str("Pair"), [
            TypeWithnessC(str("List"), [TypeWithnessC(str("Double"), [])]),
            TypeWithnessC(str("String"), []),
          ]),
        ]),
      ]),
    );
    expectDataEqual(
      internal_map_str_to_type(str("Pair<Boolean, Double>")),
      TypeWithnessC(str("Pair"), [TypeWithnessC(str("Boolean"), []), TypeWithnessC(str("Double"), [])]),
    );
    expectDataEqual(
      internal_map_str_to_type(str("Oracle<my_coin>")),
      TypeWithnessC(str("Oracle"), [TypeWithnessC(str("my_coin"), [])]),
    );
    expectDataEqual(internal_map_str_to_type(str("Zerary")), TypeWithnessC(str("Zerary"), []));
  });

  test("assert_test_fail", () => {
    expect(() => assert(() => (1 as number) === 2)).toThrow("Assertion failed");
  });

  test("expect_some_test", () => {
    expect(toBigInt(expect_some(int(42), str("Does not throw error")))).toBe(42n);
  });

  test("expect_some_test_fail", () => {
    expect(() => consume_data(expect_some(undefined, str("This throws")))).toThrow("This throws");
    expect(() => expect_some(undefined, str("This throws"))).toThrow(AikenError);
  });

  test("internal_get_data_no_fields_test", () => {
    // Empty field list: data is returned as-is
    const data = as_data(ScopePtr(int(42), str("x"), int(7)));
    expectDataEqual(internal_get_data(data, []), data);
  });

  test("internal_get_data_single_field_test", () => {
    // ScopePtr(10, "y", 5) — field index 0 is the Int 10
    const val = internal_get_data(as_data(ScopePtr(int(10), str("y"), int(5))), [int(0)]);
    expect(isInt(val) && toBigInt(val)).toBe(10n);
  });

  test("internal_get_data_nested_fields_test", () => {
    // ScopeRef { resource: ScopePtr(10, "y", 5) }:
    // field [0] = the ScopePtr, field [0, 2] = binding_idx which is 5
    const data = as_data(ScopeRef(ScopePtr(int(10), str("y"), int(5))));
    const val = internal_get_data(data, [int(0), int(2)]);
    expect(isInt(val) && toBigInt(val)).toBe(5n);
  });
});
