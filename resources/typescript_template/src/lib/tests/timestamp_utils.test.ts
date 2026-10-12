import { describe, expect, test } from "vitest";
import { int, str, toBigInt } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import * as interval from "../stdlib/interval";
import * as transaction from "../stdlib/transaction";
import { CPS, ProgramState, Scope } from "../utils/common_utils";
import { add_timestamp_to_cps, now_seconds } from "../utils/timestamp_utils";
import { expectDataEqual } from "./helpers";

function transaction_with_range(range: interval.Interval): transaction.TxView {
  return { ...transaction.placeholder(), validity_range: range };
}

function expectCpsEqual([scopes_a, state_a]: CPS, [scopes_b, state_b]: CPS): void {
  expect(scopes_a.length).toBe(scopes_b.length);
  scopes_a.forEach((scope, i) => expectDataEqual(scope, scopes_b[i]));
  expectDataEqual(state_a, state_b);
}

describe("lib/tests/timestamp_utils_tests.ak", () => {
  test("timestamp_utils_test", () => {
    const scope: Scope = dict.insert(dict.empty(), str("__TIMESTAMP__"), { list: [int(42)] });
    const cps: CPS = [[scope], ProgramState(dict.empty())];

    const [timestamp] = now_seconds(cps);

    expect(toBigInt(timestamp)).toBe(42n);
  });

  test("now_seconds_reads_updated_timestamp", () => {
    const scope: Scope = dict.insert(dict.empty(), str("__TIMESTAMP__"), { list: [int(1000)] });
    const cps: CPS = [[scope], ProgramState(dict.empty())];

    const [timestamp, returned_cps] = now_seconds(cps);
    const [same_timestamp] = now_seconds(returned_cps);

    expect(toBigInt(timestamp)).toBe(1000n);
    expect(toBigInt(same_timestamp)).toBe(1000n);
  });

  test("add_timestamp_to_cps_ignores_unbounded_range", () => {
    const cps: CPS = [[dict.empty()], ProgramState(dict.empty())];
    const updated_cps = add_timestamp_to_cps(transaction.placeholder(), cps);

    expectCpsEqual(updated_cps, cps);
  });

  test("add_timestamp_to_cps_sets_midpoint_for_small_range", () => {
    const tx = transaction_with_range(interval.between(1000, 1100));
    const cps: CPS = [[dict.empty()], ProgramState(dict.empty())];

    const updated_cps = add_timestamp_to_cps(tx, cps);
    const [timestamp] = now_seconds(updated_cps);

    expect(toBigInt(timestamp)).toBe(1050n);
  });

  test("add_timestamp_to_cps_fails_for_large_range", () => {
    const tx = transaction_with_range(interval.between(1000, 1000000));
    const cps: CPS = [[dict.empty()], ProgramState(dict.empty())];

    expect(() => add_timestamp_to_cps(tx, cps)).toThrow("Transaction validity range exceeds timestamp threshold");
  });
});
