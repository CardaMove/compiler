/** Port of `lib/utils/timestamp_utils.ak` */
import { Integer } from "@meshsdk/core";
import { expect_fail, fail, int, isInt, str, toBigInt } from "../stdlib/data";
import { TxView } from "../stdlib/transaction";
import { CPS, Str } from "./common_utils";
import { get_scope, post_scope } from "./scope_utils";

/** Key under which the current timestamp is stored in the CPS */
const timestamp_key: Str = str("__TIMESTAMP__");

/** Largest Tx validity range in milliseconds that is accepted for calculating the timestamp */
const timestamp_range_threshold = 120000n;

/** Aiken's `/` rounds towards negative infinity, while `bigint` division truncates towards zero */
function floor_div(a: bigint, b: bigint): bigint {
  const q = a / b;
  return a % b !== 0n && a < 0n !== b < 0n ? q - 1n : q;
}

/** Retrieves the current timestamp stored in the CPS */
export function now_seconds(cps: CPS): [Integer, CPS] {
  const timestamp = get_scope(timestamp_key, cps);
  if (!isInt(timestamp)) {
    return expect_fail("timestamp: Int = scope_utils.get_scope(timestamp_key, cps)");
  }

  return [timestamp, cps];
}

/**
 * Adds the current timestamp to the CPS when the transaction has a finite
 * validity range whose duration is below 120 seconds. The timestamp is the
 * midpoint of the lower and upper bounds. If either bound is missing, the CPS
 * is returned unchanged. A validity range at or above the threshold aborts.
 */
export function add_timestamp_to_cps(transaction: TxView, cps: CPS): CPS {
  const [lower_bound, upper_bound] = transaction.validity_range.fields;
  const [lower_type] = lower_bound.fields;
  const [upper_type] = upper_bound.fields;

  // Finite(bound) is constructor 1
  if (Number(lower_type.constructor) !== 1 || Number(upper_type.constructor) !== 1) {
    return cps;
  }
  const lower = toBigInt(lower_type.fields[0] as Integer);
  const upper = toBigInt(upper_type.fields[0] as Integer);

  const duration = upper - lower;
  if (duration >= timestamp_range_threshold) {
    return fail("Transaction validity range exceeds timestamp threshold");
  }
  return post_scope(timestamp_key, int(floor_div(lower + upper, 2n)), cps);
}
