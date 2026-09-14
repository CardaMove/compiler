use aiken/interval.{Finite}
use cardano/transaction.{Transaction}
use utils/common_utils.{CPS, Str}
use utils/scope_utils

/// Key under which the current timestamp is stored in the CPS
const timestamp_key: Str = "__TIMESTAMP__"

/// Largest Tx validity range in milliseconds that is accepted for calculating the timestamp
const timestamp_range_threshold: Int = 120000

/// Retrieves the current timestamp stored in the CPS
pub fn now_seconds(cps: CPS) -> (Int, CPS) {
  expect timestamp: Int = scope_utils.get_scope(timestamp_key, cps)

  (timestamp, cps)
}

/// Adds the current timestamp to the CPS when the transaction has a finite
/// validity range whose duration is below 120 seconds. The timestamp is the
/// midpoint of the lower and upper bounds. If either bound is missing, the CPS
/// is returned unchanged. A validity range at or above the threshold aborts.
pub fn add_timestamp_to_cps(transaction: Transaction, cps: CPS) -> CPS {
  let validity_range = transaction.validity_range

  when
    (
      validity_range.lower_bound.bound_type,
      validity_range.upper_bound.bound_type,
    )
  is {
    (Finite(lower_bound), Finite(upper_bound)) -> {
      let duration = upper_bound - lower_bound
      if duration >= timestamp_range_threshold {
        fail @"Transaction validity range exceeds timestamp threshold"
      } else {
        scope_utils.post_scope(
          timestamp_key,
          ( lower_bound + upper_bound ) / 2,
          cps,
        )
      }
    }
    _ -> cps
  }
}
