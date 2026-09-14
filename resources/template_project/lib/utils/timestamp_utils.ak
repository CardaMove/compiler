use utils/common_utils.{CPS, Str}
use utils/scope_utils

const timestamp_key: Str = "__TIMESTAMP__"

/// Retrieves the current timestamp stored in the CPS
pub fn now_seconds(cps: CPS) -> (Int, CPS) {
  expect timestamp: Int = scope_utils.get_scope(timestamp_key, cps)

  (timestamp, cps)
}
