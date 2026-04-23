use aiken/collection/dict.{Dict}
use aiken/collection/list
use cardano/address.{Script, VerificationKey}
use cardano/assets.{Lovelace, Value}
use cardano/transaction.{Output}
use utils/common_utils.{Addr}
use utils/signer_utils.{Signer}

/// Given a list of outputs, returns an association between each target address and its values
/// Zero-values outputs will not be returned
pub fn map_outputs_to_assets(outputs: List<Output>) -> Dict<Addr, Value> {
  let empty_dict: Dict<Addr, Value> = dict.empty

  outputs
    |> list.filter(fn(output) { !assets.is_zero(output.value) })
    |> list.reduce(
        empty_dict,
        fn(acc, output) {
          let target_addr: Addr =
            when output.address.payment_credential is {
              VerificationKey(hash) | Script(hash) -> hash
            }

          // Remove from `output.value` all zero-quantity assets
          // The result is always a non-zero value since zero values have already been removed before this reduce
          let non_empty_value: Value =
            output.value
              |> assets.reduce(
                  assets.zero,
                  fn(policy_id, asset_name, qt, value) {
                    if qt <= 0 {
                      value
                    } else {
                      assets.add(value, policy_id, asset_name, qt)
                    }
                  },
                )

          acc
            |> dict.insert_with(
                target_addr,
                non_empty_value,
                with: fn(_addr, existing_value, new_value) {
                  Some(assets.merge(existing_value, new_value))
                },
              )
        },
      )
}

/// Checks if two assets dicts are equal
/// Zero-valued assets will be considered, but should not be present for the application logic
pub fn compare_assets(
  assets_a: Dict<Addr, Value>,
  assets_b: Dict<Addr, Value>,
) -> Bool {
  if assets_a == assets_b {
    trace @"Assets match"
    True
  } else {
    trace assets_a
    trace assets_b
    fail @"Assets do not match"
  }
}

/// Assuming fees are always payed by the transaction signer, removes from his assets the required fees
/// Note: This might cause the total assets of the signer to result negative,
/// and since this cannot correspond to the actual output assets of the signer, the final comparison will fail as expected
pub fn deduce_transaction_fees_from_assets(
  assets: Dict<Addr, Value>,
  signer: Signer,
  fee: Lovelace,
) -> Dict<Addr, Value> {
  let signer_address: Addr =
    signer_utils.internal_signer_address_of_unref(signer)

  assets
    |> dict.map(
        fn(addr, existing_value) {
          if addr == signer_address {
            // Deduce the fees
            assets.from_lovelace(fee)
              |> assets.negate()
              |> assets.merge(existing_value)
          } else {
            existing_value
          }
        },
      )
}
