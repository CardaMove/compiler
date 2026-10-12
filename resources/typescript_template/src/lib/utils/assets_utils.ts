/** Port of `lib/utils/assets_utils.ak` */
import { Dict, Integer, Value } from "@meshsdk/core";
import * as assets from "../stdlib/assets";
import { data_equals, fail, toBigInt, trace } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { TxOutput } from "../stdlib/transaction";
import { Addr } from "./common_utils";
import { Signer, internal_address_of_unref } from "./signer_utils";

/**
 * Given a list of outputs, returns an association between each target address and its values
 * Zero-values outputs will not be returned
 */
export function map_outputs_to_assets(outputs: TxOutput[]): Dict<Value> {
  const empty_dict: Dict<Value> = dict.empty();

  return outputs
    .filter((output) => !assets.is_zero(output.value))
    .reduce((acc, output) => {
      // VerificationKey(hash) | Script(hash) -> hash
      const target_addr: Addr = output.address.fields[0].fields[0];

      // Remove from `output.value` all zero-quantity assets
      // The result is always a non-zero value since zero values have already been removed before this reduce
      const non_empty_value: Value = assets.reduce(output.value, assets.zero, (policy_id, asset_name, qt, value) =>
        toBigInt(qt) <= 0n ? value : assets.add(value, policy_id, asset_name, qt),
      );

      return dict.insert_with(acc, target_addr, non_empty_value, (_addr, existing_value, new_value) =>
        assets.merge(existing_value, new_value),
      );
    }, empty_dict);
}

/**
 * Checks if two assets dicts are equal
 * Zero-valued assets will be considered, but should not be present for the application logic
 */
export function compare_assets(assets_a: Dict<Value>, assets_b: Dict<Value>): boolean {
  if (data_equals(assets_a, assets_b)) {
    trace("Assets match");
    return true;
  }
  trace("assets_a", assets_a);
  trace("assets_b", assets_b);
  return fail("Assets do not match");
}

/**
 * Assuming fees are always payed by the transaction signer, removes from his assets the required fees
 * Note: This might cause the total assets of the signer to result negative,
 * and since this cannot correspond to the actual output assets of the signer, the final comparison will fail as expected
 */
export function deduce_transaction_fees_from_assets(assets_dict: Dict<Value>, signer: Signer, fee: Integer): Dict<Value> {
  const signer_address: Addr = internal_address_of_unref(signer);

  return dict.map(assets_dict, (addr, existing_value) =>
    data_equals(addr, signer_address)
      ? // Deduce the fees
        assets.merge(assets.negate(assets.from_lovelace(fee)), existing_value)
      : existing_value,
  );
}
