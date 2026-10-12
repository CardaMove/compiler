/** Port of `lib/utils/signer_utils.ak` */
import { ConStr0 } from "@meshsdk/core";
import { Data, expect_fail, isBytes, isConstr } from "../stdlib/data";
import * as list from "../stdlib/list";
import { TxView } from "../stdlib/transaction";
import { Addr, CPS, Reference, internal_deref_scope } from "./common_utils";

/** Signer of the transaction: `Signer { address }` */
export type Signer = ConStr0<[Addr]>;

export function Signer(address: Addr): Signer {
  return { constructor: 0, fields: [address] };
}

/** `if data is Signer`: constructor 0 with a single `ByteArray` field */
export function is_signer(data: Data): data is Signer {
  return isConstr(data) && Number(data.constructor) === 0 && data.fields.length === 1 && isBytes(data.fields[0]);
}

/** Defined in Move standard library */
export function address_of(signer: Reference<Signer>, [scopes, gs]: CPS): [Addr, CPS] {
  const data = internal_deref_scope(signer, [scopes, gs]);
  if (!is_signer(data)) {
    return expect_fail("Signer { address } = common_utils.internal_deref_scope(signer, (scopes, gs))");
  }
  const [address] = data.fields;
  return [address, [scopes, gs]];
}

/** Used internally instead of passing a reference to the signer */
export function internal_address_of_unref(signer: Signer): Addr {
  const [address] = signer.fields;
  return address;
}

/** Returns the signer of a transaction */
export function get_tx_signer(tx: TxView): Signer {
  const signer_addr = list.at(tx.extra_signatories, 0);
  if (signer_addr === undefined) {
    return expect_fail("Some(signer_addr) = list.at(tx.extra_signatories, 0)");
  }
  return Signer(signer_addr);
}
