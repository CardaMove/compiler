/**
 * The parts of `cardano/transaction` and `cardano/address` read by the library.
 *
 * `TxView` holds only the `Transaction` fields the library uses. Each field has the same shape
 * as in Aiken's `Transaction` (addresses and datums as Plutus data, POSIX-millisecond validity range),
 * so off-chain code sees exactly what the validator sees. Mesh types can't be used directly:
 * bech32 addresses can't hold the fake hashes used by the tests (`"addr1"`), and Mesh's
 * `ValidityRange` is expressed in slots. `tx_view_from_mesh_body` converts a Mesh tx-builder body.
 */
import {
  ByteString,
  ConStr0,
  ConStr1,
  ConStr2,
  Integer,
  MeshTxBuilderBody,
  Option,
  Output,
  OutputReference,
  PlutusData,
  SlotConfig,
  Value,
  byteString,
  deserializeAddress,
  resolveDataHash,
  serializeData,
  slotToBeginUnixTime,
} from "@meshsdk/core";
import * as assets from "./assets";
import { Data, data_equals, fromCbor, int } from "./data";
import * as interval from "./interval";

// ---------------------------------------------------------------------------
// cardano/address
// ---------------------------------------------------------------------------

export type VerificationKeyCredential = ConStr0<[ByteString]>;
export type ScriptCredential = ConStr1<[ByteString]>;
export type Credential = VerificationKeyCredential | ScriptCredential;
/** `Address { payment_credential, stake_credential: Option<StakeCredential> }`. The stake part is opaque here */
export type Address = ConStr0<[Credential, Option<PlutusData>]>;

export const VerificationKey = (hash: ByteString): VerificationKeyCredential => ({ constructor: 0, fields: [hash] });
export const Script = (hash: ByteString): ScriptCredential => ({ constructor: 1, fields: [hash] });

export function address(payment_credential: Credential, stake_credential?: PlutusData): Address {
  return {
    constructor: 0,
    fields: [
      payment_credential,
      stake_credential === undefined ? { constructor: 1, fields: [] } : { constructor: 0, fields: [stake_credential] },
    ],
  };
}

/** Converts a bech32 Shelley address into its Plutus data form (payment and inline stake credentials) */
export function address_from_bech32(bech32: string): Address {
  const { pubKeyHash, scriptHash, stakeCredentialHash, stakeScriptCredentialHash } = deserializeAddress(bech32);
  const payment = pubKeyHash ? VerificationKey(byteString(pubKeyHash)) : Script(byteString(scriptHash));
  const stake = stakeCredentialHash
    ? VerificationKey(byteString(stakeCredentialHash))
    : stakeScriptCredentialHash
      ? Script(byteString(stakeScriptCredentialHash))
      : undefined;
  // StakeCredential = Referenced<Credential>, where Inline(credential) is constructor 0
  return address(payment, stake === undefined ? undefined : { constructor: 0, fields: [stake] });
}

// ---------------------------------------------------------------------------
// cardano/transaction
// ---------------------------------------------------------------------------

export type NoDatum = ConStr0<[]>;
export type DatumHash = ConStr1<[ByteString]>;
export type InlineDatum = ConStr2<[Data]>;
export type Datum = NoDatum | DatumHash | InlineDatum;

export const NoDatum = (): NoDatum => ({ constructor: 0, fields: [] });
export const InlineDatum = (data: Data): InlineDatum => ({ constructor: 2, fields: [data] });

export interface TxOutput {
  address: Address;
  value: Value;
  datum: Datum;
}

export interface TxInput {
  output_reference: OutputReference;
  output: TxOutput;
}

export interface TxView {
  inputs: TxInput[];
  outputs: TxOutput[];
  fee: Integer;
  validity_range: interval.Interval;
  extra_signatories: ByteString[];
}

export function output_reference(transaction_id: string, output_index: number | bigint): OutputReference {
  return { constructor: 0, fields: [byteString(transaction_id.toLowerCase()), int(output_index)] };
}

/** `transaction.placeholder`, restricted to the `TxView` fields */
export function placeholder(): TxView {
  return { inputs: [], outputs: [], fee: int(0), validity_range: interval.everything(), extra_signatories: [] };
}

export function find_input(inputs: TxInput[], output_reference: OutputReference): TxInput | undefined {
  return inputs.find((input) => data_equals(input.output_reference, output_reference));
}

// ---------------------------------------------------------------------------
// Adapter from Mesh
// ---------------------------------------------------------------------------

function datum_from_mesh(datum: Output["datum"]): Datum {
  if (!datum) {
    return NoDatum();
  }
  const cbor = datum.data.type === "CBOR" ? datum.data.content : serializeData(datum.data.content, datum.data.type);
  if (datum.type === "Hash") {
    // Only the hash is visible on-chain
    return { constructor: 1, fields: [byteString(resolveDataHash(cbor, "CBOR"))] };
  }
  return InlineDatum(fromCbor(cbor));
}

export function output_from_mesh(output: Output): TxOutput {
  return {
    address: address_from_bech32(output.address),
    value: assets.from_mesh_assets(output.amount),
    datum: datum_from_mesh(output.datum),
  };
}

/**
 * Builds a `TxView` from a (completed) Mesh tx-builder body, e.g. `txBuilder.meshTxBuilderBody`.
 *
 * - Inputs carry no datum (`NoDatum`): the body doesn't record the datums of spent outputs,
 *   and the library only reads the inputs' addresses.
 * - The validity range is converted from slots to POSIX milliseconds like the ledger does:
 *   `[invalidBefore, invalidHereafter)`, with infinite bounds when unset.
 *   `slotConfig` must match the network (e.g. `SLOT_CONFIG_NETWORK.preprod`, or Yaci's own config).
 */
export function tx_view_from_mesh_body(body: MeshTxBuilderBody, slotConfig: SlotConfig): TxView {
  const { invalidBefore, invalidHereafter } = body.validityRange;
  const toPosix = (slot: number) => int(slotToBeginUnixTime(slot, slotConfig));

  return {
    inputs: body.inputs.map(({ txIn }) => {
      if (!txIn.address || !txIn.amount) {
        throw new Error(`Input ${txIn.txHash}#${txIn.txIndex} has no address or amount: complete the tx first`);
      }
      return {
        output_reference: output_reference(txIn.txHash, txIn.txIndex),
        output: {
          address: address_from_bech32(txIn.address),
          value: assets.from_mesh_assets(txIn.amount),
          datum: NoDatum(),
        },
      };
    }),
    outputs: body.outputs.map(output_from_mesh),
    fee: int(BigInt(body.fee || "0")),
    validity_range: interval.interval(
      invalidBefore === undefined
        ? interval.interval_bound(interval.negative_infinity(), true)
        : interval.interval_bound(interval.finite(toPosix(invalidBefore)), true),
      invalidHereafter === undefined
        ? interval.interval_bound(interval.positive_infinity(), true)
        : interval.interval_bound(interval.finite(toPosix(invalidHereafter)), false),
    ),
    extra_signatories: body.requiredSignatures.map((hash) => byteString(hash.toLowerCase())),
  };
}
