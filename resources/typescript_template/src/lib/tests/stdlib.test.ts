/**
 * Tests for the TS-only parts of the port (no Aiken counterpart): data helpers and the Mesh adapters.
 */
import { describe, expect, test } from "vitest";
import { SLOT_CONFIG_NETWORK, conStr0, deserializeDatum, emptyTxBuilderBody, integer, pubKeyAddress } from "@meshsdk/core";
import * as assets from "../stdlib/assets";
import { bytes, data_equals, fromCbor, int, isConstr, normalize_data, str, toCbor } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import * as interval from "../stdlib/interval";
import { InlineDatum, address_from_bech32, tx_view_from_mesh_body } from "../stdlib/transaction";
import { expectDataEqual } from "./helpers";

const yaciAddress =
  "addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex";
const policy = "c8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8";

describe("stdlib/data", () => {
  test("isConstr ignores the constructor inherited from Object.prototype", () => {
    expect(isConstr({})).toBe(false);
    expect(isConstr({ fields: [] })).toBe(false);
    expect(isConstr(conStr0([]))).toBe(true);
  });

  test("normalize_data turns decoded bigint constructor indices into numbers", () => {
    const decoded = deserializeDatum("d8799f01ff");
    expect(typeof decoded.constructor).toBe("bigint");
    const normalized = normalize_data(decoded);
    expect(isConstr(normalized) && normalized.constructor).toBe(0);
    expectDataEqual(fromCbor("d8799f01ff"), conStr0([int(1)]));
  });

  test("data_equals compares integers by value, whether number or bigint", () => {
    expect(data_equals(integer(5), int(5))).toBe(true);
    expect(data_equals(conStr0([integer(1)]), { constructor: 1, fields: [int(1)] })).toBe(false);
  });
});

describe("stdlib/dict", () => {
  test("keys stay sorted by bytes, shorter prefix first", () => {
    const d = [str("b"), str("ab"), str("a"), bytes("00ff")].reduce((acc, k) => dict.insert(acc, k, int(0)), dict.empty());
    expect(d.map.map(({ k }) => k.bytes)).toEqual([bytes("00ff").bytes, str("a").bytes, str("ab").bytes, str("b").bytes]);
  });
});

describe("stdlib/assets Mesh conversions", () => {
  test("round-trips Mesh assets without losing precision above 2^53", () => {
    const quantity = (2n ** 60n + 1n).toString();
    const value = assets.from_mesh_assets([
      { unit: `${policy}746f6b`, quantity },
      { unit: "lovelace", quantity: "5" },
      { unit: "lovelace", quantity: "-5" },
    ]);
    // Lovelace sums to zero and disappears, like in Aiken
    expect(assets.to_mesh_assets(value)).toEqual([{ unit: `${policy}746f6b`, quantity }]);
  });
});

describe("stdlib/transaction Mesh adapters", () => {
  test("address_from_bech32 matches Aiken's encoding of the same address", () => {
    // Traced from Aiken: Address { VerificationKey(payment), Some(Inline(VerificationKey(stake))) }.
    // Mesh's `addrBech32ToPlutusDataObj` (core-cst) omits the `Inline` wrapper, so it is not used
    const aiken =
      "d8799fd8799f581cc8c47610a36034aac6fc58848bdae5c278d994ff502c05455e3b3ee8ffd8799fd8799fd8799f581c" +
      "f8ed3a0eea0ef835ffa7bbfcde55f7fe9d2cc5d55ea62cecb42bab3cffffffff";
    const stake = "f8ed3a0eea0ef835ffa7bbfcde55f7fe9d2cc5d55ea62cecb42bab3c";
    expect(toCbor(address_from_bech32(yaciAddress))).toBe(aiken);
    expect(toCbor(pubKeyAddress(policy, stake))).toBe(aiken);
  });

  test("tx_view_from_mesh_body converts inputs, outputs, fee, signers and validity range", () => {
    const body = emptyTxBuilderBody();
    body.inputs = [
      {
        type: "PubKey",
        txIn: { txHash: "AA".repeat(32), txIndex: 3, address: yaciAddress, amount: [{ unit: "lovelace", quantity: "7" }] },
      },
    ];
    body.outputs = [
      {
        address: yaciAddress,
        amount: [{ unit: "lovelace", quantity: "2" }],
        datum: { type: "Inline", data: { type: "JSON", content: conStr0([int(1)]) } },
      },
    ];
    body.fee = "170000";
    body.requiredSignatures = [policy.toUpperCase()];
    body.validityRange = { invalidBefore: 86400 + 10, invalidHereafter: 86400 + 20 };

    const tx = tx_view_from_mesh_body(body, SLOT_CONFIG_NETWORK.preprod);
    const zeroTime = SLOT_CONFIG_NETWORK.preprod.zeroTime;

    expect(tx.inputs).toHaveLength(1);
    expectDataEqual(tx.inputs[0].output.address, address_from_bech32(yaciAddress));
    expectDataEqual(tx.inputs[0].output_reference, conStr0([bytes("aa".repeat(32)), int(3)]));
    expectDataEqual(tx.outputs[0].value, assets.from_lovelace(int(2)));
    expectDataEqual(tx.outputs[0].datum, InlineDatum(conStr0([int(1)])));
    expectDataEqual(tx.fee, int(170000));
    expectDataEqual(tx.extra_signatories[0], bytes(policy));
    // Ledger semantics: [invalidBefore, invalidHereafter)
    expectDataEqual(
      tx.validity_range,
      interval.interval(
        interval.interval_bound(interval.finite(int(zeroTime + 10000)), true),
        interval.interval_bound(interval.finite(int(zeroTime + 20000)), false),
      ),
    );
  });

  test("tx_view_from_mesh_body leaves unset validity bounds infinite", () => {
    const tx = tx_view_from_mesh_body(emptyTxBuilderBody(), SLOT_CONFIG_NETWORK.preprod);
    expectDataEqual(tx.validity_range, interval.everything());
  });
});
