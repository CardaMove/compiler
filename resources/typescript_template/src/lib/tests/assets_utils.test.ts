import { describe, test } from "vitest";
import { Value } from "@meshsdk/core";
import * as assets from "../stdlib/assets";
import { int, str } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { NoDatum, Script, TxOutput, VerificationKey, address } from "../stdlib/transaction";
import { deduce_transaction_fees_from_assets, map_outputs_to_assets } from "../utils/assets_utils";
import { Signer } from "../utils/signer_utils";
import { expectDataEqual, insert_all } from "./helpers";

describe("lib/tests/assets_utils_tests.ak", () => {
  test("map_outputs_to_assets_test", () => {
    const outputs: TxOutput[] = [
      // An output to an already existing address
      {
        address: address(VerificationKey(str("addr1"))),
        value: assets.from_lovelace(int(1000000)),
        datum: NoDatum(),
      },
      // An output to a new address
      {
        address: address(VerificationKey(str("addr3"))),
        value: assets.from_lovelace(int(3000000)),
        datum: NoDatum(),
      },
      // An output to a script address
      {
        address: address(Script(str("script_hash"))),
        value: assets.from_lovelace(int(5000000)),
        datum: NoDatum(),
      },
      // Additional output for addr3. The second Value is zero
      {
        address: address(VerificationKey(str("addr3"))),
        value: assets.merge(
          assets.from_lovelace(int(1000001)),
          assets.from_asset(str("my_policy"), str("my_asset"), int(0)),
        ),
        datum: NoDatum(),
      },
      // Output with custom policy and asset
      {
        address: address(VerificationKey(str("addr2"))),
        value: assets.from_asset(str("my_policy"), str("my_asset"), int(66)),
        datum: NoDatum(),
      },
      // Empty output
      {
        address: address(VerificationKey(str("addr_4"))),
        value: assets.from_asset(str("my_policy"), str("my_asset"), int(0)),
        datum: NoDatum(),
      },
      // Additional output for addr1 with a different asset
      {
        address: address(VerificationKey(str("addr1"))),
        value: assets.from_asset(str("my_policy"), str("my_asset"), int(50)),
        datum: NoDatum(),
      },
    ];

    // Empty output should not be present
    // Values in different outputs for the same address should be merged
    const expected_assets = insert_all<Value>(dict.empty(), [
      [
        str("addr1"),
        assets.merge(assets.from_lovelace(int(1000000)), assets.from_asset(str("my_policy"), str("my_asset"), int(50))),
      ],
      [str("addr2"), assets.from_asset(str("my_policy"), str("my_asset"), int(66))],
      [str("addr3"), assets.from_lovelace(int(3000000 + 1000001))],
      [str("script_hash"), assets.from_lovelace(int(5000000))],
    ]);

    expectDataEqual(map_outputs_to_assets(outputs), expected_assets);
  });

  test("deduce_transaction_fees_from_assets_test", () => {
    const initial_assets = insert_all<Value>(dict.empty(), [
      [
        str("addr1"),
        assets.merge(assets.from_lovelace(int(1000000)), assets.from_asset(str("my_policy"), str("my_asset"), int(50))),
      ],
      [str("addr2"), assets.from_asset(str("my_policy"), str("my_asset"), int(66))],
      [str("addr3"), assets.from_lovelace(int(3000000 + 1000001))],
      [str("script_hash"), assets.from_lovelace(int(5000000))],
    ]);

    const expected_assets = insert_all<Value>(dict.empty(), [
      [
        str("addr1"),
        assets.merge(
          assets.from_lovelace(int(1000000 - 500)),
          assets.from_asset(str("my_policy"), str("my_asset"), int(50)),
        ),
      ],
      [str("addr2"), assets.from_asset(str("my_policy"), str("my_asset"), int(66))],
      [str("addr3"), assets.from_lovelace(int(3000000 + 1000001))],
      [str("script_hash"), assets.from_lovelace(int(5000000))],
    ]);

    // Deducing from addr1
    expectDataEqual(
      deduce_transaction_fees_from_assets(initial_assets, Signer(str("addr1")), int(500)),
      expected_assets,
    );
  });
});
