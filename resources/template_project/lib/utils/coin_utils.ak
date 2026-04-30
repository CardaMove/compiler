use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{AssetName, PolicyId, Value}
use utils/common_utils.{Addr, ProgramState, Str, TWitness, TypeWithnessC, Reference, CPS}
use utils/signer_utils.{Signer}
use utils/reference_utils

// Defined internally in Move
pub type Coin {
  value: Int,
}

// FIXME: Unused for now. Frozen forbids deposits and withdrawals
pub type CoinStore {
  coin: Coin,
  frozen: Bool,
}

const coin_address_prefix: Str = "__COIN__"

const lovelace_resource_type: TWitness = TypeWithnessC("lovelace", [])

/// Defined internally in Move, creates a coin from two coins of the same type
/// Does never abort since Move already ensures that the CoinType(s) correspond
pub fn merge(dst_coin: Reference<Coin>, source_coin: Coin, tw_coin_type: TWitness, cps: CPS) -> (Void, CPS) {
  expect Coin { value: value1 } = reference_utils.deref(dst_coin, cps)

  let cps = reference_utils.put_deref(dst_coin, [], Coin{value: value1 + source_coin.value}, cps)

  (Void, cps)
}

// Defined internally in Move, uses CoinStore
// FIXME: Should transfer ADA only for Coin<AptosCoin>?
// FIXME: Read about policies at https://aiken-lang.github.io/stdlib/cardano/assets.html#ada_asset_name
pub fn deposit(
  account_addr: Addr,
  coin: Coin,
  coin_type: TWitness,
  cps: CPS,
) -> (Void, CPS) {
  let Coin { value: _ } = coin
  let type_addr =
    common_utils.internal_concat_str(
      [
        coin_address_prefix,
        common_utils.internal_map_type_to_str(
          TypeWithnessC("Coin", [coin_type]),
        ),
      ],
    )

  let (scopes, ProgramState(state_dict)) = cps
  let coin_data: Data = coin

  let state_dict = state_dict
    |> dict.insert_with(
        key: account_addr,
        value: dict.from_pairs([Pair(type_addr, coin_data)]),
        with: fn(
          _key: Addr,
          existing_value: Dict<Addr, Data>,
          new_value: Dict<Addr, Data>,
        ) {
          Some(
            dict.union_with(
              left: existing_value,
              right: new_value,
              // If the address already has some balance, add the coin value to it
              with: fn(_key: Addr, ex_coin_value: Data, new_coin_value: Data) {
                expect Coin { value: value1 } = ex_coin_value
                expect Coin { value: value2 } = new_coin_value

                let sum: Data = Coin{value: value1 + value2}
                Some(sum)
              },
            ),
          )
        },
      )

  (Void, (scopes, ProgramState(state_dict)))
}

/// Updates the program state so to include all assets presents in the outputs
/// The ouputs parameter should not have zero-valued assets
pub fn add_assets_to_state(
  outputs: Dict<Addr, Value>,
  (scopes, state): CPS,
) -> CPS {
  let state = outputs
    |> dict.foldr(
        state,
        with: fn(addr, value, state) {
          let assets: List<(PolicyId, AssetName, Int)> =
            assets.flatten(value)
              |> list.filter(
                  fn((_policy_id, _asset_name, qt): (PolicyId, AssetName, Int)) {
                    qt > 0
                  },
                )

          list.foldr(
            assets,
            state,
            with: fn(
              (policy_id, asset_name, qt): (PolicyId, AssetName, Int),
              state: ProgramState,
            ) -> ProgramState {
              let is_ada: Bool =
                policy_id == assets.ada_policy_id && asset_name == assets.ada_asset_name

              // Depending if ADA or not, set the rource type Coin<coin_type>
              let coin_type: TWitness =
                if is_ada {
                  lovelace_resource_type
                } else {
                  TypeWithnessC(policy_id, [TypeWithnessC(asset_name, [])])
                }

              let (_, state) = deposit(addr, Coin { value: qt }, coin_type, (scopes, state)).2nd

              state
            },
          )
        },
      )

  (scopes, state)
}

/// Given the state, returns all the assets in it grouped by the target address
/// Similar to `assets_utils.map_outputs_to_assets`
pub fn get_assets_from_state(cps: CPS) -> Dict<Addr, Value> {
  let (scopes, ProgramState(state_dict)) = cps

  let res_dict: Dict<Addr, Value> =
    state_dict
      |> dict.map(
          fn(_addr: Addr, sub_dict: Dict<Str, Data>) {
            let addr_value: Value =
              sub_dict
                |> dict.foldr(
                    assets.zero,
                    fn(resource_type, val, acc_value) {
                      if val is Coin {
                        let Coin { value: coin_value } = val

                        // The second check is needed because should still be allowed to insert Coin structures in the state without considering them as assets
                        // FIXME: Not sure, but an additional check is still good
                        if bytearray.starts_with(
                          resource_type,
                          coin_address_prefix,
                        ) && coin_value > 0 {
                          // Retrieve the Coin<coin_type>
                          let full_coin_type: TWitness =
                            resource_type
                              |> bytearray.drop(
                                  bytearray.length(coin_address_prefix),
                                )
                              |> common_utils.internal_map_str_to_type()
                          // Add the current value to the accumulated value
                          let merged_value: Value =
                            when full_coin_type is {
                              TypeWithnessC(coin_constr, [coin_type]) ->
                                if coin_constr == "Coin" {
                                  // Retrieve the policy id and asset name from the single type argument
                                  when coin_type is {
                                    // Cannot pattern match on an existing value and guards are deprecatede, so I need to use an if
                                    TypeWithnessC(_, []) ->
                                      if coin_type == lovelace_resource_type {
                                        assets.add(
                                          acc_value,
                                          assets.ada_policy_id,
                                          assets.ada_asset_name,
                                          coin_value,
                                        )
                                      } else {
                                        // The only asset that is identified by just a name is lovelace
                                        trace coin_type
                                        fail @"Unexpected type name for coin_type"
                                      }
                                    TypeWithnessC(
                                      policy_id,
                                      [TypeWithnessC(asset_name, [])],
                                    ) ->
                                      assets.add(
                                        acc_value,
                                        policy_id,
                                        asset_name,
                                        coin_value,
                                      )
                                    // A coin cannot have more than one asset_name
                                    _ -> {
                                      trace resource_type
                                        |> bytearray.drop(
                                            bytearray.length(
                                              coin_address_prefix,
                                            ),
                                          )
                                      trace coin_type
                                      fail @"Unexpected coin_type"
                                    }
                                  }
                                } else {
                                  trace coin_constr
                                  fail @"Coin constructor is not Coin"
                                }
                              _ -> {
                                trace full_coin_type
                                fail @"Malformed coin type"
                              }
                            }

                          merged_value
                        } else {
                          acc_value
                        }
                      } else {
                        acc_value
                      }
                    },
                  )

            addr_value
          },
        )
      |> dict.filter(fn(_addr, value) { !assets.is_zero(value) })

  res_dict
}

/// Given the state, removes all assets in it and returns the updated state
pub fn remove_assets_from_state(cps: CPS) -> CPS {
  let (scopes, ProgramState(state_dict)) = cps

  let state = ProgramState(
    state_dict
      |> dict.map(
          fn(_addr: Addr, sub_dict: Dict<Str, Data>) {
            sub_dict
              |> dict.filter(
                  fn(resource_type: Str, val: Data) {
                    if val is Coin {
                      // See `get_assets_from_state`
                      if bytearray.starts_with(
                        resource_type,
                        coin_address_prefix,
                      ) {
                        False
                      } else {
                        True
                      }
                    } else {
                      True
                    }
                  },
                )
          },
        ),
  )

  (scopes, state)
}

/// Deducts some amount from a coin, returning the new one as first place and the updated one as second place
/// Aborts if the input coin has not enough value
pub fn extract(coin: Reference<Coin>, amount: Int, tw_coin_type: TWitness, cps: CPS) -> (Coin, CPS) {
  expect Coin { value } = reference_utils.deref(coin, cps)

  if value < amount {
    fail @"Not enought Coin value to extract"
  } else {
    let updated_coin = Coin { value: value - amount }
    let new_coin = Coin { value: amount }

    let cps = reference_utils.put_deref(coin, [], updated_coin, cps)

    (new_coin, cps)
  }
}

/// Retrieves a certain amount of Coin<coin_type> from the signer's storage
/// Returns a pair with `(retrieved_coin, cps)`
/// Aborts if the signer has not enought amount of the Coin
pub fn withdraw(
  signer: Reference<Signer>,
  amount: Int,
  tw_coin_type: TWitness,
  cps: CPS,
) -> (Coin, CPS) {
  let type_addr =
    common_utils.internal_concat_str(
      [
        coin_address_prefix,
        common_utils.internal_map_type_to_str(
          TypeWithnessC("Coin", [tw_coin_type]),
        ),
      ],
    )

  let (signer_addr, cps) = signer_utils.signer_address_of(signer, cps)

  let (scopes, ProgramState(state_dict)) = cps
  expect existing_coin: Coin =
    state_dict
      |> dict.get(signer_addr)
      |> common_utils.expect_some("No address to withdraw from")
      |> dict.get(type_addr)
      |> common_utils.expect_some("No Coin to withdraw")

  let updated_coin = Coin { value: existing_coin.value - amount }
  let new_coin = Coin { value: amount }

  let updated_state =
    ProgramState(
      state_dict
        |> dict.map(
            fn(addr, sub_dict) {
              if addr == signer_addr {
                // Override the existing key for the coin
                sub_dict
                  |> dict.insert(type_addr, updated_coin)
              } else {
                sub_dict
              }
            },
          ),
    )

  (new_coin, (scopes, updated_state))
}

pub fn value(
  coin: Coin,
  tw_coin_type: TWitness,
  cps: CPS
) -> (Int, CPS){
  (coin.value, cps)
}