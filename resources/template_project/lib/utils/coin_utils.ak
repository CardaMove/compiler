use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{AssetName, PolicyId, Value}
use utils/common_utils.{Addr, ProgramState, Str, TWitness, TypeWithnessC}
use utils/signer_utils.{Signer}

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
pub fn coin_merge(coin1: Coin, coin2: Coin) -> Coin {
  let Coin { value: value1 } = coin1
  let Coin { value: value2 } = coin2

  Coin { value: value1 + value2 }
}

// Defined internally in Move, uses CoinStore
// FIXME: Should transfer ADA only for Coin<AptosCoin>?
// FIXME: Read about policies at https://aiken-lang.github.io/stdlib/cardano/assets.html#ada_asset_name
pub fn coin_deposit(
  account_addr: Addr,
  coin: Coin,
  coin_type: TWitness,
  state: ProgramState,
) -> ProgramState {
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

  let ProgramState(state_dict) = state
  let coin_data: Data = coin

  state_dict
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
                expect c1: Coin = ex_coin_value
                expect c2: Coin = new_coin_value

                let sum: Data = coin_merge(c1, c2)
                Some(sum)
              },
            ),
          )
        },
      )
    |> ProgramState
}

/// Updates the program state so to include all assets presents in the outputs
/// The ouputs parameter should not have zero-valued assets
pub fn add_assets_to_state(
  state: ProgramState,
  outputs: Dict<Addr, Value>,
) -> ProgramState {
  outputs
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
            ) {
              let is_ada: Bool =
                policy_id == assets.ada_policy_id && asset_name == assets.ada_asset_name

              // Depending if ADA or not, set the rource type Coin<coin_type>
              let coin_type: TWitness =
                if is_ada {
                  lovelace_resource_type
                } else {
                  TypeWithnessC(policy_id, [TypeWithnessC(asset_name, [])])
                }

              coin_deposit(addr, Coin { value: qt }, coin_type, state)
            },
          )
        },
      )
}

/// Given the state, returns all the assets in it grouped by the target address
/// Similar to `assets_utils.map_outputs_to_assets`
pub fn get_assets_from_state(state: ProgramState) -> Dict<Addr, Value> {
  let ProgramState(state_dict) = state

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
pub fn remove_assets_from_state(state: ProgramState) -> ProgramState {
  let ProgramState(state_dict) = state

  ProgramState(
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
}

/// Deducts some amount from a coin, returning the new one as first place and the updated one as second place
/// Aborts if the input coin has not enough value
pub fn coin_extract(coin: Coin, amount: Int) -> Pair<Coin, Coin> {
  let Coin { value } = coin

  if value < amount {
    fail @"Not enought Coin value to extract"
  } else {
    let updated_coin = Coin { value: value - amount }
    let new_coin = Coin { value: amount }

    Pair(new_coin, updated_coin)
  }
}

/// Retrieves a certain amount of Coin<coin_type> from the signer's storage
/// Returns a pair with `(retrieved_coin, updated_state)`
/// Aborts if the signer has not enought amount of the Coin
pub fn coin_withdraw(
  coin_type: TWitness,
  account: Signer,
  amount: Int,
  state: ProgramState,
) -> Pair<Coin, ProgramState> {
  let type_addr =
    common_utils.internal_concat_str(
      [
        coin_address_prefix,
        common_utils.internal_map_type_to_str(
          TypeWithnessC("Coin", [coin_type]),
        ),
      ],
    )

  let signer_addr = signer_utils.internal_signer_address_of_unref(account)

  let ProgramState(state_dict) = state
  expect existing_coin: Coin =
    state_dict
      |> dict.get(signer_addr)
      |> common_utils.expect_some("No address to withdraw from")
      |> dict.get(type_addr)
      |> common_utils.expect_some("No Coin to withdraw")

  let Pair(new_coin, updated_coin) = coin_extract(existing_coin, amount)

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

  Pair(new_coin, updated_state)
}
