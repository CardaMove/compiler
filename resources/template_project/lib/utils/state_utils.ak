use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/collection/pairs
use cardano/address.{Address}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}
use utils/common_utils.{Addr, ProgramState, Str, as_data}

pub type SerializedProgramState =
  Pairs<Addr, Pairs<Str, Data>>

/// Returns the deserialized output of a transaction
pub fn get_tx_output_state(
  utxo: OutputReference,
  self: Transaction,
) -> ProgramState {
  let contract_address: Address = common_utils.get_contract_address(utxo, self)

  expect Some(own_output) =
    self.outputs
      |> list.filter(fn(output) { output.address == contract_address })
      |> list.at(0)
  expect InlineDatum(untyped_datum) = own_output.datum
  expect serialized_state: SerializedProgramState = untyped_datum

  deserialize_state(serialized_state)
}

/// Returns the deserialized output of a transaction
pub fn get_tx_input_state(datum: Option<SerializedProgramState>) -> ProgramState {
  expect Some(serialized_state) = datum

  deserialize_state(serialized_state)
}

/// Checks if two states are equal
pub fn compare_tx_states(state_a: ProgramState, state_b: ProgramState) -> Bool {
  if state_a == state_b {
    trace @"States match"
    True
  } else {
    // let ProgramState(dict_a) = state_a
    // trace dict.to_pairs(dict_a)
    // trace state_b
    fail @"States do not match"
  }
}

pub fn deserialize_state(
  serialized_state: SerializedProgramState,
) -> ProgramState {
  ProgramState(
    dict.from_pairs(
      pairs.map(serialized_state, fn(_key, value) { dict.from_pairs(value) }),
    ),
  )
}

fn internal_stringify_state_inner(
  sub_state: Dict<Addr, Data>,
  indent: Str,
) -> Str {
  let stringified_state: Str =
    sub_state
      |> dict.foldr(
          "",
          fn(addr, value, acc) {
            common_utils.internal_concat_str(
              [acc, indent, addr, ": ", builtin.serialise_data(value), "\n"],
            )
          },
        )

  common_utils.internal_concat_str(["Dict(\n", stringified_state, ")\n"])
}

fn internal_stringify_state(
  state_dict: Dict<Addr, Dict<Str, Data>>,
  indent: Str,
) -> Str {
  let stringified_state: Str =
    state_dict
      |> dict.foldr(
          "",
          fn(addr, sub_dict, acc) {
            common_utils.internal_concat_str(
              [
                acc,
                indent,
                addr,
                ": ",
                internal_stringify_state_inner(
                  sub_dict,
                  common_utils.internal_concat_str([indent, indent]),
                ),
                "\n",
              ],
            )
          },
        )

  common_utils.internal_concat_str(
    ["ProgramState(\n", stringified_state, ")\n"],
  )
}

pub fn stringify_program_state(state: ProgramState) -> Str {
  let ProgramState(state_dict) = state

  internal_stringify_state(state_dict, "  ")
}

/// Cleans the state so no entries with empty substate will be present
/// Note that it only clears addresses without inner state, not resource types
pub fn purge_state(state: ProgramState) -> ProgramState {
  let ProgramState(state_dict) = state

  ProgramState(
    state_dict
      |> dict.filter(fn(_addr, sub_dict) { sub_dict != dict.empty }),
  )
}
