/** Port of `lib/utils/state_utils.ak` */
import { Dict, OutputReference, Pairs } from "@meshsdk/core";
import { Data, bytes, constrIndex, data_equals, expect_fail, fail, isBytes, isMap, str, toCbor, trace } from "../stdlib/data";
import * as dict from "../stdlib/dict";
import { Address, TxView } from "../stdlib/transaction";
import { Addr, ProgramState, Str, get_contract_address, internal_concat_str } from "./common_utils";

/** The on-chain datum: `Pairs<Addr, Pairs<Str, Data>>` */
export type SerializedProgramState = Pairs<Addr, Pairs<Str, Data>>;

/** `expect serialized_state: SerializedProgramState = data`: a map of `ByteArray` keys to maps of `ByteArray` keys */
export function is_serialized_program_state(data: Data): data is SerializedProgramState {
  return (
    isMap(data) &&
    data.map.every(({ k, v }) => isBytes(k) && isMap(v) && v.map.every((inner) => isBytes(inner.k)))
  );
}

/** Returns the deserialized output of a transaction */
export function get_tx_output_state(utxo: OutputReference, self: TxView): ProgramState {
  const contract_address: Address = get_contract_address(utxo, self);

  const own_output = self.outputs.filter((output) => data_equals(output.address, contract_address))[0];
  if (own_output === undefined) {
    return expect_fail("Some(own_output) = self.outputs |> list.filter(...) |> list.at(0)");
  }
  if (constrIndex(own_output.datum) !== 2) {
    return expect_fail("InlineDatum(untyped_datum) = own_output.datum");
  }
  const untyped_datum = own_output.datum.fields[0] as Data;
  if (!is_serialized_program_state(untyped_datum)) {
    return expect_fail("serialized_state: SerializedProgramState = untyped_datum");
  }

  return deserialize_state(untyped_datum);
}

/** Returns the deserialized input datum of a transaction (`undefined` is Aiken's `None`) */
export function get_tx_input_state(datum: SerializedProgramState | undefined): ProgramState {
  if (datum === undefined) {
    return expect_fail("Some(serialized_state) = datum");
  }
  return deserialize_state(datum);
}

/** Checks if two states are equal */
export function compare_tx_states(state_a: ProgramState, state_b: ProgramState): boolean {
  if (data_equals(state_a, state_b)) {
    trace("States match");
    return true;
  }
  return fail("States do not match");
}

export function deserialize_state(serialized_state: SerializedProgramState): ProgramState {
  return ProgramState(
    dict.from_pairs<Dict<Data>>({
      map: serialized_state.map.map(({ k, v }) => ({ k, v: dict.from_pairs<Data>(v) })),
    }),
  );
}

/** Inverse of `deserialize_state`: the datum to put on-chain (not in Aiken, where a `Dict` already is its pairs) */
export function serialize_state(state: ProgramState): SerializedProgramState {
  const [state_dict] = state.fields;
  return { map: state_dict.map.map(({ k, v }) => ({ k, v: { map: [...v.map] } })) };
}

function internal_stringify_state_inner(sub_state: Dict<Data>, indent: Str): Str {
  const stringified_state: Str = dict.foldr(sub_state, str(""), (addr, value, acc) =>
    internal_concat_str([acc, indent, addr, str(": "), bytes(toCbor(value)), str("\n")]),
  );

  return internal_concat_str([str("Dict(\n"), stringified_state, str(")\n")]);
}

function internal_stringify_state(state_dict: Dict<Dict<Data>>, indent: Str): Str {
  const stringified_state: Str = dict.foldr(state_dict, str(""), (addr, sub_dict, acc) =>
    internal_concat_str([
      acc,
      indent,
      addr,
      str(": "),
      internal_stringify_state_inner(sub_dict, internal_concat_str([indent, indent])),
      str("\n"),
    ]),
  );

  return internal_concat_str([str("ProgramState(\n"), stringified_state, str(")\n")]);
}

export function stringify_program_state(state: ProgramState): Str {
  const [state_dict] = state.fields;

  return internal_stringify_state(state_dict, str("  "));
}

/**
 * Cleans the state so no entries with empty substate will be present
 * Note that it only clears addresses without inner state, not resource types
 */
export function purge_state(state: ProgramState): ProgramState {
  const [state_dict] = state.fields;

  return ProgramState(dict.filter(state_dict, (_addr, sub_dict) => !dict.is_empty(sub_dict)));
}
