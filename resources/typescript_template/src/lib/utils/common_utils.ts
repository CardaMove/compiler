/** Port of `lib/utils/common_utils.ak` */
import { ByteString, ConStr0, ConStr1, Dict, Integer, List, OutputReference, Tuple, byteString } from "@meshsdk/core";
import * as bytearray from "../stdlib/bytearray";
import {
  Data,
  constrIndex,
  expect_fail,
  fail,
  from_bytearray,
  int,
  str,
  toBigInt,
  trace,
  unconstr_fields,
} from "../stdlib/data";
import * as dict from "../stdlib/dict";
import * as list from "../stdlib/list";
import { Address, TxView, find_input } from "../stdlib/transaction";

/** Custom String type, alias for a byte array */
export type Str = ByteString;

/** Represents any address on the blockchain */
export type Addr = ByteString;

export type Ident = ByteString;

/** Represents the type arguments passed on a function: `TypeWithnessC(Ident, List<TWitness>)` */
export interface TWitness extends ConStr0<[Ident, List<TWitness>]> {}

export function TypeWithnessC(ident: Ident, type_args: TWitness[]): TWitness {
  return { constructor: 0, fields: [ident, { list: type_args }] };
}

/**
 * A reference to an entry in the scope
 * Tracks respectively:
 *   - The height of the identifier in the scope (0 = bottom)
 *   - The name of the identifier
 *   - The index of the binding among its shadowing (0 = first binding)
 */
export type ScopePtr = ConStr0<[Integer, Ident, Integer]>;

export function ScopePtr(height: Integer, ident: Ident, binding_idx: Integer): ScopePtr {
  return { constructor: 0, fields: [height, ident, binding_idx] };
}

/** The variant part of a reference, without field indices */
export type ScopeRef = ConStr0<[ScopePtr]>;
export type GlobalStorageRef = ConStr1<[Addr, TWitness]>;
export type RefKind = ScopeRef | GlobalStorageRef;

export function ScopeRef(resource: ScopePtr): ScopeRef {
  return { constructor: 0, fields: [resource] };
}

export function GlobalStorageRef(addr: Addr, resource_type: TWitness): GlobalStorageRef {
  return { constructor: 1, fields: [addr, resource_type] };
}

/**
 * A reference to a resource, paired with the list of nested field indices to access.
 * The type parameter only documents the referenced type, like in Aiken
 */
export type Reference<_A = Data> = Tuple<[RefKind, List<Integer>]>;

/** Builds the tuple `(variant, fields)` */
export function reference<A = Data>(variant: RefKind, fields: Integer[]): Reference<A> {
  return { list: [variant, { list: fields }] };
}

/**
 * Represents a single scope, where each identifier might be rebinded multiple times.
 * The head of the list represents the last binding
 */
export type Scope = Dict<List<Data>>;

/** Represents the state: `ProgramState(Dict<Addr, Dict<Str, Data>>)` */
export type ProgramState = ConStr0<[Dict<Dict<Data>>]>;

export function ProgramState(state_dict: Dict<Dict<Data>>): ProgramState {
  return { constructor: 0, fields: [state_dict] };
}

/** Represents the whole CPS state, consisting in the accumulated scopes and the global storage */
export type CPS = [Scope[], ProgramState];

export function internal_concat_str(strs: Str[]): Str {
  return byteString(strs.map((s) => s.bytes).join(""));
}

/**
 * Given a Type, returns its string representation
 * Inverse function of `internal_map_str_to_type`
 */
export function internal_map_type_to_str(t: TWitness): Str {
  const [type_constructor, type_args] = t.fields;
  if (type_args.list.length === 0) {
    return type_constructor;
  }

  let mapped_args: Str = type_args.list
    .map((type_arg) => internal_map_type_to_str(type_arg))
    .reduceRight((acc, addr) => internal_concat_str([addr, str(", "), acc]), str(""));

  // Drop the last two characters, which are ", "
  mapped_args = bytearray.take(mapped_args, bytearray.length(mapped_args) - 2);

  return internal_concat_str([type_constructor, str("<"), mapped_args, str(">")]);
}

/**
 * Given a string representation of a type, returns the corresponding Type
 * Inverse function of `internal_map_type_to_str`
 */
export function internal_map_str_to_type(s: Str): TWitness {
  const helper = (type_args: Str): TWitness[] => {
    const len = bytearray.length(type_args);

    // ASCII: '<' == 60, '>' == 62, ' ' == 32, ',' == 44
    if (bytearray.at(type_args, 0) === 60 && bytearray.at(type_args, len - 1) === 62) {
      const no_brackets = bytearray.slice(type_args, 1, len - 2);

      // Indexes are 0-based and both inclusive
      const [depth, curr_arg, splitted_args] = bytearray.foldr<[number, Str, Str[]]>(
        no_brackets,
        [0, byteString(""), []],
        (curr_byte, [depth, curr_str, splitted]) => {
          if (curr_byte === 60) {
            return [depth + 1, bytearray.push(curr_str, curr_byte), splitted];
          }
          if (curr_byte === 62) {
            return [depth - 1, bytearray.push(curr_str, curr_byte), splitted];
          }
          if (curr_byte === 32) {
            return [depth, curr_str, splitted];
          }
          if (curr_byte === 44 && depth === 0) {
            return [depth, byteString(""), [curr_str, ...splitted]];
          }
          return [depth, bytearray.push(curr_str, curr_byte), splitted];
        },
      );

      if (depth === 0) {
        return [curr_arg, ...splitted_args].map(internal_map_str_to_type);
      }
      trace("depth", int(depth));
      return fail("internal_map_str_to_type: final depth is non zero");
    }
    trace("str", s);
    return fail("internal_map_str_to_type: malformed str");
  };

  const index = bytearray.index_of(s, str("<"));
  if (index === undefined) {
    return TypeWithnessC(s, []);
  }
  const [open_type_args_index] = index;
  return TypeWithnessC(bytearray.take(s, open_type_args_index), helper(bytearray.drop(s, open_type_args_index)));
}

export function assert(f: () => boolean): void {
  if (!f()) {
    fail("Assertion failed");
  }
}

export function as_data(data: Data): Data {
  return data;
}

/** Test helper that only "uses" a value. It never fails */
export function consume_data(_data: unknown): void {}

/**
 * Expects the input of being `Some(data)` and returns it
 * If the input is `None` (`undefined`), throws a custom error message
 */
export function expect_some<T>(val: T | undefined, error_message: Str): T {
  if (val === undefined) {
    return fail(from_bytearray(error_message));
  }
  return val;
}

/**
 * Returns the `Address` associated to this contract
 * Also see `parse_contract_address` to use it in the program
 */
export function get_contract_address(utxo: OutputReference, self: TxView): Address {
  const own_input = find_input(self.inputs, utxo);
  if (own_input === undefined) {
    return expect_fail("Some(own_input) = transaction.find_input(self.inputs, utxo)");
  }
  return own_input.output.address;
}

/** Given the contract's `Address`, retrieves his hash `Addr` so to be used in the program */
export function parse_contract_address(address: Address): Addr {
  // Since it it a contract, it must be of type constructor Script and not VerificationKey
  const payment_credential = address.fields[0];
  if (constrIndex(payment_credential) !== 1) {
    return expect_fail("Script(script_hash) = address.payment_credential");
  }
  return payment_credential.fields[0];
}

export function fail_msg(left: ByteString, right: ByteString): string {
  return from_bytearray(bytearray.concat(left, right));
}

/** Used to read a nested field of some Data by a sequence of field indices */
export function internal_get_data(data: Data, fields: Integer[]): Data {
  if (fields.length === 0) {
    return data;
  }
  const [f, ...fs] = fields;
  const constr_fields = unconstr_fields(data);
  const field_val = list.at(constr_fields, toBigInt(f));
  if (field_val === undefined) {
    return expect_fail("Some(field_val) = list.at(constr_fields, f)");
  }
  return internal_get_data(field_val, fs);
}

/**
 * Used to extract the nested fields of some Data and overwite them.
 * Note that every rebuilt constructor gets tag 0, whatever its original tag
 */
export function internal_put_data(data: Data, fields: Integer[], val: Data): Data {
  if (fields.length === 0) {
    return val;
  }
  const [f, ...fs] = fields;
  const constr_fields = unconstr_fields(data);

  const remapped_fields = constr_fields.map((el, idx) =>
    BigInt(idx) === toBigInt(f) ? internal_put_data(el, fs, val) : el,
  );

  return { constructor: 0, fields: remapped_fields };
}

/**
 * Used internally to dereference a ref to the scope
 * Defined here to avoid circular depencency
 */
export function internal_deref_scope(ref: Reference, [scopes, _gs]: CPS): Data {
  const [variant, fields] = ref.list;
  if (constrIndex(variant) !== 0) {
    return expect_fail("ScopeRef{resource: ScopePtr(height, ident, binding_idx)} = variant");
  }
  const [height, ident, binding_idx] = (variant as ScopeRef).fields[0].fields;

  const scope = list.at(scopes, BigInt(scopes.length) - toBigInt(height));
  if (scope === undefined) {
    return expect_fail("Some(scope) = list.at(scopes, list.length(scopes) - height)");
  }

  const bindings = dict.get(scope, ident);
  if (bindings === undefined) {
    return expect_fail("Some(bindings) = dict.get(scope, ident)");
  }
  const val = list.at(bindings.list, BigInt(bindings.list.length) - toBigInt(binding_idx));
  if (val === undefined) {
    return expect_fail("Some(val) = list.at(bindings, list.length(bindings) - binding_idx)");
  }

  return internal_get_data(val, fields.list);
}
