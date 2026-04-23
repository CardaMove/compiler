use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/primitive/bytearray
use aiken/primitive/string
use cardano/address.{Address, Script}
use cardano/transaction.{OutputReference, Transaction}

/// Custom String type, alias for a byte array
pub type Str =
  ByteArray

/// Represents any address on the blockchain
pub type Addr =
  ByteArray

pub type Ident =
  ByteArray

/// Represents the type arguments passed on a function
pub type TWitness {
  // No need to add a Zerary instance, since it could be represented as a type constructor with no type args
  TypeWithnessC(Ident, List<TWitness>)
}

/// A reference to an entry in the scope
/// Tracks respectively:
///   - The height of the identifier in the scope (0 = bottom)
///   - The name of the identifier
///   - The index of the binding among its shadowing (0 = first binding)
pub type ScopePtr {
  ScopePtr(Int, Ident, Int)
}

/// The variant part of a reference, without field indices
pub type RefKind {
  ScopeRef { resource: ScopePtr }
  GlobalStorageRef { addr: Addr, resource_type: TWitness }
}

/// A reference to a resource, paired with the list of nested field indices to access
pub type Reference<a> =
  (RefKind, List<Int>)

// In this case, the head of the list represents the last binding
/// Represents a single scope,
/// where each identifier might be rebinded multiple times
pub type Scope =
  Dict<Ident, List<Data>>

/// Represents the state
pub type ProgramState {
  ProgramState(Dict<Addr, Dict<Str, Data>>)
}

/// Represents the whole CPS state, consisting in the accumulated scopes and the global storage
pub type CPS =
  (List<Scope>, ProgramState)

pub fn internal_concat_str(str: List<Str>) -> Str {
  when str is {
    [] -> ""
    [head, ..tail] -> bytearray.concat(head, internal_concat_str(tail))
  }
}

/// Given a Type, returns its string representation
/// Inverse function of `internal_map_str_to_type`
pub fn internal_map_type_to_str(t: TWitness) -> Str {
  when t is {
    TypeWithnessC(type_name, []) -> type_name
    TypeWithnessC(type_constructor, type_args) -> {
      let mapped_args: Str =
        type_args
          |> list.map(fn(type_arg) { internal_map_type_to_str(type_arg) })
          |> list.foldr(
              "",
              fn(addr, acc) { internal_concat_str([addr, ", ", acc]) },
            )

      // Drop the last two characters, which are ", "
      let mapped_args =
        bytearray.take(mapped_args, bytearray.length(mapped_args) - 2)

      internal_concat_str([type_constructor, "<", mapped_args, ">"])
    }
  }
}

/// Given a string representation of a type, returns the corresponding Type
/// Inverse function of `internal_map_type_to_str`
pub fn internal_map_str_to_type(str: Str) -> TWitness {
  let helper =
    fn(type_args: Str) -> List<TWitness> {
      let len = bytearray.length(type_args)

      // ASCII: '<' == 60, '>' == 62, ' ' == 32, ',' == 44
      if bytearray.at(type_args, 0) == 60 && bytearray.at(type_args, len - 1) == 62 {
        let no_brackets =
          type_args
            |> bytearray.slice(1, len - 2)

        // Indexes are 0-based and both inclusive
        let (depth, curr_arg, splitted_args): (Int, ByteArray, List<ByteArray>) =
          bytearray.foldr(
            no_brackets,
            (0, #[], []),
            fn(curr_byte, (depth, curr_str, splitted)) {
              when (curr_byte, depth) is {
                (60, _) ->
                  (depth + 1, bytearray.push(curr_str, curr_byte), splitted)
                (62, _) ->
                  (depth - 1, bytearray.push(curr_str, curr_byte), splitted)
                (32, _) -> (depth, curr_str, splitted)
                (44, 0) -> (depth, #[], [curr_str, ..splitted])
                _ -> (depth, bytearray.push(curr_str, curr_byte), splitted)
              }
            },
          )

        if depth == 0 {
          list.map([curr_arg, ..splitted_args], internal_map_str_to_type)
        } else {
          trace depth
          fail @"internal_map_str_to_type: final depth is non zero"
        }
      } else {
        trace str
        fail @"internal_map_str_to_type: malformed str"
      }
    }

  when bytearray.index_of(str, "<") is {
    None -> TypeWithnessC(str, [])
    Some((open_type_args_index, _)) -> {
      let (type_cons, type_args) =
        (
          bytearray.take(str, open_type_args_index),
          bytearray.drop(str, open_type_args_index),
        )

      TypeWithnessC(type_cons, helper(type_args))
    }
  }
}

pub fn assert(f: fn() -> Bool) {
  if !f() {
    fail @"Assertion failed"
  } else {
    expect True
  }
}

pub fn as_data(data: Data) -> Data {
  data
}

pub fn consume_data(data: Data) {
  let val =
    if data is List<Bool> {
      1
    } else {
      2
    }

  expect val > 0
}

/// Expects the input of being `Some(data)` and returns it
/// If the input is `None`, throws a custom error message
pub fn expect_some(val: Option<value>, error_message: Str) -> value {
  when val is {
    Some(data) -> data
    None -> fail string.from_bytearray(error_message)
  }
}

/// Returns the `Address` associated to this contract
/// Also see `parse_contract_address` to use it in the program
pub fn get_contract_address(utxo: OutputReference, self: Transaction) -> Address {
  expect Some(own_input) = transaction.find_input(self.inputs, utxo)

  own_input.output.address
}

/// Given the contract's `Address`, retrieves his hash `Addr` so to be used in the program
pub fn parse_contract_address(address: Address) -> Addr {
  // Since it it a contract, it must be of type constructor Script and not VerificationKey
  expect Script(script_hash) = address.payment_credential

  script_hash
}

pub fn fail_msg(left: ByteArray, right: ByteArray) -> String {
  string.from_bytearray(bytearray.concat(left, right))
}

/// Used to read a nested field of some Data by a sequence of field indices
pub fn internal_get_data(data: Data, fields: List<Int>) -> Data {
  when fields is {
    [] -> data
    [f, ..fs] -> {
      let constr_fields: List<Data> = builtin.unconstr_fields(data)
      expect Some(field_val) = list.at(constr_fields, f)
      internal_get_data(field_val, fs)
    }
  }
}

/// Used to extract the nested fields of some Data and overwite them
pub fn internal_put_data(data: Data, fields: List<Int>, val: Data) -> Data {
  when fields is {
    [] -> val
    [f, ..fs] -> {
      let constr_fields: List<Data> = builtin.unconstr_fields(data)

      let remapped_fields: List<Data> =
        list.indexed_map(
          constr_fields,
          fn(idx, el) {
            if idx == f {
              internal_put_data(el, fs, val)
            } else {
              el
            }
          },
        )

      builtin.constr_data(0, remapped_fields)
    }
  }
}

/// Used internally to dereference a ref to the scope
/// Defined here to avoid circular depencency
pub fn internal_deref_scope(ref: Reference<a>, (scopes, gs): CPS) -> Data {
  let (variant, fields) = ref
  expect ScopeRef{resource: ScopePtr(height, ident, binding_idx)} = variant

  expect Some(scope) = list.at(scopes, list.length(scopes) - height)

  expect Some(bindings) = dict.get(scope, ident)
  expect Some(val) = list.at(bindings, list.length(bindings) - binding_idx)

  internal_get_data(val, fields)
}
