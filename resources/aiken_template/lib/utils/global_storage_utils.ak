use aiken/collection/dict
use utils/common_utils.{
  Addr, CPS, GlobalStorageRef, ProgramState, Reference, TWitness, TypeWithnessC,
  assert,
}
use utils/signer_utils.{Signer}

// If the value in state[addr][resource_type] already exists, it is overwritten
/// same behaviour of `move_to`, but used internally to translate mutable references to the storage
pub fn internal_move_to(
  addr: Addr,
  resource_type: TWitness,
  resource_value: Data,
  (scopes, state): CPS,
) -> CPS {
  let type_str = common_utils.internal_map_type_to_str(resource_type)

  let ProgramState(state_dict) = state

  (
    scopes,
    ProgramState(
      state_dict
        |> dict.insert_with(
            key: addr,
            value: dict.from_pairs([Pair(type_str, resource_value)]),
            // This function merges the `state[addr]` so to prevent overriding already existing keys in the inner dict
            // If a key in the sub dicts already exists, its value is overriden since dict.union gives priority to the first argument
            with: fn(_key, existing_value, new_value) {
              Some(dict.union(new_value, existing_value))
            },
          ),
    ),
  )
}

/// Publishes `resource_value` under `state[signer_address][resource_type]`
/// Aborts if the signer already holds the resource
pub fn move_to(
  signer: Reference<Signer>,
  resource_type: TWitness,
  resource_value: Data,
  (scopes, state): CPS,
) -> (Void, CPS) {
  let (signer_addr, (scopes, state)) = signer_utils.address_of(signer, (scopes, state))
  assert(fn() { !exists(signer_addr, resource_type, (scopes, state)) })

  (
    Void,
    internal_move_to(
      signer_addr,
      resource_type,
      resource_value,
      (scopes, state),
    ),
  )
}

// Beware https://github.com/aiken-lang/aiken/issues/1181
// test move_to_test() {
//   let deserialized_sub_state_1 =
//     dict.empty
//       |> dict.insert("key1", as_data(0))
//       |> dict.insert("key2", as_data("Hello world"))
//       |> dict.insert("key3", as_data(42))
//       |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_sub_state_2 =
//     dict.empty
//       |> dict.insert("key1", as_data(0))
//       |> dict.insert("key2", as_data("Hello world"))
//       |> dict.insert("key3", as_data(42))
//       |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_state: ProgramState =
//     ProgramState(
//       dict.empty
//         |> dict.insert("addr1", deserialized_sub_state_1)
//         |> dict.insert("addr2", deserialized_sub_state_2),
//     )
//   let coin_type: Str = "my_coin"
//   let signer: Signer = Signer { address: "addr1" }
//   let resource_type: TWitness =
//     TypeWithnessC("Oracle", [TypeWithnessC(coin_type, [])])
//   let resource_value: List<Int> = [1, 2, 3, 4, 5]

//   let (_, output_state) =
//     move_to(signer, resource_type, resource_value, ([], deserialized_state))

//   let deserialized_sub_state_1 =
//     dict.empty
//       |> dict.insert("key1", as_data(0))
//       |> dict.insert("key2", as_data("Hello world"))
//       |> dict.insert("key3", as_data(42))
//       |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))
//       |> dict.insert("Oracle<my_coin>", as_data(resource_value))

//   // Testing an insertion at state["addr1"]["Oracle<my_coin>"]
//   output_state == ProgramState(
//     dict.empty
//       |> dict.insert("addr1", deserialized_sub_state_1)
//       |> dict.insert("addr2", deserialized_sub_state_2),
//   )
// }

// test move_to_test_fail() fail {
//   let deserialized_sub_state_1 =
//     dict.empty
//       |> dict.insert("key1", as_data(0))
//       |> dict.insert("key2", as_data("Hello world"))
//       |> dict.insert("key3", as_data(42))
//       |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_sub_state_2 =
//     dict.empty
//       |> dict.insert("key1", as_data(0))
//       |> dict.insert("key2", as_data("Hello world"))
//       |> dict.insert("key3", as_data(42))
//       |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_state: ProgramState =
//     ProgramState(
//       dict.empty
//         |> dict.insert("addr1", deserialized_sub_state_1)
//         |> dict.insert("addr2", deserialized_sub_state_2),
//     )
//   let signer: Signer = Signer { address: "addr1" }
//   let resource_type: TWitness = TypeWithnessC("key4", [])
//   let resource_value: List<Int> = [1, 2, 3, 4, 5]

//   let (_, output_state) =
//     move_to(signer, resource_type, resource_value, ([], deserialized_state))

//   // Need to use the value
//   output_state == output_state
// }

/// Removes the resource under `state[addr][resource_type]` and returns it
/// Aborts if the address does not hold the resource
pub fn move_from(
  addr: Addr,
  resource_type: TWitness,
  (scopes, state): CPS,
) -> (Data, CPS) {
  let ProgramState(state_dict) = state

  expect Some(addr_state) =
    state_dict
      |> dict.get(addr)

  let type_str = common_utils.internal_map_type_to_str(resource_type)

  expect Some(resource) =
    addr_state
      |> dict.get(type_str)

  (
    resource,
    (
      scopes,
      ProgramState(
        dict.insert(state_dict, addr, dict.delete(addr_state, type_str)),
      ),
    ),
  )
}

/// Returns the corresponding resource under `state[addr][resource_type]`
/// Aborts if the address does not hold the resource
pub fn borrow_global(
  addr: Addr,
  resource_type: TWitness,
  (scopes, state): CPS,
) -> Reference<resource> {
  if exists(addr, resource_type, (scopes, state)) {
    (GlobalStorageRef { addr, resource_type }, [])
  } else {
    fail @"borrow_global invoked for non existing resource"
  }
}

// test borrow_global_test() {
//   let deserialized_sub_state_1 = dict.empty
//     |> dict.insert("key1", as_data(0))
//     |> dict.insert("key2", as_data("Hello world"))
//     |> dict.insert("key3", as_data(42))
//     |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_sub_state_2 = dict.empty
//     |> dict.insert("key1", as_data(0))
//     |> dict.insert("key2", as_data("Hello world"))
//     |> dict.insert("key3", as_data(42))
//     |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_state: ProgramState = ProgramState(
//     dict.empty
//     |> dict.insert("addr1", deserialized_sub_state_1)
//     |> dict.insert("addr2", deserialized_sub_state_2)
//   )

//   let borrowed = borrow_global("addr2", TypeWithnessC("key4", []), deserialized_state)

//   borrowed == as_data([Pair("a", 1), Pair("b", 2)])
// }

// test borrow_global_test_expect() {
//   let deserialized_sub_state_1 = dict.empty
//     |> dict.insert("key1", as_data(0))
//     |> dict.insert("key2", as_data("Hello world"))
//     |> dict.insert("key3", as_data(42))
//     |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_sub_state_2 = dict.empty
//     |> dict.insert("key1", as_data(0))
//     |> dict.insert("key2", as_data("Hello world"))
//     |> dict.insert("key3", as_data(42))
//     |> dict.insert("key4", as_data([Pair("a", 1), Pair("b", 2)]))

//   let deserialized_state: ProgramState = ProgramState(
//     dict.empty
//     |> dict.insert("addr1", deserialized_sub_state_1)
//     |> dict.insert("addr2", deserialized_sub_state_2)
//   )

//   expect borrowed: List<Pair<Str, Int>> = borrow_global("addr2", TypeWithnessC("key4", []), deserialized_state)
//   borrowed == [Pair("a", 1), Pair("b", 2)]
// }

/// Returns True if the address holds the resource, meaning if `state[addr][resource_type]` exists
pub fn exists(addr: Addr, resource_type: TWitness, (scopes, state): CPS) -> Bool {
  let ProgramState(state_dict) = state

  when dict.get(state_dict, addr) is {
    None -> False
    Some(addr_state) ->
      when
        dict.get(
          addr_state,
          common_utils.internal_map_type_to_str(resource_type),
        )
      is {
        None -> False
        Some(_) -> True
      }
  }
}

/// Returns the resource without removin it
pub fn internal_get_global_storage(
  addr: Addr,
  resource_type: TWitness,
  state: ProgramState,
) -> Data {
  let ProgramState(state_dict) = state

  when dict.get(state_dict, addr) is {
    None -> fail @"internal_get_global_storage: Missing resource addr"
    Some(addr_state) ->
      when
        dict.get(
          addr_state,
          common_utils.internal_map_type_to_str(resource_type),
        )
      is {
        None -> fail @"internal_get_global_storage: Missing resource"
        Some(resource) -> resource
      }
  }
}
