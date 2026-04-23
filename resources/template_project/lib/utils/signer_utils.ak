use aiken/collection/list
use cardano/transaction.{Transaction}
use utils/common_utils.{Addr, Reference, CPS}

/// Signer of the transaction
pub type Signer {
  address: Addr,
}

/// Defined in Move standard library
pub fn signer_address_of(signer: Reference<Signer>, (scopes, gs): CPS) -> (Addr, CPS) {
  expect Signer { address } = common_utils.internal_deref_scope(signer, (scopes, gs))
  (address, (scopes, gs))
}

/// Used internally instead of passing a reference to the signer
pub fn internal_signer_address_of_unref(signer: Signer) -> Addr {
  let Signer { address } = signer
  address
}

/// Returns the signer of a transaction
pub fn get_tx_signer(tx: Transaction) -> Signer {
  expect Some(signer_addr) = list.at(tx.extra_signatories, 0)
  Signer { address: signer_addr }
}

/// TODO: borrow_address, with CPS