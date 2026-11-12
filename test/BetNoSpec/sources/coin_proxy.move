/// Proxy over `aptos_framework::coin` for the Move Prover.
///
/// Each function forwards to the `coin` function of the same name and carries a trusted spec over a
/// SIMPLIFIED Coin model, where the balance of an account lives in `CoinStore<CoinType>`.
/// The real `coin` functions (Aptos framework, mainnet) ignore `CoinStore` and operate on the paired
/// fungible asset's primary store, possibly through custom dispatch hooks, so these specs cannot be
/// proved against them. `pragma verify = false` skips checking the bodies; callers (the functions
/// being `opaque`) rely on the specs as assumptions. Same approach as the framework's own spec of
/// `coin::withdraw`.
///
/// Every spec has a COMPLETE abort specification (no `aborts_if_is_partial`): with a partial or
/// missing one an opaque call gives callers no abort information at all, and they must assume it may
/// abort anywhere.
module CoinProxyAddr::CoinProxy {
    use aptos_framework::coin::{Coin, Self};

    public fun withdraw<CoinType>(s: &signer, amount: u64): Coin<CoinType> {
        coin::withdraw<CoinType>(s, amount)
    }

    public fun merge<CoinType>(
        dst_coin: &mut Coin<CoinType>, source_coin: Coin<CoinType>
    ): () {
        coin::merge<CoinType>(dst_coin, source_coin);
    }

    public fun deposit<CoinType>(
        account_addr: address, coin: Coin<CoinType>
    ): () {
        coin::deposit<CoinType>(account_addr, coin);
    }

    public fun value<CoinType>(coin: &Coin<CoinType>): u64 {
        coin::value<CoinType>(coin)
    }
}
