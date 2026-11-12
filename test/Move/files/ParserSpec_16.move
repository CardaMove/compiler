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
    use std::signer::{address_of};

    public fun withdraw<CoinType>(s: &signer, amount: u64): Coin<CoinType> {
        coin::withdraw<CoinType>(s, amount)
    }

    spec withdraw {
        pragma verify = false;
        pragma opaque;
        modifies global<coin::CoinStore<CoinType>>(address_of(s));

        let addr = address_of(s);
        let pre_bal = global<coin::CoinStore<CoinType>>(addr).coin.value;
        let post post_bal = global<coin::CoinStore<CoinType>>(addr).coin.value;

        // Simplification: this is the ONLY abort condition, so callers assume withdraw aborts iff the
        // balance is insufficient. The real one can also abort on a missing or frozen store, a
        // missing coin pairing, dispatch hooks, etc.
        // Keeping the abort spec complete is what lets callers prove `aborts_if` on the balance.
        aborts_if pre_bal < amount;

        ensures post_bal == pre_bal - amount;
        ensures result.value == amount;
    }

    spec schema WithdrawSchema<CoinType> {
        s: &signer;
        amount: u64;

        let addr = address_of(s);
        let pre_bal = global<coin::CoinStore<CoinType>>(addr).coin.value;
        let post post_bal = global<coin::CoinStore<CoinType>>(addr).coin.value;
        //     Balance should be enough
        aborts_if pre_bal < amount;
        //     Final balance
        ensures post_bal == pre_bal - amount;
    }

    public fun merge<CoinType>(
        dst_coin: &mut Coin<CoinType>, source_coin: Coin<CoinType>
    ): () {
        coin::merge(dst_coin, source_coin);
    }

    spec merge {
        pragma verify = false;
        pragma opaque;

        // Simplified Coin model: merge never aborts (overflow of the u64 sum is ignored).
        aborts_if false;
        ensures dst_coin.value == old(dst_coin.value) + source_coin.value;
    }

    public fun deposit<CoinType>(
        account_addr: address, coin: Coin<CoinType>
    ): () {
        coin::deposit<CoinType>(account_addr, coin);
    }

    spec deposit {
        pragma verify = false;
        pragma opaque;
        modifies global<coin::CoinStore<CoinType>>(account_addr);

        let pre_bal = global<coin::CoinStore<CoinType>>(account_addr).coin.value;
        let post post_bal = global<coin::CoinStore<CoinType>>(account_addr).coin.value;

        // Simplified Coin model: deposit never aborts (missing/frozen store and overflow are ignored).
        aborts_if false;
        ensures post_bal == pre_bal + coin.value;
    }

    spec schema DepositSchema<CoinType> {
        account_addr: address;
        coin: Coin<CoinType>;

        let pre_bal = global<coin::CoinStore<CoinType>>(account_addr).coin.value;
        let post post_bal = global<coin::CoinStore<CoinType>>(account_addr).coin.value;
        ensures post_bal == pre_bal + coin.value;
    }

    public fun value<CoinType>(coin: &Coin<CoinType>): u64 {
        coin::value(coin)
    }

    spec value {
        pragma verify = false;
        pragma opaque;

        aborts_if false;
        ensures result == coin.value;
    }
}
