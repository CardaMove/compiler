module NamedAddr::Schemas {
    use std::signer::{address_of};
    use aptos_framework::coin;

    struct Counter has key {
        value: u64
    }

    spec schema IncrementSchema<T> {
        s: &signer;
        amount: u64;

        let addr = address_of(s);
        let pre_value = global<Counter>(addr).value;
        let post post_value = global<Counter>(addr).value;
        aborts_if !exists<coin::CoinStore<T>>(addr);
        ensures post_value == pre_value + amount;
        ensures Counter { value: amount } == Counter { value: post_value - pre_value };
        modifies global<Counter>(addr);
    }

    fun local_user(account: &signer, amount: u64) {}

    // Included from its own module: its names stay local, the imported ones are qualified
    spec local_user {
        include IncrementSchema<u64> { s: account, amount };
    }
}
