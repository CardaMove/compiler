module NamedAddr::Schemas {
    use std::signer::{address_of};
    use aptos_framework::coin;

    struct Counter has key {
        value: u64
    }

    fun local_user(account: &signer, amount: u64) {}

    // Included from its own module: its names stay local, the imported ones are qualified
    spec local_user {
        let addr_0 = std::signer::address_of(account);
        let pre_value_1 = global<Counter>(addr_0).value;
        let post post_value_2 = global<Counter>(addr_0).value;
        aborts_if !exists<aptos_framework::coin::CoinStore<u64>>(addr_0);
        ensures post_value_2 == pre_value_1 + amount;
        ensures Counter { value: amount } == Counter { value: post_value_2 - pre_value_1 };
        modifies global<Counter>(addr_0);
    }
}
