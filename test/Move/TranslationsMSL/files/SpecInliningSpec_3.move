script {
    use NamedAddr::Schemas::{Self};
    use aptos_framework::coin;
    use std::signer;

    fun main(account: &signer, amount: u64) {}

    spec main {
        // Same name as a let of the schema: the clause below still reads this one
        let addr = @0x1;
        let addr_3 = std::signer::address_of(account);
        let pre_value_4 = global<NamedAddr::Schemas::Counter>(addr_3).value;
        let post post_value_5 = global<NamedAddr::Schemas::Counter>(addr_3).value;
        aborts_if !exists<aptos_framework::coin::CoinStore<u64>>(addr_3);
        ensures post_value_5 == pre_value_4 + (amount + 1);
        ensures NamedAddr::Schemas::Counter { value: amount + 1 } == NamedAddr::Schemas::Counter { value: post_value_5 - pre_value_4 };
        modifies global<NamedAddr::Schemas::Counter>(addr_3);
        ensures addr == @0x1;
        // Included twice: each copy gets its own lets
        let addr_6 = std::signer::address_of(account);
        let pre_value_7 = global<NamedAddr::Schemas::Counter>(addr_6).value;
        let post post_value_8 = global<NamedAddr::Schemas::Counter>(addr_6).value;
        aborts_if !exists<aptos_framework::coin::CoinStore<u64>>(addr_6);
        ensures post_value_8 == pre_value_7 + amount;
        ensures NamedAddr::Schemas::Counter { value: amount } == NamedAddr::Schemas::Counter { value: post_value_8 - pre_value_7 };
        modifies global<NamedAddr::Schemas::Counter>(addr_6);
    }
}
