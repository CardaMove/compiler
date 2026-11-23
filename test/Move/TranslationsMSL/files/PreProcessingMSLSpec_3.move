module NamedAddr::PreProcessingMSLStrip {
    use std::signer::{address_of};

    struct Counter has key {
        value: u64
    }

    const MAX: u64 = 10;

    public fun increment(s: &signer) acquires Counter {
        let counter = borrow_global_mut<Counter>(address_of(s));
        counter.value = counter.value + 1;
    }

    spec increment {
        ensures global<Counter>(address_of(s)).value == old(global<Counter>(address_of(s)).value) + 1;
    }

    fun helper(): u64 {
        MAX
    }

    spec schema CounterSchema {
        addr: address;
    }
}
