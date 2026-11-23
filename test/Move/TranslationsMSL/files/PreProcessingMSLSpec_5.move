module NamedAddr::PreProcessingMSLStrip {
    use std::signer::{address_of};

    struct Counter has key {
        value: u64
    }

    const MAX: u64 = 10;

    spec increment {
        ensures global<Counter>(address_of(s)).value == old(global<Counter>(address_of(s)).value) + 1;
    }

    spec schema CounterSchema {
        addr: address;
    }
}
