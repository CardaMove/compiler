module NamedAddr::Base {
    struct Store<phantom T> has key {
        value: u64
    }
}
