module NamedAddr::Base {
    struct Store<phantom T> has key {
        value: u64
    }

    spec schema HasStore<T> {
        addr: address;
        aborts_if !exists<Store<T>>(addr);
    }
}
