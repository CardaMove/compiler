module NamedAddr::PreProcessingMSLSpecBlocks {
    public fun add(a: u64, b: u64): u64 {
        a + b
    }

    public fun generic_id<T: copy + drop, U>(x: T): T {
        x
    }

    spec generic_id {
        ensures result == x;
    }

    spec schema AddSchema<T> {
        x: u64;
    }

    spec add {
        ensures result == a + b;
    }
}
