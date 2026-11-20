module NamedAddr::TraversalUtilsSpecBlocks {
    public fun add(a: u64, b: u64): u64 {
        a + b + 2
    }

    spec add {
        pragma opaque;
        let one_2 = 2;
        let post two_2 = one + 2;
        aborts_if one > 3;
        ensures result == a + b + one;
    }

    spec schema AddSchema<T> {
        x_2: u64;
        y_2<U>: U;
        let z_2 = x + 4;
        include OtherSchema { v: z };
        ensures z == x + 4;
    }
}
