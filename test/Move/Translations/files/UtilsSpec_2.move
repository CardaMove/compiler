module NamedAddr::UtilsSpecBlocks {
    public fun add(a: u64, b: u64): u64 {
        let c = a + b;
        c
    }

    spec add {
        let one = 1;
        let post two = one + 1;
        aborts_if one > 2;
        ensures result == a + b + one;
    }

    spec schema AddSchema<T> {
        x: u64;
        y<U>: U;
        let z = x + 3;
        include OtherSchema { v: z };
        ensures z == x + 3;
    }
}
