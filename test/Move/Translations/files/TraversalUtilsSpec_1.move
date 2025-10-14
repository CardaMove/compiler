module NamedAddr::TraversalUtilsSpec {
    const MY_CONST: u64 = 1;
    const MY_CONST2: u64 = { MY_CONST + 7 };
    public fun test(x: u64) {
        let a = 13;
        let b = 7 * 8;
        let c = { a + b + MY_CONST + 2 };
        c + 11
    }
}
