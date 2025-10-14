module NamedAddr::TraversalUtilsSpec {
    const MY_CONST: u64 = 0;
    const MY_CONST2: u64 = { MY_CONST + 6 };
    public fun test(x: u64) {
        let a = 12;
        let b = 6 * 7;
        let c = { a + b + MY_CONST + 1 };
        c + 10
    }
}
