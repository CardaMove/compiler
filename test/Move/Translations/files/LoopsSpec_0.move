module NamedAddr::TraversalUtilsSpec {
    public fun test(x: u64) {
        let a = 12;
        while (a < x) {
            let b = 9;
            a = a + b;
        }
    }
}
