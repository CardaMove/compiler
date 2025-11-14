module NamedAddr::TraversalUtilsSpec {
    public fun test(x: u64) { // UUID is 0
        let a = 12; // UUID is 1
        a = a + 1;

        let b = a + 1; // UUID is 2
        let r = &mut a; // UUID is 3
        let c: &mut u64 = &mut x; // UUID is 4
        r = c;
    }
}
