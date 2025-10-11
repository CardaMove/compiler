module NamedAddr::TraversalUtilsSpec {
    fun mapped_while_0(a: &mut u256, x: &mut u64) {
        // The body of the while is inside a Sequence, with the end expression as the recursive call
        // Note: a comparison between two different numeric types is not allowed in Move
        if (*a < *x) {
            {
                let b = 9;
                *a = *a + b;
            };
            mapped_while_0(a, x)
        }
    }

    // The original function calls the newly created one
    public fun test(x: u64) {
        let a = 12;
        mapped_while_0(&mut a, &mut x)
    }
}
