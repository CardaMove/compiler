module NamedAddr::TestingModule {
    // Should accept the scope and return it in a tuple
    fun test(x: u64): u64 {
        // should push a new scope
        // x should be POST-ed in the local scope
        // assignment should be rewritten with GET and PUT on lcoal scope
        x = x + 1;

        // Should return a tuple with the scope
        x
    }
}
