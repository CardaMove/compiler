module NamedAddr::TestingModule {
    struct A {
        num: u64
    }

    struct B {
        a: A
    }

    // Should accept the scope and return it in a tuple
    fun test(x: u64): u64 {
        // should push a new scope
        // POST x
        // GET x, PUT x
        x = x + 1;

        // GET x, POST a
        let a = A { num: x + 1 };

        // Reference to scope
        let n = &mut a.num;

        // GET-dereference and PUT on scope
        *n = *n + 1;

        // Should return a tuple with the scope
        x
    }
}
