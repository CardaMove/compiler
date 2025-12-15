module NamedAddr::TestingModule {
    struct A {
        num: u64
    }

    struct B {
        a: A
    }

    // Scope as params and as return
    fun test2(a: &A): B {
        // Push scope
        // GET-deref a
        // Also return tail scopes
        return B { a: *a }
    }

    // Scope as params and as return
    fun test() {
        // Push scope
        // POST a
        let a = A { num: 1 };

        // Extract sequence `let (temp_i, scopes) = {...}`
        // Add scope
        let b = {
            // Push scope
            // Scope as params and as return
            // `let (temp_i, scopes) = test2(&a, scopes)`
            // POST c, reference-state a
            let c = test2(&a);

            // (GET c).a.num, PUT c.a.num
            c.a.num = c.a.num + 1;

            // GET c
            c
        };

        // Return tail scopes
    }
}
