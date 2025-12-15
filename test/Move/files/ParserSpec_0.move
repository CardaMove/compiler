module NamedAddr::TestingModule {
    struct F<T1> has key, store {
        val: T1
    }

    struct G<phantom T2> has key {
        f: F<u64>
    }

    // Add and return scopes
    fun test(s: &signer, a1: address) acquires F, G {
        // Push scope
        // `let (f, scopes) = ...`
        let f = move_from<F<u64>>(a1);

        let g = G<bool> { f: f };

        // `let scopes = ...`
        move_to(s, g);

        // Extract sequence `let (temp_i, scopes) = {...}`
        let a = {
            // Push scope
            let g2 = borrow_global_mut<G<bool>>(address_of(s));

            // (GET g2).f.val, PUT (g2).f.val
            g2.f.val = g2.f.val * 5;

            // (GET g2).f.val
            // Also return scope
            g2.f.val
        };
    }
}
