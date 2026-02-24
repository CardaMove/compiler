module NamedAddr::TestingModule {
    use std::signer::address_of;

    struct F<T1> has key, store {
        val: T1
    }

    struct G<phantom T2> has key {
        f: F<u64>
    }

    // Add and return scopes
    fun test(s: &signer, a1: address) acquires F, G {
        // Push scope
        // Extract `move_from`
        // POST f
        let f = move_from<F<u64>>(a1);

        // GET f, PUT f.val
        f.val = f.val + 1;

        // GET f
        let g = G<bool> { f: f };

        // Extract `move_to`
        // unused temp_i expression
        move_to<G<bool>>(s, g);

        // Extract sequence `let (temp_i, scopes) = {...}`
        let a = {
            // Push scope
            // POST g2 (this is because g2.f.val marks it as mutated, even if it is a syntactic sugar)
            let g2 = borrow_global_mut<G<bool>>(address_of(s));

            // (GET g2).f.val, PUT-deref (g2.f.val)
            // Note syntactic sugar for dereferencing g2
            g2.f.val = g2.f.val * 5;

            // (GET g2).f.val
            // Also return scope
            g2.f.val
        };
    }
}
