module NamedAddr::TestingModule {
    public fun test(x: u64) { // UUID is 0
        let a = 12; // UUID 1
        x = a + 1; // x in scope

        let r = &a; // UUID 2, a in scope
        let r2 = &a; // UUID 3
        r2 = r; // r2 in scope

        if (x > 10) {
            let b = 1; // UUID 4
            let r3; // = &(1 + b); // UUID 5 To be handled with temporary variable
        };

        let c = // UUID 6
            if (x > 10) {
                let d = 1; // UUID 7
                &mut d // d in scope. Also d should be lifted
            } else {
                &mut a //1 // To be handled with temporary variable and lifted
            };

        let (e, f) = (1, 2); // UUID (8, 9)

        f = f + 1; // f in scope
    }
}
