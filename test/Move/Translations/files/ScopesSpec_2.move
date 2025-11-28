module NamedAddr::TestingModule {
    struct S {
        s: u64
    }

    fun test(x: bool): u64 {
        x = x + 1;
        let a: &mut u64 = &mut x;

        *a = *a + 1;

        let s = S { s: x + 1 };

        s.s = s.s + 1;

        x
    }
}
