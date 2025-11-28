module NamedAddr::TestingModule {
    struct S {
        s: u64
    }

    fun test(x: bool): u64 {
        let s = S { s: 12 };
        let r = &mut x;
        let r2 = &s.s;

        let a = if (*r) *r2 > 0 else !(*r);

        let b = *(&a);

        x
    }
}
