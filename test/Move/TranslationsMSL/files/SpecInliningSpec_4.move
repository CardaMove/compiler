module NamedAddr::Premises {
    struct R has key {
        v: u64
    }

    spec schema Inner {
        x: u64;
        aborts_if x == 0;
        ensures x > 0;
    }

    spec schema Outer {
        a: address;
        x: u64;
        let v = global<R>(a).v;
        // `x` is not passed: it binds by name to the variable of Outer
        include x > 1 ==> Inner;
        aborts_if v == 0;
        ensures v > 0;
        modifies global<R>(a);
    }

    fun f(addr: address, flag: bool, n: u64) {}

    spec f {
        include flag ==> Outer { a: addr, x: n };
        include flag ==> (n > 2 ==> Inner { x: n });
        // Left-associative: the premise is `flag ==> n > 2`
        include flag ==> n > 2 ==> Inner { x: n };
    }
}
