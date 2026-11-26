module NamedAddr::Premises {
    struct R has key {
        v: u64
    }

    fun f(addr: address, flag: bool, n: u64) {}

    spec f {
        let v_0 = global<R>(addr).v;
        aborts_if flag && (n > 1 && n == 0);
        ensures flag ==> (n > 1 ==> n > 0);
        aborts_if flag && v_0 == 0;
        ensures flag ==> v_0 > 0;
        modifies global<R>(addr);
        aborts_if flag && (n > 2 && n == 0);
        ensures flag ==> (n > 2 ==> n > 0);
        aborts_if (flag ==> n > 2) && n == 0;
        ensures (flag ==> n > 2) ==> n > 0;
    }
}
