module NamedAddr::Module1 {
    public fun my_test(x: u64) {
        let a = 12;
        while (a < x) {
            let b = 9;
            a = a + b;
        }
    }
}
