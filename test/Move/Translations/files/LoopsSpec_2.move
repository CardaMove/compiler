module NamedAddr::TraversalUtilsSpec {
    public fun whileLoop(n: u64): u64 {
        let sum = 0;
        let i = 1;
        while (i <= n) {
            let inner = 42;
            sum = sum + i;
            if (i == 100) {
                break;
                // Useless, but other instructions can still be added after a break
                let a = 12;
            };
            if (n == 50) {
                continue;
            };
            let (a, b): (u64, u64);
            inner + 1
        };
        sum
    }
}
