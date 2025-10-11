module NamedAddr::TraversalUtilsSpec {
    fun mapped_while_0(i: &mut u256, n: &mut u64, sum: &mut u256) {
        if (*i <= *n) {
            {
                let break_hit: bool = false;
                let continue_hit: bool = false;
                let inner = 42;
                *sum = *sum + *i;
                if (*i == 100) {
                    break_hit = true;
                    if (!break_hit && !continue_hit) {
                        let a = 12;
                    };
                };
                if (!break_hit && !continue_hit) {
                    if (*n == 50) {
                        continue_hit = true;
                    };
                    if (!break_hit && !continue_hit) {
                        let (a, b): (u64, u64);
                    };
                };
                // This end expression of a sequence needs to be wrapped
                if (!break_hit && !continue_hit) inner + 1
            };
            // The recursive call needs to be wrapped in an if-then
            if (!break_hit) mapped_while_0(i, n, sum)
        }
    }

    public fun whileLoop(n: u64): u64 {
        let sum = 0;
        let i = 1;
        mapped_while_0(&mut i, &mut n, &mut sum);
        sum
    }
}
