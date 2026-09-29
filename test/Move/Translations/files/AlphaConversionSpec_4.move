module NamedAddr::Module5 {
    struct S { x: u64 }

    fun main() {
        let x = 0;
        let S { x } = S { x: 1 };
        x
    }
}
