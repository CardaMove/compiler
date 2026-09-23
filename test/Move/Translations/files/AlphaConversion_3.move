module NamedAddr::Module5 {
    struct S { x: u64 }

    fun main() {
        let S { x } = S { x: 1 };
        let x = 2;
        x
    }
}
