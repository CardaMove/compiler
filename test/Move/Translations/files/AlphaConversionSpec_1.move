module NamedAddr::Module5 {
    fun main() {
        let a = 1;
        { let a = 2; a };
        let a = 3;
        a
    }
}
