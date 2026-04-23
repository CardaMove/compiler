script {
    use NamedAddr::Module1;

    const one: u64 = 1;

    fun main(x: u64) {
        let sum = x + one;

        NamedAddr::Module1::my_test(sum)
    }
}
