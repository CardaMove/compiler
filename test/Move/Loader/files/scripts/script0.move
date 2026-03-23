script {
    use NamedAddr::Module1;

    const ONE: u64 = 1;

    fun main(x: u64) {
        let sum = x + ONE;

        NamedAddr::Module1::my_test(sum)
    }
}
