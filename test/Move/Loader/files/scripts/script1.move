script {
    use NamedAddr::module1;

    const ONE: u64 = 1;

    fun main(x: u64) {
        let sum = x + ONE;

        NamedAddr::module1::my_test(sum)
    }
}
