module NamedAddr::TestingModule {
    struct F<T1> has key, store {
        val: T1
    }

    fun noT(a: u64): () {}

    fun twoT<T, AnotherT>(a: u64, b: bool): F<AnotherT> {
        // Note that does not compile since does not return anything
    }
}
