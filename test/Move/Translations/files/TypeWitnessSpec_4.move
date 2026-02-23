module NamedAddr::TestingModule {
    struct MyCoin<phantom CoinType, B> has key, store {
        val: u64,
        b: B
    }

    struct PosStr<A, B>(A, MyCoin<u64, B>);
}
