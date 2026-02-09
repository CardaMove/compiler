module NamedAddr::TestingModule {
    struct MyCoin<phantom CoinType> has key, store {
        val: u64
    }

    fun withdraw<CoinType>(from: address): MyCoin<CoinType> acquires MyCoin {
        move_from<MyCoin<CoinType>>(from)
    }

    fun deposit<CoinType, UnusedType>(to: &signer, coin: MyCoin<CoinType>) {
        // Interesing error when passing a non-struct type parameter,
        // such as `move_to<T>(...)`:
        // error: Expected a struct type. Global storage operations are restricted to struct types declared in the current module.
        move_to<MyCoin<CoinType>>(to, coin)
    }

    fun my_pay<CoinType>(from: address, to: &signer) acquires MyCoin {
        let c = withdraw<CoinType>(from);
        deposit<CoinType, MyCoin<u64>>(to, c)
    }
}
