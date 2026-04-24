module NamedAddr::Module3 {
    struct F<T1> has key, store {
        val: T1
    }

    fun noT(a: u64): () {}

    fun my_fun<T, AnotherT>(a: u64, b: bool, val: AnotherT): F<AnotherT> {
        F<AnotherT> { val: val }
    }

    struct MyCoin<phantom CoinType> has key, store {
        val: u64
    }

    fun withdraw<CoinType>(from: address): MyCoin<CoinType> acquires MyCoin {
        move_from<MyCoin<CoinType>>(from)
    }

    fun deposit<CoinType, UnusedType>(to: &signer, coin: MyCoin<CoinType>) {
        move_to<MyCoin<CoinType>>(to, coin)
    }

    fun my_pay<CoinType>(from: address, to: &signer) acquires MyCoin {
        let c = withdraw<CoinType>(from);
        deposit<CoinType, u64>(to, c)
    }
}
