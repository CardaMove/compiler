module NamedAddr::Module2 {
    use std::error;
    use std::signer;

    struct MyCoin<phantom CoinType>{
        val: u64
    }
}
