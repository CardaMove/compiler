script {
    fun do_nothing<CoinType>(s: &signer, amount: u64) {
    }

    spec do_nothing {
        ensures amount == amount;
    }
}
