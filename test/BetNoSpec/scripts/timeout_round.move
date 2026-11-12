script {
    use BetAddr::Bet;

    // Recaims funds
    fun timeout_round<CoinType>(bookmaker: address) {
        Bet::timeout<CoinType>(bookmaker)
    }
}
