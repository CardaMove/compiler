script {
    use BetAddr::Bet;

    // Wins a round
    fun win_round<CoinType>(
        oracle: &signer, winner: address, bookmaker: address
    ) {
        Bet::win<CoinType>(oracle, winner, bookmaker)
    }
}
