script {
    use BetAddr::Bet;

    // Initalizes a round
    fun init_round<CoinType>(
        bookmaker: &signer,
        player1: address,
        player2: address,
        oracle: address,
        stake: u64,
        deadline: u64
    ) {
        Bet::init<CoinType>(
            bookmaker,
            player1,
            player2,
            oracle,
            stake,
            deadline
        )
    }
}
