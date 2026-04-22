script {
    use BetAddr::Bet;

    // Joines a round
    fun join_round<CoinType>(partecipant: &signer, bookmaker: address) {
        Bet::join<CoinType>(partecipant, bookmaker)
    }
}
