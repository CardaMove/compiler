script {
    use std::signer::{address_of};
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

    spec init_round {
        ensures publish<Bet::Round<CoinType>>(
            address_of(bookmaker),
            Bet::Round<CoinType> { player1, player2, oracle, stake, deadline }
        );

        // Frame: only the new Round is written (`publish` already frames the other addresses).
        //     Bet::init declares the same target.
        modifies global<Bet::Round<CoinType>>(address_of(bookmaker));
    }
}
