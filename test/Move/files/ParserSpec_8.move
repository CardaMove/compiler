script {
    use BetAddr::Bet;
    use CoinProxyAddr::CoinProxy::{Self};
    use aptos_framework::timestamp;
    use aptos_framework::coin::{CoinStore};

    // Recaims funds
    fun timeout_round<CoinType>(bookmaker: address) {
        Bet::timeout<CoinType>(bookmaker)
    }

    spec timeout_round {
        let round = global<Bet::Round<CoinType>>(bookmaker);

        // 1 - move_from of the Round
        aborts_if !exists<Bet::Round<CoinType>>(bookmaker);
        //     Single remove of this type, so `remove` (which also frames every other address) works
        ensures remove<Bet::Round<CoinType>>(bookmaker);

        // 2 - Timestamp: timestamp::now_seconds aborts if the clock resource is not published
        //     (only before genesis), and the deadline must have passed
        aborts_if !exists<timestamp::CurrentTimeMicroseconds>(@aptos_framework);
        aborts_if round.deadline >= timestamp::spec_now_seconds();
        //     Explicit form of the above: a successful timeout happened strictly after the deadline
        ensures round.deadline < timestamp::spec_now_seconds();

        // 3 - move_from of the two Bets
        //     `!exists` instead of `remove`: two `remove`s of the same type contradict each other
        //     (see the note in win_round.move)
        aborts_if !exists<Bet::Bet<CoinType>>(round.player1);
        aborts_if !exists<Bet::Bet<CoinType>>(round.player2);
        //     Same player twice: the second move_from finds no Bet (clauses above use the pre-state)
        aborts_if round.player1 == round.player2;
        ensures !exists<Bet::Bet<CoinType>>(round.player1);
        ensures !exists<Bet::Bet<CoinType>>(round.player2);

        // 4 - Each player gets back exactly their own bet (CoinProxy::deposit: simplified Coin model, never aborts)
        //     `old`: the schema uses `coin` in an `ensures`, where the removed Bets would be read in the post-state.
        //     (It would still prove without `old`, since move_from keeps the old content in memory, but that is
        //     a modelling artifact: see playground_old.move)
        include CoinProxy::DepositSchema<CoinType> {
            account_addr: round.player1,
            coin: old(global<Bet::Bet<CoinType>>(round.player1).value)
        };
        include CoinProxy::DepositSchema<CoinType> {
            account_addr: round.player2,
            coin: old(global<Bet::Bet<CoinType>>(round.player2).value)
        };

        // 5 - Frame: each written resource changes only at these addresses (evaluated in the
        //     pre-state). This also frames the other Bets, which the `!exists` clauses above do not.
        //     Bet::timeout declares the same targets.
        modifies global<Bet::Round<CoinType>>(bookmaker);
        modifies global<Bet::Bet<CoinType>>(round.player1);
        modifies global<Bet::Bet<CoinType>>(round.player2);
        modifies global<CoinStore<CoinType>>(round.player1);
        modifies global<CoinStore<CoinType>>(round.player2);
    }
}
