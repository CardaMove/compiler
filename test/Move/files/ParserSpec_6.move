script {
    use BetAddr::Bet::{Round, Self};
    use std::signer::{address_of};
    use CoinProxyAddr::CoinProxy;
    use aptos_framework::coin::{Coin, CoinStore};

    // Wins a round
    fun win_round<CoinType>(
        oracle: &signer, winner: address, bookmaker: address
    ) {
        Bet::win<CoinType>(oracle, winner, bookmaker)
    }

    spec win_round {
        // Manual assert! + also condition for the move_from
        aborts_if !exists<Round<CoinType>>(bookmaker);
        let round = global<Round<CoinType>>(bookmaker);

        // Condition for the move_from
        ensures remove<Round<CoinType>>(bookmaker);

        // Manual assert!
        aborts_if address_of(oracle) != round.oracle;
        aborts_if winner != round.player1 && winner != round.player2;

        // Conditions for the move_froms of the two Bets
        // NOTE: `remove<Bet>` cannot be used here. `remove<T>(a)` also requires `T` to be unchanged at
        // every OTHER address (prover source, tag aptos-cli-v9.6.0,
        // move-prover/boogie-backend/src/spec_translator.rs ~l.2300-2359:
        // `post == $ResourceRemove(pre, a) && $ResourceExists(pre, a)`; same frame for `publish`/`update`),
        // so two removes of the same type at different addresses contradict each other.
        // `!exists` states the removal only; "existed before" is given by the aborts_if clauses.
        // (Keeping the frame would need state labels: `ensures ..S |~ remove<..>(p1); ensures S.. |~ remove<..>(p2);`)
        aborts_if !exists<Bet::Bet<CoinType>>(round.player1);
        ensures !exists<Bet::Bet<CoinType>>(round.player1);

        aborts_if !exists<Bet::Bet<CoinType>>(round.player2);
        ensures !exists<Bet::Bet<CoinType>>(round.player2);
        //     Same player twice: the first move_from takes the only Bet, the second one aborts.
        //     Not caught by the clause above, which is evaluated in the pre-state.
        aborts_if round.player1 == round.player2;

        // The winner gets both bets (CoinProxy::merge + CoinProxy::deposit: simplified Coin model, never aborts).
        // The `let`s read the Bets in the pre-state, since they no longer exist at the end.
        let bet1_value = global<Bet::Bet<CoinType>>(round.player1).value.value;
        let bet2_value = global<Bet::Bet<CoinType>>(round.player2).value.value;
        include CoinProxy::DepositSchema<CoinType> {
            account_addr: winner,
            coin: Coin<CoinType> { value: bet1_value + bet2_value }
        };

        // Frame: each written resource changes only at these addresses (evaluated in the pre-state).
        // This also restores the frame over the other Bets lost by not using `remove` (see above).
        // Bet::win declares the same targets.
        modifies global<Round<CoinType>>(bookmaker);
        modifies global<Bet::Bet<CoinType>>(round.player1);
        modifies global<Bet::Bet<CoinType>>(round.player2);
        modifies global<CoinStore<CoinType>>(winner);
    }
}
