script {
    use BetAddr::Bet;
    use aptos_framework::coin::{Coin, CoinStore};
    use CoinProxyAddr::CoinProxy::{Self};
    use std::signer::{address_of};

    // Joines a round
    fun join_round<CoinType>(partecipant: &signer, bookmaker: address) {
        Bet::join<CoinType>(partecipant, bookmaker)
    }

    spec join_round {
        let participant_addr = address_of(partecipant);
        let round = global<Bet::Round<CoinType>>(bookmaker);

        // 1 - Condition for the borrow_global_mut
        aborts_if !exists<Bet::Round<CoinType>>(bookmaker);

        // 2 - Manual assert!
        aborts_if participant_addr != round.player1
            && participant_addr != round.player2;

        // 3 - Conditions on CoinProxy::withdraw balance
        include CoinProxy::WithdrawSchema<CoinType> { s: partecipant, amount: round.stake };

        // 4 - Conditions on the move_to holding the full Bet
        //     Note: publish<> does not cover the aborts_if !exists<>, since it is checked only if the function exists normally
        //     which can't happen if the Bet does not exist. So the two specs are complementary
        //     Second note: Specs can instantiate Coin struct even if declared inside Coin module
        aborts_if exists<Bet::Bet<CoinType>>(participant_addr);
        ensures publish<Bet::Bet<CoinType>>(
            participant_addr,
            Bet::Bet<CoinType> {
                value: Coin<CoinType> { value: round.stake }
            }
        );

        // 5 - Frame: the new Bet and the participant's balance are the only resources written.
        //     Bet::join declares the same targets.
        modifies global<Bet::Bet<CoinType>>(participant_addr);
        modifies global<CoinStore<CoinType>>(participant_addr);
    }
}
