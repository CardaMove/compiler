module BetAddr::Bet {
    use aptos_framework::coin::{Coin, Self};
    use std::signer::{address_of};
    use std::timestamp;

    struct Round<phantom CoinType> has key {
        player1: address,
        player2: address,
        oracle: address,
        stake: u64,
        deadline: u64
    }

    struct Bet<phantom CoinType> has key {
        value: Coin<CoinType>
    }

    public entry fun init<CoinType>(// Accepts restrictions on the type argument, like <CoinType : store>
        bookmaker: &signer,
        player1: address,
        player2: address,
        oracle: address,
        stake: u64,
        deadline: u64
    ): () {
        let bet = Round<CoinType> { player1, player2, oracle, stake, deadline };
        move_to(bookmaker, bet);
    }

    public fun join<CoinType>(partecipant: &signer, bookmaker: address): () acquires Round {
        let round = borrow_global_mut<Round<CoinType>>(bookmaker);
        assert!(
            address_of(partecipant) == round.player1
                || address_of(partecipant) == round.player2,
            0
        );
        let bet = coin::withdraw(partecipant, round.stake);

        assert!(coin::value<CoinType>(&bet) == round.stake, 0);
        let bet = Bet { value: bet };
        move_to(partecipant, bet);
    }

    public fun win<CoinType>(
        oracle: &signer, winner: address, bookmaker: address
    ): () acquires Round, Bet {
        assert!(exists<Round<CoinType>>(bookmaker), 0);
        let Round { player1, player2, oracle: oracle_address, stake: _, deadline: _ } =
            move_from<Round<CoinType>>(bookmaker);
        assert!(address_of(oracle) == oracle_address, 0);
        assert!(winner == player1 || winner == player2, 0);

        let Bet { value: bet1 } = move_from<Bet<CoinType>>(player1);
        let Bet { value: bet2 } = move_from<Bet<CoinType>>(player2);
        coin::merge(&mut bet1, bet2);
        coin::deposit(winner, bet1);
    }

    public fun timeout<CoinType>(bookmaker: address): () acquires Round, Bet {
        let Round { player1, player2, oracle: _, stake: _, deadline } =
            move_from<Round<CoinType>>(bookmaker);
        assert!(deadline < timestamp::now_seconds(), 0);

        let Bet { value: bet1 } = move_from<Bet<CoinType>>(player1);
        let Bet { value: bet2 } = move_from<Bet<CoinType>>(player2);
        coin::deposit(player1, bet1);
        coin::deposit(player2, bet2);
    }
}
