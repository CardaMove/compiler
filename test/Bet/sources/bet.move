module BetAddr::Bet {
    use aptos_framework::coin::{Coin, CoinStore};
    use CoinProxyAddr::CoinProxy;
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
        move_to<Round<CoinType>>(bookmaker, bet);
    }

    // The resources written by each function, so the callers' specs (scripts/*_round.move) can declare
    // the same `modifies` frame: the prover requires a callee to declare modify targets for every type
    // its caller restricts.
    spec init {
        modifies global<Round<CoinType>>(address_of(bookmaker));
    }

    public fun join<CoinType>(partecipant: &signer, bookmaker: address): () acquires Round {
        let round = borrow_global_mut<Round<CoinType>>(bookmaker);
        assert!(
            address_of(partecipant) == round.player1
                || address_of(partecipant) == round.player2,
            0
        );
        let bet = CoinProxy::withdraw<CoinType>(partecipant, round.stake);

        // TODO: NOTE: Decommenting this line causes the aiken compilation error
        // "reckless opaque cast" in the next line
        // The probable error is that "bet" is inserted into the state and is retrieved wrongly
        // assert!(coin::value<CoinType>(&bet) == round.stake, 0);
        let bet = Bet<CoinType> { value: bet };
        move_to<Bet<CoinType>>(partecipant, bet);
    }

    spec join {
        modifies global<Bet<CoinType>>(address_of(partecipant));
        modifies global<CoinStore<CoinType>>(address_of(partecipant));
    }

    public fun win<CoinType>(
        oracle: &signer, winner: address, bookmaker: address
    ): () acquires Round, Bet {
        assert!(exists<Round<CoinType>>(bookmaker), 0);
        let Round { player1, player2, oracle: oracle_address, stake: _, deadline: _ } =
            move_from<Round<CoinType>>(bookmaker);
        assert!(address_of(oracle) == oracle_address, 0);
        assert!(winner == player1 || winner == player2, 0);

        let Bet<CoinType> { value: bet1 } = move_from<Bet<CoinType>>(player1);
        // TODO: Needed since currently it is not supported to POST a variable introduced by a destructuring
        let bet1 = bet1;
        let Bet<CoinType> { value: bet2 } = move_from<Bet<CoinType>>(player2);
        CoinProxy::merge<CoinType>(&mut bet1, bet2);
        CoinProxy::deposit<CoinType>(winner, bet1);
    }

    spec win {
        let round = global<Round<CoinType>>(bookmaker);
        modifies global<Round<CoinType>>(bookmaker);
        modifies global<Bet<CoinType>>(round.player1);
        modifies global<Bet<CoinType>>(round.player2);
        modifies global<CoinStore<CoinType>>(winner);
    }

    public fun timeout<CoinType>(bookmaker: address): () acquires Round, Bet {
        let Round { player1, player2, oracle: _, stake: _, deadline } =
            move_from<Round<CoinType>>(bookmaker);
        assert!(deadline < timestamp::now_seconds(), 0);

        let Bet<CoinType> { value: bet1 } = move_from<Bet<CoinType>>(player1);
        let Bet<CoinType> { value: bet2 } = move_from<Bet<CoinType>>(player2);
        CoinProxy::deposit<CoinType>(player1, bet1);
        CoinProxy::deposit<CoinType>(player2, bet2);
    }

    spec timeout {
        let round = global<Round<CoinType>>(bookmaker);
        modifies global<Round<CoinType>>(bookmaker);
        modifies global<Bet<CoinType>>(round.player1);
        modifies global<Bet<CoinType>>(round.player2);
        modifies global<CoinStore<CoinType>>(round.player1);
        modifies global<CoinStore<CoinType>>(round.player2);
    }
}
