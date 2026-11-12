script {
    use rosetta_smart_contracts::auction::{Self, Auction, Bid};
    use aptos_framework::coin::{Coin, CoinStore};
    use CoinProxyAddr::CoinProxy;
    use std::signer::{address_of};

    // Starts an auction, locking `base_amount` as the base bid
    fun auction_start<CoinType>(auctioneer: &signer, base_amount: u64) {
        auction::start<CoinType>(auctioneer, base_amount)
    }

    spec auction_start {
        let auctioneer_addr = address_of(auctioneer);

        // 1 - Conditions on CoinProxy::withdraw balance
        include CoinProxy::WithdrawSchema<CoinType> { s: auctioneer, amount: base_amount };

        // 2 - Conditions on the two move_to: neither resource can already exist
        aborts_if exists<Auction>(auctioneer_addr);
        aborts_if exists<Bid<CoinType>>(auctioneer_addr);

        // 3 - Final state: the Auction (the auctioneer is the top bidder) and the Bid holding the base amount
        ensures publish<Auction>(
            auctioneer_addr,
            Auction { auctioneer: auctioneer_addr, top_bidder: auctioneer_addr, expired: false }
        );
        ensures publish<Bid<CoinType>>(
            auctioneer_addr,
            Bid<CoinType> { coins: Coin<CoinType> { value: base_amount } }
        );

        // 4 - Frame: `publish` already frames Auction and Bid at the other addresses; `modifies`
        //     states it for every written resource, CoinStore included. auction::start declares
        //     the same targets.
        modifies global<Auction>(auctioneer_addr);
        modifies global<Bid<CoinType>>(auctioneer_addr);
        modifies global<CoinStore<CoinType>>(auctioneer_addr);
    }
}
