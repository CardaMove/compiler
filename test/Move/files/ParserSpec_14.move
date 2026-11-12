script {
    use rosetta_smart_contracts::auction::{Self, Auction, Bid};
    use aptos_framework::coin::{Coin, CoinStore};
    use CoinProxyAddr::CoinProxy;
    use std::signer::{address_of};

    // Ends the auction: the top bid is paid to the auctioneer
    fun auction_end<CoinType>(auctioneer: &signer) {
        auction::end<CoinType>(auctioneer)
    }

    spec auction_end {
        let auctioneer_addr = address_of(auctioneer);
        let auction = global<Auction>(auctioneer_addr);
        let top_bidder = auction.top_bidder;
        let top_value = global<Bid<CoinType>>(top_bidder).coins.value;

        // 1 - Condition for the borrow_global_mut
        aborts_if !exists<Auction>(auctioneer_addr);

        // 2 - Manual assert!
        aborts_if auctioneer_addr != auction.auctioneer;

        // 3 - Condition for the move_from of the top Bid
        aborts_if !exists<Bid<CoinType>>(top_bidder);

        // 4 - The top bid is deposited to the auctioneer
        include CoinProxy::DepositSchema<CoinType> {
            account_addr: auctioneer_addr,
            coin: Coin<CoinType> { value: top_value }
        };

        // 5 - Final state: expired Auction, top Bid removed
        ensures global<Auction>(auctioneer_addr) == Auction {
            auctioneer: auction.auctioneer,
            top_bidder,
            expired: true
        };
        ensures !exists<Bid<CoinType>>(top_bidder);

        // 6 - Frame: each written resource changes only at these addresses (evaluated in the
        //     pre-state). auction::end declares the same targets.
        modifies global<Auction>(auctioneer_addr);
        modifies global<Bid<CoinType>>(top_bidder);
        modifies global<CoinStore<CoinType>>(auctioneer_addr);
    }
}
