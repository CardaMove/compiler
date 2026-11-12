script {
    use rosetta_smart_contracts::auction::{Self, Auction, Bid};
    use aptos_framework::coin::{Coin, CoinStore};
    use CoinProxyAddr::CoinProxy;
    use std::signer::{address_of};

    // Bids `amount` on the auction of `auctioneer`
    fun auction_bid<CoinType>(
        acc: &signer, auctioneer: address, amount: u64
    ) {
        auction::bid<CoinType>(acc, auctioneer, amount)
    }

    spec auction_bid {
        let acc_addr = address_of(acc);
        let auction = global<Auction>(auctioneer);
        let top_bidder = auction.top_bidder;
        let top_value = global<Bid<CoinType>>(top_bidder).coins.value;

        // 1 - Condition for the borrow_global_mut
        aborts_if !exists<Auction>(auctioneer);

        // 2 - Condition for the move_from of the current top Bid
        aborts_if !exists<Bid<CoinType>>(top_bidder);

        // 3 - Manual asserts!
        aborts_if auction.expired;
        aborts_if amount <= top_value;

        // 4 - Conditions on the move_to of the new Bid.
        //     The old top Bid has already been moved out, so a bidder that is also the top bidder does not abort
        aborts_if acc_addr != top_bidder && exists<Bid<CoinType>>(acc_addr);

        // 5 - Balances. The bidder pays `amount` and the previous top bidder gets back `top_value`.
        //     When they are the same account both apply to the same CoinStore, so the schemas
        //     (which each define the final balance) cannot be included together.
        include acc_addr != top_bidder ==>
            CoinProxy::WithdrawSchema<CoinType> { s: acc, amount };
        include acc_addr != top_bidder ==>
            CoinProxy::DepositSchema<CoinType> {
                account_addr: top_bidder,
                coin: Coin<CoinType> { value: top_value }
            };
        aborts_if acc_addr == top_bidder
            && global<CoinStore<CoinType>>(acc_addr).coin.value
                < amount;
        ensures acc_addr == top_bidder ==>
            global<CoinStore<CoinType>>(acc_addr).coin.value
                == old(
                    global<CoinStore<CoinType>>(acc_addr).coin.value
                ) - amount + top_value;

        // 6 - Final state: the new top bidder and its Bid
        ensures global<Auction>(auctioneer)
            == Auction {
                auctioneer: auction.auctioneer,
                top_bidder: acc_addr,
                expired: false
            };
        ensures exists<Bid<CoinType>>(acc_addr);
        ensures global<Bid<CoinType>>(acc_addr)
            == Bid<CoinType> {
                coins: Coin<CoinType> { value: amount }
            };
        ensures acc_addr != top_bidder ==>
            !exists<Bid<CoinType>>(top_bidder);

        // 7 - Frame: each written resource changes only at these addresses (the addresses are
        //     evaluated in the pre-state). auction::bid declares the same targets.
        //     Bid<CoinType> at `top_bidder` is removed, at `acc_addr` published.
        modifies global<Auction>(auctioneer);
        modifies global<Bid<CoinType>>(acc_addr);
        modifies global<Bid<CoinType>>(top_bidder);
        modifies global<CoinStore<CoinType>>(acc_addr);
        modifies global<CoinStore<CoinType>>(top_bidder);
    }
}
