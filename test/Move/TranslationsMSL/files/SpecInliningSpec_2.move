script {
    use NamedAddr::Schemas::{Self, IncrementSchema};

    fun main(account: &signer, amount: u64) {}

    spec main {
        // Same name as a let of the schema: the clause below still reads this one
        let addr = @0x1;
        include IncrementSchema<u64> { s: account, amount: amount + 1 };
        ensures addr == @0x1;
        // Included twice: each copy gets its own lets
        include Schemas::IncrementSchema<u64> { s: account, amount };
    }
}
