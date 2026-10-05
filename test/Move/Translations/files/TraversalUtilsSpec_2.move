module NamedAddr::TraversalUtilsTimestampSpec {
    use std::timestamp;
    public fun test(): u64 {
        let seconds = timestamp::now_seconds();
        seconds
    }
}
