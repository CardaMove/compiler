module NamedAddr::TraversalUtilsTimestampSpec {
    use std::timestamp;
    public fun test() {
        let seconds = timestamp::now_seconds();
        seconds
    }
}
