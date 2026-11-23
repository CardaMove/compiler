module NamedAddr::PreProcessingMSLMissingSpecTarget {
    public fun add(a: u64, b: u64): u64 {
        a + b
    }

    spec sub {
        ensures result == a - b;
    }
}
