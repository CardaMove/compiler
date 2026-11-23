script {
    use NamedAddr::PreProcessingMSLStrip::{Self, Counter, increment};
    use NamedAddr::PreProcessingMSLStrip::{increment as inc, helper};
    use NamedAddr::PreProcessingMSLStrip;
    use NamedAddr::PreProcessingMSLStrip::{CounterSchema};
    use std::signer::{address_of};

    fun main(s: &signer) {
        inc(s)
    }

    spec main {
        ensures exists<Counter>(address_of(s));
    }
}
