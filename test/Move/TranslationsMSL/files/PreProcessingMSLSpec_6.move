script {
    use NamedAddr::PreProcessingMSLStrip::{Self, Counter};
    use NamedAddr::PreProcessingMSLStrip;
    use NamedAddr::PreProcessingMSLStrip::{CounterSchema};
    use std::signer::{address_of};

    spec main {
        ensures exists<Counter>(address_of(s));
    }
}
