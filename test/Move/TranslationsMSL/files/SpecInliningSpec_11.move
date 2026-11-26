script {
    use NamedAddr::Mid;
    use NamedAddr::Base;
    use std::signer;

    fun main<X>(acc: &signer) {}

    spec main {
        let a_0 = std::signer::address_of(acc);
        aborts_if !exists<NamedAddr::Base::Store<X>>(a_0);
        ensures exists<NamedAddr::Base::Store<X>>(a_0);
    }
}
