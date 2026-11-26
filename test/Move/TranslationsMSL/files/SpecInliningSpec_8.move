module NamedAddr::Mid {
    use NamedAddr::Base;
    use std::signer;

    spec schema Both<C> {
        s: &signer;
        let a = signer::address_of(s);
        include Base::HasStore<C> { addr: a };
        ensures exists<Base::Store<C>>(a);
    }
}
