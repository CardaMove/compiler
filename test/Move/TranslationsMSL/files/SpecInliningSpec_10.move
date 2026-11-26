script {
    use NamedAddr::Mid;

    fun main<X>(acc: &signer) {}

    spec main {
        include Mid::Both<X> { s: acc };
    }
}
