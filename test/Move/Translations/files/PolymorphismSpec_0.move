module NamedAddr::Playground6 {
    struct A has drop {
        b: bool
    }

    struct Wrapper<Content: drop> has drop {
        amount: u64,
        content: Content
    }

    fun wrap_content<Content: drop>(content: Content): Wrapper<Content> {
        let w = Wrapper<Content> { amount: 1, content: content };

        w
    }

    fun main() {
        let content = A { b: true };

        let wrapped: Wrapper<A> = wrap_content<A>(content);
    }
}
