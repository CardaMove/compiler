module NamedAddr::TraversalUtilsSchemaStructClash {
    struct Clash has drop {
        value: u64
    }

    spec schema Clash {
        value: u64;
    }
}
