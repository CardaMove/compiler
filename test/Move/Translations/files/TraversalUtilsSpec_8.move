module NamedAddr::TraversalUtilsSchemaFunctionClash {
    public fun clash(): u64 {
        1
    }

    spec schema clash {
        value: u64;
    }
}
