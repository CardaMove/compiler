module NamedAddr::TraversalUtilsSpecImport {
    use NamedAddr::TraversalUtilsSpecBlocks::{Self, AddSchema as Renamed};

    spec schema ImportingSchema {
        include Renamed<u64> { x: 1 };
    }
}
