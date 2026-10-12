.PHONY: clean sync-libs check-libs

all: build

# Stack builds automatically Move.Lexer module by calling alex
# This is because Move.Lexer is defined in exposed-modules and alex is defined under build-tools
alex:
	alex lib/Move/Lexer.x -o lib/Move/Lexer.hs

build:
	stack build

setup:
	stack setup
	stack install alex
	stack install happy

test:
	stack test

clean:
	stack clean

# Vendor the runtime libraries (Aiken utils + TypeScript port) from rosetta-redo, see doc/offchain.md
# The rosetta-redo checkout defaults to ../rosetta-redo and can be overridden with ROSETTA_REDO
sync-libs:
	node scripts/sync-runtime-libs.js

check-libs:
	node scripts/sync-runtime-libs.js --check
