.PHONY: clean

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

