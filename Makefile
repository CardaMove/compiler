.PHONY: clean

all: build

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

