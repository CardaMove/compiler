.PHONY: clean

all: build

alex:
	alex Tokens.x -o lib/Tokens.hs

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

