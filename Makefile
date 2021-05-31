all: install

install: 
	stack install

doc: 
	stack haddock --no-haddock-deps
