all: install

install: 
	stack install

doc: 
	stack haddock --no-haddock-deps

force: amend
	git push origin -f

amend:
	git add . && git commit --amend
