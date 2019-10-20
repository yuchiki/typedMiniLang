.PHONY: test

test:
	stack test
	hlint .
