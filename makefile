.PHONY: test watch

test:
	stack test
	hlint .

watch:
	stack build --test --file-watch
