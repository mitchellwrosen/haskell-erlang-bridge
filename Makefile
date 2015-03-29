.PHONY: test
test:
		cabal build test
		./dist/build/test/test
