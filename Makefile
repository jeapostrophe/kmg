.PHONY: default
default: build test

.PHONY: build
build:
	stack build --fast

.PHONY: test
test:
	./test.sh
