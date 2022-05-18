.PHONY: build test
MAKEFLAGS += --silent

review:
	cargo insta review

build:
	cargo build

test: build
	cargo test
