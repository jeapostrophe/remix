.PHONY: all setup test

all: setup test

setup:
	raco setup --check-pkg-deps --fix-pkg-deps --unused-pkg-deps --pkgs remix

test:
	raco test -p remix
