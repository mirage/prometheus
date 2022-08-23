.PHONY: clean test

test:
	dune build @check @runtest

clean:
	dune clean
