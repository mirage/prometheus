PINOPTS=-y -k git

TESTS = true

.PHONY: all clean prometheus

all: prometheus prometheus-app
	@

depends:
	opam pin add ${PINOPTS} prometheus .
	opam pin add ${PINOPTS} prometheus-app .

prometheus-app:
	ocaml pkg/pkg.ml build -n prometheus-app -q --tests true
	ocaml pkg/pkg.ml test _build/tests/test.native

prometheus:
	ocaml pkg/pkg.ml build -n prometheus -q

clean:
	ocaml pkg/pkg.ml clean
