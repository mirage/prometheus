PINOPTS=-y -k git

TESTS = true

.PHONY: all clean prometheus

all: prometheus prometheus-app
	@

depends:
	opam pin add ${PINOPTS} prometheus .
	opam pin add ${PINOPTS} prometheus-app .

prometheus-app:
	jbuilder build @runtest

prometheus:
	jbuilder build --only-packages prometheus

clean:
	rm -rf _build
