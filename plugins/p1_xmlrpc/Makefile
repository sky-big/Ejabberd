all: src

src:
	rebar compile xref

clean:
	rebar clean

test:
	rebar skip_deps=true eunit

.PHONY: clean src test
