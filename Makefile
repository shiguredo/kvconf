.PHONY: clean upgrade compile test distclean

all: clean upgrade compile test

upgrade:
	@./rebar3 upgrade

compile:
	@./rebar3 xref

clean:
	@./rebar3 clean

test:
	@./rebar3 as test eunit
	@./rebar3 as test cover

distclean:
	@./rebar3 clean --all
