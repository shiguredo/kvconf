.PHONY: clean upgrade compile test dialyzer efmt-check elint-check distclean publish

all: clean upgrade compile dialyzer test

upgrade:
	@./rebar3 plugins upgrade --all
	@./rebar3 upgrade --all

compile:
	@./rebar3 xref

clean:
	@./rebar3 clean

test:
	@./rebar3 as test eunit
	@./rebar3 as test cover

dialyzer:
	@./rebar3 dialyzer

efmt-check:
	@RUST_LOG=warn efmt --check --parallel --check-line-length 120

# デフォルトの tests/ ではなく test/ を明示する
elint-check:
	@elint src/ test/

distclean:
	@./rebar3 clean --all

publish:
	@./rebar3 hex publish package
