REPO        ?= gamerl

.PHONY: rel deps

all: quick_compile

init: deps compile

compile:
	./rebar compile

e: quick_compile

quick_compile:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps

clean: 
	./rebar clean skip_deps=true

distclean: clean deps_clean testclean

deps_clean:
	./rebar delete-deps

TEST_LOG_FILE := eunit.log
quick_testclean:
	@rm -f $(TEST_LOG_FILE)

testclean:
	rm -rf .eunit

DIALYZER_APPS = kernel stdlib sasl erts ssl tools runtime_tools crypto inets \
	xmerl syntax_tools compiler public_key snmp

include tools.mk

typer:
	typer --annotate -I ../ --plt $(PLT) -r src

rel: quick_compile
	./rebar generate skip_deps=true
