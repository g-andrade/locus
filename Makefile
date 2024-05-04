CLI_ARTIFACT_PATH = _build/escriptize/bin/locus

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

export ERL_FLAGS = -enable-feature maybe_expr # needed for katana-code under OTP 25

## General Rules

all: compile
.PHONY: all
.NOTPARALLEL: all

compile:
	@rebar3 compile
.PHONY: compile

clean:
	@rebar3 clean -a
.PHONY: clean

check: xref hank-dead-code-cleaner elvis-linter dialyzer
.NOTPARALLEL: check
.PHONY: check

test: eunit ct cli
	./locus check --log-level debug test/priv/GeoLite2-Country.tar.gz
.NOTPARALLEL: test
.PHONY: test

## Tests

ct:
	@rebar3 do ct, cover
.PHONY: ct

eunit:
	@rebar3 eunit
.PHONY: eunit

## Checks

xref:
	@rebar3 xref
.PHONY: xref

hank-dead-code-cleaner:
	@if rebar3 plugins list | grep '^rebar3_hank\>' >/dev/null; then \
		rebar3 hank; \
	else \
		echo >&2 "WARN: skipping rebar3_hank check"; \
	fi
.PHONY: hank-dead-code-cleaner

elvis-linter:
	@if rebar3 plugins list | grep '^rebar3_lint\>' >/dev/null; then \
		rebar3 lint; \
	else \
		echo >&2 "WARN: skipping rebar3_lint check"; \
	fi
.PHONY: elvis-linter

dialyzer:
	@rebar3 dialyzer
.PHONY: dialyzer

eqwalizer:
	@rebar3 as eqwalizer,test compile
	elp eqwalize-all

## Shell, docs and publication

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as shell shell

cli:
	@rebar3 as escriptize escriptize
	cp -p "$(CLI_ARTIFACT_PATH)" ./

doc-dry:
	@rebar3 hex build
.PHONY: doc-dry

publish:
publish: doc
	@rebar3 hex publish
