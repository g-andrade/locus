CLI_ARTIFACT_PATH = _build/escriptize/bin/locus

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

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

check: check-fast check-slow
.NOTPARALLEL: check
.PHONY: check

check-fast: check-formatted xref hank-dead-code-cleaner elvis-linter
.NOTPARALLEL: check-fast
.PHONY: check-fast

check-slow: dialyzer
.NOTPARALLEL: check-slow
.PHONY: check-slow

test: eunit ct cli
	./locus check --log-level debug test/priv/GeoLite2-Country.tar.gz
.NOTPARALLEL: test
.PHONY: test

format:
	@rebar3 fmt
.NOTPARALLEL: format
.PHONY: format

## Tests

ct:
	@rebar3 do ct, cover
.PHONY: ct

eunit:
	@rebar3 eunit
.PHONY: eunit

## Checks

check-formatted:
	@if rebar3 plugins list | grep '^erlfmt\>' >/dev/null; then \
		rebar3 fmt --check; \
	else \
		echo >&2 "WARN: skipping rebar3 erlfmt check"; \
	fi
.PHONY: check-formatted

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

publish:
publish: doc
	@rebar3 hex publish

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as shell shell

cli:
	@rebar3 as escriptize escriptize
	cp -p "$(CLI_ARTIFACT_PATH)" ./

doc: SOURCE_REF := $(shell git describe --tags --exact-match 2>/dev/null || git rev-parse --short HEAD)
doc: tmp/ex_doc
doc:
	./tmp/ex_doc "locus" "2.3.13" \
		_build/default/lib/locus/ebin \
		-c doc.config \
		--source-ref "${SOURCE_REF}";
.PHONY: doc

tmp/ex_doc: EX_DOC_VER=0.40.2
tmp/ex_doc: OTP_VER := $(shell erl -noshell -eval 'io:fwrite("~s", [erlang:system_info(otp_release)]), init:stop().')
tmp/ex_doc: | tmp
tmp/ex_doc:
	curl -fL -o tmp/ex_doc \
		"https://github.com/elixir-lang/ex_doc/releases/download/v${EX_DOC_VER}/ex_doc_otp_${OTP_VER}"; \
		chmod a+x tmp/ex_doc

tmp:
	mkdir tmp
