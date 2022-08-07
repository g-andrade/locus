CLI_ARTIFACT_PATH = _build/escriptize/bin/locus

export ERL_FLAGS = -enable-feature maybe_expr # needed for katana-code under OTP 25

.PHONY: all build clean check
.PHONY: xref hank-dead-code-cleaner elvis-linter dialyzer
.PHONY: test cover
.PHONY: shell console doc-dry publish cli

.NOTPARALLEL: check cover test

all: build

build:
	@rebar3 compile

clean:
	@rebar3 clean

check: xref hank-dead-code-cleaner elvis-linter dialyzer

xref:
	@rebar3 xref

hank-dead-code-cleaner:
	@if rebar3 plugins list | grep '\<rebar3_hank\>' >/dev/null; then \
		rebar3 hank; \
	else \
		echo >&2 "skipping rebar3_hank check"; \
	fi

elvis-linter:
	@rebar3 lint

dialyzer:
	@rebar3 dialyzer

eqwalizer:
	@rebar3 as eqwalizer,test compile
	elp eqwalize-all

test: cli
	@rebar3 do eunit, ct, cover
	./locus check --log-level debug test/priv/GeoLite2-Country.tar.gz

cover: test

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as shell shell

console: shell

doc-dry:
	@rebar3 hex docs --dry-run

publish:
	@rebar3 hex publish

cli:
	@rebar3 as escriptize escriptize
	cp -p "$(CLI_ARTIFACT_PATH)" ./
