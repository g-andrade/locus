REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

ifeq ($(wildcard rebar3),rebar3)
	REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")
PANDOC = pandoc

ifeq ($(REBAR3),)
	REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: all build clean check dialyzer xref test cover console doc publish

all: build

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

clean:
	@$(REBAR3) clean

check: dialyzer xref

dialyzer:
	@$(REBAR3) dialyzer

xref:
	@$(REBAR3) xref

test:
	@$(REBAR3) as test ct

cover: test
	@$(REBAR3) as test cover

console: export ERL_FLAGS =? +pc unicode
console:
	@$(REBAR3) as development shell --apps locus

doc:
	@$(REBAR3) edoc
	@$(PANDOC) --from html --to markdown doc/overview-summary.html -o README.md

publish:
	@$(REBAR3) as publication hex publish
