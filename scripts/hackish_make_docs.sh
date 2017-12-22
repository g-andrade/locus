#!/usr/bin/env bash
set -ex

# sigh.....
rebar3 as generate_documentation compile
mkdir -p _build/generate_documentation/lib/locus/doc/
cp -p overview.edoc _build/generate_documentation/lib/locus/doc/
erl -pa _build/generate_documentation/lib/*/ebin -noshell -run edoc_run application "locus"
erl -pa _build/generate_documentation/lib/*/ebin -noshell -run edoc_run application "locus" '[{doclet, edown_doclet}, {top_level_readme, {"README.md", "https://github.com/g-andrade/locus", "master"}}]'
rm -rf doc
mv _build/generate_documentation/lib/locus/doc ./
sed -i -e 's/^\(---------\)$/\n\1/g' README.md
rm doc/*.{html,css,png,edoc} doc/edoc-info
