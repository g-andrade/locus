#!/usr/bin/env bash

# Based on:
# * https://github.com/beam-telemetry/telemetry/blob/main/docs.sh

set -eu

OTP_VERSION=$(erl -eval 'io:format("~ts", [erlang:system_info(otp_release)]), halt().'  -noshell)

if [[ "$OTP_VERSION" < "24" ]]; then
    >&2 echo "Doc generation requires OTP 24+ (found: ${OTP_VERSION})"
    exit 1
fi

OUR_DIRECTORY=$(dirname $0)
LIB_VERSION=$(git describe --tags)

rebar3 compile
rebar3 as docs edoc
ex_doc locus "$LIB_VERSION" \
    _build/default/lib/locus/ebin \
    --source-ref "$LIB_VERSION" \
    --config "$OUR_DIRECTORY/generate_docs.config"
