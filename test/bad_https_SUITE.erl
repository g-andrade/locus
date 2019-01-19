%% Copyright (c) 2017-2019 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% locus is an independent project and has not been authorized, sponsored,
%% or otherwise approved by MaxMind.

-module(bad_https_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(assertRecv(Pattern),
        ((fun () -> receive Msg -> ?assertMatch((Pattern), Msg)
                    after 30000 -> error(timeout) end end)())).

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, element(1, GroupDef)} || GroupDef <- groups()].

groups() ->
    [{GroupName, [parallel], individual_tests()}
     || GroupName <- [secure_setup, insecure_setup]].

individual_tests() ->
    Exports = ?MODULE:module_info(exports),
    [Name || {Name,1} <- Exports, lists:suffix("_test", atom_to_list(Name))].

init_per_group(GroupName, Config) ->
    {ok, _} = application:ensure_all_started(locus),
    ok = locus_logger:set_loglevel(debug),
    Secure = case GroupName of secure_setup -> true; insecure_setup -> false end,
    [{secure,Secure} | Config].

end_per_group(_GroupName, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

expired_https_test(Config) ->
    do_https_test(expired_https_test, "expired.badssl.com", "certificate expired",
                  Config).

wronghost_https_test(Config) ->
    do_https_test(wronghost_https_test, "wrong.host.badssl.com", "handshake failure",
                  Config).

selfsigned_https_test(Config) ->
    do_https_test(selfsigned_https_test, "self-signed.badssl.com", "bad certificate",
                  Config).

untrusted_https_test(Config) ->
    do_https_test(untrusted_https_test, "untrusted-root.badssl.com", "unknown ca",
                  Config).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

do_https_test(Loader, Host, ExpectedTlsAlert, Config) ->
    Noise = crypto:strong_rand_bytes(32),
    HashedNoise = crypto:hash(sha512, Noise),
    Path = base64:encode(HashedNoise),
    URL = "https://" ++ Host ++ "/" ++ binary_to_list(Path),
    case lists:keyfind(secure, 1, Config) of
        {secure, true} ->
            LoaderOpts = [no_cache, {event_subscriber, self()}],
            ok = locus:start_loader(Loader, URL, LoaderOpts),
            try
                ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
                ?assertRecv({locus, Loader, {download_failed_to_start,
                                             {error,
                                              {failed_connect,
                                               [{to_address,{Host,443}},
                                                {inet, [inet], {tls_alert,ExpectedTlsAlert}}]}}}})
            after
                ok = locus:stop_loader(Loader)
            end;
        {secure, false} ->
            LoaderOpts = [insecure, no_cache, {event_subscriber, self()}],
            ok = locus:start_loader(Loader, URL, LoaderOpts),
            try
                ?assertRecv({locus, Loader, {request_sent, URL, _Headers}}),
                ?assertRecv({locus, Loader, {download_failed_to_start,
                                             {http, {404 = _StatusCode, _StatusDesc}, _Headers, _Body}}})
            after
                ok = locus:stop_loader(Loader)
            end
    end.
