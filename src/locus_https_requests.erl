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

%% @private
-module(locus_https_requests).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [ssl_opts_for_ca_authentication/1
   ]).

%% ------------------------------------------------------------------
%% Internal Function Exports
%% ------------------------------------------------------------------

-export(
   [partial_chain/1
   ]).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-ifdef(SSL_OLD_CLIENT_OPTIONS).
-type ssl_option() :: ssl:connect_option().
-else.
-type ssl_option() :: ssl:tls_client_option().
-endif.
-export_type([ssl_option/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec ssl_opts_for_ca_authentication(string()) -> [ssl_option(), ...].
ssl_opts_for_ca_authentication(URL) ->
    case http_uri:parse(URL) of
        {ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query}} ->
            ssl_opts_for_ca_authentication_(Host);
        {ok, {_Scheme, _UserInfo, Host, _Port, _Path, _Query, _Fragment}} ->
            ssl_opts_for_ca_authentication_(Host)
    end.

%% ------------------------------------------------------------------
%% Exported Internal Function Definitions
%% ------------------------------------------------------------------

-spec partial_chain([public_key:der_encoded()]) -> {trusted_ca, binary()} | unknown_ca.
partial_chain(Certs) ->
    % Taken from frisky, which took it from hackney,
    % licensed under Apache 2 license,
    % which in turn took it from rebar3, licensed under BSD.
    Certs1 = lists:reverse([{Cert, public_key:pkix_decode_cert(Cert, otp)} ||
                            Cert <- Certs]),
    CACerts = certifi:cacerts(),
    CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],

    case lists_anymap(
           fun ({_, Cert}) ->
                   check_cert(CACerts1, Cert)
           end,
           Certs1)
    of
        {true, Trusted} ->
            {trusted_ca, element(1, Trusted)};
        false ->
            unknown_ca
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ssl_opts_for_ca_authentication_(Host) when is_list(Host) ->
    % Taken from frisky, which took it from hackney,
    % licensed under Apache 2 license.
    CACerts = certifi:cacerts(),
    VerifyFun = {fun ssl_verify_hostname:verify_fun/3, [{check_hostname, Host}]},
    [{verify, verify_peer},
     {depth, 99},
     {cacerts, CACerts},
     {partial_chain, fun ?MODULE:partial_chain/1},
     {verify_fun, VerifyFun}].

check_cert(CACerts, Cert) ->
    % Taken from frisky, which took it from hackney,
    % licensed under Apache 2 license.
    CertPKI = extract_public_key_info(Cert),
    lists:any(
      fun(CACert) ->
              extract_public_key_info(CACert) =:= CertPKI
      end, CACerts).

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

lists_anymap(Fun, [H|T]) ->
    case Fun(H) of
        true -> {true, H};
        false -> lists_anymap(Fun, T)
    end;
lists_anymap(_Fun, []) ->
    false.
