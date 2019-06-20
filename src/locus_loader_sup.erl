%% Copyright (c) 2019 Guilherme Andrade
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
%%
%% locus includes code extracted from OTP source code, by Ericsson AB,
%% released under the Apache License 2.0.

%% @private
-module(locus_loader_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).                      -ignore_xref({start_link,1}).
-export([start_child/2]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(URLType) -> {ok, pid()}
        when URLType :: http | filesystem.
start_link(URLType) ->
    ServerName = server_name(URLType),
    supervisor:start_link({local,ServerName}, ?MODULE, [URLType]).

-spec start_child(URLType, [term()]) -> ok | {error, term()}
        when URLType :: http | filesystem.
start_child(URLType, Args) ->
    ServerName = server_name(URLType),
    case supervisor:start_child(ServerName, Args) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, already_started};
        {error, {shutdown, already_started}} ->
            {error, already_started};
        {error, Reason} ->
            {error, Reason}
    end.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...])
    -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}
        when InitArg :: http | filesystem.
init([InitArg]) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 10,
          period => 5
         },
    ChildSpec = child_spec(InitArg),
    {ok, {SupFlags, [ChildSpec]}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

server_name(http) ->
    locus_http_loader_sup;
server_name(filesystem) ->
    locus_filesystem_loader_sup.

child_spec(http) ->
    locus_http_loader:dynamic_child_spec(loader);
child_spec(filesystem) ->
    locus_filesystem_loader:dynamic_child_spec(loader).
