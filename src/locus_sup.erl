%% Copyright (c) 2017 Guilherme Andrade <locus.lib@gandrade.net>
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
-module(locus_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).                      -ignore_xref({start_link,0}).
-export([start_child/2]).
-export([stop_child/1]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?CB_MODULE, []).

-spec start_child(atom(), nonempty_string())
        -> ok | {error, already_started}.
start_child(DatabaseId, DatabaseURL) when is_atom(DatabaseId) ->
    ChildSpec = child_spec(DatabaseId, DatabaseURL),
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok, _Pid} ->
            ok;
        {error, already_present} ->
            {error, already_started};
        {error, {already_started, _Pid}} ->
            {error, already_started}
    end.

-spec stop_child(atom()) -> ok | {error, not_found}.
stop_child(DatabaseId) when is_atom(DatabaseId) ->
    ChildId = child_id(DatabaseId),
    case supervisor:terminate_child(?SERVER, ChildId) of
        ok ->
            case supervisor:delete_child(?SERVER, ChildId) of
                ok -> ok;
                {error, not_found} ->
                    {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, {#{ strategy := one_for_one }, []}}.
init([]) ->
    SupFlags = #{ strategy => one_for_one },
    {ok, {SupFlags, []}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

child_spec(DatabaseId, DatabaseURL) ->
    Args = [DatabaseId, DatabaseURL],
    #{ id => child_id(DatabaseId),
       start => {locus_http_loader, start_link, Args}
     }.

child_id(DatabaseId) ->
    {http_loader, DatabaseId}.
