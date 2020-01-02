%% Copyright (c) 2017-2020 Guilherme Andrade
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
-module(locus_database_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).                      -ignore_xref({start_link,0}).
-export([start_child/1]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local,?SERVER}, ?MODULE, []).

-spec start_child([term()]) -> {ok,pid()} | {error,term()}.
start_child(Args) ->
    try supervisor:start_child(?SERVER, Args) of
        Result -> Result
    catch
        exit:{Reason,{gen_server,call,[?SERVER|_]}}
          when Reason =:= noproc;
               Reason =:= normal;
               Reason =:= shutdown;
               (tuple_size(Reason) =:= 2 andalso element(1, Reason) =:= shutdown) ->
            {error, application_not_running}
    end.

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([])
    -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}.
init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 10,
          period => 5
         },
    ChildSpec = locus_database:dynamic_child_spec(database),
    {ok, {SupFlags, [ChildSpec]}}.
