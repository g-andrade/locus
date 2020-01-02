%% Copyright (c) 2019-2020 Guilherme Andrade
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

-module(custom_loader_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/2,
    start_link/3,
    stop/1
   ]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom(), string()) -> {ok, pid()}.
start_link(DatabaseId, DatabaseURL) ->
    supervisor:start_link(?MODULE, [DatabaseId, DatabaseURL]).

-spec start_link(atom(), string(), [term()]) -> {ok, pid()}.
start_link(DatabaseId, DatabaseURL, Opts) ->
    supervisor:start_link(?MODULE, [DatabaseId, DatabaseURL, Opts]).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen:stop(Pid, normal, 5000).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...])
         -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}
        when InitArg :: DatabaseId | DatabaseURL,
             DatabaseId :: atom(),
             DatabaseURL :: string().
init([DatabaseId, DatabaseURL | TailArgs])
  when length(TailArgs) =:= 0;
       length(TailArgs) =:= 1 ->
    SupFlags =
        #{ strategy => one_for_one,
           intensity => 5,
           period => 1
         },
    ChildSpecs =
        [apply(locus, loader_child_spec, [DatabaseId, DatabaseURL | TailArgs])
        ],
    {ok, {SupFlags, ChildSpecs}}.
