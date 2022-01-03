%% Copyright (c) 2017-2022 Guilherme Andrade
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
-module(locus_app).
-behaviour(application).

%% ------------------------------------------------------------------
%% application Function Exports
%% ------------------------------------------------------------------

-export([start/2]).
-export([stop/1]).

%% ------------------------------------------------------------------
%% application Function Definitions
%% ------------------------------------------------------------------

-spec start(term(), list()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case locus_sup:start_link() of
        {ok, Pid} ->
            locus_logger:on_app_start(),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
