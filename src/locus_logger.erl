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

-module(locus_logger).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([should_log/1]).
-export([set_loglevel/1]).              -ignore_xref({set_loglevel,1}).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(LOGGING_LEVELS,
        (#{ %debug => 0,
            info => 1,
            %notice => 2,
            warning => 4,
            error => 5
            %critical => 6,
            %alert => 7,
            %emergency => 7
         })).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec should_log(info | warning | error) -> boolean().
%% @private
should_log(Level) ->
    Weight = maps:get(Level, ?LOGGING_LEVELS),
    MinimumLevel = application:get_env(locus, log_level, undefined),
    MinimumWeight = maps:get(MinimumLevel, ?LOGGING_LEVELS, none),
    Weight >= MinimumWeight.

%% @doc Changes the logging verbosity in runtime
%%
%% `Level' must be either `info', `warning', `error' or `none'.
-spec set_loglevel(info | warning | error | none) -> ok.
set_loglevel(Level) when Level =:= info;
                         Level =:= warning;
                         Level =:= error;
                         Level =:= none ->
    application:set_env(locus, log_level, Level).
