%% Copyright (c) 2017-2018 Guilherme Andrade
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
-behaviour(locus_event_subscriber).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([set_loglevel/1]).              -ignore_xref({set_loglevel,1}).

%% ------------------------------------------------------------------
%% locus_event_subscriber Function Exports
%% ------------------------------------------------------------------

-export([report/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(LOGGING_LEVELS,
        (#{ debug => ?debug,
            info => ?info,
            warning => ?warning,
            error => ?error
         })).

-define(debug, 0).
-define(info, 1).
-define(warning, 2).
-define(error, 3).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Changes the logging verbosity in runtime
%%
%% `Level' must be either `debug', `info', `warning', `error' or `none'.
-spec set_loglevel(debug | info | warning | error | none) -> ok.
set_loglevel(Level) when Level =:= debug;
                         Level =:= info;
                         Level =:= warning;
                         Level =:= error;
                         Level =:= none ->
    application:set_env(locus, log_level, Level).

%% ------------------------------------------------------------------
%% locus_event_subscriber Function Definitions
%% ------------------------------------------------------------------

%% @private
-spec report(atom(), locus_event_subscriber:event()) -> ok.
report(DatabaseId, Event) ->
    MinLevel = application:get_env(locus, log_level, undefined),
    MinWeight = maps:get(MinLevel, ?LOGGING_LEVELS, infinity),
    report(MinWeight, DatabaseId, Event).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec report(non_neg_integer() | infinity, atom(), locus_event_subscriber:event()) -> ok.
report(MinWeight, DatabaseId, {request_sent, URL, Headers}) ->
    if MinWeight =< ?debug ->
           log_info("~p database download request sent (url ~p, headers ~p)", [DatabaseId, URL, Headers]);
       MinWeight =< ?info ->
           log_info("~p database download request sent", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {download_dismissed, HttpResponse}) ->
    if MinWeight =< ?debug ->
           log_info("~p database download canceled: ~p", [DatabaseId, HttpResponse]);
       MinWeight =< ?info ->
           log_info("~p database download canceled", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {download_failed_to_start, Reason}) ->
    if MinWeight =< ?error ->
           log_error("~p database download failed to start: ~p", [DatabaseId, Reason]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {download_started, Headers}) ->
    if MinWeight =< ?debug ->
           log_info("~p database download started (headers ~p)", [DatabaseId, Headers]);
       MinWeight =< ?info ->
           log_info("~p database download started", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {download_finished, BodySize, {ok, TrailingHeaders}}) ->
    if MinWeight =< ?debug ->
           log_info("~p database download succeeded after ~p bytes (trailing headers ~p)",
                     [DatabaseId, BodySize, TrailingHeaders]);
       MinWeight =< ?info ->
           log_info("~p database download succeeded after ~p bytes",
                     [DatabaseId, BodySize]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {download_finished, BodySize, {error, Error}}) ->
    if MinWeight =< ?error ->
           log_error("~p database download failed after ~p bytes: ~p",
                     [DatabaseId, BodySize, Error]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_started, Source}) ->
    if MinWeight =< ?debug ->
           log_info("~p database load attempt started from ~p", [DatabaseId, Source]);
       MinWeight =< ?info ->
           log_info("~p database load attempt started (~p)", [DatabaseId, resumed_source(Source)]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_dismissed, _Source}) ->
    if MinWeight =< ?info ->
           log_info("~p database load attempt dismissed", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_finished, Source, {ok, Version}}) ->
    if MinWeight =< ?debug ->
           log_info("~p database version ~p loaded from ~p", [DatabaseId, Version, Source]);
       MinWeight =< ?info ->
           log_info("~p database version ~p loaded (~p)", [DatabaseId, Version, resumed_source(Source)]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_finished, Source, {error, Error}}) ->
    if MinWeight =< ?debug ->
           log_warning("~p database failed to load from ~p: ~p", [DatabaseId, Source, Error]);
       MinWeight =< ?warning ->
           log_warning("~p database failed to load (~p): ~p", [DatabaseId, resumed_source(Source), Error]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {cache_attempt_finished, Filename, ok}) ->
    if MinWeight =< ?debug ->
           log_info("~p database cached at ~p", [DatabaseId, Filename]);
       MinWeight =< ?info ->
           log_info("~p database cached", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {cache_attempt_finished, Filename, {error, Error}}) ->
    if MinWeight =< ?debug ->
           log_error("~p database failed to cache in ~p: ~p", [DatabaseId, Filename, Error]);
       MinWeight =< ?error  ->
           log_error("~p database failed to cache: ~p", [DatabaseId, Error]);
       true ->
           ok
    end.

log_info(Fmt, Args) ->
    log(info_msg, Fmt, Args).

log_warning(Fmt, Args) ->
    log(warning_msg, Fmt, Args).

log_error(Fmt, Args) ->
    log(error_msg, Fmt, Args).

log(Fun, Fmt, Args) ->
    FullFmt = "[locus] " ++ Fmt ++ "~n",
    error_logger:(Fun)(FullFmt, Args).

-spec resumed_source(locus_mmdb:source()) -> cache | remote | filesystem.
resumed_source({SourceType, _SourceLocation}) ->
    SourceType.
