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

-module(locus_logger).
-behaviour(locus_event_subscriber).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([on_app_start/0]).
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

-define(is_loglevel(V),
        ((V) =:= debug orelse
         (V) =:= info orelse
         (V) =:= warning orelse
         (V) =:= error orelse
         (V) =:= none)).

-define(case_match(Value, Pattern, Then, OrElse),
        (case (Value) of (Pattern) -> (Then); _ -> (OrElse) end)).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec on_app_start() -> ok.
%% @private
-ifdef(NO_LOGGER).
on_app_start() -> ok.
-else.
on_app_start() ->
    CurrentLevel = application:get_env(locus, log_level, undefined),
    _ = logger:set_application_level(locus, CurrentLevel),
    ok.
-endif.

%% @doc Changes the logging verbosity in runtime
%%
%% `Level' must be either `debug', `info', `warning', `error' or `none'.
-spec set_loglevel(debug | info | warning | error | none) -> ok.
-ifdef(NO_LOGGER).
set_loglevel(Level) when ?is_loglevel(Level) ->
    application:set_env(locus, log_level, Level).
-else.
set_loglevel(Level) when ?is_loglevel(Level) ->
    application:set_env(locus, log_level, Level),
    _ = logger:set_application_level(locus, Level),
    ok.
-endif.

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
    {Weight,LogFun}= ?case_match(Source, {cache,_}, {?warning,fun log_warning/2}, {?error,fun log_error/2}),
    if MinWeight =< ?debug ->
           LogFun("~p database failed to load from ~p: ~p", [DatabaseId, Source, Error]);
       MinWeight =< Weight ->
           LogFun("~p database failed to load (~p): ~p", [DatabaseId, resumed_source(Source), Error]);
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

-ifdef(NO_LOGGER).
log_info(Fmt, Args) ->
    log_to_error_logger(info_msg, Fmt, Args).

log_warning(Fmt, Args) ->
    log_to_error_logger(warning_msg, Fmt, Args).

log_error(Fmt, Args) ->
    log_to_error_logger(error_msg, Fmt, Args).

-else.
log_info(Fmt, Args) ->
    case use_error_logger() of
        true -> log_to_error_logger(info_msg, Fmt, Args);
        false -> log_to_logger(info, Fmt, Args)
    end.

log_warning(Fmt, Args) ->
    case use_error_logger() of
        true -> log_to_error_logger(warning_msg, Fmt, Args);
        false -> log_to_logger(warning, Fmt, Args)
    end.

log_error(Fmt, Args) ->
    case use_error_logger() of
        true -> log_to_error_logger(error_msg, Fmt, Args);
        false -> log_to_logger(error, Fmt, Args)
    end.

log_to_logger(Fun, Fmt, Args) ->
    FullFmt = "[locus] " ++ Fmt,
    logger:Fun(FullFmt, Args).

% `lager' and `logger' don`t play nice with each other (as of Jun 2019)
% * https://github.com/erlang-lager/lager/issues/492
% * https://github.com/erlang-lager/lager/pull/488
use_error_logger() ->
    has_lager() andalso not has_usable_logger().

% Taken from: https://github.com/ferd/cth_readable/pull/23
has_lager() ->
    % Module is present
    erlang:function_exported(logger, module_info, 0).

% Taken from: https://github.com/ferd/cth_readable/pull/23
has_usable_logger() ->
    %% The config is set (lager didn't remove it)
    erlang:function_exported(logger, get_handler_config, 1) andalso
    logger:get_handler_config(default) =/= {error, {not_found, default}}.
-endif.

log_to_error_logger(Fun, Fmt, Args) ->
    FullFmt = "[locus] " ++ Fmt ++ "~n",
    error_logger:(Fun)(FullFmt, Args).

-spec resumed_source(locus_loader:source()) -> cache | remote | filesystem.
resumed_source({SourceType, _SourceLocation}) ->
    SourceType.
