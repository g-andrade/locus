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
%% Private Function Exports
%% ------------------------------------------------------------------

-export([log_warning/2]).

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

-define(is_http_download_event(Event),
        (element(1, (Event)) =:= request_sent orelse
         element(1, (Event)) =:= download_dismissed orelse
         element(1, (Event)) =:= download_failed_to_start orelse
         element(1, (Event)) =:= download_started orelse
         element(1, (Event)) =:= download_finished)).

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
report(MinWeight, DatabaseId, Event)
  when ?is_http_download_event(Event) ->
    report_http_download_event(MinWeight, DatabaseId, database, Event);
report(MinWeight, DatabaseId, {checksum, ChecksumEvent})
  when ?is_http_download_event(ChecksumEvent) ->
    report_http_download_event(MinWeight, DatabaseId, checksum, ChecksumEvent);
report(MinWeight, DatabaseId, {load_attempt_started, Source}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] database load attempt started from ~p", [DatabaseId, Source]);
       MinWeight =< ?info ->
           log_info("[~ts] database load attempt started (~p)", [DatabaseId, resumed_source(Source)]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_dismissed, _Source}) ->
    if MinWeight =< ?info ->
           log_info("[~ts] database load attempt dismissed", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_finished, Source, {ok, Version}}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] database version ~p loaded from ~p", [DatabaseId, Version, Source]);
       MinWeight =< ?info ->
           log_info("[~ts] database version ~p loaded (~p)", [DatabaseId, Version, resumed_source(Source)]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {load_attempt_finished, Source, {error, Reason}}) ->
    {Weight,LogFun}= ?case_match(Source, {cache,_}, {?warning,fun log_warning/2}, {?error,fun log_error/2}),
    if MinWeight =< ?debug ->
           LogFun("[~ts] database failed to load from ~p: ~p", [DatabaseId, Source, Reason]);
       MinWeight =< Weight ->
           LogFun("[~ts] database failed to load (~p): ~p", [DatabaseId, resumed_source(Source), Reason]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {cache_attempt_finished, Filename, ok}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] database cached at \"~ts\"", [DatabaseId, Filename]);
       MinWeight =< ?info ->
           log_info("[~ts] database cached", [DatabaseId]);
       true ->
           ok
    end;
report(MinWeight, DatabaseId, {cache_attempt_finished, Filename, {error, Reason}}) ->
    if MinWeight =< ?debug ->
           log_error("[~ts] database failed to cache in \"~ts\": ~p", [DatabaseId, Filename, Reason]);
       MinWeight =< ?error  ->
           log_error("[~ts] database failed to cache: ~p", [DatabaseId, Reason]);
       true ->
           ok
    end.

-spec report_http_download_event(non_neg_integer() | infinity, atom(), checksum | database,
                                 locus_http_download:event()) -> ok.
report_http_download_event(MinWeight, DatabaseId, DownloadType, {request_sent, URL, Headers}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] ~s download request sent (url \"~ts\", headers ~p)",
                    [DatabaseId, DownloadType, URL, Headers]);
       MinWeight =< ?info ->
           log_info("[~ts] ~s download request sent", [DatabaseId, DownloadType]);
       true ->
           ok
    end;
report_http_download_event(MinWeight, DatabaseId, DownloadType, {download_dismissed, HttpResponse}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] ~s download canceled: ~p", [DatabaseId, DownloadType, HttpResponse]);
       MinWeight =< ?info ->
           log_info("[~ts] ~s download canceled", [DatabaseId, DownloadType]);
       true ->
           ok
    end;
report_http_download_event(MinWeight, DatabaseId, DownloadType, {download_failed_to_start, Reason}) ->
    if MinWeight =< ?error ->
           log_error("[~ts] ~s download failed to start: ~p", [DatabaseId, DownloadType, Reason]);
       true ->
           ok
    end;
report_http_download_event(MinWeight, DatabaseId, DownloadType, {download_started, Headers}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] ~s download started (headers ~p)", [DatabaseId, DownloadType, Headers]);
       MinWeight =< ?info ->
           log_info("[~ts] ~s download started", [DatabaseId, DownloadType]);
       true ->
           ok
    end;
report_http_download_event(MinWeight, DatabaseId, DownloadType, {download_finished, BodySize,
                                                                 {ok, TrailingHeaders}}) ->
    if MinWeight =< ?debug ->
           log_info("[~ts] ~s download succeeded after ~b bytes (trailing headers ~p)",
                     [DatabaseId, DownloadType, BodySize, TrailingHeaders]);
       MinWeight =< ?info ->
           log_info("[~ts] ~s download succeeded after ~b bytes",
                     [DatabaseId, DownloadType, BodySize]);
       true ->
           ok
    end;
report_http_download_event(MinWeight, DatabaseId, DownloadType, {download_finished, BodySize,
                                                                 {error, Reason}}) ->
    if MinWeight =< ?error ->
           log_error("[~ts] ~s download failed after ~b bytes: ~p",
                     [DatabaseId, DownloadType, BodySize, Reason]);
       true ->
           ok
    end.

-ifdef(NO_LOGGER).
log_info(Fmt, Args) ->
    log_to_error_logger(info_msg, Fmt, Args).

-spec log_warning(string(), list()) -> ok.
%% @private
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

-spec log_warning(string(), list()) -> ok.
%% @private
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
