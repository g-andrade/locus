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
%%
%% locus includes code extracted from OTP source code, by Ericsson AB,
%% released under the Apache License 2.0.

%% @private
-module(locus_cli).
-ifdef(ESCRIPTIZING).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([main/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec main([string()]) -> ok | no_return().
main(Args) ->
    ensure_apps_are_started([locus, getopt]),
    case Args of
        ["analyze" | CmdArgs] ->
            handle_analysis_command(CmdArgs);
        _ ->
            fall_from_grace(
              "~n"
              "Usage: locus [<command>] [<command_args>]~n"
              "~n"
              "Available commands:~n"
              "  analyze")
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ensure_apps_are_started(Apps) ->
    lists:foreach(
      fun (App) ->
              {ok, _} = application:ensure_all_started(App)
      end,
      Apps).

handle_analysis_command(CmdArgs) ->
    OptSpecList =
        [{load_timeout, undefined, "load-timeout", {integer,30}, "Database load timeout (in seconds)"},
         {log_level,    undefined, "log-level",    {string,"error"}, "debug | info | warning | error"},
         {url,          undefined, undefined,      utf8_binary, "Database URL (local or remote)"}],

    case getopt:parse_and_check(OptSpecList, CmdArgs) of
        {ok, {ParsedArgs, []}} ->
            {load_timeout,LoadTimeoutSecs} = lists:keyfind(load_timeout, 1, ParsedArgs),
            {log_level,StrLogLevel} = lists:keyfind(log_level, 1, ParsedArgs),
            {url,DatabaseURL} = lists:keyfind(url, 1, ParsedArgs),
            LoadTimeout = timer:seconds(LoadTimeoutSecs),
            LogLevel = list_to_atom(StrLogLevel),
            ok = locus_logger:set_loglevel(LogLevel),
            prepare_analysis(DatabaseURL, LoadTimeout);
        _ ->
            getopt:usage(OptSpecList, "locus analyze"),
            fall_from_grace()
    end.

prepare_analysis(DatabaseURL, LoadTimeout) ->
    DatabaseId = cli_analysis,
    WaitRef = make_ref(),
    BaseOpts = [{async_waiter, {self(),WaitRef}}],
    ExtraOpts =
        case http_uri:parse(DatabaseURL) of
            {ok, _} -> [no_cache];
            {error, _} -> []
        end,

    stderr_println("Loading database from \"~ts\"...", [DatabaseURL]),
    case locus:start_loader(DatabaseId, DatabaseURL, BaseOpts ++ ExtraOpts) of
        ok ->
            wait_for_analysis_database_load(DatabaseId, WaitRef, LoadTimeout)
    end.

wait_for_analysis_database_load(DatabaseId, WaitRef, LoadTimeout) ->
    receive
        {WaitRef, {ok, LoadedVersion}} ->
            stderr_println("Database version ~p successfully loaded", [LoadedVersion]),
            perform_analysis(DatabaseId);
        {WaitRef, {error, Reason}} ->
            fall_from_grace("Failed to load database: ~p", [Reason])
    after
        LoadTimeout ->
            fall_from_grace("Timeout loading the database")
    end.

perform_analysis(DatabaseId) ->
    stderr_println("Analyzing database for flaws..."),
    case locus:analyze(DatabaseId) of
        ok ->
            stderr_println("Database is wholesome.");
        {error, {flawed, Flaws}} ->
            fall_from_grace("Database is corrupt or incompatible:~n"
                            ++ lists:flatten(["* ~p~n" || _ <- Flaws]),
                            Flaws)
    end.

fall_from_grace() ->
    fall_from_grace("", []).

fall_from_grace(MsgFmt) ->
    fall_from_grace(MsgFmt, []).

fall_from_grace(MsgFmt, MsgArgs) ->
    _ = MsgFmt =/= "" andalso stderr_println("[ERROR] " ++ MsgFmt, MsgArgs),
    erlang:halt(1, [{flush,true}]).

stderr_println(Fmt) ->
    stderr_println(Fmt, []).

stderr_println(Fmt, Args) ->
    io:format(standard_error, Fmt ++ "~n", Args).

-endif.
