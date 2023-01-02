%% Copyright (c) 2020-2023 Guilherme Andrade
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

%% @doc Downloads an unpacked database edition from MaxMind, as well as
%% its checksum (which it then verifies), without blocking the caller.
-module(locus_maxmind_download).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [validate_opts/1,
    start_link/3
   ]).

-ignore_xref(
   [start_link/3
   ]).

%% ------------------------------------------------------------------
%% proc_lib Function Exports
%% ------------------------------------------------------------------

-export(
   [init_/1
   ]).

-ignore_xref(
   [init_/1
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() ::
    {license_key, binary() | string()} | % TODO support reading it from file or environment
    {date, calendar:date()} |
    locus_http_download:opt().
-export_type([opt/0]).

-type msg() ::
    {event, event()} |
    {finished, {error, no_license_key_defined}} |
    {finished, {error, {checksum_download, term()}}} |
    {finished, {error, {bad_checksum, #{expected := binary(),
                                        actual := binary()}}}} |
    {finished, {error, {bad_checksum_format, binary()}}} |
    locus_http_download:msg().
-export_type([msg/0]).

-type event() ::
    locus_http_download:event() |
    {checksum, locus_http_download:event()}.
-export_type([event/0]).

-type success() :: locus_http_download:success().
-export_type([success/0]).

-record(state, {
          owner_pid :: pid(),
          edition :: atom(),
          opts :: [opt()],
          http_download_opts :: [locus_http_download:opt()],
          license_key :: binary(),
          database_download_pid :: undefined | pid(),
          database_download_success :: undefined | locus_http_download:success(),
          checksum_download_pid :: undefined | pid()
         }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec validate_opts(proplists:proplist())
        -> {ok, {[opt()], proplists:proplist()}} |
           {error, BadOpt :: term()}.
%% @private
validate_opts(MixedOpts) ->
    try
        lists:partition(
          fun ({license_key, Value} = Opt) ->
                  validate_license_key_opt(Value)
                  orelse error({badopt, Opt});
              ({date, Value} = Opt) ->
                  validate_date_opt(Value)
                  orelse error({badopt, Opt});
              (_) ->
                  false
          end,
          MixedOpts)
    of
        {MyOpts, OtherOpts} ->
            ExtendedOtherOpts =
                case proplists:get_value(censor_query, OtherOpts) of
                    undefined ->
                        [{censor_query, [license_key]} | OtherOpts];
                    _ ->
                        OtherOpts
                end,

            case locus_http_download:validate_opts(ExtendedOtherOpts) of
                {ok, {HttpDownloadOpts, RemainingOpts}} ->
                    {ok, {MyOpts ++ HttpDownloadOpts, RemainingOpts}};
                {error, BadOpt} ->
                    {error, BadOpt}
            end
    catch
        error:{badopt, BadOpt} ->
            {error, BadOpt}
    end.

-spec start_link(atom(), locus_http_download:headers(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Edition, RequestHeaders, Opts) ->
    proc_lib:start_link(?MODULE, init_, [[self(), Edition, RequestHeaders, Opts]]).

%% ------------------------------------------------------------------
%% proc_lib Function Definitions
%% ------------------------------------------------------------------

-spec init_([InitArg, ...]) -> no_return()
        when InitArg :: OwnerPid | Edition | RequestHeaders | Opts,
             OwnerPid :: pid(),
             Edition :: atom(),
             RequestHeaders :: locus_http_download:headers(),
             Opts :: [opt()].
%% @private
init_([OwnerPid, Edition, RequestHeaders, Opts]) ->
    _ = process_flag(trap_exit, true),
    proc_lib:init_ack(OwnerPid, {ok, self()}),
    {MyOpts, HttpDownloadOpts} =
        lists:partition(
          fun ({Opt, _}) ->
                  lists:member(Opt, [license_key, date]);
              (_) ->
                  false
          end,
          Opts),

    case get_license_key(Opts) of
        {ok, LicenseKey} ->
            URL = build_download_url(Edition, LicenseKey, Opts, "tar.gz"),
            {ok, DatabaseDownloadPid} = locus_http_download:start_link(URL, RequestHeaders,
                                                                       HttpDownloadOpts),
            State =
                #state{
                   owner_pid = OwnerPid,
                   edition = Edition,
                   opts = MyOpts,
                   http_download_opts = HttpDownloadOpts,
                   license_key = LicenseKey,
                   database_download_pid = DatabaseDownloadPid
                  },
            gen_server:enter_loop(?MODULE, [], State);
        {error, Reason} ->
            notify_owner_process(OwnerPid, {finished, {error, Reason}}),
            exit(normal)
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init(_) -> no_return().
%% @private
init(_) ->
    exit(not_called).

-spec handle_call(term(), {pid(), reference()}, state())
        -> {stop, unexpected_call, state()}.
%% @private
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {stop, unexpected_cast, state()}.
%% @private
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()}
         | {stop, normal, state()}
         | {stop, {database_download_stopped, pid(), term()}, state()}
         | {stop, {checksum_download_stopped, pid(), term()}, state()}
         | {stop, unexpected_info, state()}.
%% @private
handle_info({DatabaseDownloadPid, Msg}, State)
  when DatabaseDownloadPid =:= State#state.database_download_pid ->
    handle_database_download_msg(Msg, State);
handle_info({ChecksumDownloadPid, Msg}, State)
  when ChecksumDownloadPid =:= State#state.checksum_download_pid ->
    handle_checksum_download_msg(Msg, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_linked_process_death(Pid, Reason, State);
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
%% @private
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
%% @private
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec validate_license_key_opt(term()) -> boolean().
validate_license_key_opt(Value) ->
    locus_util:is_utf8_binary(Value) orelse
    locus_util:is_unicode_string(Value).

-spec validate_date_opt(term()) -> boolean().
validate_date_opt(Date) ->
    locus_util:is_date(Date).

-spec get_license_key([opt()]) -> {ok, binary()} | {error, no_license_key_defined}.
get_license_key(Opts) ->
    OptValue = proplists:get_value(license_key, Opts),
    AppConfigValue = application:get_env(locus, license_key, undefined),

    if is_binary(OptValue) ->
           {ok, OptValue};
       is_binary(AppConfigValue), AppConfigValue =/= <<"YOUR_LICENSE_KEY">> ->
           {ok, AppConfigValue};
       length(AppConfigValue) >= 0, AppConfigValue =/= "YOUR_LICENSE_KEY" ->
           <<Value/bytes>> = unicode:characters_to_binary(AppConfigValue),
           {ok, Value};
       true ->
           {error, no_license_key_defined}
    end.

build_download_url(Edition, LicenseKey, Opts, Suffix) ->
    BinEdition = atom_to_binary(Edition, utf8),
    BaseQueryIoPairs = [["edition_id=", locus_util:url_query_encode(BinEdition)],
                        ["license_key=", locus_util:url_query_encode(LicenseKey)],
                        ["suffix=", locus_util:url_query_encode(Suffix)]],
    QueryIoPairs =
        lists:foldl(
          fun ({date, Date}, Acc) ->
                  {DateYear, DateMonth, DateDay} = Date,
                  IoDate = io_lib:format("~4..0B~2..0B~2..0B", [DateYear, DateMonth, DateDay]),
                  [["date=", IoDate] | Acc];
              (_, Acc) ->
                  Acc
          end,
          BaseQueryIoPairs, Opts),

    QueryIoString = lists:join($&, QueryIoPairs),
    Binary = iolist_to_binary(["https://download.maxmind.com/app/geoip_download?", QueryIoString]),
    binary_to_list(Binary).

-spec handle_database_download_msg(locus_http_download:msg(), state())
        -> {noreply, state()} | {stop, normal, state()}.
handle_database_download_msg({finished, Result} = Msg, State) ->
    locus_util:expect_linked_process_termination(State#state.database_download_pid),
    UpdatedState = State#state{ database_download_pid = undefined },
    case Result of
        {success, Success} ->
            handle_database_download_success(Success, UpdatedState);
        _ ->
            notify_owner(Msg, UpdatedState),
            {stop, normal, State}
    end;
handle_database_download_msg(Msg, State) ->
    notify_owner(Msg, State),
    {noreply, State}.

-spec handle_database_download_success(locus_http_download:success(), state())
        -> {noreply, state()}.
handle_database_download_success(Success, State) ->
    ChecksumURL = build_checksum_download_url(Success, State),
    ChecksumDownloadRequestHeaders = [],
    ChecksumDownloadHttpOpts = State#state.http_download_opts,
    {ok, ChecksumDownloadPid} =
        locus_http_download:start_link(ChecksumURL, ChecksumDownloadRequestHeaders,
                                       ChecksumDownloadHttpOpts),

    UpdatedState = State#state{ database_download_success = Success,
                                checksum_download_pid = ChecksumDownloadPid },
    {noreply, UpdatedState}.

-spec build_checksum_download_url(locus_http_download:success(), state()) -> string().
build_checksum_download_url(DatabaseDownloadSuccess, State) ->
    #state{edition = Edition, license_key = LicenseKey} = State,
    ChecksumDownloadOpts = checksum_download_opts(DatabaseDownloadSuccess, State),
    build_download_url(Edition, LicenseKey, ChecksumDownloadOpts, "tar.gz.sha256").

checksum_download_opts(DatabaseDownloadSuccess, State) ->
    #state{edition = Edition, opts = BaseOpts} = State,
    case proplists:get_value(date, BaseOpts) =:= undefined
         andalso date_of_downloaded_database(Edition, DatabaseDownloadSuccess)
    of
        {_, _, _} = DateFromHeaders ->
            [{date, DateFromHeaders} | BaseOpts];
        _ ->
            BaseOpts
    end.

-spec date_of_downloaded_database(atom(), locus_http_download:success())
        -> calendar:date() | unknown.
date_of_downloaded_database(Edition, DownloadSuccess) ->
    #{headers := Headers} = DownloadSuccess,
    ParseSuccesses =
        lists:filtermap(
          fun (Pair) -> date_of_downloaded_database_from_header(Pair, Edition) end,
          Headers),

    case maps:from_list(ParseSuccesses) of
        #{"content-disposition" := Date} ->
            Date;
        #{"last-modified" := Date} ->
            Date;
        #{} ->
            unknown
    end.

date_of_downloaded_database_from_header({"content-disposition", Value}, Edition) ->
    EditionStr = atom_to_list(Edition),
    Regex = "filename=" ++ EditionStr ++ "_([0-9]{4})([0-9]{2})([0-9]{2}).tar.gz",
    RegexOpts = [{capture, all_but_first, list}],

    case re:run(Value, Regex, RegexOpts) of
        {match, [_, _, _] = Parts} ->
            date_of_downloaded_database_from_header_str_parts("content-disposition", Parts);
        nomatch ->
            false
    end;
date_of_downloaded_database_from_header({"last-modified", Value}, _) ->
    try httpd_util:convert_request_date(Value) of
        {{_, _, _} = Date, _} ->
            {true, {"last-modified", Date}}
    catch
        _:_ ->
            false
    end;
date_of_downloaded_database_from_header({_, _}, _) ->
    false.

date_of_downloaded_database_from_header_str_parts(HeaderName, Parts) ->
    [Year, Month, Day] = lists:map(fun list_to_integer/1, Parts),
    case calendar:valid_date(Year, Month, Day) of
        true -> {true, {HeaderName, {Year, Month, Day}}};
        false -> false
    end.

-spec handle_checksum_download_msg(locus_http_download:msg(), state())
        -> {noreply, state()} | {stop, normal, state()}.
handle_checksum_download_msg({finished, Result}, State) ->
    locus_util:expect_linked_process_termination(State#state.checksum_download_pid),
    UpdatedState = State#state{ checksum_download_pid = undefined },
    case Result of
        {success, Success} ->
            handle_checksum_download_success(Success, UpdatedState);
        {error, Reason} ->
            notify_owner({finished, {error, {checksum_download, Reason}}}, State),
            {stop, normal, State}
    end;
handle_checksum_download_msg({event, Event}, State) ->
    notify_owner({event, {checksum, Event}}, State),
    {noreply, State}.

-spec handle_checksum_download_success(locus_http_download:success(), state())
        -> {stop, normal, state()}.
handle_checksum_download_success(Success, State) ->
    ActualDatabaseChecksum = actual_database_checksum(State),
    ActualDatabaseChecksumSize = byte_size(ActualDatabaseChecksum),

    case extract_expected_database_checksum(Success, ActualDatabaseChecksumSize) of
        {ok, ActualDatabaseChecksum} ->
            #state{database_download_success = DatabaseDownloadSuccess} = State,
            notify_owner({finished, {success, DatabaseDownloadSuccess}}, State),
            {stop, normal, State};
        {ok, ExpectedDatabaseChecksum} ->
            ErrorReason = {bad_checksum, #{expected => ExpectedDatabaseChecksum,
                                           actual => ActualDatabaseChecksum}},
            notify_owner({finished, {error, ErrorReason}}, State),
            {stop, normal, State};
        {error, ErrorReason} ->
            notify_owner({finished, {error, ErrorReason}}, State),
            {stop, normal, State}
    end.

-spec extract_expected_database_checksum(locus_http_download:success(), pos_integer())
        -> {ok, binary()} | {error, {bad_checksum_format, binary()}}.
extract_expected_database_checksum(ChecksumDownloadSuccess, ActualDatabaseChecksumSize) ->
    case ChecksumDownloadSuccess of
        #{body := <<ExpectedDatabaseChecksum:ActualDatabaseChecksumSize/bytes>>} ->
            {ok, ExpectedDatabaseChecksum};
        #{body := <<ExpectedDatabaseChecksum:ActualDatabaseChecksumSize/bytes,
                    " ", _Filename/bytes>>} ->
            {ok, ExpectedDatabaseChecksum};
        #{body := <<BadChecksum/bytes>>} ->
            {error, {bad_checksum_format, BadChecksum}}
    end.

-spec actual_database_checksum(state()) -> binary().
actual_database_checksum(State) ->
    #state{database_download_success = DownloadSuccess} = State,
    #{body := ResponseBody} = DownloadSuccess,
    Hash = crypto:hash(sha256, ResponseBody),
    ChecksumStr = locus_util:bin_to_hex_str(Hash),
    list_to_binary(ChecksumStr).

-spec notify_owner(msg(), state()) -> ok.
notify_owner(Msg, State) ->
    #state{owner_pid = OwnerPid} = State,
    notify_owner_process(OwnerPid, Msg).

-spec notify_owner_process(pid(), msg()) -> ok.
notify_owner_process(OwnerPid, Msg) ->
    _ = erlang:send(OwnerPid, {self(), Msg}, [noconnect]),
    ok.

-spec handle_linked_process_death(pid(), term(), state())
        -> {stop, normal, state()}
         | {stop, {database_download_stopped, pid(), term()}, state()}
         | {stop, {checksum_download_stopped, pid(), term()}, state()}.
handle_linked_process_death(Pid, _, State)
  when Pid =:= State#state.owner_pid ->
    {stop, normal, State};
handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.database_download_pid ->
    {stop, {database_download_stopped, Pid, Reason}, State};
handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.checksum_download_pid ->
    {stop, {checksum_download_stopped, Pid, Reason}, State}.
