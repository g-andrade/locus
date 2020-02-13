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

-module(locus_loader).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [validate_opts/2,
    start_link/4
   ]).

-ignore_xref(
   [start_link/4
   ]).

-ifdef(TEST).
-export(
   [cached_database_path_for_maxmind_edition/2,
    cached_database_path_for_url/1
   ]).
-endif.

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
%% Macro Definitions
%% ------------------------------------------------------------------

-ifndef(NO_GEN_SERVER_HIBERNATE_AFTER).
-define(HIBERNATE_AFTER, (timer:seconds(5))).
-endif.

-define(is_pos_integer(V), ((is_integer((V)) andalso ((V) >= 1)))).

% https://en.wikipedia.org/wiki/Gzip
-define(GZIP_MAGIC_BYTES, 16#1f,16#8b).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() :: loader_opt() | fetcher_opt().
-export_type([opt/0]).

-type loader_opt() ::
    {update_period, milliseconds_interval()} |
    {error_retries, error_retry_behaviour()} |
    no_cache |
    deprecated_loader_opt().
-export_type([loader_opt/0]).

-type milliseconds_interval() :: pos_integer().
-export_type([milliseconds_interval/0]).

-type error_retry_behaviour() ::
    {backoff, milliseconds_interval()} |
    {exponential_backoff, exponential_backoff_params()}.
-export_type([error_retry_behaviour/0]).

-type exponential_backoff_params() ::
    #{min_interval := milliseconds_interval(),
      max_interval := milliseconds_interval(),
      growth_base := milliseconds_interval(),
      growth_exponent := number()
     }.
-export_type([exponential_backoff_params/0]).

-type deprecated_loader_opt() ::
    {pre_readiness_update_period, milliseconds_interval()} |
    {post_readiness_update_period, milliseconds_interval()}.
-export_type([deprecated_loader_opt/0]).

-type fetcher_opt() :: locus_http_download:opt().
-export_type([fetcher_opt/0]).

-type fetcher_msg() :: locus_http_download:msg() | locus_filesystem_load:msg().
-type fetcher_success() :: locus_http_download:success() | locus_filesystem_load:success().
-type cacher_msg() :: locus_filesystem_store:msg().

-record(state, {
          owner_pid :: pid(),
          database_id :: atom(),
          origin :: origin(),
          settings :: settings(),
          fetcher_opts :: [fetcher_opt()],

          update_timer :: reference() | undefined,
          last_modified :: calendar:datetime() | undefined,

          fetcher_pid :: pid() | undefined,
          fetcher_source :: source() | undefined,
          error_backoff_count :: non_neg_integer(),

          cacher_pid :: pid() | undefined,
          cacher_path :: locus_filesystem_store:path() | undefined,
          cacher_source :: source() | undefined
         }).
-type state() :: #state{}.

-record(settings, {
          update_period :: pos_integer(),
          error_retry_behaviour :: error_retry_behaviour(),
          error_retry_behaviour_applies_after_readiness :: boolean(),
          use_cache :: boolean()
         }).
-type settings() :: #settings{}.

-type update_period() :: pos_integer() | uniformly_distributed_update_period().
-export_type([update_period/0]).

-type uniformly_distributed_update_period() :: #{ min := pos_integer(), max := pos_integer() }.
-export_type([uniformly_distributed_update_period/0]).

-type blob_format() :: tgz | tarball | gzip | gzipped_mmdb | mmdb | unknown.

-type origin() ::
    {maxmind, atom()} |
    {http, locus_http_download:url()} |
    {filesystem, locus_filesystem_load:path()}.
-export_type([origin/0]).

-type maxmind_origin_params() ::
    #{ license_key := unicode:unicode_binary(),
       date => calendar:date()
     }.
-export_type([maxmind_origin_params/0]).

-type event() ::
    locus_maxmind_download:event() |
    locus_http_download:event() |
    locus_filesystem_load:event() |
    event_cache_attempt_finished().
-export_type([event/0]).

-type event_cache_attempt_finished() ::
    {cache_attempt_finished, locus_filesystem_store:path(), ok} |
    {cache_attempt_finished, locus_filesystem_store:path(), {error, term()}}.
-export_type([event_cache_attempt_finished/0]).

-type source() ::
    {remote, atom() | locus_http_download:url()} |
    locus_filesystem_load:source().
-export_type([source/0]).

-type msg() ::
    {event, event()} |
    {load_success, source(), calendar:datetime(), locus_mmdb:parts()} |
    {load_failure, source(), Reason :: term()}.
-export_type([msg/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec validate_opts(origin(), proplists:proplist())
        -> {ok, {[opt()], [fetcher_opt()], proplists:proplist()}} |
           {error, BadOpt :: term()}.
%% @private
validate_opts(Origin, MixedOpts) ->
    case validate_fetcher_opts(Origin, MixedOpts) of
        {ok, {FetcherOpts, RemainingMixedOpts}} ->
            validate_loader_opts(RemainingMixedOpts, FetcherOpts);
        {error, BadOpt} ->
            {error, BadOpt}
    end.

-spec start_link(atom(), origin(), [loader_opt()], [fetcher_opt()]) -> {ok, pid()}.
%% @private
start_link(DatabaseId, Origin, LoaderOpts, FetcherOpts) ->
    Opts = [self(), DatabaseId, Origin, LoaderOpts, FetcherOpts],
    ServerOpts = server_opts(),
    gen_server:start_link(?MODULE, Opts, ServerOpts).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([pid() | atom() | origin() | [loader_opt()] | [fetcher_opt()], ...])
    -> {ok, state()}.
%% @private
init([OwnerPid, DatabaseId, Origin, LoaderOpts, FetcherOpts]) ->
    _ = process_flag(trap_exit, true),
    DefaultSettings = default_settings(Origin),
    Settings = customized_settings(DatabaseId, DefaultSettings, LoaderOpts),
    State =
        #state{
           owner_pid = OwnerPid,
           database_id = DatabaseId,
           origin = Origin,
           settings = Settings,
           fetcher_opts = FetcherOpts,
           error_backoff_count = 0
          },
    self() ! finish_initialization,
    {ok, State}.

-spec handle_call(term(), {pid(),reference()}, state())
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
        -> {noreply, state()} |
           {stop, term(), state()}.
%% @private
handle_info(finish_initialization, State)
  when State#state.update_timer =:= undefined ->
    UpdatedState = finish_initialization(State),
    {noreply, UpdatedState};
handle_info(begin_update, State) ->
    false = erlang:cancel_timer(State#state.update_timer),
    State2 = State#state{ update_timer = undefined },
    State3 = begin_update(State2),
    {noreply, State3};
handle_info({FetcherPid, Msg}, State)
  when FetcherPid =:= State#state.fetcher_pid ->
    handle_fetcher_msg(Msg, State);
handle_info({CacherPid, Msg}, State)
  when CacherPid =:= State#state.cacher_pid ->
    handle_cacher_msg(Msg, State);
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
%% Internal Function Definitions - Initialization
%% ------------------------------------------------------------------

-ifndef(NO_GEN_SERVER_HIBERNATE_AFTER).
server_opts() -> [{locus_util:dialyzer_opaque_atom(hibernate_after), ?HIBERNATE_AFTER}].
-else.
server_opts() -> [].
-endif.

-spec validate_fetcher_opts(origin(), list())
        -> {ok, {[fetcher_opt()], list()}} |
           {error, term()}.
validate_fetcher_opts({maxmind,_}, MixedOpts) ->
    locus_maxmind_download:validate_opts(MixedOpts);
validate_fetcher_opts({http,_}, MixedOpts) ->
    locus_http_download:validate_opts(MixedOpts);
validate_fetcher_opts({filesystem,_}, MixedOpts) ->
    {ok, {[], MixedOpts}}.

-spec validate_loader_opts(list(), [fetcher_opt()])
        -> {ok, {[loader_opt()], [fetcher_opt()], list()}} |
           {error, term()}.
validate_loader_opts(MixedOpts, FetcherOpts) ->
    try
        lists:partition(
          fun ({update_period, Interval} = Opt) ->
                  ?is_pos_integer(Interval) orelse error({badopt,Opt});
              ({error_retries, Behaviour} = Opt) ->
                  is_error_retry_behaviour(Behaviour) orelse error({badopt,Opt});
              (no_cache) ->
                  true;
              %
              % Legacy options
              %
              ({pre_readiness_update_period, Interval} = Opt) ->
                  ?is_pos_integer(Interval) orelse error({badopt,Opt});
              ({post_readiness_update_period, Interval} = Opt) ->
                  ?is_pos_integer(Interval) orelse error({badopt,Opt});
              (_) ->
                  false
          end,
          MixedOpts)
    of
        {LoaderOpts, OtherOpts} ->
            {ok, {LoaderOpts, FetcherOpts, OtherOpts}}
    catch
        error:{badopt,BadOpt} ->
            {error, BadOpt}
    end.

-spec is_error_retry_behaviour(term()) -> boolean().
is_error_retry_behaviour({backoff, Interval}) ->
    ?is_pos_integer(Interval);
is_error_retry_behaviour({exponential_backoff, #{min_interval := MinInterval,
                                                 max_interval := MaxInterval,
                                                 growth_base := GrowthBase,
                                                 growth_exponent := GrowthExponent}}) ->
    ?is_pos_integer(MinInterval)
    andalso ?is_pos_integer(MaxInterval)
    andalso ?is_pos_integer(GrowthBase) andalso GrowthBase >= 1
    andalso is_number(GrowthExponent) andalso GrowthExponent >= 0
    andalso MaxInterval >= MinInterval;
is_error_retry_behaviour(_) ->
    false.

-spec default_settings(origin()) -> settings().
default_settings({maxmind,_}) ->
    default_http_origin_settings();
default_settings({http,_}) ->
    default_http_origin_settings();
default_settings({filesystem,_}) ->
    default_filesystem_origin_settings().

-spec default_http_origin_settings() -> settings().
default_http_origin_settings() ->
    #settings{
       update_period = timer:hours(6),
       error_retry_behaviour = default_http_origin_error_retry_behaviour(),
       error_retry_behaviour_applies_after_readiness = true,
       use_cache = true
      }.

default_http_origin_error_retry_behaviour() ->
    {exponential_backoff, #{min_interval => timer:seconds(1),
                            max_interval => timer:minutes(15),
                            growth_base => timer:seconds(2),
                            growth_exponent => 0.625}}.

-spec default_filesystem_origin_settings() -> settings().
default_filesystem_origin_settings() ->
    #settings{
       update_period = timer:seconds(30),
       error_retry_behaviour = default_filesystem_origin_error_retry_behaviour(),
       error_retry_behaviour_applies_after_readiness = true,
       use_cache = false
      }.

default_filesystem_origin_error_retry_behaviour() ->
    {exponential_backoff, #{min_interval => timer:seconds(1),
                            max_interval => timer:seconds(30),
                            growth_base => timer:seconds(2),
                            growth_exponent => 1.0}}.

-spec customized_settings(atom(), settings(), [loader_opt()]) -> settings().
customized_settings(DatabaseId, Settings, LoaderOpts) ->
    lists:foldl(
      fun ({update_period, Interval}, Acc) ->
              Acc#settings{ update_period = Interval };
          ({error_retries, Behaviour}, Acc) ->
              Acc#settings{ error_retry_behaviour = Behaviour };
          (no_cache, Acc) ->
              Acc#settings{ use_cache = false};
          %
          % Legacy options
          %
          ({pre_readiness_update_period, Interval} = Opt, Acc) ->
              log_warning_on_use_of_legacy_opt(DatabaseId, Opt),
              ErrorRetryBehaviour = {backoff, Interval},
              Acc#settings{ error_retry_behaviour = ErrorRetryBehaviour,
                            error_retry_behaviour_applies_after_readiness = false };
          ({post_readiness_update_period, Interval} = Opt, Acc) ->
              log_warning_on_use_of_legacy_opt(DatabaseId, Opt),
              Acc#settings{ update_period = Interval }
      end,
      Settings, LoaderOpts).

log_warning_on_use_of_legacy_opt(DatabaseId, {pre_readiness_update_period, Interval}) ->
    locus_logger:log_warning(
      "[~ts] You've specified the following legacy option: {pre_readiness_update_period, ~b}.~n"
      "However, the default behaviour is now exponential backoff, and therefore much improved.~n"
      "If you don't want to give it a chance and prefer to keep constant backoffs~n"
      "while silencing this warning, use `{error_retries, {backoff, ~b}}` instead.",
      [DatabaseId, Interval, Interval]);
log_warning_on_use_of_legacy_opt(DatabaseId, {post_readiness_update_period, Interval}) ->
    locus_logger:log_warning(
      "[~ts] You've specified the following legacy option: {post_readiness_update_period, ~b}.~n"
      "To silence this warning, use {update_period, ~b} instead.",
      [DatabaseId, Interval, Interval]).

-spec finish_initialization(state()) -> state().
finish_initialization(State)
  when (State#state.settings)#settings.use_cache ->
    CachedDatabasePath = cached_database_path(State),
    Source = {cache,CachedDatabasePath},
    {ok, FetcherPid} = locus_filesystem_load:start_link(Source, undefined),
    State#state{ fetcher_pid = FetcherPid, fetcher_source = Source };
finish_initialization(State) ->
    schedule_update(0, State).

-spec schedule_update(0 | milliseconds_interval(), state()) -> state().
schedule_update(Interval, State)
  when State#state.update_timer =:= undefined ->
    NewTimer = erlang:send_after(Interval, self(), begin_update),
    State#state{ update_timer = NewTimer }.

-spec cached_database_path(state()) -> nonempty_string().
cached_database_path(State) ->
    case State#state.origin of
        {maxmind, Edition} ->
            FetcherOpts = State#state.fetcher_opts,
            MaybeDate = proplists:get_value(date, FetcherOpts),
            cached_database_path_for_maxmind_edition(Edition, MaybeDate);
        {http, URL} ->
            cached_database_path_for_url(URL)
    end.

-spec cached_database_path_for_maxmind_edition(atom(), undefined | calendar:date())
        -> nonempty_string().
%% @private
cached_database_path_for_maxmind_edition(Edition, MaybeDate) ->
    DirectoryPath = cache_directory_path(),
    BinEdition = atom_to_binary(Edition, utf8),
    BaseFilename = locus_util:filesystem_safe_name(BinEdition),
    FilenameIoData =
        case MaybeDate of
            undefined ->
                io_lib:format("~ts.mmdb.gz", [BaseFilename]);
            {Year, Month, Day} ->
                io_lib:format("~ts.~4..0B-~2..0B-~2..0B.mmdb.gz",
                              [BaseFilename, Year, Month, Day])
        end,
    Filename = unicode:characters_to_list(FilenameIoData),
    filename:join(DirectoryPath, Filename).

-spec cached_database_path_for_url(string()) -> nonempty_string().
%% @private
cached_database_path_for_url(URL) ->
    DirectoryPath = cache_directory_path(),
    URLHash = crypto:hash(sha256, URL),
    HexURLHash = locus_util:bin_to_hex_str(URLHash),
    Filename = HexURLHash ++ ".mmdb.gz",
    filename:join(DirectoryPath, Filename).

-spec cache_directory_path() -> nonempty_string().
-ifdef(TEST).
cache_directory_path() ->
    filename:join(
      filename:basedir(user_cache, "locus_erlang"),
      "tests").
-else.
cache_directory_path() ->
    filename:basedir(user_cache, "locus_erlang").
-endif.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Database Updates
%% ------------------------------------------------------------------

-spec begin_update(state()) -> state().
begin_update(State)
  when State#state.fetcher_pid =:= undefined ->
    #state{origin = Origin,
           fetcher_opts = FetcherOpts,
           last_modified = LastModified} = State,

    case Origin of
        {maxmind, Edition} ->
            Headers = http_request_headers(LastModified),
            {ok, FetcherPid} = locus_maxmind_download:start_link(Edition, Headers, FetcherOpts),
            State#state{ fetcher_pid = FetcherPid, fetcher_source = {remote,Edition} };
        {http, URL} ->
            Headers = http_request_headers(LastModified),
            {ok, FetcherPid} = locus_http_download:start_link(URL, Headers, FetcherOpts),
            State#state{ fetcher_pid = FetcherPid, fetcher_source = {remote,URL} };
        {filesystem, _} = Source ->
            {ok, FetcherPid} = locus_filesystem_load:start_link(Source, LastModified),
            State#state{ fetcher_pid = FetcherPid, fetcher_source = Source }
    end.

http_request_headers(undefined) ->
    http_base_request_headers();
http_request_headers(LastModified) ->
    LocalLastModified = calendar:universal_time_to_local_time(LastModified),
    [{"if-modified-since", httpd_util:rfc1123_date(LocalLastModified)}
     | http_base_request_headers()].

http_base_request_headers() ->
    [{"accept", join_http_header_values(
                  ["application/gzip",
                   "application/x-gzip",
                   "application/x-gtar",
                   "application/x-tgz",
                   "application/x-tar",
                   "application/octet-stream"
                  ])},
     {"connection", "close"}].

join_http_header_values(Values) ->
    string:join(Values, "; ").

-spec handle_fetcher_msg(fetcher_msg(), state()) -> {noreply, state()}.
handle_fetcher_msg({event,Event}, State) ->
    #state{fetcher_source = Source} = State,
    case Source of
        {cache,_} ->
            {noreply, State};
        _ ->
            report_event(Event, State),
            {noreply, State}
    end;
handle_fetcher_msg({finished,Status}, State) ->
    #state{fetcher_pid = FetcherPid, fetcher_source = Source} = State,
    locus_util:expect_linked_process_termination(FetcherPid),
    UpdatedState = State#state{ fetcher_pid = undefined, fetcher_source = undefined },
    case Status of
        dismissed ->
            handle_database_fetch_dismissal(Source, UpdatedState);
        {success, Success} ->
            handle_database_fetch_success(Source, Success, UpdatedState);
        {error, Reason} ->
            handle_database_fetch_error(Source, Reason, UpdatedState)
    end.

-spec handle_cacher_msg(cacher_msg(), state()) -> {noreply, state()}.
handle_cacher_msg({finished,Status}, State) ->
    #state{cacher_pid = CacherPid, cacher_path = CacherPath, cacher_source = Source} = State,

    locus_util:expect_linked_process_termination(CacherPid),
    UpdatedState = State#state{ cacher_pid = undefined,
                                cacher_path = undefined,
                                cacher_source = undefined },
    case Status of
        success ->
            report_event({cache_attempt_finished, CacherPath, ok}, UpdatedState),
            handle_load_attempt_conclusion(Source, reset, UpdatedState);
        {error, Reason} ->
            report_event({cache_attempt_finished, CacherPath, {error,Reason}}, UpdatedState),
            handle_load_attempt_conclusion(Source, reset, UpdatedState)
    end.

-spec handle_database_fetch_dismissal(source(), state()) -> {noreply, state()}.
handle_database_fetch_dismissal(Source, State)
  when State#state.last_modified =/= undefined -> % sanity check
    handle_load_attempt_conclusion(Source, reset, State).

-spec handle_database_fetch_success(source(), fetcher_success(), state()) -> {noreply, state()}.
handle_database_fetch_success(Source, Success, State) ->
    {BlobFormat, Blob} = fetched_database_format_and_blob(Source, Success),
    case decode_database_from_blob(Source, BlobFormat, Blob) of
        {ok, Version, Parts, BinDatabase} ->
            LastModified = fetched_database_modification_datetime(Source, Success),
            handle_database_decode_success(Source, Version, Parts, BinDatabase, LastModified, State);
        {error, Reason} ->
            handle_database_decode_error(Source, Reason, State)
    end.

-spec handle_database_decode_success(source(), calendar:datetime(), locus_mmdb:parts(),
                                     binary(), calendar:datetime(), state()) -> {noreply, state()}.
handle_database_decode_success(Source, Version, Parts, BinDatabase, LastModified, State) ->
    State2 = State#state{ last_modified = LastModified },
    notify_owner({load_success, Source, Version, Parts}, State2),

    case Source of
        {remote,_} when (State2#state.settings)#settings.use_cache ->
            CachedDatabasePath = cached_database_path(State2),
            CachedDatabaseBlob = make_cached_database_blob(CachedDatabasePath, BinDatabase),
            {ok, CacherPid} = locus_filesystem_store:start_link(CachedDatabasePath, CachedDatabaseBlob,
                                                                LastModified),
            State3 = State2#state{ cacher_pid = CacherPid,
                                   cacher_path = CachedDatabasePath,
                                   cacher_source = Source },
            {noreply, State3};
        _ ->
            handle_load_attempt_conclusion(Source, reset, State2)
    end.

-spec handle_database_decode_error(source(), term(), state()) -> {noreply, state()}.
handle_database_decode_error(Source, Reason, State) ->
    notify_owner({load_failure, Source, Reason}, State),
    handle_load_attempt_conclusion(Source, +1, State).

-spec handle_database_fetch_error(source(), term(), state()) -> {noreply, state()}.
handle_database_fetch_error(Source, Reason, State) ->
    notify_owner({load_failure, Source, Reason}, State),
    case Source of
        {cache,_} -> handle_load_attempt_conclusion(Source, reset, State);
        _         -> handle_load_attempt_conclusion(Source, +1, State)
    end.

-spec handle_load_attempt_conclusion(source(), reset | +1, state()) -> {noreply, state()}.
handle_load_attempt_conclusion(Source, ErrorBackoffUpdate, State) ->
    State2 = update_error_backoff_count(ErrorBackoffUpdate, State),
    TimeToNextUpdate = time_to_next_update(Source, State2),
    State3 = schedule_update(TimeToNextUpdate, State2),
    {noreply, State3}.

-spec update_error_backoff_count(reset | +1, state()) -> state().
update_error_backoff_count(reset, State) ->
    State#state{ error_backoff_count = 0 };
update_error_backoff_count(Increment, State) ->
    Current = State#state.error_backoff_count,
    State#state{ error_backoff_count = Current + Increment }.

-spec time_to_next_update(source(), state()) -> milliseconds_interval().
time_to_next_update(LastFetchSource, State) ->
    #state{settings = Settings, error_backoff_count = ErrorBackoffCount} = State,
    {LastFetchSourceType, _} = LastFetchSource,
    HasAchievedReadiness = (State#state.last_modified =/= undefined),

    if LastFetchSourceType =:= cache ->
           0;

       ErrorBackoffCount > 0,
       (Settings#settings.error_retry_behaviour_applies_after_readiness
        orelse not HasAchievedReadiness) ->
           error_backoff_interval(ErrorBackoffCount, Settings#settings.error_retry_behaviour);

       HasAchievedReadiness ->
           Settings#settings.update_period
    end.

-spec error_backoff_interval(pos_integer(), error_retry_behaviour()) -> milliseconds_interval().
error_backoff_interval(_, {backoff, Interval}) ->
    Interval;
error_backoff_interval(Count, {exponential_backoff, Params}) ->
    exponential_error_backoff_interval(Count, Params).

exponential_error_backoff_interval(Count, Params) ->
    #{min_interval := Min, max_interval := Max,
      growth_base := GrowthBase, growth_exponent := GrowthExponent} = Params,

    MultipliedGrowthExponent = (Count - 1) * GrowthExponent,
    Growth = (1000 * math:pow(GrowthBase / 1000, MultipliedGrowthExponent)) - Min,
    min(Max, Min + trunc(Growth)).

-spec decode_database_from_blob(source(), blob_format(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_tgz_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_tarball_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_blob(Source, tgz, Blob) ->
    decode_database_from_tgz_blob(Source, Blob);
decode_database_from_blob(Source, tarball, Blob) ->
    decode_database_from_tarball_blob(Source, Blob);
decode_database_from_blob(Source, gzip, Blob) ->
    decode_database_from_gzip_blob(Source, Blob);
decode_database_from_blob(Source, gzipped_mmdb, Blob) ->
    decode_database_from_gzipped_mmdb_blob(Source, Blob);
decode_database_from_blob(Source, mmdb, Blob) ->
    decode_database_from_mmdb_blob(Source, Blob);
decode_database_from_blob(Source, unknown, Blob) ->
    decode_database_from_unknown_blob(Source, Blob).

-spec decode_database_from_tgz_blob(source(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_tarball_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_tgz_blob(Source, Blob) ->
    try zlib:gunzip(Blob) of
        Tarball ->
            decode_database_from_tarball_blob(Source, Tarball)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            {error, {decode_database_from_tgz_blob, {Class, SaferReason, SaferStacktrace}}}
    end.

-spec decode_database_from_gzip_blob(source(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_gzip_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_tarball_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_gzip_blob(Source, Blob) ->
    try zlib:gunzip(Blob) of
        Uncompressed ->
            decode_database_from_unknown_blob(Source, Uncompressed)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            {error, {decode_database_from_gzip_blob, {Class, SaferReason, SaferStacktrace}}}
    end.

-spec decode_database_from_gzipped_mmdb_blob(source(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_gzipped_mmdb_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_gzipped_mmdb_blob(Source, Blob) ->
    try zlib:gunzip(Blob) of
        Uncompressed ->
            decode_database_from_mmdb_blob(Source, Uncompressed)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            {error, {decode_database_from_gzipped_mmdb_blob, {Class, SaferReason, SaferStacktrace}}}
    end.

-spec decode_database_from_unknown_blob(source(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_tarball_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_unknown_blob(Source, Blob) ->
    case decode_database_from_tarball_blob(Source, Blob) of
        {ok, _, _, _} = Success ->
            Success;
        _ ->
            decode_database_from_mmdb_blob(Source, Blob)
    end.

-spec decode_database_from_tarball_blob(source(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_tarball_blob, {atom(), term(), [term()]}}} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_tarball_blob(Source, Tarball) ->
    try extract_mmdb_from_tarball_blob(Tarball) of
        BinDatabase ->
            decode_database_from_mmdb_blob(Source, BinDatabase)
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            {error, {decode_database_from_tarball_blob, {Class, SaferReason, SaferStacktrace}}}
    end.

-spec decode_database_from_mmdb_blob(source(), binary())
        -> {ok, calendar:datetime(), locus_mmdb:parts(), binary()} |
           {error, {decode_database_from_mmdb_blob, {atom(), term(), [term()]}}}.
decode_database_from_mmdb_blob(Source, BinDatabase) ->
    try locus_mmdb:decode_database_parts(Source, BinDatabase) of
        {Version, Parts} ->
            {ok, Version, Parts, BinDatabase}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            {error, {decode_database_from_mmdb_blob, {Class, SaferReason, SaferStacktrace}}}
    end.

-spec extract_mmdb_from_tarball_blob(binary()) -> binary().
extract_mmdb_from_tarball_blob(Tarball) ->
    {ok, ContainedPaths} = erl_tar:table({binary,Tarball}),
    {true, DatabasePath} = locus_util:lists_anymap(fun has_mmdb_extension/1, ContainedPaths),
    {ok, [{DatabasePath, BinDatabase}]} =
        erl_tar:extract({binary,Tarball}, [{files, [DatabasePath]}, memory]),
    BinDatabase.

%-spec has_mmdb_extension(nonempty_string()) -> boolean().
has_mmdb_extension({Filename, _Type, _Size, _MTime, _Mode, _Uid, _Gid}) ->
    % FIXME: this a placeholder for OTP 20; due to the incomplete spec
    % of erl_tar:table/2, Dialyzer comes to believe that no strings
    % can be returned, only the above tuple, which in fact is only returned
    % if the 'verbose' option is picked, something that we are definitely
    % not doing.
    case filename_extension_parts(Filename) of
        ["mmdb"|_] -> {true, Filename};
        _ -> false
    end;
has_mmdb_extension(Filename) ->
    case filename_extension_parts(Filename) of
        ["mmdb"|_] -> true;
        _ -> false
    end.

filename_extension_parts(Filename) ->
    filename_extension_parts_recur(Filename, []).

filename_extension_parts_recur(Filename, Acc) ->
    case filename:extension(Filename) of
        "." ++ RawExtension ->
            Extension = string:to_lower(RawExtension),
            UpdatedFilename = lists:sublist(Filename, length(Filename) - length(Extension) - 1),
            UpdatedAcc = [Extension | Acc],
            filename_extension_parts_recur(UpdatedFilename, UpdatedAcc);
        _ ->
            lists:reverse(Acc)
    end.

-spec fetched_database_format_and_blob(source(), fetcher_success()) -> {blob_format(), binary()}.
fetched_database_format_and_blob({remote,_}, #{headers := Headers, body := Body}) ->
    case {lists:keyfind("content-type", 1, Headers), Body} of
        {{_,"application/gzip"}, _} ->
            {gzip, Body};
        {{_,"application/x-gzip"}, _} ->
            {gzip, Body};
        {{_,"application/x-gtar"}, <<?GZIP_MAGIC_BYTES,_/bytes>>} ->
            {tgz, Body};
        {{_,"application/x-tgz"}, _} ->
            {tgz, Body};
        {{_,"application/x-tar"}, _} ->
            {tarball, Body};
        {_, <<?GZIP_MAGIC_BYTES,_/bytes>>} ->
            {gzip, Body};
        _ ->
            {unknown, Body}
    end;
fetched_database_format_and_blob({SourceType,Path}, #{content := Content})
  when SourceType =:= cache;
       SourceType =:= filesystem ->
    case {filename_extension_parts(Path), Content} of
        {["tgz"|_], _} ->
            {tgz, Content};
        {["gz","tar"|_], _} ->
            {tgz, Content};
        {["tar"|_], _} ->
            {tarball, Content};
        {["mmdb"|_], _} ->
            {mmdb, Content};
        {["gz","mmdb"|_], _} ->
            {gzipped_mmdb, Content};
        {_, <<?GZIP_MAGIC_BYTES,_/bytes>>} ->
            {gzip, Content};
        _ ->
            {unknown, Content}
    end.

-spec fetched_database_modification_datetime(source(), fetcher_success()) -> calendar:datetime().
fetched_database_modification_datetime({remote,_}, #{headers := Headers}) ->
    case lists:keyfind("last-modified", 1, Headers) of
        {"last-modified", LastModified} ->
            ({_,_} = ModificationDate) = httpd_util:convert_request_date(LastModified),
            ModificationDate;
        false ->
            {{1970,1,1}, {0,0,0}}
    end;
fetched_database_modification_datetime({cache,_}, #{modified_on := ModificationDate}) ->
    ModificationDate;
fetched_database_modification_datetime({filesystem,_}, #{modified_on := ModificationDate}) ->
    ModificationDate.

%-spec make_cached_database_blob(nonempty_string(), binary()) -> binary().
make_cached_database_blob(CachedTarballPath, BinDatabase) ->
    case filename_extension_parts(CachedTarballPath) of
        ["gz","mmdb"|_] -> zlib:gzip(BinDatabase)
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Monitoring and Events
%% ------------------------------------------------------------------

-spec handle_linked_process_death(pid(), term(), state())
        -> {stop, Reason, state()}
    when Reason :: normal | FetcherStopped | CacherStopped,
         FetcherStopped :: {fetched_stopped, pid(), term()},
         CacherStopped :: {cached_stopped, pid(), term()}.
handle_linked_process_death(Pid, _, State)
  when Pid =:= State#state.owner_pid ->
    {stop, normal, State};
handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.fetcher_pid ->
    {stop, {fetcher_stopped, Pid, Reason}, State};
handle_linked_process_death(Pid, Reason, State)
  when Pid =:= State#state.cacher_pid ->
    {stop, {cacher_stopped, Pid, Reason}, State}.

-spec report_event(event(), state()) -> ok.
report_event(Event, State) ->
    notify_owner({event,Event}, State).

-spec notify_owner(msg(), state()) -> ok.
notify_owner(Msg, State) ->
    #state{owner_pid = OwnerPid} = State,
    _ = erlang:send(OwnerPid, {self(),Msg}, [noconnect]),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Unit Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

constant_error_backoff_test() ->
    lists:foreach(
      fun (_) ->
              Interval = rand:uniform(10000),
              RetryBehaviour = {backoff, Interval},
              lists:foreach(
                fun (ConsecutiveErrors) ->
                        ?assertEqual(Interval, error_backoff_interval(ConsecutiveErrors, RetryBehaviour))
                end,
                lists:seq(1, 1000))
      end,
      lists:seq(1, 100)).

exponential_error_backoff_test() ->
    Trial1 = {exponential_backoff,
              #{min_interval => timer:seconds(1),
                max_interval => timer:hours(24),
                growth_base => timer:seconds(2),
                growth_exponent => 0.625}},
	?assertEqual(1000, error_backoff_interval(1, Trial1)),
	?assertEqual(1542, error_backoff_interval(2, Trial1)),
	?assertEqual(2378, error_backoff_interval(3, Trial1)),
	?assertEqual(3668, error_backoff_interval(4, Trial1)),
	?assertEqual(5656, error_backoff_interval(5, Trial1)),
	?assertEqual(8724, error_backoff_interval(6, Trial1)),
	?assertEqual(13454, error_backoff_interval(7, Trial1)),
	?assertEqual(20749, error_backoff_interval(8, Trial1)),
	?assertEqual(32000, error_backoff_interval(9, Trial1)),
	?assertEqual(49350, error_backoff_interval(10, Trial1)),
	?assertEqual(76109, error_backoff_interval(11, Trial1)),
	?assertEqual(117376, error_backoff_interval(12, Trial1)),
	?assertEqual(181019, error_backoff_interval(13, Trial1)),
	?assertEqual(279169, error_backoff_interval(14, Trial1)),
	?assertEqual(430538, error_backoff_interval(15, Trial1)),
	?assertEqual(663981, error_backoff_interval(16, Trial1)),
	?assertEqual(1024000, error_backoff_interval(17, Trial1)),
	?assertEqual(1579223, error_backoff_interval(18, Trial1)),
	?assertEqual(2435496, error_backoff_interval(19, Trial1)),
	?assertEqual(3756048, error_backoff_interval(20, Trial1)),
	?assertEqual(5792618, error_backoff_interval(21, Trial1)),
	?assertEqual(8933439, error_backoff_interval(22, Trial1)),
	?assertEqual(13777246, error_backoff_interval(23, Trial1)),
	?assertEqual(21247419, error_backoff_interval(24, Trial1)),
	?assertEqual(32768000, error_backoff_interval(25, Trial1)),
	?assertEqual(50535164, error_backoff_interval(26, Trial1)),
	?assertEqual(77935877, error_backoff_interval(27, Trial1)),
	?assertEqual(86400000, error_backoff_interval(28, Trial1)),
	?assertEqual(86400000, error_backoff_interval(29, Trial1)),
	?assertEqual(86400000, error_backoff_interval(30, Trial1)),

    Trial2 = {exponential_backoff,
              #{min_interval => timer:seconds(1),
                max_interval => timer:hours(24),
                growth_base => timer:seconds(3),
                growth_exponent => 0.5}},
	?assertEqual(1000, error_backoff_interval(1, Trial2)),
	?assertEqual(1732, error_backoff_interval(2, Trial2)),
	?assertEqual(3000, error_backoff_interval(3, Trial2)),
	?assertEqual(5196, error_backoff_interval(4, Trial2)),
	?assertEqual(9000, error_backoff_interval(5, Trial2)),
	?assertEqual(15588, error_backoff_interval(6, Trial2)),
	?assertEqual(27000, error_backoff_interval(7, Trial2)),
	?assertEqual(46765, error_backoff_interval(8, Trial2)),
	?assertEqual(81000, error_backoff_interval(9, Trial2)),
	?assertEqual(140296, error_backoff_interval(10, Trial2)),
	?assertEqual(243000, error_backoff_interval(11, Trial2)),
	?assertEqual(420888, error_backoff_interval(12, Trial2)),
	?assertEqual(729000, error_backoff_interval(13, Trial2)),
	?assertEqual(1262665, error_backoff_interval(14, Trial2)),
	?assertEqual(2187000, error_backoff_interval(15, Trial2)),
	?assertEqual(3787995, error_backoff_interval(16, Trial2)),
	?assertEqual(6561000, error_backoff_interval(17, Trial2)),
	?assertEqual(11363985, error_backoff_interval(18, Trial2)),
	?assertEqual(19683000, error_backoff_interval(19, Trial2)),
	?assertEqual(34091956, error_backoff_interval(20, Trial2)),
	?assertEqual(59049000, error_backoff_interval(21, Trial2)),
	?assertEqual(86400000, error_backoff_interval(22, Trial2)),
	?assertEqual(86400000, error_backoff_interval(23, Trial2)),
	?assertEqual(86400000, error_backoff_interval(24, Trial2)),
	?assertEqual(86400000, error_backoff_interval(25, Trial2)),
	?assertEqual(86400000, error_backoff_interval(26, Trial2)),
	?assertEqual(86400000, error_backoff_interval(27, Trial2)),
	?assertEqual(86400000, error_backoff_interval(28, Trial2)),
	?assertEqual(86400000, error_backoff_interval(29, Trial2)),
	?assertEqual(86400000, error_backoff_interval(30, Trial2)),

    Trial3 = {exponential_backoff,
              #{min_interval => timer:seconds(1),
                max_interval => timer:hours(24),
                growth_base => timer:seconds(1.5),
                growth_exponent => 2.0}},
	?assertEqual(1000, error_backoff_interval(1, Trial3)),
	?assertEqual(2250, error_backoff_interval(2, Trial3)),
	?assertEqual(5062, error_backoff_interval(3, Trial3)),
	?assertEqual(11390, error_backoff_interval(4, Trial3)),
	?assertEqual(25628, error_backoff_interval(5, Trial3)),
	?assertEqual(57665, error_backoff_interval(6, Trial3)),
	?assertEqual(129746, error_backoff_interval(7, Trial3)),
	?assertEqual(291929, error_backoff_interval(8, Trial3)),
	?assertEqual(656840, error_backoff_interval(9, Trial3)),
	?assertEqual(1477891, error_backoff_interval(10, Trial3)),
	?assertEqual(3325256, error_backoff_interval(11, Trial3)),
	?assertEqual(7481827, error_backoff_interval(12, Trial3)),
	?assertEqual(16834112, error_backoff_interval(13, Trial3)),
	?assertEqual(37876752, error_backoff_interval(14, Trial3)),
	?assertEqual(85222692, error_backoff_interval(15, Trial3)),
	?assertEqual(86400000, error_backoff_interval(16, Trial3)),
	?assertEqual(86400000, error_backoff_interval(17, Trial3)),
	?assertEqual(86400000, error_backoff_interval(18, Trial3)),
	?assertEqual(86400000, error_backoff_interval(19, Trial3)),
	?assertEqual(86400000, error_backoff_interval(20, Trial3)),
	?assertEqual(86400000, error_backoff_interval(21, Trial3)),
	?assertEqual(86400000, error_backoff_interval(22, Trial3)),
	?assertEqual(86400000, error_backoff_interval(23, Trial3)),
	?assertEqual(86400000, error_backoff_interval(24, Trial3)),
	?assertEqual(86400000, error_backoff_interval(25, Trial3)),
	?assertEqual(86400000, error_backoff_interval(26, Trial3)),
	?assertEqual(86400000, error_backoff_interval(27, Trial3)),
	?assertEqual(86400000, error_backoff_interval(28, Trial3)),
	?assertEqual(86400000, error_backoff_interval(29, Trial3)),
	?assertEqual(86400000, error_backoff_interval(30, Trial3)).

-endif.
