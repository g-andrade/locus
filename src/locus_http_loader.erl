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

-include("locus_pre_otp19_compat.hrl").

-module(locus_http_loader).
-behaviour(?gen_statem).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).                    -ignore_xref({start_link, 3}).
-export([wait/2]).

-ifdef(TEST).
-export([whereis/1]).
-export([list_subscribers/1]).
-export([cached_tarball_name_for_url/1]).
-endif.

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------

-export([callback_mode/0]).
-export([init/1]).
%% states
-export([initializing/3]).                  -ignore_xref({initializing,3}).
-export([ready/3]).                         -ignore_xref({ready,3}).
-export([waiting_stream_start/3]).          -ignore_xref({waiting_stream_start,3}).
-export([waiting_stream_end/3]).            -ignore_xref({waiting_stream_end,3}).
-export([processing_update/3]).             -ignore_xref({processing_update,3}).
%%
-export([code_change/4]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).

-define(DEFAULT_HTTP_CONNECT_TIMEOUT, (timer:seconds(8))).
-define(DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT, (timer:seconds(5))).
-define(DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT, (timer:seconds(5))).
-define(DEFAULT_PRE_READINESS_UPDATE_PERIOD, (timer:minutes(1))).
-define(DEFAULT_POST_READINESS_UPDATE_PERIOD, (timer:hours(6))).

-define(is_timeout(V), ((is_integer((V)) andalso ((V) >= 0)) orelse ((V) =:= infinity))).
-define(is_pos_integer(V), ((is_integer((V)) andalso ((V) >= 1)))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type from() :: {To :: pid(), Tag :: term()}.

-type opt() ::
    {event_subscriber, module() | pid()} |
    {connect_timeout, timeout()} |
    {download_start_timeout, timeout()} |
    {idle_download_timeout, timeout()} |
    {pre_readiness_update_period, pos_integer()} |
    {post_readiness_update_period, pos_integer()} |
    no_cache |
    {internal, internal_opt()}.
-export_type([opt/0]).

-opaque internal_opt() ::
    {async_waiter, {pid(),reference()}}.
-export_type([internal_opt/0]).

-record(state_data, {
          id :: atom(),
          url :: url(),
          waiters :: [from()],
          event_subscribers :: [module() | pid()],
          connect_timeout :: timeout(),
          download_start_timeout :: timeout(),
          idle_download_timeout :: timeout(),
          pre_readiness_update_period :: pos_integer(),
          post_readiness_update_period :: pos_integer(),
          no_cache :: boolean(),
          insecure :: boolean(),

          request_id :: reference() | undefined,
          last_response_headers :: headers() | undefined,
          last_response_body :: binary() | undefined,
          last_modified :: calendar:datetime() | undefined,
          last_version :: calendar:datetime() | undefined
         }).
-type state_data() :: #state_data{}.

-type filename() :: string().
-export_type([filename/0]).

-type url() :: string().
-export_type([url/0]).

-type response_status() :: {100..999, binary()}.
-export_type([response_status/0]).

-type headers() :: [{string(), string()}].
-export_type([headers/0]).

-type body() :: binary().
-export_type([body/0]).

-type event() ::
        event_request_sent() |
        event_download_dismissed() |
        event_download_failed_to_start() |
        event_download_started() |
        event_download_finished() |
        event_load_attempt_finished() |
        event_cache_attempt_finished().
-export_type([event/0]).

-type event_request_sent() ::
        {request_sent, url(), headers()}.
-export_type([event_request_sent/0]).

-type event_download_dismissed() ::
        {download_dismissed, {http, response_status(), headers(), body()}}.
-export_type([event_download_dismissed/0]).

-type event_download_failed_to_start() ::
        {download_failed_to_start, {http, response_status(), headers(), body()}} |
        {download_failed_to_start, {error, term()}} |
        {download_failed_to_start, timeout}.
-export_type([event_download_failed_to_start/0]).

-type event_download_started() ::
        {download_started, headers()}.
-export_type([event_download_started/0]).

-type event_download_finished() ::
        {download_finished, BodySize :: non_neg_integer(), {ok, TrailingHeaders :: headers()}} |
        {download_finished, BodySize :: non_neg_integer(), {error, term()}} |
        {download_finished, BodySize :: non_neg_integer(), {error, timeout}}.
-export_type([event_download_finished/0]).

-type event_load_attempt_finished() ::
        {load_attempt_finished, locus_mmdb:http_loader_source(), {ok, Version :: calendar:datetime()}} |
        {load_attempt_finished, locus_mmdb:http_loader_source(), {error, term()}}.
-export_type([event_load_attempt_finished/0]).

-type event_cache_attempt_finished() ::
        {cache_attempt_finished, filename(), ok} |
        {cache_attempt_finished, filename(), {error, term()}}.
-export_type([event_cache_attempt_finished/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom(), string(), [opt()]) -> {ok, pid()}.
%% @private
start_link(Id, URL, Opts) ->
    ServerName = server_name(Id),
    ?gen_statem:start_link({local, ServerName}, ?CB_MODULE, [Id, URL, Opts], []).

-spec wait(atom(), timeout())
        -> {ok, LoadedVersion :: calendar:datetime()} |
           {error, database_unknown | timeout | {loading, term()}}.
%% @private
wait(Id, Timeout) ->
    ServerName = server_name(Id),
    try ?gen_statem:call(ServerName, wait, Timeout) of
        {ok, LoadedVersion} ->
            {ok, LoadedVersion};
        {error, LoadingError} ->
            {error, {loading, LoadingError}}
    catch
        exit:{timeout, {?gen_statem,call,[ServerName|_]}} when Timeout =/= infinity ->
            {error, timeout};
        %exit:{{nodedown,_RemoteNode}, {?gen_statem,call,[ServerName|_}} ->
        %    % Cannot happen (loader is always local)
        %    {error, database_unknown};
        exit:{noproc, {?gen_statem,call,[ServerName|_]}} ->
            {error, database_unknown};
        exit:{normal, {?gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{shutdown, {?gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown};
        exit:{{shutdown,_Reason}, {?gen_statem,call, [ServerName|_]}} ->
            {error, database_unknown}
    end.

-ifdef(TEST).
%% @private
whereis(Id) ->
    ServerName = server_name(Id),
    erlang:whereis(ServerName).

%% @private
list_subscribers(Id) ->
    ServerName = server_name(Id),
    {_State, StateData} = sys:get_state(ServerName),
    StateData#state_data.event_subscribers.
-endif.

%% ------------------------------------------------------------------
%% gen_statem Function Definitions
%% ------------------------------------------------------------------

-spec callback_mode() -> [state_functions | state_enter, ...].
%% @private
callback_mode() -> [state_functions, state_enter].

-spec init([atom() | string() | [opt()], ...])
        -> ?gen_statem:init_result(initializing).
%% @private
init([Id, URL, Opts]) ->
    locus_rand_compat:seed(),
    locus_mmdb:create_table(Id),
    init(Id, URL, Opts).

-spec initializing(enter, atom(), state_data())
                   -> keep_state_and_data;
                  (info, maybe_load_from_cache, state_data())
                   -> {next_state, ready, state_data(),
                       {next_event, internal, update_database}}.
%% @private
initializing(enter, _PrevState, _StateData) ->
    keep_state_and_data;
initializing(info, maybe_load_from_cache, #state_data{no_cache = true} = StateData) ->
    {next_state, ready, StateData, {next_event, internal, update_database}};
initializing(info, maybe_load_from_cache, StateData) ->
    CachedTarballName = cached_tarball_name(StateData),
    CachedTarballLookup = locus_util:read_file_and_its_modification_date(CachedTarballName),
    {StateData2, Replies} = handle_cached_tarball_lookup(CachedTarballLookup, CachedTarballName, StateData),
    {next_state, ready, StateData2, Replies ++ [{next_event, internal, update_database}]}.

-spec ready(enter, atom(), state_data())
            -> {keep_state_and_data, {state_timeout, pos_integer(), update_database}};
           ({call,from()}, wait, state_data())
           -> {keep_state, state_data(), [?gen_statem:reply_action()]};
           (info, {'DOWN', reference(), process, pid(), term()}, state_data())
           -> {keep_state, state_data()};
           (internal, update_database, state_data())
            -> {next_state, waiting_stream_start, state_data()};
           (state_timeout, update_database, state_data())
            -> {repeat_state_and_data, {next_event, internal, update_database}}.
%% @private
ready(enter, _PrevState, StateData) ->
    {keep_state_and_data, {state_timeout, update_period(StateData), update_database}};
ready({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
ready(info, {'DOWN', _Ref, process, Pid, _Reason}, StateData) ->
    handle_monitored_process_death(Pid, StateData);
ready(internal, update_database, StateData) ->
    #state_data{url = URL, connect_timeout = ConnectTimeout} = StateData,
    Headers = request_headers(StateData),
    Request = {URL, Headers},
    BaseHTTPOptions = [{connect_timeout, ConnectTimeout}],
    ExtraHTTPOptions =
        case StateData#state_data.insecure orelse
             http_uri:parse(URL)
        of
            true ->
                [];
            {ok, ParsedURL} when element(1, ParsedURL) =:= http ->
                [];
            {ok, ParsedURL} when element(1, ParsedURL) =:= https ->
                [{ssl,locus_https_requests:ssl_opts_for_ca_authentication(URL)}]
        end,
    HTTPOptions = BaseHTTPOptions  ++ ExtraHTTPOptions,
    Options = [{sync, false}, {stream, self}],
    {ok, RequestId} = httpc:request(get, Request, HTTPOptions, Options),
    true = is_reference(RequestId),
    UpdatedStateData = StateData#state_data{ request_id = RequestId },
    report_event({request_sent, URL, Headers}, StateData),
    {next_state, waiting_stream_start, UpdatedStateData};
ready(state_timeout, update_database, _StateData) ->
    {repeat_state_and_data, {next_event, internal, update_database}}.

-spec waiting_stream_start(enter, atom(), state_data())
                            -> {keep_state_and_data, {state_timeout, pos_integer(), timeout}};
                          ({call,from()}, wait, state_data())
                            -> {keep_state, state_data(), [?gen_statem:reply_action()]};
                          (info, {http, {reference(), stream_start, headers()}}, state_data())
                            -> {next_state, waiting_stream_end, state_data()};
                          (info, {http, {reference(), {{string(), integer(), string()},
                                                       headers(), binary()}}}, state_data())
                            -> {next_state, ready, state_data()} |
                               {next_state, ready, state_data(), [?gen_statem:reply_action()]};
                          (info, {http, {reference(), {error, term()}}}, state_data())
                            -> {next_state, ready, state_data(), [?gen_statem:reply_action()]};
                          (info, {'DOWN', reference(), process, pid(), term()}, state_data())
                          -> {keep_state, state_data()};
                          (state_timeout, timeout, state_data())
                            -> {next_state, ready, state_data(), [?gen_statem:reply_action()]}.
%% @private
waiting_stream_start(enter, _PrevState, StateData) ->
    #state_data{download_start_timeout = StreamStartTimeout} = StateData,
    {keep_state_and_data, {state_timeout, StreamStartTimeout, timeout}};
waiting_stream_start({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
waiting_stream_start(info, {http, {RequestId, stream_start, Headers}},
                     #state_data{ request_id = RequestId } = StateData) ->
    report_event({download_started, Headers}, StateData),
    UpdatedStateData =
        StateData#state_data{
          last_response_headers = Headers,
          last_response_body = <<>>
         },
    {next_state, waiting_stream_end, UpdatedStateData};
waiting_stream_start(info,
                     {http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, Headers, Body}}},
                     #state_data{ request_id = RequestId } = StateData)
  when StatusCode =:= 304 ->
    report_event({download_dismissed, {http, {StatusCode, StatusDesc}, Headers, Body}}, StateData),
    UpdatedStateData = StateData#state_data{request_id = undefined},
    {next_state, ready, UpdatedStateData};
waiting_stream_start(info,
                     {http, {RequestId, {{_HttpVersion, StatusCode, StatusDesc}, Headers, Body}}},
                     #state_data{ request_id = RequestId } = StateData) ->
    report_event({download_failed_to_start, {http, {StatusCode, StatusDesc}, Headers, Body}}, StateData),
    StateData2 = StateData#state_data{request_id = undefined},
    {StateData3, Replies} = reply_to_waiters({error, {http, StatusCode, StatusDesc}}, StateData2),
    {next_state, ready, StateData3, Replies};
waiting_stream_start(info, {http, {RequestId, {error, Reason}}},
                     #state_data{ request_id = RequestId } = StateData) ->
    report_event({download_failed_to_start, {error, Reason}}, StateData),
    StateData2 = StateData#state_data{request_id = undefined},
    {StateData3, Replies} = reply_to_waiters({error, {http, Reason}}, StateData2),
    {next_state, ready, StateData3, Replies};
waiting_stream_start(info, {'DOWN', _Ref, process, Pid, _Reason}, StateData) ->
    handle_monitored_process_death(Pid, StateData);
waiting_stream_start(state_timeout, timeout, #state_data{request_id = RequestId} = StateData) ->
    report_event({download_failed_to_start, timeout}, StateData),
    StateData2 = StateData#state_data{request_id = undefined},
    {StateData3, Replies} = reply_to_waiters({error, {timeout, waiting_stream_start}}, StateData2),
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    {next_state, ready, StateData3, Replies}.

-spec waiting_stream_end(enter, atom(), state_data())
                        -> {keep_state_and_data, {state_timeout, pos_integer(), timeout}};
                        ({call,from()}, wait, state_data())
                        -> {keep_state, state_data(), [?gen_statem:reply_action()]};
                        (info, {http, {reference(), stream, binary()}}, state_data())
                        -> {keep_state, state_data(), {state_timeout, pos_integer(), timeout}};
                        (info, {http, {reference(), stream_end, headers()}}, state_data())
                        -> {next_state, processing_update, state_data(),
                            {next_event, internal, execute}};
                        (info, {http, {reference(), {error, term()}}}, state_data())
                        -> {next_state, ready, state_data(), [?gen_statem:reply_action()]};
                        (info, {'DOWN', reference(), process, pid(), term()}, state_data())
                        -> {keep_state, state_data()};
                        (state_timeout, timeout, state_data())
                        -> {next_state, ready, state_data(), [?gen_statem:reply_action()]}.
%% @private
waiting_stream_end(enter, _PrevState, StateData) ->
    #state_data{idle_download_timeout = IdleStreamTimeout} = StateData,
    {keep_state_and_data, {state_timeout, IdleStreamTimeout, timeout}};
waiting_stream_end({call,From}, wait, StateData) ->
    {StateData2, Actions} = maybe_enqueue_waiter(From, StateData),
    {keep_state, StateData2, Actions};
waiting_stream_end(info, {http, {RequestId, stream, BinBodyPart}},
                   #state_data{ request_id = RequestId } = StateData) ->
    #state_data{idle_download_timeout = IdleStreamTimeout,
                last_response_body = Acc} = StateData,
    UpdatedAcc = <<Acc/binary, BinBodyPart/binary>>,
    UpdatedStateData = StateData#state_data{last_response_body = UpdatedAcc},
    %?log_info("~p database download in progress - ~.3f MiB received so far",
    %          [StateData#state_data.id,
    %           byte_size(UpdatedAcc) / (1 bsl 20)]),
    {keep_state, UpdatedStateData, {state_timeout, IdleStreamTimeout, timeout}};
waiting_stream_end(info, {http, {RequestId, stream_end, TrailHeaders}}, % no chunked encoding
                   #state_data{ request_id = RequestId } = StateData) ->
    #state_data{last_response_headers = PrevHeaders,
                last_response_body = LastResponseBody} = StateData,
    BodySize = byte_size(LastResponseBody),
    report_event({download_finished, BodySize, {ok, TrailHeaders}}, StateData),
    UpdatedHeaders = lists:usort(PrevHeaders ++ TrailHeaders),
    UpdatedStateData =
        StateData#state_data{
          request_id = undefined,
          last_response_headers = UpdatedHeaders
         },
    {next_state, processing_update, UpdatedStateData,
     {next_event, internal, execute}};
waiting_stream_end(info, {http, {RequestId, {error, Reason}}},
                   #state_data{ request_id = RequestId } = StateData) ->
    #state_data{last_response_body = LastResponseBody} = StateData,
    BodySizeSoFar = byte_size(LastResponseBody),
    report_event({download_finished, BodySizeSoFar, {error, Reason}}, StateData),
    StateData2 =
        StateData#state_data{
          request_id = undefined,
          last_response_headers = undefined,
          last_response_body = undefined
         },
    {StateData3, Replies} = reply_to_waiters({error, {http, Reason}}, StateData2),
    {next_state, ready, {StateData3, Replies}};
waiting_stream_end(info, {'DOWN', _Ref, process, Pid, _Reason}, StateData) ->
    handle_monitored_process_death(Pid, StateData);
waiting_stream_end(state_timeout, timeout, StateData) ->
    #state_data{request_id = RequestId, last_response_body = LastResponseBody} = StateData,
    BodySizeSoFar = byte_size(LastResponseBody),
    report_event({download_finished, BodySizeSoFar, {error, timeout}}, StateData),
    StateData2 =
        StateData#state_data{
          request_id = undefined,
          last_response_headers = undefined,
          last_response_body = undefined
         },
    {StateData3, Replies} = reply_to_waiters({error, {timeout, waiting_stream_end}}, StateData2),
    ok = httpc:cancel_request(RequestId),
    clear_inbox_of_late_http_messages(RequestId),
    {next_state, ready, StateData3, Replies}.

-spec processing_update(enter, atom(), state_data())
                        -> keep_state_and_data;
                       (internal, execute, state_data())
                       -> {next_state, ready, state_data(), [?gen_statem:reply_action()]}.
%% @private
processing_update(enter, _PrevState, _StateData) ->
    keep_state_and_data;
processing_update(internal, execute, StateData) ->
    #state_data{id = Id, url = URL,
                last_response_headers = Headers,
                last_response_body = Body
               } = StateData,
    StateData2 =
        StateData#state_data{
          last_response_headers = undefined,
          last_response_body = undefined
         },
    Source = {remote, URL},
    case locus_util:load_database_from_tarball(Id, Body, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, StateData2),
            LastModified = extract_last_modified_datetime_from_response_headers(Headers),
            StateData3 = StateData2#state_data{ last_modified = LastModified,
                                                last_version = Version },
            maybe_try_saving_cached_tarball(Body, LastModified, StateData3),
            {StateData4, Replies} = reply_to_waiters({ok, Version}, StateData3),
            {next_state, ready, StateData4, Replies};
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, StateData2),
            {StateData3, Replies} = reply_to_waiters({error, Error}, StateData2),
            {next_state, ready, StateData3, Replies}
    end.

-spec code_change(term(), atom(), state_data(), term()) -> {ok, atom(), state_data()}.
%% @private
code_change(_OldVsn, State, #state_data{} = StateData, _Extra) ->
    % lib is at version 1.7 or higher
    {ok, State, StateData};
code_change(_OldVsn, State, #{} = OldStateData, _Extra) ->
    % lib was at version 1.6 or older
    UpdatedStateData =
        #state_data{
           id = maps:get(id, OldStateData),
           url = maps:get(url, OldStateData),
           waiters = maps:get(waiters, OldStateData),
           event_subscribers =
                maps:get(event_subscribers, OldStateData,
                         [locus_logger]),
           connect_timeout =
                maps:get(connect_timeout, OldStateData,
                         ?DEFAULT_HTTP_CONNECT_TIMEOUT),
           download_start_timeout =
                maps:get(download_start_timeout, OldStateData,
                         ?DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT),
           idle_download_timeout =
                maps:get(idle_download_timeout, OldStateData,
                         ?DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT),
           pre_readiness_update_period =
                maps:get(pre_readiness_update_period, OldStateData,
                         ?DEFAULT_PRE_READINESS_UPDATE_PERIOD),
           post_readiness_update_period =
                maps:get(post_readiness_update_period, OldStateData,
                         ?DEFAULT_POST_READINESS_UPDATE_PERIOD),
           no_cache =
                maps:get(no_cache, OldStateData, false),
           insecure =
                maps:get(insecure, OldStateData, false),

           request_id =
                maps:get(request_id, OldStateData, undefined),
           last_response_headers =
                maps:get(last_response_headers, OldStateData, undefined),
           last_response_body =
                maps:get(last_response_body, OldStateData, undefined),
           last_modified =
                maps:get(last_modified, OldStateData, undefined),
           last_version =
                maps:get(last_version, OldStateData, undefined)
          },
    {ok, State, UpdatedStateData}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec server_name(atom()) -> atom().
server_name(Id) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Id)).

-spec init(atom(), string(), [opt()])
        -> ?gen_statem:init_result(initializing) |
           {stop, {invalid_opt, term()}}.
init(Id, URL, Opts) ->
    BaseStateData =
        #state_data{
           id = Id,
           url = URL,
           waiters = [],
           event_subscribers = [],
           connect_timeout = ?DEFAULT_HTTP_CONNECT_TIMEOUT,
           download_start_timeout = ?DEFAULT_HTTP_DOWNLOAD_START_TIMEOUT,
           idle_download_timeout = ?DEFAULT_HTTP_IDLE_DOWNLOAD_TIMEOUT,
           pre_readiness_update_period = ?DEFAULT_PRE_READINESS_UPDATE_PERIOD,
           post_readiness_update_period = ?DEFAULT_POST_READINESS_UPDATE_PERIOD,
           no_cache = false,
           insecure = false
          },
    init_opts(Opts, BaseStateData).

init_opts([{event_subscriber, Module} | Opts], StateData) when is_atom(Module), Module =/= undefined ->
    #state_data{ event_subscribers = Subscribers } = StateData,
    UpdatedSubscribers = [Module | Subscribers],
    UpdatedStateData = StateData#state_data{ event_subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedStateData);
init_opts([{event_subscriber, Pid} | Opts], StateData) when is_pid(Pid) ->
    _ = monitor(process, Pid),
    #state_data{ event_subscribers = Subscribers } = StateData,
    UpdatedSubscribers = [Pid | Subscribers],
    UpdatedStateData = StateData#state_data{ event_subscribers = UpdatedSubscribers },
    init_opts(Opts, UpdatedStateData);
init_opts([{connect_timeout, Timeout} | Opts], StateData) when ?is_timeout(Timeout) ->
    NewStateData = StateData#state_data{ connect_timeout = Timeout },
    init_opts(Opts, NewStateData);
init_opts([{download_start_timeout, Timeout} | Opts], StateData) when ?is_timeout(Timeout) ->
    NewStateData = StateData#state_data{ download_start_timeout = Timeout },
    init_opts(Opts, NewStateData);
init_opts([{idle_download_timeout, Timeout} | Opts], StateData) when ?is_timeout(Timeout) ->
    NewStateData = StateData#state_data{ idle_download_timeout = Timeout },
    init_opts(Opts, NewStateData);
init_opts([{pre_readiness_update_period, Interval} | Opts], StateData) when ?is_pos_integer(Interval) ->
    NewStateData = StateData#state_data{ pre_readiness_update_period = Interval },
    init_opts(Opts, NewStateData);
init_opts([{post_readiness_update_period, Interval} | Opts], StateData) when ?is_pos_integer(Interval) ->
    NewStateData = StateData#state_data{ post_readiness_update_period = Interval },
    init_opts(Opts, NewStateData);
init_opts([no_cache | Opts], StateData) ->
    NewStateData = StateData#state_data{ no_cache = true },
    init_opts(Opts, NewStateData);
init_opts([insecure | Opts], StateData) ->
    NewStateData = StateData#state_data{ insecure = true },
    init_opts(Opts, NewStateData);
init_opts([{internal, {async_waiter, {Pid,Ref}=From}} | Opts], StateData) when is_pid(Pid), is_reference(Ref) ->
    {NewStateData, []} = enqueue_waiter(From, StateData),
    init_opts(Opts, NewStateData);
init_opts([InvalidOpt | _], _StateData) ->
    {stop, {invalid_opt, InvalidOpt}};
init_opts([], StateData) ->
    self() ! maybe_load_from_cache,
    {ok, initializing, StateData}.

-spec cached_tarball_name(state_data()) -> nonempty_string().
cached_tarball_name(StateData) ->
    #state_data{url = URL} = StateData,
    cached_tarball_name_for_url(URL).

-spec cached_tarball_name_for_url(string()) -> nonempty_string().
%% @private
cached_tarball_name_for_url(URL) ->
    Hash = crypto:hash(sha256, URL),
    HexHash = bin_to_hex_str(Hash),
    Filename = HexHash ++ ".tgz",
    UserCachePath = ?filename_basedir(user_cache, "locus_erlang"),
    filename:join(UserCachePath, Filename).

-spec handle_cached_tarball_lookup(LookupResult, nonempty_string(), state_data())
        -> {state_data(), [?gen_statem:reply_action()]}
             when LookupResult :: ({ok, binary(), calendar:datetime()} |
                                   {error, term()}).
handle_cached_tarball_lookup({ok, Content, ModificationDate}, CachedTarballName, StateData) ->
    #state_data{ id = Id } = StateData,
    Source = {cache, CachedTarballName},
    case locus_util:load_database_from_tarball(Id, Content, Source) of
        {ok, Version} ->
            report_event({load_attempt_finished, Source, {ok, Version}}, StateData),
            {State2, Replies} = reply_to_waiters({ok, Version}, StateData),
            {State2#state_data{ last_modified = ModificationDate,
                           last_version = Version },
             Replies};
        {error, Error} ->
            report_event({load_attempt_finished, Source, {error, Error}}, StateData),
            {StateData, []}
    end;
handle_cached_tarball_lookup({error, Error}, CachedTarballName, StateData) ->
    Source = {cache, CachedTarballName},
    report_event({load_attempt_finished, Source, {error, Error}}, StateData),
    {StateData, []}.

-spec maybe_try_saving_cached_tarball(binary(), calendar:datetime(), state_data()) -> ok.
maybe_try_saving_cached_tarball(_Tarball, _LastModified, #state_data{no_cache = true}) ->
    ok;
maybe_try_saving_cached_tarball(Tarball, LastModified, StateData) ->
    case save_cached_tarball(Tarball, LastModified, StateData) of
        {ok, Filename} ->
            report_event({cache_attempt_finished, Filename, ok}, StateData);
        {{error, Error}, Filename} ->
            report_event({cache_attempt_finished, Filename, {error, Error}}, StateData)
    end.

-spec save_cached_tarball(binary(), calendar:datetime(), state_data())
        -> {Status, Filename} when Status :: ok | {error, Exception},
                                   Filename :: nonempty_string(),
                                   Exception :: {exception, Class, Reason, Stacktrace},
                                   Class :: atom(),
                                   Reason :: term(),
                                   Stacktrace :: [term()].
save_cached_tarball(Tarball, LastModified, StateData) ->
    Filename = cached_tarball_name(StateData),
    TmpSuffix = ".tmp." ++ integer_to_list(locus_rand_compat:uniform(1 bsl 32), 36),
    TmpFilename = Filename ++ TmpSuffix,
    FileInfoMod = #file_info{ mtime = LastModified },
    try
        ok = filelib:ensure_dir(Filename),
        {ok, IoDevice} = file:open(TmpFilename, [write, exclusive, raw]),
        ok = file:write(IoDevice, Tarball),
        ok = file:close(IoDevice),
        ok = file:write_file_info(TmpFilename, FileInfoMod, [{time,universal}]),
        ok = file:rename(TmpFilename, Filename),
        {ok, Filename}
    catch
        Class:Reason ->
            Stacktrace = erlang:get_stacktrace(),
            {{error, {exception, Class, Reason, Stacktrace}},
             Filename}
    end.

-spec bin_to_hex_str(binary()) -> [48..57 | 97..102].
bin_to_hex_str(Bin) ->
    bin_to_hex_str_recur(Bin, []).

-spec bin_to_hex_str_recur(bitstring(), [48..57 | 97..102]) -> [48..57 | 97..102].
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) when Nibble < 10 ->
    bin_to_hex_str_recur(Rest, [$0 + Nibble | Acc]);
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) ->
    bin_to_hex_str_recur(Rest, [$a + Nibble | Acc]);
bin_to_hex_str_recur(<<>>, Acc) ->
    lists:reverse(Acc).

-spec update_period(state_data()) -> pos_integer().
update_period(#state_data{last_modified = undefined} = StateData) ->
    StateData#state_data.pre_readiness_update_period;
update_period(StateData) ->
    StateData#state_data.post_readiness_update_period.

request_headers(#state_data{last_modified = undefined}) ->
    base_request_headers();
request_headers(#state_data{last_modified = LastModified}) ->
    LocalLastModified = calendar:universal_time_to_local_time(LastModified),
    [{"if-modified-since", httpd_util:rfc1123_date(LocalLastModified)}
     | base_request_headers()].

base_request_headers() ->
    [{"accept", join_header_values(
                  ["application/gzip",
                   "application/x-gzip",
                   "application/x-gtar",
                   "application/x-tgz"])},
     {"content-encoding", "identity"},
     {"connection", "close"}
    ].

join_header_values(Values) ->
    string:join(Values, "; ").

-spec extract_last_modified_datetime_from_response_headers(headers()) -> calendar:datetime().
extract_last_modified_datetime_from_response_headers(Headers) ->
    CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
    case lists:keyfind("last-modified", 1, CiHeaders) of
        {"last-modified", LastModified} ->
            ({_,_} = ModificationDate) = httpd_util:convert_request_date(LastModified),
            ModificationDate;
        false ->
            {{1970,1,1}, {0,0,0}}
    end.

-spec clear_inbox_of_late_http_messages(reference()) -> ok.
clear_inbox_of_late_http_messages(RequestId) ->
    % Only a best effort. It's possible that a delayed message could
    % arrive after the timeout.
    receive
        {http, {RequestId, stream_start, _Headers}} ->
            clear_inbox_of_late_http_messages(RequestId);
        {http, {RequestId, stream, _BinBodyPart}} ->
            clear_inbox_of_late_http_messages(RequestId);
        {http, {RequestId, stream_end, _Headers}} ->
            clear_inbox_of_late_http_messages(RequestId);
        {http, {RequestId, {error, _Reason}}} ->
            clear_inbox_of_late_http_messages(RequestId)
    after
        1000 -> ok
    end.

-spec maybe_enqueue_waiter(from(), state_data())
        -> {state_data(), [?gen_statem:reply_action()]}.
maybe_enqueue_waiter(From, #state_data{last_version = undefined} = StateData) ->
    enqueue_waiter(From, StateData);
maybe_enqueue_waiter(From, #state_data{last_version = LastVersion} = StateData) ->
    {StateData, [{reply, From, {ok, LastVersion}}]}.

enqueue_waiter(From, StateData) ->
    #state_data{waiters = Waiters} = StateData,
    UpdatedWaiters = [From | Waiters],
    UpdatedStateData = StateData#state_data{waiters = UpdatedWaiters},
    {UpdatedStateData, []}.

-spec reply_to_waiters({ok, calendar:datetime()} | {error, term()}, state_data())
        -> {state_data(), [?gen_statem:reply_action()]}.
reply_to_waiters(Result, StateData) ->
    #state_data{waiters = Waiters} = StateData,
    Replies = [{reply, From, Result} || From <- Waiters],
    {StateData#state_data{ waiters = [] }, Replies}.

-spec report_event(event(), state_data()) -> ok.
report_event(Event, #state_data{id = Id, event_subscribers = Subscribers}) ->
    lists:foreach(
      fun (Module) when is_atom(Module) ->
              Module:report(Id, Event);
          (Pid) ->
              erlang:send(Pid, {locus, Id, Event}, [noconnect])
      end,
      Subscribers).

handle_monitored_process_death(Pid, StateData) ->
    #state_data{event_subscribers = Subscribers} = StateData,
    {ok, UpdatedSubscribers} = locus_util:lists_take(Pid, Subscribers),
    UpdatedStateData = StateData#state_data{event_subscribers = UpdatedSubscribers},
    {keep_state, UpdatedStateData}.
