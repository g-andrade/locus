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

-module(locus_http_download).
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

-define(DEFAULT_CONNECT_TIMEOUT, (timer:seconds(8))).
-define(DEFAULT_DOWNLOAD_START_TIMEOUT, (timer:seconds(5))).
-define(DEFAULT_IDLE_DOWNLOAD_TIMEOUT, (timer:seconds(5))).

-define(is_timeout(V), ((is_integer((V)) andalso ((V) >= 0)) orelse ((V) =:= infinity))).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type opt() ::
    {connect_timeout, timeout()} |
    {download_start_timeout, timeout()} |
    {idle_download_timeout, timeout()} |
    insecure |
    {insecure, boolean()}.
-export_type([opt/0]).

-type msg() ::
    {event, event()} |
    {finished, {success, success()}} |
    {finished, dismissed} |
    {finished, {error, term()}}.
-export_type([msg/0]).

-type event() ::
    event_request_sent() |
    event_download_dismissed() |
    event_download_failed_to_start() |
    event_download_started() |
    event_download_finished().
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

-type success() ::
    #{ headers := headers(),
       body := binary()
     }.
-export_type([success/0]).

-type url() :: string().
-export_type([url/0]).

-type response_status() :: {100..999, binary()}.
-export_type([response_status/0]).

% case insensitive
-type headers() :: [{string(), string()}].
-export_type([headers/0]).

-type body() :: binary().
-export_type([body/0]).

-record(state, {
          owner_pid :: pid(),
          url :: url(),
          headers :: headers(),
          opts :: [opt()],
          timeouts :: #{ term() => infinity | reference() },
          request_id :: reference() | undefined,
          response_headers :: headers() | undefined,
          response_body :: iodata() | undefined
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
          fun ({connect_timeout, Value} = Opt) ->
                  ?is_timeout(Value) orelse error({badopt,Opt});
              ({download_start_timeout, Value} = Opt) ->
                  ?is_timeout(Value) orelse error({badopt,Opt});
              ({idle_download_timeout, Value} = Opt) ->
                  ?is_timeout(Value) orelse error({badopt,Opt});
              (insecure) ->
                  true;
              ({insecure,Insecure} = Opt) ->
                  is_boolean(Insecure) orelse error({badopt,Opt});
              (_) ->
                  false
          end,
          MixedOpts)
    of
        {MyOpts, OtherOpts} ->
            {ok, {MyOpts, OtherOpts}}
    catch
        error:{badopt,BadOpt} ->
            {error, BadOpt}
    end.

-spec start_link(url(), headers(), [opt()]) -> {ok, pid()}.
%% @private
start_link(URL, Headers, Opts) ->
    gen_server:start_link(?MODULE, [self(), URL, Headers, Opts], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...]) -> {ok, state()}
        when InitArg :: OwnerPid | URL | Headers | Opts,
             OwnerPid :: pid(),
             URL :: url(),
             Headers :: headers(),
             Opts :: [opt()].
%% @private
init([OwnerPid, URL, Headers, Opts]) ->
    _ = process_flag(trap_exit, true),
    self() ! send_request,
    CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
    {ok, #state{
            owner_pid = OwnerPid,
            url = URL,
            headers = CiHeaders,
            opts = Opts,
            timeouts = #{}
           }}.

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
           {stop, normal, state()} |
           {stop, unexpected_info, state()}.
%% @private
handle_info(send_request, State) ->
    UpdatedState = send_request(State),
    {noreply, UpdatedState};
handle_info({http, Msg}, State)
  when element(1, Msg) =:= State#state.request_id ->
    handle_httpc_message(Msg, State);
handle_info({timeout,OptName}, State) ->
    #state{timeouts = Timeouts} = State,
    #{OptName := _} = Timeouts,
    UpdatedTimeouts = maps:remove(OptName, Timeouts),
    UpdatedState = State#state{ timeouts = UpdatedTimeouts },
    handle_timeout(OptName, UpdatedState);
handle_info({'EXIT', Pid, _}, State) ->
    handle_linked_process_death(Pid, State);
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

-spec send_request(state()) -> state().
send_request(State)
  when State#state.request_id =:= undefined ->
    #state{url = URL, headers = Headers, opts = Opts} = State,
    ConnectTimeout = proplists:get_value(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    Insecure = proplists:get_value(insecure, Opts, false),

    Request = {URL, Headers},
    BaseHTTPOpts = [{connect_timeout, ConnectTimeout}],
    ExtraHTTPOpts =
        case Insecure orelse http_uri:parse(URL) of
            true ->
                [];
            {ok, ParsedURL} when element(1, ParsedURL) =:= http ->
                [];
            {ok, ParsedURL} when element(1, ParsedURL) =:= https ->
                [{ssl,locus_https_requests:ssl_opts_for_ca_authentication(URL)}]
        end,
    HTTPOpts = BaseHTTPOpts  ++ ExtraHTTPOpts,

    RequestOpts = [{sync, false}, {stream, self}],
    {ok, RequestId} = httpc:request(get, Request, HTTPOpts, RequestOpts),
    true = is_reference(RequestId),

    report_event({request_sent, URL, Headers}, State),

    State2 = State#state{ request_id = RequestId },
    _State3 = schedule_download_start_timeout(State2).

-spec handle_httpc_message(tuple(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_httpc_message(Msg, State)
  when State#state.response_headers =:= undefined ->
    case Msg of
        {_, stream_start, Headers} ->
            CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
            State2 = cancel_download_start_timeout(State),
            State3 = schedule_idle_download_timeout(State2),
            State4 = State3#state{ response_headers = CiHeaders, response_body = <<>> },
            report_event({download_started, CiHeaders}, State4),
            {noreply, State4};
        {_, {{_,StatusCode,StatusDesc}, Headers, Body}} when StatusCode =:= 304 ->
            CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
            report_event({download_dismissed, {http, {StatusCode, StatusDesc}, CiHeaders, Body}}, State),
            notify_owner({finished, dismissed}, State),
            {stop, normal, State};
        {_, {{_,StatusCode,StatusDesc}, Headers, Body}} ->
            CiHeaders = lists:keymap(fun string:to_lower/1, 1, Headers),
            report_event({download_failed_to_start, {http, {StatusCode, StatusDesc}, CiHeaders, Body}}, State),
            notify_owner({finished, {error, {http,StatusCode,StatusDesc}}}, State),
            {stop, normal, State};
        {_, {error, Reason}} ->
            report_event({download_failed_to_start, {error, Reason}}, State),
            notify_owner({finished, {error, {http,Reason}}}, State),
            {stop, normal, State}
    end;
handle_httpc_message(Msg, State)
  when State#state.response_body =/= undefined ->
    case Msg of
        {_, stream, BodyPart} ->
            #state{response_body = BodyAcc} = State,
            UpdatedBodyAcc = [BodyAcc, BodyPart],
            State2 = State#state{ response_body = UpdatedBodyAcc },
            State3 = reschedule_idle_download_timeout(State2),
            {noreply, State3};
        {_, stream_end, TrailingHeaders} -> % no chunked encoding
            #state{response_headers = HeadersAcc, response_body = BodyAcc} = State,
            CiTrailingHeaders = lists:keymap(fun string:to_lower/1, 1, TrailingHeaders),
            Headers = lists:usort(HeadersAcc ++ CiTrailingHeaders),
            Body = iolist_to_binary(BodyAcc),
            BodySize = byte_size(Body),
            report_event({download_finished, BodySize, {ok,CiTrailingHeaders}}, State),
            handle_successful_download_conclusion(Headers, Body, State);
        {_, {error, Reason}} ->
            #state{response_body = BodyAcc} = State,
            BodySizeSoFar = iolist_size(BodyAcc),
            report_event({download_finished, BodySizeSoFar, {error, Reason}}, State),
            notify_owner({finished, {error, {http,Reason}}}, State),
            {stop, normal, State}
    end.

handle_successful_download_conclusion(Headers, Body, State) ->
    ActualContentLength = integer_to_list( byte_size(Body) ),

    case lists:keyfind("content-length", 1, Headers) of
        {_, DeclaredContentLength} when DeclaredContentLength =/= ActualContentLength ->
            ErrorReason = {body_size_mismatch, #{declared_content_length => DeclaredContentLength,
                                                 actual_content_length => ActualContentLength}},
            notify_owner({finished, {error, ErrorReason}}, State),
            {stop, normal, State};
        _ ->
            Success = #{headers => Headers, body => Body},
            notify_owner({finished, {success, Success}}, State),
            {stop, normal, State}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Timeouts
%% ------------------------------------------------------------------

-spec schedule_download_start_timeout(state()) -> state().
schedule_download_start_timeout(State) ->
    schedule_timeout(download_start_timeout, ?DEFAULT_DOWNLOAD_START_TIMEOUT, State).

-spec cancel_download_start_timeout(state()) -> state().
cancel_download_start_timeout(State) ->
    cancel_timeout(download_start_timeout, State).

-spec schedule_idle_download_timeout(state()) -> state().
schedule_idle_download_timeout(State) ->
    schedule_timeout(idle_download_timeout, ?DEFAULT_IDLE_DOWNLOAD_TIMEOUT, State).

-spec reschedule_idle_download_timeout(state()) -> state().
reschedule_idle_download_timeout(State) ->
    reschedule_timeout(idle_download_timeout, ?DEFAULT_IDLE_DOWNLOAD_TIMEOUT, State).

schedule_timeout(OptName, DefaultValue, State) ->
    #state{opts = Opts, timeouts = Timeouts} = State,
    false = maps:is_key(OptName, Timeouts),
    case proplists:get_value(OptName, Opts, DefaultValue) of
        infinity ->
            UpdatedTimeouts = Timeouts#{ OptName => infinity },
            State#state{ timeouts = UpdatedTimeouts };
        Interval ->
            TimeoutMsg = {timeout,OptName},
            Timer = erlang:send_after(Interval, self(), TimeoutMsg),
            UpdatedTimeouts = Timeouts#{ OptName => Timer },
            State#state{ timeouts = UpdatedTimeouts }
    end.

cancel_timeout(OptName, State) ->
    #state{timeouts = Timeouts} = State,
    #{OptName := MaybeTimer} = Timeouts,
    UpdatedTimeouts = maps:remove(OptName, Timeouts),
    case MaybeTimer of
        infinity ->
            State#state{ timeouts = UpdatedTimeouts };
        Timer ->
            TimeoutMsg = {timeout,OptName},
            true = cancel_or_flush_timer(Timer, TimeoutMsg),
            State#state{ timeouts = UpdatedTimeouts }
    end.

cancel_or_flush_timer(Timer, TimeoutMsg) ->
    is_integer( erlang:cancel_timer(Timer) )
    orelse receive
               TimeoutMsg -> true
           after
               0 -> false
           end.

reschedule_timeout(OptName, DefaultValue, State) ->
    State2 = cancel_timeout(OptName, State),
    schedule_timeout(OptName, DefaultValue, State2).

handle_timeout(download_start_timeout, State) ->
    ok = httpc:cancel_request(State#state.request_id),
    report_event({download_failed_to_start, timeout}, State),
    notify_owner({finished, {error, {timeout, waiting_stream_start}}}, State),
    {stop, normal, State};
handle_timeout(idle_download_timeout, State) ->
    ok = httpc:cancel_request(State#state.request_id),
    #state{response_body = BodyAcc} = State,
    BodySizeSoFar = iolist_size(BodyAcc),
    report_event({download_finished, BodySizeSoFar, {error, timeout}}, State),
    notify_owner({finished, {error, {timeout, waiting_stream_end}}}, State),
    {stop, normal, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Events
%% ------------------------------------------------------------------

-spec report_event(event(), state()) -> ok.
report_event(Event, State) ->
    notify_owner({event,Event}, State).

-spec notify_owner(msg(), state()) -> ok.
notify_owner(Msg, State) ->
    #state{owner_pid = OwnerPid} = State,
    _ = erlang:send(OwnerPid, {self(),Msg}, [noconnect]),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Death
%% ------------------------------------------------------------------

-spec handle_linked_process_death(pid(), state()) -> {stop, normal, state()}.
handle_linked_process_death(Pid, State)
  when Pid =:= State#state.owner_pid ->
    {stop, normal, State}.
