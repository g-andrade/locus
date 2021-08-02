%% Copyright (c) 2021 Guilherme Andrade
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

-module(locus_custom_fetcher).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% Callback Declarations
%% ------------------------------------------------------------------

-callback description(Args) ->  description()
        when Args :: term().

-callback fetch(Args)
    ->   {fetched, Success}
       | {error, Reason}
        when Args :: term(),
             Success :: success(),
             Reason :: term().

-callback conditionally_fetch(Args, {depending_on, PreviousFetchMetadata})
    ->   {fetched, Success}
       | dismissed
       | {error, Reason}
        when Args :: term(),
             PreviousFetchMetadata :: successful_fetch_metadata(),
             Success :: success(),
             Reason :: term().

-ignore_xref(behaviour_info/1).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [source/2,
    description/2,
    start_link/4
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Callback Record and Type Definitions
%% ------------------------------------------------------------------

-type description()
    :: #{ database_is_stored_remotely := boolean(),
          database_is_fetched_from := term()
        }.
-export_type([description/0]).

-type success()
    :: #{ format := locus_loader:blob_format(),
          content := binary(),
          metadata := successful_fetch_metadata()
        }.
-export_type([success/0]).

-type successful_fetch_metadata()
    :: #{ fetched_from := term(),
          modified_on := calendar:datetime() | unknown
        }.
-export_type([successful_fetch_metadata/0]).

%% ------------------------------------------------------------------
%% "Internal" Record and Type Definitions
%% ------------------------------------------------------------------

-type msg() ::
    {event, event()} |
    {finished, {success, success()}} |
    {finished, dismissed} |
    {finished, {error, term()}}.
-export_type([msg/0]).

-type event() ::
    event_load_attempt_started() |
    event_load_attempt_dismissed().
-export_type([event/0]).

-type event_load_attempt_started() :: {load_attempt_started, source()}.
-export_type([event_load_attempt_started/0]).

-type event_load_attempt_dismissed() :: {load_attempt_dismissed, source()}.
-export_type([event_load_attempt_dismissed/0]).

-type source() :: {local|remote, {custom, term()}}.
-export_type([source/0]).

-record(state, {
          owner_pid :: pid(),
          source :: source(),
          module :: module(),
          args :: term(),
          previous_fetch_metadata :: successful_fetch_metadata() | undefined
         }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec source(module(), term()) -> source().
%% @private
source(Module, Args) ->
    Description = description(Module, Args),
    #{database_is_stored_remotely := IsRemote,
      database_is_fetched_from := FetchedFrom
     } = Description,

    SourceType = source_type(IsRemote),
    {SourceType, {custom, FetchedFrom}}.

-spec description(module(), term()) -> description().
%% @private
description(Module, Args) ->
    case Module:description(Args) of
        #{database_is_stored_remotely := IsRemote,
          database_is_fetched_from := _
         } = Description
          when is_boolean(IsRemote) ->
            Description
    end.

-spec start_link(source(), module(), term(), successful_fetch_metadata() | undefined)
        -> {ok, pid()}.
%% @private
start_link(Source, Module, Args, PreviousFetchMetadata) ->
    gen_server:start_link(?MODULE, [self(), Source, Module, Args, PreviousFetchMetadata], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...]) -> {ok, state(), {continue, fetch}}
        when InitArg :: OwnerPid | Source | Module | Args | PreviousFetchMetadata,
             OwnerPid :: pid(),
             Source :: source(),
             Module :: module(),
             Args :: term(),
             PreviousFetchMetadata :: successful_fetch_metadata() | undefined.
%% @private
init([OwnerPid, Source, Module, Args, PreviousFetchMetadata]) ->
    State = #state{
               owner_pid = OwnerPid,
               source = Source,
               module = Module,
               args = Args,
               previous_fetch_metadata = PreviousFetchMetadata
              },
    {ok, State, {continue, fetch}}.

-spec handle_continue(fetch, state()) -> {stop, normal, state()}.
%% @private
handle_continue(fetch, State) ->
    #state{source = Source} = State,
    report_event({load_attempt_started, Source}, State),

    case State#state.previous_fetch_metadata of
        undefined ->
            handle_unconditional_fetch(State);
        #{modified_on := unknown} ->
            handle_unconditional_fetch(State);
        #{modified_on := _} = PreviousFetchMetadata ->
            handle_conditional_fetch(PreviousFetchMetadata, State)
    end.

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
        -> {stop, unexpected_info, state()}.
%% @private
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

source_type(true = _IsRemote) ->
    remote;
source_type(false = _IsRemote) ->
    local.

handle_unconditional_fetch(State) ->
    #state{module = Module, args = Args} = State,

    case Module:fetch(Args) of
        {fetched, Success} ->
            handle_success(Success, State);
        {error, Reason} ->
            handle_error(Reason, State)
    end.

handle_conditional_fetch(PreviousFetchMetadata, State) ->
    #state{module = Module, args = Args} = State,

    case Module:conditionally_fetch(Args, {depending_on, PreviousFetchMetadata}) of
        {fetched, Success} ->
            handle_success(Success, State);
        dismissed ->
            handle_dismissal(State);
        {error, Reason} ->
            handle_error(Reason, State)
    end.

-spec handle_success(term(), state()) -> {stop, normal, state()}.
handle_success(Success, State) ->
    case validate_success(Success) of
        ok ->
            notify_owner({finished, {success, Success}}, State),
            {stop, normal, State};
        {error, Reason} ->
            WrappedReason = {invalid_success_return_value, #{value => Success, why => Reason}},
            notify_owner({finished, {error, WrappedReason}}, State),
            {stop, normal, State}
    end.

validate_success(#{format := Format, content := Content, metadata := Metadata}) ->
    try {validate_format(Format), validate_content(Content), validate_metadata(Metadata)} of
        {ok, ok, ok} ->
            ok
    catch
        throw:Reason ->
            {error, Reason}
    end;
validate_success(#{} = BadSuccess) ->
    MissingKeys = [format, content, metadata] -- maps:keys(BadSuccess),
    {error, {missing_map_keys, MissingKeys}};
validate_success(BadSuccess) ->
    {error, {not_a_map, BadSuccess}}.

validate_format(Format) ->
    ValidFormats = locus_loader:valid_blob_formats(),
    case lists:member(Format, ValidFormats) of
        true ->
            ok;
        false ->
            throw({format, Format, 'not one of', ValidFormats})
    end.

validate_content(Content) ->
    case is_binary(Content) of
        true ->
            ok;
        false ->
            throw({'content not a binary', Content})
    end.

validate_metadata(#{fetched_from := _FetchedFrom, modified_on := ModifiedOn}) ->
    validate_modified_on(ModifiedOn);
validate_metadata(#{} = BadMetadata) ->
    MissingKeys = [fetched_from, modified_on] -- maps:keys(BadMetadata),
    {error, {missing_metadata_keys, MissingKeys}};
validate_metadata(BadMetadata) ->
    {error, {metadata_not_a_map, BadMetadata}}.

validate_modified_on(unknown) ->
    ok;
validate_modified_on(DateTime) ->
    try calendar:datetime_to_gregorian_seconds(DateTime) of
        _GregorianSeconds ->
            ok
    catch
        Class:Reason when Class =/= error, Reason =/= undef ->
            throw({'`modified_on` neither `unknown` nor a valid calendar:datetime()', DateTime})
    end.

-spec handle_dismissal(state()) -> {stop, normal, state()}.
handle_dismissal(State) ->
    #state{source = Source} = State,
    report_event({load_attempt_dismissed, Source}, State),
    notify_owner({finished, dismissed}, State),
    {stop, normal, State}.

-spec handle_error(term(), state()) -> {stop, normal, state()}.
handle_error(Reason, State) ->
    notify_owner({finished, {error, Reason}}, State),
    {stop, normal, State}.

-spec report_event(event(), state()) -> ok.
report_event(Event, State) ->
    notify_owner({event, Event}, State).

-spec notify_owner(msg(), state()) -> ok.
notify_owner(Msg, State) ->
    #state{owner_pid = OwnerPid} = State,
    _ = erlang:send(OwnerPid, {self(), Msg}, [noconnect]),
    ok.
