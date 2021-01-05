%% Copyright (c) 2017-2021 Guilherme Andrade
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

-module(locus_filesystem_load).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/2
   ]).

-ignore_xref(
   [start_link/2
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

-type msg() ::
    {event, event()} |
    {finished, {success,success()}} |
    {finished, dismissed} |
    {finished, {error,not_found}} |
    {finished, {error,term()}}.
-export_type([msg/0]).

-type event() ::
    event_load_attempt_started() |
    event_load_attempt_dismissed().
-export_type([event/0]).

-type event_load_attempt_started() :: {load_attempt_started, source()}.
-export_type([event_load_attempt_started/0]).

-type event_load_attempt_dismissed() :: {load_attempt_dismissed, source()}.
-export_type([event_load_attempt_dismissed/0]).

-type source() :: {cache|filesystem, path()}.
-export_type([source/0]).

-type success() ::
    #{ modified_on := calendar:datetime(),
       content := binary()
     }.
-export_type([success/0]).

-type path() :: nonempty_string().
-export_type([path/0]).

-record(state, {
          owner_pid :: pid(),
          source :: source(),
          previously_modified_on :: calendar:datetime() | undefined
         }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(source(), calendar:datetime() | undefined) -> {ok, pid()}.
%% @private
start_link(Source, PrevModificationDT) ->
    gen_server:start_link(?MODULE, [self(), Source, PrevModificationDT], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...]) -> {ok, state()}
        when InitArg :: OwnerPid | Source | PrevModificationDT,
             OwnerPid :: pid(),
             Source :: source(),
             PrevModificationDT :: calendar:datetime() | undefined.
%% @private
init([OwnerPid, Source, PrevModificationDT]) ->
    _ = process_flag(trap_exit, true),
    self() ! read,
    {ok, #state{
            owner_pid = OwnerPid,
            source = Source,
            previously_modified_on = PrevModificationDT
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
        -> {stop, normal, state()} |
           {stop, unexpected_info, state()}.
%% @private
handle_info(read, State) ->
    handle_read(State);
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

-spec handle_read(state()) -> {stop, normal, state()}.
handle_read(State) ->
    #state{source = Source, previously_modified_on = PrevModificationDT} = State,
    {_, Path} = Source,
    report_event({load_attempt_started, Source}, State),

    case file:read_file_info(Path, [{time,universal}]) of
        {ok, #file_info{ mtime = ModificationDT }}
          when ModificationDT =:= PrevModificationDT ->
            report_event({load_attempt_dismissed, Source}, State),
            notify_owner({finished, dismissed}, State),
            {stop, normal, State};
        {ok, #file_info{ mtime = ModificationDT }} ->
            handle_new_read(Path, ModificationDT, State);
        {error, enoent} ->
            notify_owner({finished, {error, not_found}}, State),
            {stop, normal, State};
        {error, Reason} ->
            notify_owner({finished, {error, {read_file_info,Reason}}}, State),
            {stop, normal, State}
    end.

-spec handle_new_read(path(), calendar:datetime(), state()) -> {stop, normal, state()}.
handle_new_read(Path, ModificationDT, State) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Success =
                #{ modified_on => ModificationDT,
                   content => Content
                 },
            notify_owner({finished, {success,Success}}, State),
            {stop, normal, State};
        {error, enoent} ->
            notify_owner({finished, {error, not_found}}, State),
            {stop, normal, State};
        {error, Reason} ->
            notify_owner({finished, {error, {read_file,Reason}}}, State),
            {stop, normal, State}
    end.

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
