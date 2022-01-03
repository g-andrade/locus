%% Copyright (c) 2021-2022 Guilherme Andrade
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

-module(locus_test_custom_fetcher).
-behaviour(locus_custom_fetcher).
-behaviour(gen_server).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start/3,
    set_modification_datetime/2,
    get_database_is_fetched_from/1,
    stop/1
   ]).

%% ------------------------------------------------------------------
%% locus_custom_fetcher Function Exports
%% ------------------------------------------------------------------

-export(
   [description/1,
    fetch/1,
    conditionally_fetch/2
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

-type args() :: #{locality := local | remote,
                  pid := pid()}.

-record(state, {
          path :: nonempty_string(),
          database :: database() | undefined
         }).

-type state() :: #state{}.

-record(database, {
          format :: locus_loader:blob_format(),
          content :: binary(),
          modified_on :: calendar:datetime() | undefined
         }).

-type database() :: #database{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(nonempty_string(), nonempty_string(), calendar:datetime()) -> {ok, pid()}.
start(BaseFilename, FileExtension, ModifiedOn) ->
    gen_server:start(?MODULE, [BaseFilename, FileExtension, ModifiedOn], []).

-spec set_modification_datetime(pid(), calendar:datetime()) -> ok.
set_modification_datetime(Pid, ModifiedOn) ->
    ?assertMatch({{_, _, _}, {_, _, _}}, ModifiedOn),
    gen_server:call(Pid, {set_modification_datetime, ModifiedOn}).

-spec get_database_is_fetched_from(pid()) -> nonempty_string().
get_database_is_fetched_from(Pid) ->
    gen_server:call(Pid, get_path).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% ------------------------------------------------------------------
%% locus_custom_fetcher Function Definitions
%% ------------------------------------------------------------------

-spec description(Args) -> Description
        when Args :: args(),
             Description :: locus_custom_fetcher:description().
description(Args) ->
    #{locality := Locality, pid := Pid} = Args,
    FetchedFrom = gen_server:call(Pid, get_path),

    #{database_is_stored_remotely => (Locality =:= remote),
      database_is_fetched_from => FetchedFrom}.

-spec fetch(Args) -> {fetched, Success} | {error, Reason}
        when Args :: args(),
             Success :: locus_custom_fetcher:success(),
             Reason :: term().
fetch(Args) ->
    #{pid := Pid} = Args,
    gen_server:call(Pid, fetch).

-spec conditionally_fetch(Args, {depending_on, PreviousFetchMetadata})
    -> {fetched, Success}
       | dismissed
       | {error, Reason}
        when Args :: args(),
             PreviousFetchMetadata :: locus_custom_fetcher:successful_fetch_metadata(),
             Success :: locus_custom_fetcher:success(),
             Reason :: term().
conditionally_fetch(Args, {depending_on, PreviousFetchMetadata}) ->
    #{pid := Pid} = Args,
    gen_server:call(Pid, {conditionally_fetch, PreviousFetchMetadata}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([nonempty_string() | calendar:datetime()]) -> {ok, state()}.
init([BaseFilename, FileExtension, ModifiedOn]) ->
    PathWithTestTarballs = locus_test_utils:path_with_test_tarballs(),
    DatabaseFilename = BaseFilename ++ "." ++ FileExtension,
    DatabasePath = filename:join(PathWithTestTarballs, DatabaseFilename),

    case file:read_file(DatabasePath) of
        {ok, DatabaseBlob} ->
            DatabaseFormat = database_format(FileExtension),
            Database = #database{format = DatabaseFormat,
                                 content = DatabaseBlob,
                                 modified_on = ModifiedOn},
            {ok, #state{path = DatabasePath,
                        database = Database}};

        {error, enoent} ->
            {ok, #state{path = DatabasePath}}
    end.

-spec handle_call(term(), {pid(), reference()}, state())
        -> {reply, ok, state()} |
           {reply, {loaded_from, nonempty_string()}, state()} |
           {reply, {fetched, locus_custom_fetcher:success()}, state()} |
           {reply, dismissed, state()} |
           {reply, {error, not_found}, state()} |
           {stop, {unexpected_call, #{request := _, from := {pid(), reference()}}}, state()}.
handle_call({set_modification_datetime, ModifiedOn}, _From, State) ->
    Database = State#state.database,
    UpdatedDatabase = Database#database{modified_on= ModifiedOn},
    UpdatedState = State#state{database = UpdatedDatabase},
    {reply, ok, UpdatedState};
handle_call(get_path, _From, State) ->
    Path = State#state.path,
    {reply, Path, State};
handle_call(fetch, _From, #state{database = Database} = State)
  when Database =/= undefined ->
    handle_fetch(_PreviouslyModifiedOn = unknown, Database, State);
handle_call(fetch, _From, State) ->
    Reply = {error, not_found},
    {reply, Reply, State};
handle_call({conditionally_fetch, PreviousFetchMetadata}, _From,
            #state{database = Database} = State)
  when Database =/= undefined ->
    ?assertEqual(State#state.path, maps:get(fetched_from, PreviousFetchMetadata)),
    PreviouslyModifiedOn = maps:get(modified_on, PreviousFetchMetadata),
    handle_fetch(PreviouslyModifiedOn, Database, State);
handle_call({conditionally_fetch, _PreviousFetchMetadata}, _From, State) ->
    Reply = {error, not_found},
    {reply, Reply, State};
handle_call(Request, From, State) ->
    ErrorDetails = #{request => Request, from => From},
    {stop, {unexpected_call, ErrorDetails}, State}.

-spec handle_cast(term(), state())
        -> {stop, {unexpected_cast, term()}, state()}.
handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

-spec handle_info(term(), state())
        -> {stop, {unexpected_info, term()}, state()}.
handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state() | term(), term())
        -> {ok, state()} | {error, {cannot_convert_state, term()}}.
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State};
code_change(_OldVsn, State, _Extra) ->
    {error, {cannot_convert_state, State}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

database_format(Extension) ->
    maps:get(Extension, #{"tar.gz" => tgz,
                          "tgz" => tgz,
                          "tar" => tarball,
                          "mmdb" => mmdb,
                          "mmdb.gz" => gzipped_mmdb}).

handle_fetch(PreviouslyModifiedOn, Database, State) ->
    case Database#database.modified_on of
        undefined ->
            NewMetadata = #{fetched_from => State#state.path, modified_on => unknown},
            Success = #{format => Database#database.format,
                        content => Database#database.content,
                        metadata => NewMetadata},
            Reply = {fetched, Success},
            {reply, Reply, State};
        ModifiedOn
          when ModifiedOn =/= PreviouslyModifiedOn ->
            NewMetadata = #{fetched_from => State#state.path, modified_on => ModifiedOn},
            Success = #{format => Database#database.format,
                        content => Database#database.content,
                        metadata => NewMetadata},
            Reply = {fetched, Success},
            {reply, Reply, State};
        ModifiedOn
          when ModifiedOn =:= PreviouslyModifiedOn ->
           Reply = dismissed,
           {reply, Reply, State}
    end.
