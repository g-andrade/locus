%% Copyright (c) 2017-2024 Guilherme Andrade
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

%% @doc Stores a file in the filesystem without blocking the caller
-module(locus_filesystem_store).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/3
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
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type msg() ::
    {finished, success} |
    {finished, {error, term()}}.
-export_type([msg/0]).

-type path() :: file:filename_all().
-export_type([path/0]).

-record(state, {
          owner_pid :: pid(),
          path :: path(),
          content :: iodata(),
          modified_on :: calendar:datetime()
         }).
-opaque state() :: #state{}.
-export_type([state/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(path(), iodata(), calendar:datetime()) -> {ok, pid()}.
%% @private
start_link(Path, Content, ModificationDT) ->
    gen_server:start_link(?MODULE, [self(), Path, Content, ModificationDT], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([InitArg, ...]) -> {ok, state()}
        when InitArg :: OwnerPid | Path | Content | ModificationDT,
             OwnerPid :: pid(),
             Path :: path(),
             Content :: iodata(),
             ModificationDT :: calendar:datetime().
%% @private
init([OwnerPid, Path, Content, ModificationDT]) ->
    _ = process_flag(trap_exit, true),
    self() ! write,
    {ok, #state{
            owner_pid = OwnerPid,
            path = Path,
            content = Content,
            modified_on = ModificationDT
           }}.

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
        -> {stop, normal, state()} |
           {stop, unexpected_info, state()}.
%% @private
handle_info(write, State) ->
    handle_write(State);
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

-spec handle_write(state()) -> {stop, normal, state()}.
handle_write(State) ->
    try do_write(State) of
        ok ->
            notify_owner({finished, success}, State),
            {stop, normal, State}
    catch
        Class:Reason:Stacktrace ->
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            notify_owner({finished, {error, {Class, SaferReason, SaferStacktrace}}}, State),
            {stop, normal, State}
    end.

-spec do_write(state()) -> ok | no_return().
do_write(State) ->
    #state{path = Path, content = Content, modified_on = ModificationDT} = State,
    TmpSuffix = ".tmp." ++ integer_to_list(rand:uniform(1 bsl 32), 36),
    TmpPath = unicode:characters_to_list([Path, TmpSuffix]),
    FileInfoMod = #file_info{ mtime = ModificationDT },

    ok = filelib:ensure_dir(Path),
    {ok, IoDevice} = file:open(TmpPath, [write, exclusive, raw]),
    ok = file:write(IoDevice, Content),
    ok = file:close(IoDevice),
    ok = file:write_file_info(TmpPath, FileInfoMod, [{time, universal}]),
    ok = file:rename(TmpPath, Path).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Events
%% ------------------------------------------------------------------

%-spec notify_owner(msg(), state()) -> ok.
notify_owner(Msg, State) ->
    #state{owner_pid = OwnerPid} = State,
    _ = erlang:send(OwnerPid, {self(), Msg}, [noconnect]),
    ok.
