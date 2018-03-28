%% Copyright (c) 2018 Guilherme Andrade
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

%% @private
-module(locus_util).

-include_lib("kernel/include/file.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [read_file_and_its_modification_date/1,
    maybe_read_file_and_its_modification_date/2,
    load_database_from_tarball/3
   ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec read_file_and_its_modification_date(nonempty_string())
        -> {ok, binary(), calendar:datetime()} |
           {error, {read_file | read_file_info, term()}}.
read_file_and_its_modification_date(Filename) ->
    maybe_read_file_and_its_modification_date(Filename, undefined).

-spec maybe_read_file_and_its_modification_date(nonempty_string(),
                                                calendar:datetime() | undefined)
        -> {ok, unchanged} |
           {ok, binary(), calendar:datetime()} |
           {error, {read_file | read_file_info, term()}}.
maybe_read_file_and_its_modification_date(Filename, PrevModificationDate) ->
    case file:read_file_info(Filename, [{time,universal}]) of
        {ok, #file_info{ mtime = ModificationDate }}
          when ModificationDate =:= PrevModificationDate ->
            {ok, unchanged};
        {ok, #file_info{ mtime = ModificationDate }} ->
            case file:read_file(Filename) of
                {ok, Content} ->
                    {ok, Content, ModificationDate};
                {error, Error} ->
                    {error, {read_file, Error}}
            end;
        {error, Error} ->
            {error, {read_file_info, Error}}
    end.

-spec load_database_from_tarball(atom(), binary(), locus_mmdb:source())
        -> {ok, calendar:datetime()} |
           {error, {exception, atom(), term()}}.
load_database_from_tarball(Id, Tarball, Source) ->
    try
        BinDatabase = extract_database_from_tarball(Tarball),
        Version = locus_mmdb:decode_and_update(Id, BinDatabase, Source),
        {ok, Version}
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec extract_database_from_tarball(binary()) -> binary().
extract_database_from_tarball(Tarball) ->
    {ok, ContainedPaths} = erl_tar:table({binary, Tarball}, [compressed]),
    {true, DatabasePath} = lists_anymap(fun has_mmdb_extension/1, ContainedPaths),
    {ok, [{DatabasePath, BinDatabase}]} =
        erl_tar:extract({binary, Tarball}, [{files, [DatabasePath]}, memory, compressed]),
    BinDatabase.

lists_anymap(Fun, [H|T]) ->
    case Fun(H) of
        {true, Mapped} -> {true, Mapped};
        true -> {true, H};
        false -> lists_anymap(Fun, T)
    end;
lists_anymap(_Fun, []) ->
    false.

%-spec has_mmdb_extension(nonempty_string()) -> boolean().
has_mmdb_extension({Filename, _Type, _Size, _MTime, _Mode, _Uid, _Gid}) ->
    % FIXME: this a placeholder for OTP 20; due to the incomplete spec
    % of erl_tar:table/2, Dialyzer comes to believe that no strings
    % can be returned, only the above tuple, which in fact is only returned
    % if the 'verbose' option is picked, something that we are definitely
    % not doing.
    filename:extension(Filename) =:= ".mmdb" andalso {true, Filename};
has_mmdb_extension(Filename) ->
    filename:extension(Filename) =:= ".mmdb".
