%% Copyright (c) 2019 Guilherme Andrade
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

-module(bad_mmdb_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(PROJECT_ROOT, "../../../../").
-define(DATABASES_ROOT_DIR, "test/MaxMind-DB/bad-data").

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [{group, individual_tests}].

groups() ->
    [{individual_tests, [],
      [Function || {Function,1} <- ?MODULE:module_info(exports),
                   lists:suffix("_test", atom_to_list(Function))]
     }].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(locus),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(locus).

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

offset_integer_overflow_test(_Config) ->
    expect_database_decode_failure("libmaxminddb/libmaxminddb-offset-integer-overflow.mmdb").

cyclic_data_structure_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/cyclic-data-structure.mmdb").

invalid_bytes_length_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/invalid-bytes-length.mmdb").

invalid_data_record_offset_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/invalid-data-record-offset.mmdb").

invalid_map_key_length_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/invalid-map-key-length.mmdb").

invalid_string_length_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/invalid-string-length.mmdb").

metadata_is_an_uint128_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/metadata-is-an-uint128.mmdb").

unexpected_bytes_test(_Config) ->
    expect_database_decode_failure("maxminddb-golang/unexpected-bytes.mmdb").

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

expect_database_decode_failure(TailPath) ->
    {ok, BinDatabase} = read_database(TailPath),
    try decode_database_parts(BinDatabase) of
        {_DatabaseParts, Version} ->
            ct:pal("Database version ~p successfully decoded", [Version]),
            error(unexpected_success)
    catch
        Class:Reason ->
            ct:pal("Unable to decode database because of ~p:~p on ~p",
                   [Class, Reason, erlang:get_stacktrace()])
    end.

read_database(TailPath) ->
    FullPath = database_path(TailPath),
    file:read_file(FullPath).

database_path(TailPath) ->
    filename:join([?PROJECT_ROOT, ?DATABASES_ROOT_DIR, TailPath]).

decode_database_parts(BinDatabase) ->
    Source = {filesystem, ""},
    locus_mmdb:decode_database_parts(BinDatabase, Source).
