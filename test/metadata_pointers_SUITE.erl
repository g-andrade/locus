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

-module(metadata_pointers_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(PROJECT_ROOT, "../../../../").
-define(DATABASE_PATH, "test/MaxMind-DB/MaxMind-DB-test-metadata-pointers.mmdb").

%% ------------------------------------------------------------------
%% Setup
%% ------------------------------------------------------------------

all() ->
    [analysis_test].

%% ------------------------------------------------------------------
%% Test Cases
%% ------------------------------------------------------------------

analysis_test(_Config) ->
    {skip, "Unable to find IPv4 tree subroot even though it's an IPv6 database."}.
    %{ok, _} = application:ensure_all_started(locus),
    %DatabasePath = filename:join([?PROJECT_ROOT, ?DATABASE_PATH]),
    %{ok, BinDatabase} = file:read_file(DatabasePath),
    %{DatabaseParts, _DatabaseVersion} = decode_database_parts(BinDatabase),
    %?assertEqual(ok, locus_mmdb:analyze_([{database, DatabaseParts}])).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

decode_database_parts(BinDatabase) ->
    Source = {filesystem, ""},
    locus_mmdb:decode_database_parts(BinDatabase, Source).
