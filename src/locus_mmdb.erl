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

%% @reference <a href="https://maxmind.github.io/MaxMind-DB/">MaxMind DB File Format Specification</a>

-module(locus_mmdb).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create_table/1]).
-export([decode_database_parts/2]).
-export([update/2]).
-export([lookup/2]).
-export([get_parts/1]).
-export([get_shared_binaries/1]).
-export([analyze/1]).

-ifdef(TEST).
-export([lookup_/2]).
-endif.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(METADATA_MARKER, "\xab\xcd\xefMaxMind.com").

-define(assert(Cond, Error), ((Cond) orelse error((Error)))).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type bin_database() :: <<_:64,_:_*8>>.
-export_type([bin_database/0]).

-type source() :: locus_loader:source().
-export_type([source/0]).

-type parts() ::
    #{ tree := binary(),
       data_section := binary(),
       metadata := metadata(),
       ipv4_root_index := non_neg_integer(),
       source := source(),
       version := calendar:datetime()
     }.
-export_type([parts/0]).

-type metadata() :: locus_mmdb_data:decoded_map().
-export_type([metadata/0]).

-type mmdb_value() :: locus_mmdb_data:decoded_value().
-export_type([mmdb_value/0]).

-type ip_address_prefix() :: locus_mmdb_tree:ip_address_prefix().
-export_type([ip_address_prefix/0]).

-type analysis_flaw() :: locus_mmdb_analysis:flaw().
-export_type([analysis_flaw/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec create_table(atom()) -> ok.
%% @private
create_table(Id) ->
    Table = table_name(Id),
    _ = ets:new(Table, [named_table, protected, {read_concurrency,true}]),
    ok.

-spec decode_database_parts(source(), bin_database()) -> {calendar:datetime(), parts()}.
%% @private
decode_database_parts(Source, BinDatabase) ->
    BinMetadataMarkerParts = binary:matches(BinDatabase, <<?METADATA_MARKER>>),
    {BinMetadataStart, _BinMetadataMarkerLength} = lists:last(BinMetadataMarkerParts),
    <<TreeAndDataSection:BinMetadataStart/bytes, ?METADATA_MARKER, BinMetadata/bytes>>
        = BinDatabase,
    Metadata = decode_metadata(BinMetadata),
    RecordSize = maps:get(<<"record_size">>, Metadata),
    NodeCount = maps:get(<<"node_count">>, Metadata),
    BuildEpoch = maps:get(<<"build_epoch">>, Metadata),
    FmtMajorVersion = maps:get(<<"binary_format_major_version">>, Metadata),
    FmtMinorVersion = maps:get(<<"binary_format_minor_version">>, Metadata),
    ?assert(is_known_database_format(FmtMajorVersion),
            {unknown_database_format_version, FmtMajorVersion, FmtMinorVersion}),
    TreeSize = ((RecordSize * 2) div 8) * NodeCount,
    <<Tree:TreeSize/bytes, 0:128, DataSection/bytes>> = TreeAndDataSection,
    IPv4RootIndex = locus_mmdb_tree:find_ipv4_root_index(Metadata, Tree),
    Version = epoch_to_datetime(BuildEpoch),
    DatabaseParts = #{ tree => Tree, data_section => DataSection,
                       metadata => Metadata, ipv4_root_index => IPv4RootIndex,
                       source => Source, version => Version },
    {Version, DatabaseParts}.

-spec update(atom(), parts()) -> true.
%% @private
update(Id, DatabaseParts) ->
    Table = table_name(Id),
    ets:insert(Table, {database,DatabaseParts}).

-spec lookup(atom(), inet:ip_address() | nonempty_string() | binary())
        -> {ok, #{ prefix => {inet:ip_address(), 0..128},
                   unicode:unicode_binary() => mmdb_value() }} |
           {error, (not_found | invalid_address | ipv4_database |
                    database_unknown | database_not_loaded)}.
%% @private
lookup(Id, Address) ->
    case locus_util:parse_ip_address(Address) of
        {ok, ParsedAddress} ->
            with_database_parts(
              Id,
              fun (DatabaseParts) ->
                      lookup_(ParsedAddress, DatabaseParts)
              end);
        {error, einval} ->
            {error, invalid_address}
    end.

-spec get_parts(atom()) -> {ok, parts()} | {error, database_unknown | database_not_loaded}.
%% @private
get_parts(Id) ->
    with_database_parts(Id, fun (DatabaseParts) -> {ok, DatabaseParts} end).

-spec get_shared_binaries(atom()) -> {ok, [binary(),...]} |
                                     {error, database_unknown | database_not_loaded}.
%% @private
get_shared_binaries(Id) ->
    with_database_parts(
      Id,
      fun (#{data_section := DataSection, tree := Tree}) ->
              {ok, [DataSection,Tree]}
      end).

-spec analyze(atom())
        -> ok |
           {error, {flawed, [analysis_flaw(), ...]}} |
           {error, database_unknown} |
           {error, database_not_loaded}.
%% @private
analyze(Id) ->
    with_database_parts(Id, fun locus_mmdb_analysis:run/1).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec table_name(atom()) -> atom().
table_name(Id) ->
    list_to_atom("locus_mmdb_" ++ atom_to_list(Id)).

-spec decode_metadata(binary()) -> metadata().
decode_metadata(BinMetadata) ->
    {Metadata, _FinalChunk} = locus_mmdb_data:decode_on_index(0, BinMetadata),
    Metadata.

is_known_database_format(FmtMajorVersion) ->
    FmtMajorVersion =:= 2.

-spec epoch_to_datetime(integer()) -> calendar:datetime().
epoch_to_datetime(Epoch) ->
    GregorianEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    calendar:gregorian_seconds_to_datetime(GregorianEpoch + Epoch).

with_database_parts(Id, Fun) ->
    Table = table_name(Id),
    case ets:info(Table, name) =:= Table andalso ets:lookup(Table, database) of
        false ->
            {error, database_unknown};
        [] ->
            {error, database_not_loaded};
        [{_,DatabaseParts}] ->
            Fun(DatabaseParts)
    end.

lookup_(Address, DatabaseParts) ->
    #{data_section := DataSection, ipv4_root_index := IPv4RootIndex,
      metadata := Metadata, tree := Tree} = DatabaseParts,

    case locus_mmdb_tree:lookup(Address, IPv4RootIndex, Metadata, Tree) of
        {ok, {DataIndex, Prefix}} ->
            {#{} = Entry, _} = locus_mmdb_data:decode_on_index(DataIndex, DataSection),
            ExtendedEntry = Entry#{ prefix => Prefix },
            {ok, ExtendedEntry};
        {error, Reason} ->
            {error, Reason}
    end.
