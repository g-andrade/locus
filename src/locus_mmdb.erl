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
-export([analyze/1]).

-ifdef(TEST).
-export([lookup_/2]).
-export([analyze_/1]).
-endif.

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(METADATA_MARKER, "\xab\xcd\xefMaxMind.com").
-define(pointer, 1).
-define(utf8_string, 2).
-define(double, 3).
-define(bytes, 4).
-define(uint16, 5).
-define(uint32, 6).
-define(map, 7).

-define(extended_int32, 1).
-define(extended_uint64, 2).
-define(extended_uint128, 3).
-define(extended_array, 4).
-define(extended_data_cache_container, 5).
-define(extended_end_marker, 6).
-define(extended_boolean, 7).
-define(extended_float, 8).

-define(assert(Cond, Error), ((Cond) orelse error((Error)))).

% https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
-define(IPV4_IPV6_PREFIX, <<0:80, 16#FFFF:16>>).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type bin_database() :: <<_:64,_:_*8>>.
-export_type([bin_database/0]).

-type source() :: locus_loader:source().
-export_type([source/0]).

-ifdef(POST_OTP_18).
-type parts() ::
        #{ tree := binary(),
           data_section := binary(),
           metadata := metadata(),
           ipv4_root_index := non_neg_integer(),
           source := source(),
           version := calendar:datetime()
         }.
-else.
-type parts() ::
        #{ tree => binary(),
           data_section => binary(),
           metadata => metadata(),
           ipv4_root_index => non_neg_integer(),
           source => source(),
           version => calendar:datetime()
         }.
-endif.
-export_type([parts/0]).

-type metadata() ::
        #{ binary() => term() }.
-export_type([metadata/0]).

-type analysis_flaw() ::
        max_depth_exceeded() |
        node_dereference_failed() |
        bad_record_data_type() |
        data_record_decoding_failed().
-export_type([analysis_flaw/0]).

-ifdef(POST_OTP_18).
-type max_depth_exceeded() ::
        {max_depth_exceeded, #{ tree_prefix := {inet:ip_address(), 0..128},
                                node_index := non_neg_integer(),
                                depth := 33 | 129
                              }}.
-export_type([max_depth_exceeded/0]).
-else.
-type max_depth_exceeded() ::
        {max_depth_exceeded, #{ tree_prefix => {inet:ip_address(), 0..128},
                                node_index => non_neg_integer(),
                                depth => 33 | 129
                              }}.
-export_type([max_depth_exceeded/0]).
-endif.

-ifdef(POST_OTP_18).
-type node_dereference_failed() ::
        {node_dereference_failed, #{ tree_prefix := {inet:ip_address(), 0..128},
                                     node_index := non_neg_integer(),
                                     class := error | throw | exit,
                                     reason := term()
                                   }}.
-export_type([node_dereference_failed/0]).
-else.
-type node_dereference_failed() ::
        {node_dereference_failed, #{ tree_prefix => {inet:ip_address(), 0..128},
                                     node_index => non_neg_integer(),
                                     class => error | throw | exit,
                                     reason => term()
                                   }}.
-export_type([node_dereference_failed/0]).
-endif.

-ifdef(POST_OTP_18).
-type bad_record_data_type() ::
         {bad_record_data_type, #{ data_index := non_neg_integer(),
                                   data_record := term(),
                                   tree_prefixes := [{inet:ip_address(), 0..128}, ...]
                                 }}.
-export_type([bad_record_data_type/0]).
-else.
-type bad_record_data_type() ::
         {bad_record_data_type, #{ data_index => non_neg_integer(),
                                   data_record => term(),
                                   tree_prefixes => [{inet:ip_address(), 0..128}, ...]
                                 }}.
-export_type([bad_record_data_type/0]).
-endif.

-ifdef(POST_OTP_18).
-type data_record_decoding_failed() ::
        {data_record_decoding_failed, #{ data_index := non_neg_integer(),
                                         class := error | throw | exit,
                                         reason := term(),
                                         tree_prefixes := [{inet:ip_address(), 0..128}, ...]
                                       }}.
-export_type([data_record_decoding_failed/0]).
-else.
-type data_record_decoding_failed() ::
        {data_record_decoding_failed, #{ data_index => non_neg_integer(),
                                         class => error | throw | exit,
                                         reason => term(),
                                         tree_prefixes => [{inet:ip_address(), 0..128}, ...]
                                       }}.
-export_type([data_record_decoding_failed/0]).
-endif.

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
    IPv4RootIndex = find_ipv4_root_index(Tree, Metadata),
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
                   binary() => term() }} |
           {error, (not_found | invalid_address | ipv4_database |
                    database_unknown | database_not_loaded)}.
%% @private
lookup(Id, Address) ->
    case locus_util:parse_ip_address(Address) of
        {ok, ParsedAddress} ->
            Table = table_name(Id),
            DatabaseLookup = (ets:info(Table, name) =:= Table andalso
                              ets:lookup(Table, database)),
            lookup_(DatabaseLookup, ParsedAddress);
        {error, einval} ->
            {error, invalid_address}
    end.

-spec get_parts(atom()) -> {ok, parts()} | {error, database_unknown | database_not_loaded}.
%% @private
get_parts(Id) ->
    Table = table_name(Id),
    case ets:info(Table, name) =:= Table andalso
         ets:lookup(Table, database)
    of
        false ->
            {error, database_unknown};
        [] ->
            {error, database_not_loaded};
        [{database, Parts}] ->
            {ok, Parts}
    end.

-spec analyze(atom())
        -> ok |
           {error, {flawed, [analysis_flaw(), ...]}} |
           {error, database_unknown} |
           {error, database_not_loaded}.
%% @private
analyze(Id) ->
    Table = table_name(Id),
    DatabaseLookup = ets:info(Table, name) =:= Table andalso ets:lookup(Table, database),
    analyze_(DatabaseLookup).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Initialization and Data Decoding
%% ------------------------------------------------------------------

-spec table_name(atom()) -> atom().
table_name(Id) ->
    list_to_atom("locus_mmdb_" ++ atom_to_list(Id)).

-spec decode_metadata(binary()) -> metadata().
decode_metadata(BinMetadata) ->
    {Metadata, _FinalChunk} = consume_data_section_on_index(BinMetadata, 0),
    Metadata.

is_known_database_format(FmtMajorVersion) ->
    FmtMajorVersion =:= 2.

-spec epoch_to_datetime(integer()) -> calendar:datetime().
epoch_to_datetime(Epoch) ->
    GregorianEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    calendar:gregorian_seconds_to_datetime(GregorianEpoch + Epoch).

consume_data_section_on_index(DataSection, Index) ->
    Chunk = binary:part(DataSection, {Index, byte_size(DataSection) - Index}),
    consume_data_section_chunk(DataSection, Chunk).

consume_data_section_chunk(DataSection, Chunk) ->
    case Chunk of
        <<?pointer:3, 0:2, Pointer:11, Remaining/bytes>> ->
            {Value, _} = consume_data_section_on_index(DataSection, Pointer),
            {Value, Remaining};
        <<?pointer:3, 1:2, BasePointer:19, Remaining/bytes>> ->
            {Value, _} = consume_data_section_on_index(DataSection, BasePointer+2048),
            {Value, Remaining};
        <<?pointer:3, 2:2, BasePointer:27, Remaining/bytes>> ->
            {Value, _} = consume_data_section_on_index(DataSection, BasePointer+526336),
            {Value, Remaining};
        <<?pointer:3, _:5, Pointer:32, Remaining/bytes>> ->
            {Value, _} = consume_data_section_on_index(DataSection, Pointer),
            {Value, Remaining};

        <<?utf8_string:3, Size:5, Remaining/bytes>> when Size < 29 ->
            consume_bytes(Size, Remaining);
        <<?utf8_string:3, 29:5, BaseSize, Remaining/bytes>> ->
            consume_bytes(BaseSize+29, Remaining);
        <<?utf8_string:3, 30:5, BaseSize:16, Remaining/bytes>> ->
            consume_bytes(BaseSize+285, Remaining);
        <<?utf8_string:3, _:5, BaseSize:24, Remaining/bytes>> ->
            consume_bytes(BaseSize+65821, Remaining);

        <<?double:3, 8:5, Double:64/float, Remaining/bytes>> ->
            {Double, Remaining};

        <<?bytes:3, Size:5, Remaining/bytes>> when Size < 29 ->
            consume_bytes(Size, Remaining);
        <<?bytes:3, 29:5, BaseSize, Remaining/bytes>> ->
            consume_bytes(BaseSize+29, Remaining);
        <<?bytes:3, 30:5, BaseSize:16, Remaining/bytes>> ->
            consume_bytes(BaseSize+285, Remaining);
        <<?bytes:3, _:5, BaseSize:24, Remaining/bytes>> ->
            consume_bytes(BaseSize+65821, Remaining);

        <<?uint16:3, Size:5, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 2 ->
            {Integer, Remaining};
        <<?uint32:3, Size:5, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 4 ->
            {Integer, Remaining};

        <<?map:3, Size:5, Remaining/bytes>> when Size < 29 ->
            consume_map(DataSection, Size, Remaining);
        <<?map:3, 29:5, BaseSize, Remaining/bytes>> ->
            consume_map(DataSection, 29 + BaseSize, Remaining);
        <<?map:3, 30:5, BaseSize:16, Remaining/bytes>> ->
            consume_map(DataSection, 285 + BaseSize, Remaining);
        <<?map:3, _:5, BaseSize:24, Remaining/bytes>> ->
            consume_map(DataSection, 65821 + BaseSize, Remaining);

        <<0:3, Size:5, ?extended_int32, Integer:Size/signed-integer-unit:8, Remaining/bytes>>
          when Size =< 4 ->
            {Integer, Remaining};
        <<0:3, Size:5, ?extended_uint64, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 8 ->
            {Integer, Remaining};
        <<0:3, Size:5, ?extended_uint128, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 16 ->
            {Integer, Remaining};

        <<0:3, Size:5, ?extended_array, Remaining/bytes>> when Size < 29 ->
            consume_array(DataSection, Size, Remaining);
        <<0:3, 29:5, ?extended_array, BaseSize, Remaining/bytes>> ->
            consume_array(DataSection, BaseSize+29, Remaining);
        <<0:3, 30:5, ?extended_array, BaseSize:16, Remaining/bytes>> ->
            consume_array(DataSection, BaseSize+285, Remaining);
        <<0:3, _:5, ?extended_array, BaseSize:24, Remaining/bytes>> ->
            consume_array(DataSection, BaseSize+65821, Remaining);

        <<0:3, 0:5, ?extended_data_cache_container, _/bytes>> ->
            error({unexpected_marker, data_cache_container});
        <<0:3, 0:5, ?extended_end_marker, _/bytes>> ->
            error({unexpected_marker, 'end'});

        <<0:3, 0:5, ?extended_boolean, Remaining/bytes>> ->
            {false, Remaining};
        <<0:3, 1:5, ?extended_boolean, Remaining/bytes>> ->
            {true, Remaining};

        <<0:3, 4:5, ?extended_float, Float:32/float, Remaining/bytes>> ->
            {Float, Remaining}
    end.

consume_bytes(Size, Chunk) ->
    <<Bytes:Size/bytes, Remaining/bytes>> = Chunk,
    CopiedBytes = binary:copy(Bytes),
    {CopiedBytes, Remaining}.

consume_map(DataSection, Size, Chunk) ->
    consume_map_recur(DataSection, Size, Chunk, []).

consume_map_recur(_DataSection, 0, Remaining, KvAcc) ->
    Map = maps:from_list(KvAcc),
    {Map, Remaining};
consume_map_recur(DataSection, Size, Chunk, KvAcc) ->
    {Key, Chunk2} = consume_map_key(DataSection, Chunk),
    {Value, Chunk3} = consume_data_section_chunk(DataSection, Chunk2),
    UpdatedKvAcc = [{Key,Value} | KvAcc],
    consume_map_recur(DataSection, Size - 1, Chunk3, UpdatedKvAcc).

consume_map_key(DataSection, Chunk) ->
    case Chunk of
        <<?pointer:3, 0:2, Pointer:11, _/bytes>> ->
            <<_:Pointer/bytes, NewChunk/bytes>> = DataSection,
            consume_direct_map_key(NewChunk);
        <<?pointer:3, 1:2, BasePointer:19, _/bytes>> ->
            Pointer = BasePointer + 2048,
            <<_:Pointer/bytes, NewChunk/bytes>> = DataSection,
            consume_direct_map_key(NewChunk);
        <<?pointer:3, 2:2, BasePointer:27, _/bytes>> ->
            Pointer = BasePointer + 526336,
            <<_:Pointer/bytes, NewChunk/bytes>> = DataSection,
            consume_direct_map_key(NewChunk);
        <<?pointer:3, _:5, Pointer:32, _/bytes>> ->
            <<_:Pointer/bytes, NewChunk/bytes>> = DataSection,
            consume_direct_map_key(NewChunk);
        _ ->
            consume_direct_map_key(Chunk)
    end.

consume_direct_map_key(Chunk) ->
    case Chunk of
        <<?utf8_string:3, Size:5, Remaining/bytes>> when Size < 29 ->
            consume_bytes(Size, Remaining);
        <<?utf8_string:3, 29:5, BaseSize, Remaining/bytes>> ->
            consume_bytes(BaseSize+29, Remaining);
        <<?utf8_string:3, 30:5, BaseSize:16, Remaining/bytes>> ->
            consume_bytes(BaseSize+285, Remaining);
        <<?utf8_string:3, _:5, BaseSize:24, Remaining/bytes>> ->
            consume_bytes(BaseSize+65821, Remaining)
    end.

consume_array(DataSection, Size, Chunk) ->
    consume_array_recur(DataSection, Size, Chunk, []).

consume_array_recur(_DataSection, 0, Remaining, RevAcc) ->
    List = lists:reverse(RevAcc),
    {List, Remaining};
consume_array_recur(DataSection, Size, Chunk, RevAcc) ->
    {Value, Remaining} = consume_data_section_chunk(DataSection, Chunk),
    UpdatedRevAcc = [Value | RevAcc],
    consume_array_recur(DataSection, Size - 1, Remaining, UpdatedRevAcc).

find_ipv4_root_index(_Tree, #{ <<"ip_version">> := 4 } = _Metadata) ->
    0;
find_ipv4_root_index(Tree, #{ <<"ip_version">> := 6 } = Metadata) ->
    find_node_index_for_prefix(?IPV4_IPV6_PREFIX, Tree, Metadata).

find_node_index_for_prefix(Bitstring, Tree, Metadata) ->
    NodeCount = maps:get(<<"node_count">>, Metadata),
    RecordSize = maps:get(<<"record_size">>, Metadata),
    NodeSize = (RecordSize * 2) div 8,
    find_node_index_for_prefix_recur(Bitstring, Tree, NodeSize, RecordSize, 0, NodeCount).

find_node_index_for_prefix_recur(<<Bit:1,NextBits/bits>>, Tree, NodeSize, RecordSize, NodeIndex, NodeCount)
  when NodeIndex < NodeCount ->
    % regular node
    Node = binary:part(Tree, {NodeIndex * NodeSize, NodeSize}),
    ChildNodeIndex = extract_node_record(Bit, Node, RecordSize),
    find_node_index_for_prefix_recur(NextBits, Tree, NodeSize, RecordSize, ChildNodeIndex, NodeCount);
find_node_index_for_prefix_recur(<<>>, _Tree, _NodeSize, _RecordSize, NodeIndex, _NodeCount) ->
    % the end of the line
    NodeIndex.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Looking Up
%% ------------------------------------------------------------------

metadata_get(Key, #{ metadata := Metadata } = _DatabaseParts) ->
    maps:get(Key, Metadata).

ip_address_to_bitstring({A,B,C,D}, DatabaseParts) ->
    RootNodeIndex = maps:get(ipv4_root_index, DatabaseParts),
    {ok, <<A,B,C,D>>, RootNodeIndex};
ip_address_to_bitstring({A,B,C,D,E,F,G,H}, DatabaseParts) ->
    case metadata_get(<<"ip_version">>, DatabaseParts) of
        4 -> {error, ipv4_database};
        6 -> {ok, <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>, 0}
    end.

%% @private
lookup_(false, _Address) ->
    {error, database_unknown};
lookup_([] = _DatabaseLookup, _Address) ->
    {error, database_not_loaded};
lookup_([{database, DatabaseParts}] = _DatabaseLookup, Address) ->
    case ip_address_to_bitstring(Address, DatabaseParts) of
        {ok, BitAddress, RootNodeIndex} ->
            #{ tree := Tree, data_section := DataSection } = DatabaseParts,
            NodeCount = metadata_get(<<"node_count">>, DatabaseParts),
            RecordSize = metadata_get(<<"record_size">>, DatabaseParts),
            NodeSize = (RecordSize * 2) div 8,
            Result =
                lookup_recur(BitAddress, Tree, DataSection,
                             NodeSize, RecordSize, RootNodeIndex, NodeCount),
            handle_recursive_lookup_result(Result, BitAddress);
        {error, Error} ->
            {error, Error}
    end.

lookup_recur(<<Bit:1,NextBits/bits>>, Tree, DataSection, NodeSize, RecordSize,
             NodeIndex, NodeCount)
  when NodeIndex < NodeCount ->
    % regular node
    Node = binary:part(Tree, {NodeIndex * NodeSize, NodeSize}),
    ChildNodeIndex = extract_node_record(Bit, Node, RecordSize),
    lookup_recur(NextBits, Tree, DataSection, NodeSize, RecordSize,
                 ChildNodeIndex, NodeCount);
lookup_recur(_BitAddress, _Tree, _DataSection, _NodeSize, _RecordSize,
             NodeIndex, NodeCount)
  when NodeIndex =:= NodeCount ->
    % leaf node
    {error, not_found};
lookup_recur(BitAddress, _Tree, DataSection, _NodeSize, _RecordSize,
             NodeIndex, NodeCount) ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    case consume_data_section_on_index(DataSection, DataIndex) of
        {#{} = DataRecord, _FinalChunk} ->
            SuffixSize = bit_size(BitAddress),
            {ok, DataRecord, SuffixSize}
    end.

extract_node_record(0 = _Bit, Node, RecordSize) when byte_size(Node) band 1 =:= 0 ->
    <<Left:RecordSize, _/bits>> = Node,
    Left;
extract_node_record(0 = _Bit, Node, RecordSize) ->
    LeftWholeSz = (RecordSize bsr 3) bsl 3,
    LeftRemainderSz = RecordSize band 2#111,
    <<LeftLow:LeftWholeSz, LeftHigh:LeftRemainderSz, _/bits>> = Node,
    (LeftHigh bsl LeftWholeSz) bor LeftLow;
extract_node_record(1 = _Bit, Node, RecordSize) ->
    <<_:RecordSize, Right:RecordSize>> = Node,
    Right.

handle_recursive_lookup_result({ok, Entry, SuffixSize}, BitAddress) ->
    Prefix = ip_address_prefix(BitAddress, SuffixSize),
    ExtendedEntry = Entry#{ prefix => Prefix },
    {ok, ExtendedEntry};
handle_recursive_lookup_result({error, Error}, _BitAddress) ->
    {error, Error}.

ip_address_prefix(BitAddress, SuffixSize) when bit_size(BitAddress) =:= 32 ->
    PrefixSize = 32 - SuffixSize,
    <<Prefix:PrefixSize/bits, _Suffix/bits>> = BitAddress,
    BitBaseAddress = <<Prefix/bits, 0:SuffixSize>>,
    <<A,B,C,D>> = BitBaseAddress,
    {{A,B,C,D}, PrefixSize};
ip_address_prefix(BitAddress, SuffixSize) when bit_size(BitAddress) =:= 128 ->
    PrefixSize = 128 - SuffixSize,
    <<Prefix:PrefixSize/bits, _Suffix/bits>> = BitAddress,
    BitBaseAddress = <<Prefix/bits, 0:SuffixSize>>,
    <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>> = BitBaseAddress,
    {{A,B,C,D,E,F,G,H}, PrefixSize}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Analysis
%% ------------------------------------------------------------------

%% @private
analyze_(false) ->
    {error, database_unknown};
analyze_([]) ->
    {error, database_not_loaded};
analyze_([{database, DatabaseParts}]) ->
    DataSectionIndices = ets:new(data_section_indices, []),
    try
        analyze_data_section(DatabaseParts, DataSectionIndices),
        analyze_tree(DatabaseParts, DataSectionIndices)
    catch
        error:{data_section, Chunk, Reason} ->
            #{data_section := DataSection} = DatabaseParts,
            ErrorIndex = byte_size(DataSection) - byte_size(Chunk),
            {error, {flawed, {data_section,ErrorIndex,Reason}}}
    after
        ets:delete(DataSectionIndices)
    end.

analyze_data_section(DatabaseParts, Indices) ->
    #{data_section := DataSection} = DatabaseParts,
    analyze_data_section_recur(DataSection, DataSection, Indices).

analyze_data_section_recur(DataSection, Chunk, Indices) ->
    ParseChunkResult = parse_data_section_chunk_for_analysis(DataSection, Chunk),
    save_analysis_data_section_chunk_index(DataSection, Chunk, Indices),
    analyze_data_section_recur(DataSection, Chunk, Indices, ParseChunkResult).

analyze_data_section_recur(DataSection, CurChunk, Indices, {map,Size,MapChunk}) ->
    try analyze_data_section_map(DataSection, Size, MapChunk, Indices, #{}) of
        ChunkAfterMap ->
            analyze_data_section_recur(DataSection, ChunkAfterMap, Indices)
    catch
        error:Reason ->
            error({data_section, CurChunk, {bad_map, Reason}})
    end;
analyze_data_section_recur(DataSection, CurChunk, Indices, {array,Size,ArrayChunk}) ->
    try analyze_data_section_array(DataSection, Size, ArrayChunk, Indices, #{}) of
        ChunkAfterArray ->
            analyze_data_section_recur(DataSection, ChunkAfterArray, Indices)
    catch
        error:Reason ->
            error({data_section, CurChunk, {bad_map, Reason}})
    end;
analyze_data_section_recur(DataSection, CurChunk, Indices, {pointer, Pointer, NextChunk}) ->
    <<_:Pointer/bytes, PointerChunk/bytes>> = DataSection,
    case parse_data_section_chunk_for_analysis(DataSection, PointerChunk) of
        {pointer,_,_} ->
            error({data_section, CurChunk, {bad_pointer_value, another_pointer}});
        finished ->
            error({data_section, CurChunk, {bad_pointer_value, end_marker}});
        _ ->
            save_analysis_data_section_chunk_index(DataSection, PointerChunk, Indices),
            analyze_data_section_recur(DataSection, NextChunk, Indices)
    end;
analyze_data_section_recur(DataSection, _, Indices, ImplicitSuccess) ->
    case ImplicitSuccess of
        {utf8, _, NextChunk} ->
            analyze_data_section_recur(DataSection, NextChunk, Indices);
        {float, _, NextChunk} ->
            analyze_data_section_recur(DataSection, NextChunk, Indices);
        {integer, _, NextChunk} ->
            analyze_data_section_recur(DataSection, NextChunk, Indices);
        {bytes, _, NextChunk} ->
            analyze_data_section_recur(DataSection, NextChunk, Indices);
        {boolean, _, NextChunk} ->
            analyze_data_section_recur(DataSection, NextChunk, Indices);
        finished ->
            ok
    end.

analyze_data_section_map(DataSection, Size, MapChunk, Indices, RecordsAbove) ->
    UpdatedRecordsAbove = check_for_data_section_loop(DataSection, MapChunk, RecordsAbove),
    analyze_data_section_map_recur(DataSection, Size, MapChunk, Indices, UpdatedRecordsAbove, maps:new()).

analyze_data_section_map_recur(_, 0, ChunkAfterMap, _, _, _) ->
    ChunkAfterMap;
analyze_data_section_map_recur(DataSection, Size, KeyChunk, Indices, RecordsAbove, KeyValuesAcc) ->
    {KeyValue, ChunkAfterKey} = analyze_data_section_map_key(DataSection, KeyChunk, Indices),
    case maps:put(KeyValue, defined, KeyValuesAcc) of
        UpdatedKeyValuesAcc when map_size(UpdatedKeyValuesAcc) =:= map_size(KeyValuesAcc) ->
            error({data_section, KeyChunk, {repeated_map_key, KeyValue}});
        UpdatedKeyValuesAcc ->
            ChunkAfterValue = analyze_data_section_map_value(DataSection, ChunkAfterKey,
                                                             Indices, RecordsAbove),
            analyze_data_section_map_recur(DataSection, Size - 1, ChunkAfterValue,
                                           Indices, RecordsAbove, UpdatedKeyValuesAcc)
    end.

analyze_data_section_map_key(DataSection, KeyChunk, Indices) ->
    ParseChunkResult = parse_data_section_chunk_for_analysis(DataSection, KeyChunk),
    save_analysis_data_section_chunk_index(DataSection, KeyChunk, Indices),
    analyze_data_section_map_key(DataSection, KeyChunk, Indices, ParseChunkResult).

analyze_data_section_map_key(DataSection, KeyChunk, Indices, {pointer,Pointer,ChunkAfterKey}) ->
    <<_:Pointer/bytes, PointerChunk/bytes>> = DataSection,
    case parse_data_section_chunk_for_analysis(DataSection, PointerChunk) of
        {utf8, KeyValue, _} ->
            save_analysis_data_section_chunk_index(DataSection, PointerChunk, Indices),
            {KeyValue, ChunkAfterKey};
        Other ->
            error({data_section, KeyChunk, {bad_map_key, {pointer,Pointer}, Other}})
    end;
analyze_data_section_map_key(_, _, _, {utf8, KeyValue, ChunkAfterKey}) ->
    {KeyValue, ChunkAfterKey};
analyze_data_section_map_key(_, KeyChunk, _, Other) ->
    error({data_section, KeyChunk, {bad_map_key, Other}}).

analyze_data_section_map_value(DataSection, ValueChunk, Indices, RecordsAbove) ->
    ParseChunkResult = parse_data_section_chunk_for_analysis(DataSection, ValueChunk),
    save_analysis_data_section_chunk_index(DataSection, ValueChunk, Indices),
    analyze_data_section_map_value(DataSection, ValueChunk, Indices, RecordsAbove, ParseChunkResult).

analyze_data_section_map_value(DataSection, ValueChunk, Indices, RecordsAbove, {map,Size,SubMapChunk}) ->
    try analyze_data_section_map(DataSection, Size, SubMapChunk, Indices, RecordsAbove) of
        ChunkAfterValue ->
            ChunkAfterValue
    catch
        error:Reason ->
            error({data_section, ValueChunk, {bad_map, Reason}})
    end;
analyze_data_section_map_value(DataSection, ValueChunk, Indices, RecordsAbove, {array,Size,SubArrayChunk}) ->
    try analyze_data_section_array(DataSection, Size, SubArrayChunk, Indices, RecordsAbove) of
        ChunkAfterValue ->
            ChunkAfterValue
    catch
        error:Reason ->
            error({data_section, ValueChunk, {bad_array, Reason}})
    end;
analyze_data_section_map_value(_, _, _, _, {_,_,ChunkAfterValue}) ->
    ChunkAfterValue;
analyze_data_section_map_value(_, ValueChunk, _, _, finished) ->
    error({data_section, ValueChunk, {bad_map_value, finished}}).

analyze_data_section_array(DataSection, Size, ArrayChunk, Indices, RecordsAbove) ->
    UpdatedRecordsAbove = check_for_data_section_loop(DataSection, ArrayChunk, RecordsAbove),
    analyze_data_section_array_recur(DataSection, Size, ArrayChunk, Indices, UpdatedRecordsAbove).

analyze_data_section_array_recur(_, 0, ChunkAfterArray, _, _) ->
    ChunkAfterArray;
analyze_data_section_array_recur(DataSection, Size, ArrayChunk, Indices, RecordsAbove) ->
    ChunkAfterValue = analyze_data_section_array_value(DataSection, ArrayChunk, Indices, RecordsAbove),
    analyze_data_section_array_recur(DataSection, Size - 1, ChunkAfterValue, Indices, RecordsAbove).

analyze_data_section_array_value(DataSection, ValueChunk, Indices, RecordsAbove) ->
    ParseChunkResult = parse_data_section_chunk_for_analysis(DataSection, ValueChunk),
    save_analysis_data_section_chunk_index(DataSection, ValueChunk, Indices),
    analyze_data_section_array_value(DataSection, ValueChunk, Indices, RecordsAbove, ParseChunkResult).

analyze_data_section_array_value(DataSection, ValueChunk, Indices, RecordsAbove, {map,Size,SubMapChunk}) ->
    try analyze_data_section_map(DataSection, Size, SubMapChunk, Indices, RecordsAbove) of
        ChunkAfterValue ->
            ChunkAfterValue
    catch
        error:Reason ->
            error({data_section, ValueChunk, {bad_map, Reason}})
    end;
analyze_data_section_array_value(DataSection, ValueChunk, Indices, RecordsAbove, {array,Size,SubArrayChunk}) ->
    try analyze_data_section_array(DataSection, Size, SubArrayChunk, Indices, RecordsAbove) of
        ChunkAfterValue ->
            ChunkAfterValue
    catch
        error:Reason ->
            error({data_section, ValueChunk, {bad_array, Reason}})
    end;
analyze_data_section_array_value(_, _, _, _, {_,_,ChunkAfterValue}) ->
    ChunkAfterValue;
analyze_data_section_array_value(_, ValueChunk, _, _, finished) ->
    error({data_section, ValueChunk, {bad_array_value, finished}}).

parse_data_section_chunk_for_analysis(DataSection, Chunk) ->
    case Chunk of
        <<?pointer:3, 0:2, Pointer:11, Remaining/bytes>>
          when Pointer < byte_size(DataSection) ->
            {pointer, Pointer, Remaining};
        <<?pointer:3, 1:2, BasePointer:19, Remaining/bytes>>
          when BasePointer+2048 < byte_size(DataSection) ->
            {pointer, BasePointer+2048, Remaining};
        <<?pointer:3, 2:2, BasePointer:27, Remaining/bytes>>
          when BasePointer+526336 < byte_size(DataSection) ->
            {pointer, BasePointer+526336, Remaining};
        <<?pointer:3, _:5, Pointer:32, Remaining/bytes>>
          when Pointer < byte_size(DataSection) ->
            {pointer, Pointer, Remaining};

        <<?utf8_string:3, Size:5, Text:Size/bytes, Remaining/bytes>>
          when Size < 29 ->
            _ = (is_utf8_binary(Text) 
                 orelse error({data_section, Chunk, {not_utf8, binary:copy(Text)}})),
            {utf8, Text, Remaining};
        <<?utf8_string:3, 29:5, BaseSize, BaseRemaining/bytes>>
          when byte_size(BaseRemaining) >= BaseSize + 29 ->
            Size = BaseSize + 29,
            <<Text:Size/bytes, Remaining/bytes>> = BaseRemaining,
            _ = (is_utf8_binary(Text) 
                 orelse error({data_section, Chunk, {not_utf8, binary:copy(Text)}})),
            {utf8, Text, Remaining};
        <<?utf8_string:3, 30:5, BaseSize:16, BaseRemaining/bytes>>
          when byte_size(BaseRemaining) >= BaseSize + 285 ->
            Size = BaseSize + 285,
            <<Text:Size/bytes, Remaining/bytes>> = BaseRemaining,
            _ = (is_utf8_binary(Text) 
                 orelse error({data_section, Chunk, {not_utf8, binary:copy(Text)}})),
            {utf8, Text, Remaining};
        <<?utf8_string:3, _:5, BaseSize:24, BaseRemaining/bytes>>
          when byte_size(BaseRemaining) >= BaseSize + 65821 ->
            Size = BaseSize + 65821,
            <<Text:Size/bytes, Remaining/bytes>> = BaseRemaining,
            _ = (is_utf8_binary(Text) 
                 orelse error({data_section, Chunk, {not_utf8, binary:copy(Text)}})),
            {utf8, Text, Remaining};

        <<?double:3, 8:5, Float:64/float, Remaining/bytes>> ->
            {float, Float, Remaining};

        <<?bytes:3, Size:5, Blob:Size/bytes, Remaining/bytes>>
          when Size < 29 ->
            {bytes, Blob, Remaining};
        <<?bytes:3, 29:5, BaseSize, BaseRemaining/bytes>>
          when byte_size(BaseRemaining) >= BaseSize + 29 ->
            Size = BaseSize + 29,
            <<Blob:Size/bytes, Remaining/bytes>> = BaseRemaining,
            {bytes, Blob, Remaining};
        <<?bytes:3, 30:5, BaseSize:16, BaseRemaining/bytes>>
          when byte_size(BaseRemaining) >= BaseSize + 285 ->
            Size = BaseSize + 285,
            <<Blob:Size/bytes, Remaining/bytes>> = BaseRemaining,
            {bytes, Blob, Remaining};
        <<?bytes:3, _:5, BaseSize:24, BaseRemaining/bytes>>
          when byte_size(BaseRemaining) >= BaseSize + 65821 ->
            Size = BaseSize + 65821,
            <<Blob:Size/bytes, Remaining/bytes>> = BaseRemaining,
            {bytes, Blob, Remaining};

        <<?uint16:3, Size:5, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 2 ->
            {integer, Integer, Remaining};
        <<?uint32:3, Size:5, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 4 ->
            {integer, Integer, Remaining};

        <<?map:3, Size:5, Remaining/bytes>> when Size < 29 ->
            {map, Size, Remaining};
        <<?map:3, 29:5, BaseSize, Remaining/bytes>> ->
            {map, BaseSize+29, Remaining};
        <<?map:3, 30:5, BaseSize:16, Remaining/bytes>> ->
            {map, BaseSize+285, Remaining};
        <<?map:3, _:5, BaseSize:24, Remaining/bytes>> ->
            {map, BaseSize+65821, Remaining};

        <<0:3, Size:5, ?extended_int32, Integer:Size/signed-integer-unit:8, Remaining/bytes>>
          when Size =< 4 ->
            {integer, Integer, Remaining};
        <<0:3, Size:5, ?extended_uint64, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 8 ->
            {integer, Integer, Remaining};
        <<0:3, Size:5, ?extended_uint128, Integer:Size/integer-unit:8, Remaining/bytes>>
          when Size =< 16 ->
            {integer, Integer, Remaining};

        <<0:3, Size:5, ?extended_array, Remaining/bytes>> when Size < 29 ->
            {array, Size, Remaining};
        <<0:3, 29:5, ?extended_array, BaseSize, Remaining/bytes>> ->
            {array, BaseSize+29, Remaining};
        <<0:3, 30:5, ?extended_array, BaseSize:16, Remaining/bytes>> ->
            {array, BaseSize+285, Remaining};
        <<0:3, _:5, ?extended_array, BaseSize:24, Remaining/bytes>> ->
            {array, BaseSize+65821, Remaining};

        <<0:3, 0:5, ?extended_data_cache_container, _/bytes>> ->
            error({data_section, Chunk, {unexpected_marker, data_cache_container}});
        <<0:3, 0:5, ?extended_end_marker>> ->
            finished;
        %
        <<0:3, 0:5, ?extended_boolean, Remaining/bytes>> ->
            {boolean, false, Remaining};
        <<0:3, 1:5, ?extended_boolean, Remaining/bytes>> ->
            {boolean, true, Remaining};
        %
        <<0:3, 4:5, ?extended_float, Float:32/float, Remaining/bytes>> ->
            {float, Float, Remaining};

        <<>> ->
            finished;

        <<Unparseable/bytes>> ->
            error({data_section, Chunk, {unparseable, Unparseable}})
    end.

save_analysis_data_section_chunk_index(DataSection, CurChunk, Indices) ->
    Index = byte_size(DataSection) - byte_size(CurChunk),
    %ets:insert(Indices, {Index}).
    ok.

check_for_data_section_loop(DataSection, CurChunk, RecordsAbove) ->
    CurIndex = byte_size(DataSection) - byte_size(CurChunk),
    case maps:put(CurIndex, visited, RecordsAbove) of
        UpdatedRecordsAbove
          when map_size(UpdatedRecordsAbove) =:= map_size(RecordsAbove) ->
            error({data_section, CurChunk, {loop,RecordsAbove}});
        UpdatedRecordsAbove ->
            UpdatedRecordsAbove
    end.

is_utf8_binary(<<0:1,_:7, Next/bytes>>) ->
    is_utf8_binary(Next);
is_utf8_binary(<<6:3,_:5, 2:2,_:6, Next/bytes>>) ->
    is_utf8_binary(Next);
is_utf8_binary(<<14:4,_:4, 2:2,_:6, 2:2,_:6, Next/bytes>>) ->
    is_utf8_binary(Next);
is_utf8_binary(<<30:5,_:3, 2:2,_:6, 2:2,_:6, 2:2,_:6, Next/bytes>>) ->
    is_utf8_binary(Next);
is_utf8_binary(<<>>) ->
    true;
is_utf8_binary(<<_/bytes>>) ->
    false.


analyze_tree(DatabaseParts, DataSectionIndices) ->
    #{tree := Tree} = DatabaseParts,
    NodeCount = metadata_get(<<"node_count">>, DatabaseParts),
    RecordSize = metadata_get(<<"record_size">>, DatabaseParts),
    NodeSize = (RecordSize * 2) div 8,
    MaxDepth =
        case metadata_get(<<"ip_version">>, DatabaseParts) of
            4 -> 32;
            6 -> 128
        end,

    Params =
        #{ tree => Tree,
           node_size => NodeSize,
           record_size => RecordSize,
           node_count => NodeCount,
           max_depth => MaxDepth,
           data_section_indices => DataSectionIndices
         },
    analyze_tree_recur(Params, 0, 0, 0).

analyze_tree_recur(#{max_depth := MaxDepth}, NodeIndex, Depth, Prefix)
  when Depth > MaxDepth ->
    error({tree, {NodeIndex,Depth,Prefix}, max_depth_exceeded});
analyze_tree_recur(#{node_count := NodeCount} = Params, NodeIndex, Depth, Prefix)
  when NodeIndex < NodeCount ->
    % regular node
    #{tree := Tree, node_size := NodeSize, record_size := RecordSize} = Params,
    try binary:part(Tree, {NodeIndex * NodeSize, NodeSize}) of
        Node ->
            {LeftNodeIndex, RightNodeIndex} = extrace_node_records(Node, RecordSize),
            analyze_tree_recur(Params, LeftNodeIndex, Depth + 1, Prefix bsl 1),
            analyze_tree_recur(Params, RightNodeIndex, Depth + 1, (Prefix bsl 1) bor 1)
    catch
        Class:Reason ->
            error({tree, {NodeIndex,Depth,Prefix}, {node_dereference_failed,Class,Reason}})
    end;
analyze_tree_recur(#{node_count := NodeCount}, NodeIndex, _Depth, _Prefix)
  when NodeIndex =:= NodeCount ->
    % leaf node
    ok;
analyze_tree_recur(#{node_count := NodeCount, data_section_indices := DataSectionIndices},
                   NodeIndex, _Depth, _Prefix) ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    case true orelse ets:member(DataSectionIndices, DataIndex) of
        true -> ok;
        false -> error(todo)
    end.

extrace_node_records(Node, RecordSize) when byte_size(Node) band 1 =:= 0 ->
    <<Left:RecordSize, Right:RecordSize>> = Node,
    {Left, Right};
extrace_node_records(Node, RecordSize) ->
    LeftWholeSz = (RecordSize bsr 3) bsl 3,
    LeftRemainderSz = RecordSize band 2#111,
    <<LeftLow:LeftWholeSz, LeftHigh:LeftRemainderSz, Right:RecordSize>> = Node,
    Left = (LeftHigh bsl LeftWholeSz) bor LeftLow,
    {Left, Right}.

analysis_flaw_prefix(MaxDepth, Depth, Prefix) ->
    ShiftAmount = MaxDepth - Depth,
    ShiftedPrefix = Prefix bsl ShiftAmount,
    BitAddress = <<ShiftedPrefix:MaxDepth>>,
    ip_address_prefix(BitAddress, ShiftAmount).

