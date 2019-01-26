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

%% @reference
%%
%% * [MaxMind DB File Format Specification](https://maxmind.github.io/MaxMind-DB/)

-module(locus_mmdb).
-compile([inline_list_funcs]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create_table/1]).
-export([decode_and_update/3]).
-export([lookup/2]).
-export([get_parts/1]).
-export([analyze/1]).

-ifdef(TEST).
-export([decode_database_parts/2]).
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
-define(int32, 8).
-define(uint64, 9).
-define(uint128, 10).
-define(map, 7).
-define(array, 11).
-define(data_cache_container, 12).
-define(end_marker, 13).
-define(boolean, 14).
-define(float, 15).

-define(assert(Cond, Error), ((Cond) orelse error((Error)))).

% https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
-define(IPV4_IPV6_PREFIX, <<0:80, 16#FFFF:16>>).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type bin_database() :: <<_:64,_:_*8>>.
-export_type([bin_database/0]).

-type source() ::
        http_loader_source() |
        filesystem_loader_source().
-export_type([source/0]).

-type http_loader_source() ::
        {cache, Path :: string()} |
        {remote, URL :: string()}.
-export_type([http_loader_source/0]).

-type filesystem_loader_source() ::
        {filesystem, Path :: string()}.
-export_type([filesystem_loader_source/0]).

-ifdef(POST_OTP_18).
-type parts() ::
        #{ tree := binary(),
           data_section := binary(),
           metadata := metadata(),
           ipv4_root_index := non_neg_integer(),
           source := string(),
           version := calendar:datetime()
         }.
-else.
-type parts() ::
        #{ tree => binary(),
           data_section => binary(),
           metadata => metadata(),
           ipv4_root_index => non_neg_integer(),
           source => string(),
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
        bad_data_record_type() |
        data_record_decoding_failed().
-export_type([analysis_flaw/0]).

-type max_depth_exceeded() ::
        {max_depth_exceeded, #{ tree_prefix := {inet:ip_address(), 0..128},
                                node_index := non_neg_integer(),
                                depth := 33 | 129 }}.
-export_type([max_depth_exceeded/0]).

-type node_dereference_failed() ::
        {node_dereference_failed, #{ tree_prefix := {inet:ip_address(), 0..128},
                                     node_index := non_neg_integer(),
                                     class := error | throw | exit,
                                     reason := term() }}.
-export_type([node_dereference_failed/0]).

-type bad_data_record_type() ::
         {bad_data_record_type, #{ tree_prefix := {inet:ip_address(), 0..128},
                                   data_index := non_neg_integer(),
                                   data_record := term() }}.
-export_type([bad_data_record_type/0]).

-type data_record_decoding_failed() ::
        {data_record_decoding_failed, #{ tree_prefix := {inet:ip_address(), 0..128},
                                         data_index := non_neg_integer(),
                                         class := error | throw | exit,
                                         reason := term() }}.
-export_type([data_record_decoding_failed/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec create_table(atom()) -> ok.
%% @private
create_table(Id) ->
    Table = table_name(Id),
    _ = ets:new(Table, [named_table, protected, {read_concurrency,true}]),
    ok.

-spec decode_and_update(atom(), bin_database(), source()) -> calendar:datetime().
%% @private
decode_and_update(Id, BinDatabase, Source) ->
    Table = table_name(Id),
    {DatabaseParts, Version} = decode_database_parts(BinDatabase, Source),
    ets:insert(Table, {database, DatabaseParts}),
    Version.

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
%% Internal Function Definitions - Initialization
%% ------------------------------------------------------------------

-spec table_name(atom()) -> atom().
table_name(Id) ->
    list_to_atom("locus_mmdb_" ++ atom_to_list(Id)).

-spec decode_database_parts(bin_database(), source()) -> {parts(), calendar:datetime()}.
decode_database_parts(BinDatabase, Source) ->
    BinMetadataMarkerParts = binary:matches(BinDatabase, <<?METADATA_MARKER>>),
    {BinMetadataStart, _BinMetadataMarkerLength} = lists:last(BinMetadataMarkerParts),
    <<TreeAndDataSection:BinMetadataStart/binary, ?METADATA_MARKER, BinMetadata/binary>>
        = BinDatabase,
    Metadata = decode_metadata(BinMetadata),
    RecordSize = maps:get(<<"record_size">>, Metadata),
    NodeCount = maps:get(<<"node_count">>, Metadata),
    BuildEpoch = maps:get(<<"build_epoch">>, Metadata),
    FmtMajorVersion = maps:get(<<"binary_format_major_version">>, Metadata),
    FmtMinorVersion = maps:get(<<"binary_format_minor_version">>, Metadata),
    ?assert(is_known_database_format(FmtMajorVersion, FmtMinorVersion),
            {unknown_database_format_version, FmtMajorVersion, FmtMinorVersion}),
    TreeSize = ((RecordSize * 2) div 8) * NodeCount,
    <<Tree:TreeSize/binary, 0:128, DataSection/binary>> = TreeAndDataSection,
    IPv4RootIndex = find_ipv4_root_index(Tree, Metadata),
    Version = epoch_to_datetime(BuildEpoch),
    DatabaseParts = #{ tree => Tree, data_section => DataSection,
                       metadata => Metadata, ipv4_root_index => IPv4RootIndex,
                       source => Source, version => Version },
    {DatabaseParts, Version}.

-spec decode_metadata(binary()) -> metadata().
decode_metadata(BinMetadata) ->
    {Metadata, _FinalIndex} = decode_data(BinMetadata, 0),
    Metadata.

is_known_database_format(FmtMajorVersion, FmtMinorVersion) ->
    lists:member({FmtMajorVersion, FmtMinorVersion},
                 [{2,0}]).

-spec epoch_to_datetime(integer()) -> calendar:datetime().
epoch_to_datetime(Epoch) ->
    GregorianEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    calendar:gregorian_seconds_to_datetime(GregorianEpoch + Epoch).

decode_data(Data, Index) ->
    case binary:part(Data, {Index,1}) of
        <<0:3, SizeTag:5>> ->
            ExtendedType = binary:at(Data, Index + 1),
            Type = 7 + ExtendedType,
            NewIndex = Index + 2,
            decode_data_payload_size(Type, SizeTag, Data, NewIndex);
        <<Type:3, SizeTag:5>> ->
            NewIndex = Index + 1,
            decode_data_payload_size(Type, SizeTag, Data, NewIndex)
    end.

decode_data_payload_size(Type, SizeTag, Data, Index) when SizeTag < 29 ->
    PayloadSize = SizeTag,
    decode_data_payload(Type, PayloadSize, Data, Index);
decode_data_payload_size(Type, SizeTag, Data, Index) when SizeTag =:= 29 ->
    <<ExtendedSize>> = binary:part(Data, {Index,1}),
    PayloadSize = 29 + ExtendedSize,
    NewIndex = Index + 1,
    decode_data_payload(Type, PayloadSize, Data, NewIndex);
decode_data_payload_size(Type, SizeTag, Data, Index) when SizeTag =:= 30 ->
    <<ExtendedSize:16>> = binary:part(Data, {Index,2}),
    PayloadSize = 285 + ExtendedSize,
    NewIndex = Index + 2,
    decode_data_payload(Type, PayloadSize, Data, NewIndex);
decode_data_payload_size(Type, SizeTag, Data, Index) when SizeTag =:= 31 ->
    <<ExtendedSize:24>> = binary:part(Data, {Index,3}),
    PayloadSize = 65821 + ExtendedSize,
    NewIndex = Index + 3,
    decode_data_payload(Type, PayloadSize, Data, NewIndex).

decode_data_payload(Type, Size, Data, Index)
  when Type =:= ?pointer ->
    <<SS:2, VV:3>> = <<Size:5>>,
    {DataIndex, NewIndex} = decode_pointer_index(SS, VV, Data, Index),
    {Value, _DiscardedIndex} = decode_data(Data, DataIndex),
    {Value, NewIndex};
decode_data_payload(Type, Size, Data, Index)
  when Type =:= ?utf8_string ->
    Text = binary:part(Data, {Index, Size}),
    CopiedText = binary:copy(Text),
    ?assert(is_utf8_text(CopiedText), {not_utf8_printable_text, CopiedText}),
    {CopiedText, Index + Size};
decode_data_payload(Type, Size, Data, Index)
  when (Type =:= ?double andalso Size =:= 8);
       (Type =:= ?float andalso Size =:= 4) ->
    BitSize = Size * 8,
    <<Float:BitSize/float>> = binary:part(Data, {Index, Size}),
    {Float, Index + Size};
decode_data_payload(Type, Size, Data, Index) when Type =:= ?bytes ->
    Bytes = binary:part(Data, {Index, Size}),
    CopiedBytes = binary:copy(Bytes),
    {CopiedBytes, Index + Size};
decode_data_payload(Type, Size, Data, Index)
  when (Type =:= ?uint16 andalso Size =< 2);
       (Type =:= ?uint32 andalso Size =< 4);
       (Type =:= ?uint64 andalso Size =< 8);
       (Type =:= ?uint128 andalso Size =< 16) ->
    <<Integer:Size/integer-unit:8>> = binary:part(Data, {Index, Size}),
    {Integer, Index + Size};
decode_data_payload(Type, Size, Data, Index)
  when Type =:= ?int32, Size =< 4 ->
    <<Integer:Size/signed-integer-unit:8>> = binary:part(Data, {Index, Size}),
    {Integer, Index + Size};
decode_data_payload(Type, Size, Data, Index)
  when Type =:= ?map ->
    lists:foldl(
      fun (_Counter, {MapAcc1, IndexAcc1}) ->
              {Key, IndexAcc2} = decode_data(Data, IndexAcc1),
              ?assert(is_utf8_text(Key), {invalid_map_key, Key}),
              ?assert(not maps:is_key(Key, MapAcc1), {repeated_map_key, Key}),
              {Value, IndexAcc3} = decode_data(Data, IndexAcc2),
              MapAcc2 = maps:put(Key, Value, MapAcc1),
              {MapAcc2, IndexAcc3}
      end,
      {#{}, Index},
      lists:seq(1, Size));
decode_data_payload(Type, Size, Data, Index) when Type =:= ?array ->
    lists:mapfoldl(
      fun (_Counter, IndexAcc) ->
              decode_data(Data, IndexAcc)
      end,
      Index, lists:seq(1, Size));
decode_data_payload(Type, _Size, _Data, _Index) when Type =:= ?data_cache_container ->
    error({unsupported_data_type, data_cache_container});
decode_data_payload(Type, Size, Data, _Index) when Type =:= ?end_marker, Size =:= 0, Data =:= <<>> ->
    error({unsupported_data_type, end_marker});
decode_data_payload(Type, Size, _Data, Index) when Type =:= ?boolean, Size =:= 0 ->
    {false, Index};
decode_data_payload(Type, Size, _Data, Index) when Type =:= ?boolean, Size =:= 1 ->
    {true, Index}.

decode_pointer_index(SS, VV, Data, Index) when SS =:= 0 ->
    <<ExtendedVV>> = binary:part(Data, {Index,1}),
    <<Offset:11>> = <<VV:3, ExtendedVV:8>>,
    {Offset, Index + 1};
decode_pointer_index(SS, VV, Data, Index) when SS =:= 1 ->
    <<ExtendedVV:16>> = binary:part(Data, {Index,2}),
    <<BaseOffset:19>> = <<VV:3, ExtendedVV:16>>,
    {BaseOffset + 2048, Index + 2};
decode_pointer_index(SS, VV, Data, Index) when SS =:= 2 ->
    <<ExtendedVV:24>> = binary:part(Data, {Index,3}),
    <<BaseOffset:27>> = <<VV:3, ExtendedVV:24>>,
    {BaseOffset + 526336, Index + 3};
decode_pointer_index(SS, _VV, Data, Index) when SS =:= 3 ->
    <<Offset:32>> = binary:part(Data, {Index,4}),
    {Offset, Index + 4}.

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
    % end of the line
    {error, not_found};
lookup_recur(BitAddress, _Tree, DataSection, _NodeSize, _RecordSize,
             NodeIndex, NodeCount) ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    {#{} = DecodedData, _NewDataIndex} = decode_data(DataSection, DataIndex),
    SuffixSize = bit_size(BitAddress),
    {ok, DecodedData, SuffixSize}.

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

is_utf8_text(<<Binary/binary>>) ->
    case unicode:characters_to_list(Binary, utf8) of
        String when is_list(String) ->
            io_lib:printable_unicode_list(String);
        _Failure ->
            false
    end;
is_utf8_text(_Value) ->
    false.

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
%% Internal Function Definitions - Walking
%% ------------------------------------------------------------------

analyze_(false) ->
    {error, database_unknown};
analyze_([]) ->
    {error, database_not_loaded};
analyze_([{database, DatabaseParts}]) ->
    #{ tree := Tree, data_section := DataSection } = DatabaseParts,
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
           data_section => DataSection,
           node_size => NodeSize,
           record_size => RecordSize ,
           node_count => NodeCount,
           max_depth => MaxDepth
         },

    case analyze_recur(Params, 0, 0, 0, [], gb_sets:empty()) of
        {[], _} ->
            ok;
        {RevFlaws, _} ->
            Flaws = lists:reverse(RevFlaws),
            {error, {flawed, Flaws}}
    end.

analyze_recur(#{max_depth := MaxDepth} = Params, NodeIndex, Depth, Prefix,
              FlawsAcc, VisitedDataRecordsAcc)
  when Depth > MaxDepth ->
    {[{max_depth_exceeded, #{ tree_prefix => analysis_flaw_prefix(Params, Depth, Prefix),
                              node_index => NodeIndex }}
      | FlawsAcc],
     VisitedDataRecordsAcc};
analyze_recur(#{node_count := NodeCount} = Params, NodeIndex, Depth, Prefix,
              FlawsAcc, VisitedDataRecordsAcc)
  when NodeIndex < NodeCount ->
    % regular node
    #{tree := Tree, node_size := NodeSize, record_size := RecordSize} = Params,
    try binary:part(Tree, {NodeIndex * NodeSize, NodeSize}) of
        Node ->
            {LeftNodeIndex, RightNodeIndex} = extrace_node_records(Node, RecordSize),
            {FlawsAcc2, VisitedDataRecordsAcc2} =
                analyze_recur(Params, LeftNodeIndex, Depth + 1, Prefix bsl 1,
                              FlawsAcc, VisitedDataRecordsAcc),
            analyze_recur(Params, RightNodeIndex, Depth + 1, (Prefix bsl 1) bor 1,
                          FlawsAcc2, VisitedDataRecordsAcc2)
    catch
        Class:Reason ->
            {[{node_dereference_failed, #{ tree_prefix => analysis_flaw_prefix(Params, Depth, Prefix),
                                           node_index => NodeIndex,
                                           class => Class,
                                           reason => Reason }}
              | FlawsAcc],
             VisitedDataRecordsAcc}
    end;
analyze_recur(#{node_count := NodeCount}, NodeIndex, _Depth, _Prefix,
              FlawsAcc, VisitedDataRecordsAcc)
  when NodeIndex =:= NodeCount ->
    % leaf node
    {FlawsAcc, VisitedDataRecordsAcc};
analyze_recur(#{node_count := NodeCount, data_section := DataSection} = Params,
              NodeIndex, Depth, Prefix,
              FlawsAcc, VisitedDataRecordsAcc) ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    try gb_sets:is_element(DataIndex, VisitedDataRecordsAcc) orelse
        {first_visit, decode_data(DataSection, DataIndex)}
    of
        true ->
            % we've already visited the data record referenced by DataIndex
            {FlawsAcc, VisitedDataRecordsAcc};
        {first_visit, {#{} = _DecodedData, _NewDataIndex}} ->
            {FlawsAcc, VisitedDataRecordsAcc};
        {first_visit, {DecodedDataOfWrongType, _NewDataIndex}} ->
            {[{bad_data_record_type, #{ tree_prefix => analysis_flaw_prefix(Params, Depth, Prefix),
                                        data_index => DataIndex,
                                        data_record => DecodedDataOfWrongType }}
              | FlawsAcc],
             gb_sets:add(DataIndex, VisitedDataRecordsAcc)}
    catch
        Class:Reason ->
            {[{data_record_decoding_failed, #{ tree_prefix => analysis_flaw_prefix(Params, Depth, Prefix),
                                               data_index => DataIndex,
                                               class => Class,
                                               reason => Reason }}
              | FlawsAcc],
             gb_sets:add(DataIndex, VisitedDataRecordsAcc)}
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

analysis_flaw_prefix(#{max_depth := MaxDepth}, Depth, Prefix) ->
    ShiftAmount = MaxDepth - Depth,
    ShiftedPrefix = Prefix bsl ShiftAmount,
    BitAddress = <<ShiftedPrefix:MaxDepth>>,
    ip_address_prefix(BitAddress, ShiftAmount).
