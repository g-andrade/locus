%% Copyright (c) 2017 Guilherme Andrade <locus.lib@gandrade.net>
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

%% @private
-module(locus_mmdb).
-compile([inline, inline_list_funcs]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([create_table/1]).
-export([decode_and_update/2]).
-export([database_type/1]).
-export([lookup/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(is_ip_address(V), (?is_ip4_address((V)) orelse ?is_ip6_address((V)))).

-define(is_ip4_address(V), (is_tuple((V)) andalso tuple_size((V)) =:= 4 andalso
                            ?is_uint8(element(1, ((V)))) andalso ?is_uint8(element(2, (V))) andalso
                            ?is_uint8(element(3, ((V)))) andalso ?is_uint8(element(4, (V))))).

-define(is_ip6_address(V), (is_tuple((V)) andalso tuple_size((V)) =:= 8 andalso
                            ?is_uint16(element(1, ((V)))) andalso ?is_uint16(element(2, (V))) andalso
                            ?is_uint16(element(3, ((V)))) andalso ?is_uint16(element(4, (V))) andalso
                            ?is_uint16(element(5, ((V)))) andalso ?is_uint16(element(7, (V))) andalso
                            ?is_uint16(element(6, ((V)))) andalso ?is_uint16(element(8, (V))))).

-define(is_uint8(V), (is_integer((V)) andalso ((V) band 16#FF =:= (V)))).
-define(is_uint16(V), (is_integer((V)) andalso ((V) band 16#FFFF =:= (V)))).

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

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type bin_database() :: <<_:64,_:_*8>>.
-export_type([bin_database/0]).

-type parts() ::
        #{ tree => binary(),
           data_section => binary(),
           metadata => metadata()
         }.
-export_type([parts/0]).

-type metadata() ::
        #{ atom() => term() }.
-export_type([metadata/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec create_table(atom()) -> ok.
create_table(Id) ->
    Table = table_name(Id),
    _ = ets:new(Table, [named_table, protected, {read_concurrency,true}]),
    ok.

-spec decode_and_update(atom(), bin_database()) -> calendar:datetime().
decode_and_update(Id, BinDatabase) ->
    Table = table_name(Id),
    {DatabaseParts, Version} = decode_database_parts(BinDatabase),
    ets:insert(Table, {database, DatabaseParts}),
    Version.

-spec database_type(atom()) -> {ok, binary()} | {error, database_unknown | database_not_loaded}.
database_type(Id) ->
    Table = table_name(Id),
    case ets:info(Table, name) =:= Table andalso
         ets:lookup(Table, database)
    of
        false -> {error, database_unknown};
        [] -> {error, database_not_loaded};
        [{database, #{ metadata := Metadata }}] ->
            {ok, maps:get(database_type, Metadata)}
    end.

-spec lookup(atom(), inet:ip_address() | nonempty_string() | binary())
        -> {ok, metadata(), #{}} |
           {error, (not_found | invalid_address | ipv4_database |
                    database_unknown | database_not_loaded)}.
lookup(Id, Address) when ?is_ip_address(Address) ->
    Table = table_name(Id),
    DatabaseLookup = (ets:info(Table, name) =:= Table andalso
                      ets:lookup(Table, database)),
    lookup_(DatabaseLookup, Address);
lookup(Id, Binary) when is_binary(Binary) ->
    String = binary_to_list(Binary),
    lookup(Id, String);
lookup(Id, String) when is_list(String) ->
    case inet:parse_strict_address(String) of
        {ok, Address} ->
            lookup(Id, Address);
        {error, einval} ->
            {error, invalid_address}
    end;
lookup(_Id, _Other) ->
    {error, invalid_address}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Initialization
%% ------------------------------------------------------------------

-spec table_name(atom()) -> atom().
table_name(Id) ->
    list_to_atom("locus_mmdb_" ++ atom_to_list(Id)).

-spec decode_database_parts(bin_database()) -> {parts(), calendar:datetime()}.
decode_database_parts(BinDatabase) ->
    BinMetadataMarkerParts = binary:matches(BinDatabase, <<?METADATA_MARKER>>),
    {BinMetadataStart, _BinMetadataMarkerLength} = lists:last(BinMetadataMarkerParts),
    <<TreeAndDataSection:BinMetadataStart/binary, ?METADATA_MARKER, BinMetadata/binary>>
        = BinDatabase,
    Metadata = decode_metadata(BinMetadata),
    RecordSize = maps:get(record_size, Metadata),
    NodeCount = maps:get(node_count, Metadata),
    BuildEpoch = maps:get(build_epoch, Metadata),
    TreeSize = ((RecordSize * 2) div 8) * NodeCount,
    <<Tree:TreeSize/binary, 0:128, DataSection/binary>> = TreeAndDataSection,
    DatabaseParts = #{ tree => Tree, data_section => DataSection, metadata => Metadata },
    Version = epoch_to_datetime(BuildEpoch),
    {DatabaseParts, Version}.

-spec decode_metadata(binary()) -> metadata().
decode_metadata(BinMetadata) ->
    {Metadata, _FinalIndex} = decode_data(BinMetadata, 0),
    Metadata.

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
    ValidatedText = unicode:characters_to_binary(CopiedText, utf8),
    {ValidatedText, Index + Size};
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
              {<<BinKey/binary>>, IndexAcc2} = decode_data(Data, IndexAcc1),
              Key = binary_to_atom(BinKey, utf8),
              {Value, IndexAcc3} = decode_data(Data, IndexAcc2),
              false = maps:is_key(Key, MapAcc1),
              MapAcc2 = MapAcc1#{ Key => Value },
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

%% ------------------------------------------------------------------
%% Internal Function Definitions - Looking Up
%% ------------------------------------------------------------------

metadata_get(Key, #{ metadata := Metadata } = _DatabaseParts) ->
    maps:get(Key, Metadata).

ip_address_to_bitstring({A,B,C,D}, 4 = _IpVersion) ->
    {ok, <<A,B,C,D>>};
ip_address_to_bitstring({A,B,C,D}, 6 = _IpVersion) ->
    % https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
    {ok, <<0:80, 16#FFFF:16, A,B,C,D>>};
ip_address_to_bitstring({_,_,_,_,_,_,_,_}, 4 = _IpVersion) ->
    {error, ipv4_database};
ip_address_to_bitstring({A,B,C,D,E,F,G,H}, 6 = _IpVersion) ->
    {ok,<<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>}.

lookup_(false, _Address) ->
    {error, database_unknown};
lookup_([] = _DatabaseLookup, _Address) ->
    {error, database_not_loaded};
lookup_([{database, DatabaseParts}] = _DatabaseLookup, Address) ->
    IpVersion = metadata_get(ip_version, DatabaseParts),
    case ip_address_to_bitstring(Address, IpVersion) of
        {ok, BitAddress} ->
            #{ tree := Tree, data_section := DataSection } = DatabaseParts,
            NodeCount = metadata_get(node_count, DatabaseParts),
            RecordSize = metadata_get(record_size, DatabaseParts),
            NodeSize = (RecordSize * 2) div 8,
            case lookup_recur(BitAddress, Tree, DataSection,
                              NodeSize, RecordSize, 0, NodeCount)
            of
                {ok, Entry} ->
                    Metadata = maps:get(metadata, DatabaseParts),
                    {ok, Metadata, Entry};
                Other -> Other
            end;
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
lookup_recur(_BitAddress, _Tree, DataSection, _NodeSize, _RecordSize,
             NodeIndex, NodeCount) ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    {#{} = DecodedData, _NewDataIndex} = decode_data(DataSection, DataIndex),
    {ok, DecodedData}.

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
