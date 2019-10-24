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

%% @private
-module(locus_mmdb_tree).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [find_ipv4_root_index/2,
    lookup/4,
    foldl/4,
    bitstring_ip_address_prefix/2
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
-define(IPV4_IPV6_PREFIX, <<0:80, 16#FFFF:16>>).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type index() :: non_neg_integer().
-export_type([index/0]).

-type ip_address_prefix() :: ip4_address_prefix() | ip6_address_prefix().
-type ip4_address_prefix() :: {inet:ip4_address(), 0..32}.
-type ip6_address_prefix() :: {inet:ip6_address(), 0..128}.
-export_type([ip_address_prefix/0, ip4_address_prefix/0, ip6_address_prefix/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec find_ipv4_root_index(locus_mmdb:metadata(), binary()) -> index().
find_ipv4_root_index(#{<<"ip_version">> := 4}, _) ->
    0;
find_ipv4_root_index(#{<<"ip_version">> := 6} = Metadata, Tree) ->
    find_node_index_for_prefix(?IPV4_IPV6_PREFIX, Metadata, Tree).

-spec lookup(Address, IPv4RootIndex, Metadata, Tree) -> {ok, Success} | {error, Reason}
        when Address :: inet:ip_address(),
             IPv4RootIndex :: index(),
             Metadata :: locus_mmdb:metadata(),
             Tree :: binary(),
             Success :: {locus_mmdb_data:index(), ip_address_prefix()},
             Reason :: ipv4_database | not_found .
lookup(Address, IPv4RootIndex, Metadata, Tree) ->
    case ip_address_to_bitstring(Address, IPv4RootIndex, Metadata) of
        {ok, BitAddress, RootNodeIndex} ->
            lookup_(BitAddress, RootNodeIndex, Metadata, Tree);
        {error, Reason} ->
            {error, Reason}
    end.

-spec foldl(Fun, Acc0, Metadata, Tree) -> AccN
        when Fun :: fun ((IntegerPrefix, TreeDepth, NodeIndex, DataIndex, Acc1) -> Acc2),
             IntegerPrefix :: non_neg_integer(),
             TreeDepth :: non_neg_integer(),
             NodeIndex :: integer(),
             DataIndex :: locus_mmdb_data:index(),
             Acc0 :: term(),
             Acc1 :: term(),
             Acc2 :: term(),
             AccN :: term(),
             Metadata :: locus_mmdb:metadata(),
             Tree :: binary().
foldl(Fun, Acc0, Metadata, Tree) ->
    #{<<"node_count">> := NodeCount, <<"record_size">> := RecordSize} = Metadata,
    NodeSize = (RecordSize * 2) div 8,
    RootNodeIndex = 0,
    Params =
        #{ node_size => NodeSize,
           record_size => RecordSize,
           node_count => NodeCount
         },
    foldl_recur(Fun, 0, 0, Acc0, Tree, RootNodeIndex, Params).

-spec bitstring_ip_address_prefix(<<_:32>>,  0..32)  -> ip4_address_prefix();
                                 (<<_:128>>, 0..128) -> ip6_address_prefix().
bitstring_ip_address_prefix(BitAddress, SuffixSize) when bit_size(BitAddress) =:= 32 ->
    PrefixSize = 32 - SuffixSize,
    <<Prefix:PrefixSize/bits, _Suffix/bits>> = BitAddress,
    BitBaseAddress = <<Prefix/bits, 0:SuffixSize>>,
    <<A,B,C,D>> = BitBaseAddress,
    {{A,B,C,D}, PrefixSize};
bitstring_ip_address_prefix(BitAddress, SuffixSize) when bit_size(BitAddress) =:= 128 ->
    PrefixSize = 128 - SuffixSize,
    <<Prefix:PrefixSize/bits, _Suffix/bits>> = BitAddress,
    BitBaseAddress = <<Prefix/bits, 0:SuffixSize>>,
    <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>> = BitBaseAddress,
    {{A,B,C,D,E,F,G,H}, PrefixSize}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Looking Up
%% ------------------------------------------------------------------

find_node_index_for_prefix(Bitstring, Metadata, Tree) ->
    #{<<"node_count">> := NodeCount, <<"record_size">> := RecordSize} = Metadata,
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

ip_address_to_bitstring({A,B,C,D}, IPv4RootIndex, _) ->
    {ok, <<A,B,C,D>>, IPv4RootIndex};
ip_address_to_bitstring({A,B,C,D,E,F,G,H}, _, Metadata) ->
    case maps:get(<<"ip_version">>, Metadata) of
        4 -> {error, ipv4_database};
        6 -> {ok, <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>, 0}
    end.

%% @private
lookup_(BitAddress, RootNodeIndex, Metadata, Tree) ->
    #{<<"node_count">> := NodeCount, <<"record_size">> := RecordSize} = Metadata,
    NodeSize = (RecordSize * 2) div 8,
    case lookup_recur(BitAddress, Tree, NodeSize, RecordSize, RootNodeIndex, NodeCount) of
        {ok, {DataIndex, SuffixSize}} ->
            Prefix = bitstring_ip_address_prefix(BitAddress, SuffixSize),
            {ok, {DataIndex, Prefix}};
        {error, Reason} ->
            {error, Reason}
    end.

lookup_recur(<<Bit:1,NextBits/bits>>, Tree, NodeSize, RecordSize, NodeIndex, NodeCount)
  when NodeIndex < NodeCount ->
    % regular node
    Node = binary:part(Tree, {NodeIndex * NodeSize, NodeSize}),
    ChildNodeIndex = extract_node_record(Bit, Node, RecordSize),
    lookup_recur(NextBits, Tree, NodeSize, RecordSize, ChildNodeIndex, NodeCount);
lookup_recur(_BitAddress, _Tree, _NodeSize, _RecordSize, NodeIndex, NodeCount)
  when NodeIndex =:= NodeCount ->
    % leaf node
    {error, not_found};
lookup_recur(BitAddress, _Tree, _NodeSize, _RecordSize, NodeIndex, NodeCount)
  when NodeIndex >= (NodeCount + 16) ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    SuffixSize = byte_size(BitAddress),
    {ok, {DataIndex, SuffixSize}}.

extract_node_record(0, Node, RecordSize) ->
    extract_left_node_record(Node, RecordSize);
extract_node_record(1, Node, RecordSize) ->
    extract_right_node_record(Node, RecordSize).

extract_left_node_record(Node, RecordSize) when byte_size(Node) band 1 =:= 0 ->
    <<Left:RecordSize, _/bits>> = Node,
    Left;
extract_left_node_record(Node, RecordSize) ->
    LeftWholeSz = (RecordSize bsr 3) bsl 3,
    LeftRemainderSz = RecordSize band 2#111,
    <<LeftLow:LeftWholeSz, LeftHigh:LeftRemainderSz, _/bits>> = Node,
    (LeftHigh bsl LeftWholeSz) bor LeftLow.

extract_right_node_record(Node, RecordSize) ->
    <<_:RecordSize, Right:RecordSize>> = Node,
    Right.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Folding
%% ------------------------------------------------------------------

foldl_recur(Fun, IntegerPrefix, Depth, Acc, Tree, NodeIndex, #{node_count := NodeCount} = Params)
  when NodeIndex < NodeCount ->
    % regular node
    #{node_size := NodeSize, record_size := RecordSize} = Params,
    Node = binary:part(Tree, {NodeIndex * NodeSize, NodeSize}),
    LeftChildIndex = extract_left_node_record(Node, RecordSize),
    LeftIntegerPrefix = IntegerPrefix bsl 1,
    RightChildIndex = extract_right_node_record(Node, RecordSize),
    RightIntegerPrefix = LeftIntegerPrefix bor 1,
    Acc2 = foldl_recur(Fun, LeftIntegerPrefix,  Depth+1, Acc,  Tree, LeftChildIndex,  Params),
    _    = foldl_recur(Fun, RightIntegerPrefix, Depth+1, Acc2, Tree, RightChildIndex, Params);
foldl_recur(_, _, _, Acc, _, NodeIndex, #{node_count := NodeCount})
  when NodeIndex =:= NodeCount ->
    % leaf node
    Acc;
foldl_recur(Fun, IntegerPrefix, Depth, Acc, _, NodeIndex, #{node_count := NodeCount})
  when NodeIndex >= NodeCount ->
    % pointer to the data section
    DataIndex = (NodeIndex - NodeCount) - 16,
    Fun(IntegerPrefix, Depth, NodeIndex, DataIndex, Acc).
