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

%% @reference <a target="_parent" href="https://maxmind.github.io/MaxMind-DB/">
%% MaxMind DB File Format Specification</a>

%% @doc API for working with MMDB - tree section
-module(locus_mmdb_tree).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/5, lookup/2]).

%% ------------------------------------------------------------------
%% "Private" API Function Exports
%% ------------------------------------------------------------------

-export([validate/4]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

% https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
-define(IPV4_IPV6_PREFIX, <<0:80, 16#FFFF:16>>).

%% ------------------------------------------------------------------
%% API Record and Type Definitions
%% ------------------------------------------------------------------

-type bad_tree_error()
    :: {invalid_node_index_for_ipv4_root, map()}
    |  {ipv4_root_data_index_out_of_range, map()}.
-export_type([bad_tree_error/0]).

-record(tree, {
          data :: binary(),
          node_count :: non_neg_integer(),
          record_size :: non_neg_integer(),
          ip_version :: 4 | 6,
          node_size :: non_neg_integer(),
          ipv4_root_index :: ipv4_root_index()
         }).
-opaque t() :: #tree{}.
-export_type([t/0]).

-type index() :: non_neg_integer().
-export_type([index/0]).

-type ip_address_prefix() :: ip4_address_prefix() | ip6_address_prefix().
-type ip4_address_prefix() :: {inet:ip4_address(), 0..32}.
-type ip6_address_prefix() :: {inet:ip6_address(), 0..128}.
-export_type([ip_address_prefix/0, ip4_address_prefix/0, ip6_address_prefix/0]).

%% ------------------------------------------------------------------
%% Internal Record and Type Definitions
%% ------------------------------------------------------------------

-type ipv4_root_index()
    :: {tree_index, index()}
    |  none
    |  {data_index, locus_mmdb_data_codec:index()}.

-record(validation_aux, {
          node_count :: non_neg_integer(),
          node_size :: non_neg_integer(),
          record_size :: non_neg_integer(),
          callback :: fun ((locus_mmdb_data_codec:index()) -> ok | {error, term()}),
          walk_manager :: locus_mmdb_tree_walk_manager:handle(),
          journal :: locus_mmdb_check_journal:t(),
          data :: binary(),
          max_depth :: 128|32
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Instantiates a new `Tree' out of `TreeData', a few
%% metadata properties and `DataSectionSize'
-spec new(TreeData, NodeCount, RecordSize, IpVersion, DataSectionSize)
        -> {ok, Tree} | {error, Reason}
    when TreeData :: binary(),
         NodeCount :: non_neg_integer(),
         RecordSize :: non_neg_integer(),
         IpVersion :: 4|6,
         DataSectionSize :: non_neg_integer(),
         Tree :: t(),
         Reason :: term().
new(TreeData, NodeCount, RecordSize, IpVersion, DataSectionSize) ->
    NodeSize = (RecordSize * 2) div 8,
    ExpectedDataSize = NodeCount * NodeSize,
    ActualDataSize = byte_size(TreeData),
    ?assertMatch({true, _, _}, {ExpectedDataSize =:= ActualDataSize,
                                ExpectedDataSize, ActualDataSize}),

    case find_ipv4_root_index(TreeData, NodeCount, RecordSize, IpVersion,
                              NodeSize, DataSectionSize) of
        {ok, Ipv4RootIndex} ->
            Tree = #tree{data = TreeData,
                         node_count = NodeCount,
                         record_size = RecordSize,
                         ip_version = IpVersion,
                         node_size = NodeSize,
                         ipv4_root_index = Ipv4RootIndex},
            {ok, Tree};
        {error, _} = Error ->
            Error
    end.

%% @doc Looks up for a `DataIndex' for `Address' within `Tree'
-spec lookup(Address, Tree) -> {ok, DataIndex} | not_found | {error, Reason}
        when Address :: inet:ip_address(),
             Tree :: t(),
             DataIndex :: locus_mmdb_data_codec:index(),
             Reason :: ipv4_database | not_found.
lookup(Address, Tree) ->
    case ip_address_to_bitstring(Address, Tree) of
        {ok, BitAddress, RootIndex} ->
            lookup_bit_address(BitAddress, RootIndex, Tree);
        {error, Reason} ->
            {error, Reason};
        {no_need, DataIndex} ->
            {ok, DataIndex}
    end.

%% ------------------------------------------------------------------
%% "Private" API Function Definitions
%% ------------------------------------------------------------------

-spec validate(fun ((locus_mmdb_data_codec:index()) -> ok | {error, term()}),
               locus_mmdb_tree_walk_manager:handle(),
               locus_mmdb_check_journal:t(),
               t())
    -> ok.
%% @private
validate(Fun, WalkManager, Journal, Tree) ->
    case locus_mmdb_tree_walk_manager:take_index(WalkManager) of
        {ok, NodeIndex, Path} ->
            Depth = tree_depth_from_path(Path),
            validate_(NodeIndex, Fun, WalkManager, Journal, Tree, Path, Depth);
        stop ->
            ok
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Looking Up
%% ------------------------------------------------------------------

find_ipv4_root_index(Data, NodeCount, RecordSize, IpVersion,
                     NodeSize, DataSectionSize) ->
    case IpVersion of
        6 ->
            find_ipv4_root_index_recur(?IPV4_IPV6_PREFIX, Data, NodeCount, RecordSize,
                                       NodeSize, DataSectionSize, _NodeIndex = 0);
        4 ->
            {ok, 0}
    end.

find_ipv4_root_index_recur(<<Bit:1, NextBits/bits>>, Data, NodeCount, RecordSize,
                           NodeSize, DataSectionSize, NodeIndex)
  when NodeIndex < NodeCount ->
    % regular node
    Offset = NodeIndex * NodeSize,
    ChildNodeIndex = extract_node_record(Bit, Offset, Data, RecordSize),
    find_ipv4_root_index_recur(NextBits, Data, NodeCount, RecordSize,
                               NodeSize, DataSectionSize, ChildNodeIndex);
find_ipv4_root_index_recur(<<>>, _Data, NodeCount, _RecordSize,
                           _NodeSize, _DataSectionSize, NodeIndex)
  when NodeIndex < NodeCount ->
    {ok, {tree_index, _RootIndex = NodeIndex}};
find_ipv4_root_index_recur(<<_/bits>>, _Data, NodeCount, _RecordSize,
                           _NodeSize, _DataSectionSize, NodeIndex)
  when NodeIndex =:= NodeCount ->
    % This database does not map any IPv4 addresses
    {ok, none};
find_ipv4_root_index_recur(<<BitsLeft/bits>>, _Data, NodeCount, _RecordSize,
                           _NodeSize, DataSectionSize, NodeIndex) ->
    HowManyBitsLeft = bit_size(BitsLeft),
    HowManyZeroesAfter = 128 - bit_size(?IPV4_IPV6_PREFIX),
    FullBitAddress = <<?IPV4_IPV6_PREFIX/bits, 0:HowManyZeroesAfter>>,
    SuffixSize = HowManyBitsLeft + HowManyZeroesAfter,
    Prefix = bitstring_ip_address_prefix(FullBitAddress, SuffixSize),
    case NodeIndex - NodeCount - 16 of
        DataIndex when DataIndex >= 0, DataIndex < DataSectionSize ->
            {ok, {data_index, DataIndex}};
        InvalidDataIndex ->
            fail_to_find_ipv4_root_index(Prefix, NodeIndex, NodeCount,
                                         InvalidDataIndex, DataSectionSize)
    end.

fail_to_find_ipv4_root_index(Prefix, NodeIndex, NodeCount,
                             InvalidDataIndex, DataSectionSize) ->
    Details = #{for_prefix => Prefix, node_index => NodeIndex, node_count => NodeCount},

    case InvalidDataIndex of
        _ when InvalidDataIndex < 0 ->
            {error, {invalid_node_index_for_ipv4_root, Details}};
        _ when InvalidDataIndex >= DataSectionSize ->
            ExtendedDetails = Details#{data_section_size => DataSectionSize},
            {error, {ipv4_root_data_index_out_of_range, ExtendedDetails}}
    end.

ip_address_to_bitstring({A, B, C, D}, Tree) ->
    case Tree#tree.ipv4_root_index of
        {tree_index, RootIndex} ->
            {ok, <<A, B, C, D>>, RootIndex};
        none ->
            {error, not_found};
        {data_index, DataIndex} ->
            {no_need, DataIndex}
    end;
ip_address_to_bitstring({A, B, C, D, E, F, G, H}, Tree) ->
    case Tree#tree.ip_version of
        6 ->
            RootIndex = 0,
            {ok, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>, RootIndex};
        4 ->
            {error, ipv4_database}
    end.

lookup_bit_address(BitAddress, RootIndex, Tree) ->
    case lookup_bit_address_recur(BitAddress, _NodeIndex = RootIndex, Tree) of
        {ok, DataIndex} ->
            {ok, DataIndex};
        not_found ->
            not_found;
        {error, {node_index_out_of_range, NodeIndex, NrOfBitsLeft}} ->
            SuffixSize = NrOfBitsLeft,
            Prefix = bitstring_ip_address_prefix(BitAddress, SuffixSize),
            {error, {node_index_out_of_range, #{for_prefix => Prefix,
                                                node_index => NodeIndex,
                                                node_count => Tree#tree.node_count}}}
    end.

lookup_bit_address_recur(<<Bit:1, NextBits/bits>>, NodeIndex, Tree)
  when NodeIndex < Tree#tree.node_count ->
    % regular node
    Offset = NodeIndex * Tree#tree.node_size,
    ChildNodeIndex = extract_node_record(Bit, Offset, Tree#tree.data, Tree#tree.record_size),
    lookup_bit_address_recur(NextBits, ChildNodeIndex, Tree);
lookup_bit_address_recur(_BitAddress, NodeIndex, Tree)
  when NodeIndex >= (Tree#tree.node_count + 16) ->
    % pointer to the data section
    DataIndex = (NodeIndex - Tree#tree.node_count) - 16,
    {ok, DataIndex};
lookup_bit_address_recur(_BitAddress, NodeIndex, Tree)
  when NodeIndex =:= Tree#tree.node_count ->
    % leaf node
    not_found;
lookup_bit_address_recur(<<BitsLeft/bits>>, NodeIndex, _Tree) ->
    NrOfBitsLeft = bit_size(BitsLeft),
    {error, {node_index_out_of_range, NodeIndex, NrOfBitsLeft}}.

extract_node_record(0, Offset, Data, RecordSize) ->
    extract_left_node_record(Offset, Data, RecordSize);
extract_node_record(1, Offset, Data, RecordSize) ->
    extract_right_node_record(Offset, Data, RecordSize).

extract_left_node_record(Offset, Data, RecordSize)
 when RecordSize band 2#100 =:= 0 ->
    <<_:Offset/bytes, Left:RecordSize, _/bits>> = Data,
    Left;
extract_left_node_record(Offset, Data, RecordSize) ->
    LeftWholeSz = (RecordSize bsr 3) bsl 3,
    LeftRemainderSz = RecordSize band 2#111,
    <<_:Offset/bytes, LeftLow:LeftWholeSz, LeftHigh:LeftRemainderSz, _/bits>> = Data,
    (LeftHigh bsl LeftWholeSz) bor LeftLow.

extract_right_node_record(Offset, Data, RecordSize) ->
    <<_:Offset/bytes, _Left:RecordSize, Right:RecordSize, _/bytes>> = Data,
    Right.

extract_node_records(Offset, Data, RecordSize)
  when RecordSize band 2#100 =:= 0 ->
    <<_:Offset/bytes, Left:RecordSize, Right:RecordSize, _/bytes>> = Data,
    {Left, Right};
extract_node_records(Offset, Data, RecordSize) ->
    LeftWholeSz = (RecordSize bsr 3) bsl 3,
    LeftRemainderSz = RecordSize band 2#111,
    <<_:Offset/bytes,
      LeftLow:LeftWholeSz, LeftHigh:LeftRemainderSz,
      Right:RecordSize,
      _/bytes>> = Data,
    Left = (LeftHigh bsl LeftWholeSz) bor LeftLow,
    {Left, Right}.

-spec bitstring_ip_address_prefix(<<_:32>> | <<_:128>>,  0..32 | 0..128)
    -> ip4_address_prefix() | ip6_address_prefix().
%% @private
bitstring_ip_address_prefix(BitAddress, SuffixSize) when bit_size(BitAddress) =:= 32 ->
    PrefixSize = 32 - SuffixSize,
    <<Prefix:PrefixSize/bits, _Suffix/bits>> = BitAddress,
    BitBaseAddress = <<Prefix/bits, 0:SuffixSize>>,
    <<A, B, C, D>> = BitBaseAddress,
    {{A, B, C, D}, PrefixSize};
bitstring_ip_address_prefix(BitAddress, SuffixSize) when bit_size(BitAddress) =:= 128 ->
    PrefixSize = 128 - SuffixSize,
    <<Prefix:PrefixSize/bits, _Suffix/bits>> = BitAddress,
    BitBaseAddress = <<Prefix/bits, 0:SuffixSize>>,
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>> = BitBaseAddress,
    {{A, B, C, D, E, F, G, H}, PrefixSize}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Validation
%% ------------------------------------------------------------------

tree_depth_from_path(Path) ->
    floor( math:log2(Path) ) + 1.

validate_(NodeIndex, Fun, WalkManager, Journal, Tree, Path, Depth) ->
    Aux = #validation_aux{node_count = Tree#tree.node_count,
                          node_size = Tree#tree.node_size,
                          record_size = Tree#tree.record_size,
                          callback = Fun,
                          walk_manager = WalkManager,
                          journal = Journal,
                          data = Tree#tree.data,
                          max_depth = max_tree_depth(Tree)},

    _ = validate_recur(NodeIndex, _NotFreshlyReceived = false, Path, Depth, Aux),
   validate(Fun, WalkManager, Journal, Tree).

max_tree_depth(Tree) ->
    case Tree#tree.ip_version of
        6 -> 128;
        4 -> 32
    end.

validate_recur(NodeIndex, NotFreshlyReceived, Path, Depth,
               #validation_aux{node_count = NodeCount,
                               node_size = NodeSize,
                               record_size = RecordSize,
                               walk_manager = WalkManager,
                               data = Data,
                               max_depth = MaxDepth} = Aux)
  when NodeIndex < NodeCount, Depth =< MaxDepth ->
    % Regular node
    case NotFreshlyReceived
         andalso locus_mmdb_tree_walk_manager:maybe_give_index(WalkManager, NodeIndex, Path)
    of
        false ->
            NodeOffset = NodeIndex * NodeSize,
            {Left, Right} = extract_node_records(NodeOffset, Data, RecordSize),
            LeftPath = Path bsl 1,
            RightPath = LeftPath bor 1,
            UpdatedDepth = Depth + 1,
            validate_recur(Left, true, LeftPath, UpdatedDepth, Aux),
            validate_recur(Right, true, RightPath, UpdatedDepth, Aux);
        true ->
            ok
    end;
validate_recur(NodeIndex, _NotFreshlyReceived, Path, _Depth,
               #validation_aux{node_count = NodeCount} = Aux)
  when NodeIndex > NodeCount ->
    case NodeIndex - NodeCount - 16 of
        DataIndex when DataIndex >= 0 ->
            validate_data_index(DataIndex, Path, Aux);
        _BadDataIndex ->
            JournalPrefix = journal_prefix(Path, Aux),
            JournalPath = journal_path(Path, Aux),
            locus_mmdb_check_journal:bad_node_index_in_tree(Aux#validation_aux.journal,
                                                            NodeIndex, JournalPrefix,
                                                            JournalPath)
    end;
validate_recur(NodeIndex, _NotFreshlyReceived, _Path, _Depth, Aux)
  when NodeIndex =:= Aux#validation_aux.node_count ->
    % Leaf node
    ok;
validate_recur(NodeIndex, _NotFreshlyReceived, Path, Depth, Aux)
  when Depth > Aux#validation_aux.max_depth ->
    #validation_aux{node_count = NodeCount, journal = Journal} = Aux,
    ?assertMatch({true, _, _}, {NodeIndex < NodeCount, NodeIndex, NodeCount}),
    JournalPrefix = journal_prefix(Path, Aux),
    JournalPath = journal_path(Path, Aux),
    locus_mmdb_check_journal:excessively_long_path_in_tree(Journal, NodeIndex,
                                                           JournalPrefix, JournalPath).

validate_data_index(DataIndex, Path, Aux) ->
    case (Aux#validation_aux.callback)(DataIndex) of
        ok ->
            ok;
        {error, Reason} ->
            JournalPrefix = journal_prefix(Path, Aux),
            JournalPath = journal_path(Path, Aux),
            locus_mmdb_check_journal:bad_data_index_in_tree(Aux#validation_aux.journal,
                                                            DataIndex, Reason,
                                                            JournalPrefix, JournalPath)
    end.

journal_prefix(Path, Aux) ->
    journal_prefix_recur(Path, Aux, _Acc = <<>>).

journal_prefix_recur(Path, Aux, Acc)
  when Path > 1 ->
    Bit = Path band 1,
    UpdatedAcc = <<Bit:1, Acc/bits>>,
    RemainingPath = Path bsr 1,
    journal_prefix_recur(RemainingPath, Aux, UpdatedAcc);
journal_prefix_recur(Path, Aux, Prefix)
  when Path =:= 1 ->
    % root we explicitly marked in `locus_mmdb_tree_walk_manager'
    PrefixSize = bit_size(Prefix),
    Address = case Aux#validation_aux.max_depth of
                  128 ->
                      ?assertMatch({true, _}, {PrefixSize =< 128, Prefix}),
                      SuffixSize = 128 - PrefixSize,
                      BitAddress = <<Prefix/bits, 0:SuffixSize>>,
                      <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>> = BitAddress,
                      {A, B, C, D, E, F, G, H};
                  32 ->
                      ?assertMatch({true, _}, {PrefixSize =< 32, Prefix}),
                      SuffixSize = 32 - PrefixSize,
                      BitAddress = <<Prefix/bits, 0:SuffixSize>>,
                      <<A, B, C, D>> = BitAddress,
                      {A, B, C, D}
              end,

    StringAddress = [_|_] = inet:ntoa(Address),
    StringAddress ++ "/" ++ integer_to_list(PrefixSize).

journal_path(Path, Aux) ->
    Decisions = path_decisions(Path),
    journal_path_recur(_NodeIndex = 0, Decisions, Aux, _Acc = []).

journal_path_recur(NodeIndex, [Decision | Next], Aux, Acc) ->
    #validation_aux{node_count = NodeCount,
                    node_size = NodeSize,
                    record_size = RecordSize,
                    data = Data} = Aux,
    ?assertMatch({true, _, _}, {NodeIndex < NodeCount, NodeIndex, NodeCount}),
    Offset = NodeIndex * NodeSize,
    UpdatedAcc = [NodeIndex | Acc],
    ChildIndex = extract_node_record(Decision, Offset, Data, RecordSize),
    journal_path_recur(ChildIndex, Next, Aux, UpdatedAcc);
journal_path_recur(_NodeIndex, [], _Aux, Acc) ->
    lists:reverse(Acc).

path_decisions(Path) ->
    path_decisions_recur(Path, []).

path_decisions_recur(Path, Acc)
  when Path > 1 ->
    Decision = Path band 1,
    RemainingPath = Path bsr 1,
    UpdatedAcc = [Decision | Acc],
    path_decisions_recur(RemainingPath, UpdatedAcc);
path_decisions_recur(Path, Decisions)
  when Path =:= 1 ->
    % root we explicitly marked in `locus_mmdb_tree_walk_manager'
    Decisions.
