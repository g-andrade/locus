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

%% @doc API for working with MMDB - data codec
-module(locus_mmdb_data_codec).

-hank([{unnecessary_function_arguments, [{just_the_value, 2, 1}]}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([parse_on_index/3]).

%% ------------------------------------------------------------------
%% "Private" API Function Exports
%% ------------------------------------------------------------------

-export([validate_indices_in_tree/7]).

%% ------------------------------------------------------------------
%% Debug API Function Exports
%% ------------------------------------------------------------------

-export([parse_all/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

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
-define(extended_data_cache_container, 5). % Unimplemented
-define(extended_end_marker, 6).
-define(extended_boolean, 7).
-define(extended_float, 8).

%% ------------------------------------------------------------------
%% API Record and Type Definitions
%% ------------------------------------------------------------------

-type index() :: non_neg_integer().
-export_type([index/0]).

%% ------------------------------------------------------------------
%% Internal Record and Type Definitions
%% ------------------------------------------------------------------

-record(parse_opts, {
          wrapping_fun :: fun ((atom(), locus_mmdb_data:value())
                               -> locus_mmdb_data_raw:value()
                                  | locus_mmdb_data:value())
         }).

-record(validation_aux, {
          indices_in_tree :: locus_shared_bitarray:t(),
          visited :: locus_shared_bitarray:t(),
          valid_map_keys :: locus_shared_bitarray:t(),
          batch_size :: pos_integer(),
          data :: binary(),
          journal :: locus_mmdb_check_journal:t()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Attempts to parse either the `Value' or `RawValue'
%% (depending on the `Raw' flag) at `Index' in `DataSection'.
%%
%% Will crash upon invalid/unrecognized data, invalid pointers
%% or cyclic pointer chasing (i.e. loops.)
%%
-spec parse_on_index(Index, DataSection, Raw)
    -> {Value | RawValue, RemainingData}
         when Index :: index(),
              DataSection :: binary(),
              Raw :: boolean(),
              Value :: locus_mmdb_data:value(),
              RawValue :: locus_mmdb_data_raw:value(),
              RemainingData :: binary().
parse_on_index(Index, FullData, Raw) ->
    WrappingFun = parser_wrapping_fun(Raw),
    Opts = #parse_opts{wrapping_fun = WrappingFun},
    parse_on_index(Index, FullData, Opts, []).

%% ------------------------------------------------------------------
%% "Private" API Function Definitions
%% ------------------------------------------------------------------

-spec validate_indices_in_tree(locus_shared_bitarray:t(),
                               locus_shared_bitarray:t(),
                               locus_shared_bitarray:t(),
                               pos_integer(), non_neg_integer(),
                               binary(),
                               locus_mmdb_check_journal:t()) -> ok.
%% @private
validate_indices_in_tree(BitArray, VisitedBitArray, MapKeysBitArray,
                         BatchSize, BatchOffset, Data, Journal) ->
    Aux = #validation_aux{indices_in_tree = BitArray,
                          visited = VisitedBitArray,
                          valid_map_keys = MapKeysBitArray,
                          batch_size = BatchSize,
                          data = Data,
                          journal = Journal},

    validate_positions_in_tree_recur(Aux, _CurrentOffset = BatchOffset).

%% ------------------------------------------------------------------
%% Debug API Function Definitions
%% ------------------------------------------------------------------

-spec parse_all(binary(), boolean()) -> [locus_mmdb_data:value()
                                         | locus_mmdb_data_raw:value()].
%% @private
parse_all(FullData, Raw) ->
    WrappingFun = parser_wrapping_fun(Raw),
    Opts = #parse_opts{wrapping_fun = WrappingFun},
    parse_all_recur(_Chunk = FullData, FullData, Opts, _Acc = []).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Parsing
%% ------------------------------------------------------------------

parse_on_index(Index, FullData, Opts, Path) ->
    UpdatedPath = [Index | Path],
    case lists:member(Index, Path) orelse FullData of
        true ->
            error({circular_path, UpdatedPath});
        <<_:Index/bytes, Chunk/bytes>> ->
            parse_chunk(Chunk, FullData, Opts, UpdatedPath);
        <<_InsufficientData/bytes>> ->
            error({invalid_index, Index, #{under_path => Path,
                                           full_data_size => byte_size(FullData)}})
    end.

parse_chunk(Chunk, FullData, Opts, Path) ->
    case parse_chunk_head(Chunk) of
        {pointer, Pointer, RemainingData} ->
            {MaybeWrapped, _} = parse_on_index(Pointer, FullData, Opts, Path),
            {MaybeWrapped, RemainingData};

        {utf8_string, Bytes, RemainingData} ->
            Text = decode_utf8_string(Bytes, Path),
            MaybeWrapped = maybe_wrap(utf8_string, Text, Opts),
            {MaybeWrapped, RemainingData};

        {bytes, Bytes, RemainingData} ->
            Blob = binary:copy(Bytes),
            MaybeWrapped = maybe_wrap(bytes, Blob, Opts),
            {MaybeWrapped, RemainingData};

        {map, Count, RemainingData} ->
            parse_map(Count, RemainingData, FullData, Opts, Path);

        {array, Count, RemainingData} ->
            parse_array(Count, RemainingData, FullData, Opts, Path);

        {Type, Value, RemainingData} ->
            MaybeWrapped = maybe_wrap(Type, Value, Opts),
            {MaybeWrapped, RemainingData};

        {error, Reason} ->
            error({failed_to_parse_chunk, #{why => Reason,
                                            under_path => Path}})
    end.

decode_utf8_string(Bytes, Path) ->
    Copy = binary:copy(Bytes),
    case unicode:characters_to_binary(Copy) of
        <<_/bytes>> ->
            Copy;
        _ ->
            error({not_utf8_text, #{bytes => Copy,
                                    under_path => Path}})
    end.

parse_map(Count, Chunk, FullData, Opts, Path) ->
    parse_map_recur(Count, Chunk, FullData, Opts, Path, []).

parse_map_recur(Count, Chunk, FullData, Opts, Path, KvAcc)
  when Count > 0 ->
    {Key, RemainingData} = parse_map_key(Chunk, FullData, Path),
    {MaybeWrappedValue, RemainingData2} = parse_chunk(RemainingData, FullData, Opts, Path),
    UpdatedKvAcc = [{Key, MaybeWrappedValue} | KvAcc],
    parse_map_recur(Count - 1, RemainingData2, FullData, Opts, Path, UpdatedKvAcc);
parse_map_recur(0, RemainingData, _, Opts, _, KvAcc) ->
    Map = maps:from_list(KvAcc),
    MaybeWrapped = maybe_wrap(map, Map, Opts),
    {MaybeWrapped, RemainingData}.

parse_map_key(Chunk, FullData, Path) ->
    case parse_chunk_head(Chunk) of
        {pointer, Pointer, RemainingData} ->
            {Text, _} = parse_map_key_on_index(Pointer, FullData, Path),
            {Text, RemainingData};
        {utf8_string, Bytes, RemainingData} ->
            Text = decode_utf8_string(Bytes, Path),
            {Text, RemainingData};
        {KeyType, KeyValue, _} ->
            error({unexpected_map_key, #{type => KeyType,
                                         value => KeyValue,
                                         under_path => Path}});
        {error, Reason} ->
            error({failed_to_parse_map_key, #{why => Reason,
                                              under_path => Path}})
    end.

parse_map_key_on_index(Index, FullData, Path) ->
    UpdatedPath = [Index | Path],
    case lists:member(Index, Path) of
        true ->
            error({circular_path, UpdatedPath});
        _ ->
            <<_:Index/bytes, Chunk/bytes>> = FullData,
            parse_map_key(Chunk, FullData, UpdatedPath)
    end.

parse_array(Count, Chunk, FullData, Opts, Path) ->
    parse_array_recur(Count, Chunk, FullData, Opts, Path, []).

parse_array_recur(Count, Chunk, FullData, Opts, Path, Acc)
  when Count > 0 ->
    {MaybeWrapped, RemainingData} = parse_chunk(Chunk, FullData, Opts, Path),
    UpdatedAcc = [MaybeWrapped | Acc],
    parse_array_recur(Count - 1, RemainingData, FullData, Opts, Path, UpdatedAcc);
parse_array_recur(0, RemainingData, _, Opts, _, Acc) ->
    List = lists:reverse(Acc),
    MaybeWrapped = maybe_wrap(array, List, Opts),
    {MaybeWrapped, RemainingData}.

-spec parse_chunk_head(binary())
        -> {locus_mmdb_data_raw:value_tag(), locus_mmdb_data:value(), binary()}
           | {error, term()}.
parse_chunk_head(Data) ->
    case Data of
        <<?pointer:3, 0:2, Pointer:11, RemainingData/bytes>> ->
            {pointer, Pointer, RemainingData};
        <<?pointer:3, 1:2, BasePointer:19, RemainingData/bytes>> ->
            {pointer, BasePointer + 2048, RemainingData};
        <<?pointer:3, 2:2, BasePointer:27, RemainingData/bytes>> ->
            {pointer, BasePointer + 526336, RemainingData};
        <<?pointer:3, _:5, Pointer:32, RemainingData/bytes>> ->
            {pointer, Pointer, RemainingData};

        <<?utf8_string:3, Size:5, Bytes:Size/bytes, RemainingData/bytes>>
          when Size < 29 ->
            {utf8_string, Bytes, RemainingData};
        <<?utf8_string:3, 29:5, BaseSize, Tail/bytes>> ->
            Size = BaseSize + 29,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {utf8_string, Bytes, RemainingData};
        <<?utf8_string:3, 30:5, BaseSize:16, Tail/bytes>> ->
            Size = BaseSize + 285,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {utf8_string, Bytes, RemainingData};
        <<?utf8_string:3, _:5, BaseSize:24, Tail/bytes>> ->
            Size = BaseSize + 65821,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {utf8_string, Bytes, RemainingData};

        <<?double:3, 8:5, Value:64/float, RemainingData/bytes>> ->
            {double, Value, RemainingData};
        <<?double:3, 8:5, Signal:1, Exponent:11, Mantissa:52, RemainingData/bytes>>
          when Signal =:= 0, Exponent =:= ((1 bsl 11) - 1), Mantissa =:= 0 ->
            {double, '#Inf', RemainingData};
        <<?double:3, 8:5, Signal:1, Exponent:11, Mantissa:52, RemainingData/bytes>>
          when Signal =:= 1, Exponent =:= ((1 bsl 11) - 1), Mantissa =:= 0 ->
            {double, '#-Inf', RemainingData};

        <<?bytes:3, Size:5, Bytes:Size/bytes, RemainingData/bytes>>
          when Size < 29 ->
            {bytes, Bytes, RemainingData};
        <<?bytes:3, 29:5, BaseSize, Tail/bytes>> ->
            Size = BaseSize + 29,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {bytes, Bytes, RemainingData};
        <<?bytes:3, 30:5, BaseSize:16, Tail/bytes>> ->
            Size = BaseSize + 285,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {bytes, Bytes, RemainingData};
        <<?bytes:3, _:5, BaseSize:24, Tail/bytes>> ->
            Size = BaseSize + 65821,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {bytes, Bytes, RemainingData};

        <<?uint16:3, Size:5, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size =< 2 ->
            {uint16, Value, RemainingData};
        <<?uint32:3, Size:5, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size =< 4 ->
            {uint32, Value, RemainingData};

        <<?map:3, Count:5, RemainingData/bytes>>
          when Count < 29 ->
            {map, Count, RemainingData};
        <<?map:3, 29:5, BaseCount, RemainingData/bytes>> ->
            Count = BaseCount + 29,
            {map, Count, RemainingData};
        <<?map:3, 30:5, BaseCount:16, RemainingData/bytes>> ->
            Count = BaseCount + 285,
            {map, Count, RemainingData};
        <<?map:3, _:5, BaseCount:24, RemainingData/bytes>> ->
            Count = BaseCount + 65821,
            {map, Count, RemainingData};

        <<0:3, Size:5, ?extended_int32, Value:Size/signed-integer-unit:8, RemainingData/bytes>>
          when Size =:= 4 ->
            {int32, Value, RemainingData};
        <<0:3, Size:5, ?extended_int32, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size < 4 ->
            % As per the spec:
            % "When storing a signed integer, fields shorter than the maximum byte length
            %  are always positive. When the field is the maximum length, e.g., 4 bytes
            %  for 32-bit integers, the left-most bit is the sign.
            %  A 1 is negative and a 0 is positive."
            %
            {int32, Value, RemainingData};
        <<0:3, Size:5, ?extended_uint64, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size =< 8 ->
            {uint64, Value, RemainingData};
        <<0:3, Size:5, ?extended_uint128, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size =< 16 ->
            {uint128, Value, RemainingData};

        <<0:3, Count:5, ?extended_array, RemainingData/bytes>> when Count < 29 ->
            {array, Count, RemainingData};
        <<0:3, 29:5, ?extended_array, BaseCount, RemainingData/bytes>> ->
            Count = BaseCount + 29,
            {array, Count, RemainingData};
        <<0:3, 30:5, ?extended_array, BaseCount:16, RemainingData/bytes>> ->
            Count = BaseCount + 285,
            {array, Count, RemainingData};
        <<0:3, _:5, ?extended_array, BaseCount:24, RemainingData/bytes>> ->
            Count = BaseCount + 65821,
            {array, Count, RemainingData};

        <<0:3, _:5, ?extended_data_cache_container, _RemainingData/bytes>> ->
            {error, '`data cache container` type not yet supported'};

        <<0:3, 0:5, ?extended_end_marker>> ->
            {error, {finished, extended_end_marker}};

        <<0:3, 0:5, ?extended_boolean, RemainingData/bytes>> ->
            {boolean, false, RemainingData};
        <<0:3, 1:5, ?extended_boolean, RemainingData/bytes>> ->
            {boolean, true, RemainingData};

        <<0:3, 4:5, ?extended_float, Value:32/float, RemainingData/bytes>> ->
            {float, Value, RemainingData};
        <<0:3, 4:5, ?extended_float, Signal:1, Exponent:8, Mantissa:23, RemainingData/bytes>>
          when Signal =:= 0, Exponent =:= ((1 bsl 8) - 1), Mantissa =:= 0 ->
            {float, '#Inf', RemainingData};
        <<0:3, 4:5, ?extended_float, Signal:1, Exponent:8, Mantissa:23, RemainingData/bytes>>
          when Signal =:= 1, Exponent =:= ((1 bsl 8) - 1), Mantissa =:= 0 ->
            {float, '#-Inf', RemainingData};

        <<>> ->
            {error, {finished, no_more_data}};

        _Invalid ->
            fail_to_parse_chunk_head(Data)
    end.

fail_to_parse_chunk_head(Data) ->
    case Data of
        <<?pointer:3, 0:2, InsufficientData/bits>> ->
            {error, {insufficient_data, pointer_0, bitstring_copy(InsufficientData)}};
        <<?pointer:3, 1:2, InsufficientData/bits>> ->
            {error, {insufficient_data, pointer_1, bitstring_copy(InsufficientData)}};
        <<?pointer:3, 2:2, InsufficientData/bits>> ->
            {error, {insufficient_data, pointer_2, bitstring_copy(InsufficientData)}};
        <<?pointer:3, _:5, InsufficientData/bits>> ->
            {error, {insufficient_data, pointer_3, bitstring_copy(InsufficientData)}};

        <<?utf8_string:3, Size:5, InsufficientData/bytes>>
          when Size < 29 ->
            {error, {insufficient_data, utf8_string_1, {Size, bytes},
                     binary:copy(InsufficientData)}};
        <<?utf8_string:3, 29:5, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, utf8_string_2, binary:copy(InsufficientData)}};
        <<?utf8_string:3, 30:5, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, utf8_string_3, binary:copy(InsufficientData)}};
        <<?utf8_string:3, 31:5, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, utf8_string_4, binary:copy(InsufficientData)}};

        <<?double:3, 8:5, InvalidDouble:64/bits, _RemainingData/bytes>> ->
            {error, {invalid_double, binary:copy(InvalidDouble)}};
        <<?double:3, 8:5, InsufficientData/bytes>> ->
            {error, {insufficient_data, double, binary:copy(InsufficientData)}};
        <<?double:3, SizeTag:5, _RemainingData/bytes>> ->
            {error, {invalid_double_size_tag, SizeTag}};

        <<?bytes:3, Size:5, InsufficientData/bytes>>
          when Size < 29 ->
            {error, {insufficient_data, bytes_1, {Size, bytes}, binary:copy(InsufficientData)}};
        <<?bytes:3, 29:5, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, bytes_2, binary:copy(InsufficientData)}};
        <<?bytes:3, 30:5, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, bytes_3, binary:copy(InsufficientData)}};
        <<?bytes:3, _:5, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, bytes_4, binary:copy(InsufficientData)}};

        <<?uint16:3, Size:5, _RemainingData/bytes>>
          when Size > 2 ->
            {error, {too_large, uint16, {Size, bytes}}};
        <<?uint16:3, Size:5, InsufficientData/bytes>> ->
            {error, {insufficient_data, uint16, {Size, bytes}, binary:copy(InsufficientData)}};

        <<?uint32:3, Size:5, _RemainingData/bytes>>
          when Size > 4 ->
            {error, {too_large, uint32, {Size, bytes}}};
        <<?uint32:3, Size:5, InsufficientData/bytes>> ->
            {error, {insufficient_data, uint32, {Size, bytes}, binary:copy(InsufficientData)}};

        <<?map:3, 29:5, InsufficientData/bits>> ->
            {error, {insufficient_data_for_size, map_2, bitstring_copy(InsufficientData)}};
        <<?map:3, 30:5, InsufficientData/bits>> ->
            {error, {insufficient_data_for_size, map_3, bitstring_copy(InsufficientData)}};
        <<?map:3, 31:5, InsufficientData/bits>> ->
            {error, {insufficient_data_for_size, map_4, bitstring_copy(InsufficientData)}};

        <<0:3, Size:5, ?extended_int32, _RemainingData/bytes>>
          when Size > 4 ->
            {error, {too_large, int32, {Size, bytes}}};
        <<0:3, Size:5, ?extended_int32, InsufficientData/bytes>> ->
            {error, {insufficient_data, int32, {Size, bytes}, binary:copy(InsufficientData)}};

        <<0:3, Size:5, ?extended_uint64, _RemainingData/bytes>>
          when Size > 8 ->
            {error, {too_large, uint64, {Size, bytes}}};
        <<0:3, Size:5, ?extended_uint64, InsufficientData/bytes>> ->
            {error, {insufficient_data, uint64, {Size, bytes}, binary:copy(InsufficientData)}};

        <<0:3, Size:5, ?extended_uint128, _RemainingData/bytes>>
          when Size > 16 ->
            {error, {too_large, uint128, {Size, bytes}}};
        <<0:3, Size:5, ?extended_uint128, InsufficientData/bytes>> ->
            {error, {insufficient_data, uint128, {Size, bytes}, binary:copy(InsufficientData)}};

        <<0:3, 29:5, ?extended_array, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, array_2, binary:copy(InsufficientData)}};
        <<0:3, 30:5, ?extended_array, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, array_3, binary:copy(InsufficientData)}};
        <<0:3, 31:5, ?extended_array, InsufficientData/bytes>> ->
            {error, {insufficient_data_for_size, array_4, binary:copy(InsufficientData)}};

        <<0:3, 0:5, ?extended_end_marker, ExcessiveData/bytes>> ->
            {error, {data_beyond_end_marker,
                     locus_util:purge_term_of_very_large_binaries(ExcessiveData)}};

        <<0:3, 4:5, ?extended_float, InvalidFloat:32/bits, _RemainingData/bytes>> ->
            {error, {invalid_float, binary:copy(InvalidFloat)}};
        <<0:3, 4:5, ?extended_float, InsufficientData/bytes>> ->
            {error, {insufficient_data, float, binary:copy(InsufficientData)}};
        <<0:3, SizeTag:5, ?extended_float, _RemainingData/bytes>> ->
            {error, {invalid_float_size_tag, SizeTag}};

        <<0:3, _SizeTag:5, UnknownExtendedType, _RemainingData/bytes>> ->
            {error, {unknown_extended_type, UnknownExtendedType}};
        <<0:3, _SizeTag:5, InsufficientData/bytes>> ->
            {error, {insufficient_data, binary:copy(InsufficientData)}}
    end.

bitstring_copy(Bits) ->
    SuffixSize = bit_size(Bits) rem 8,
    PrefixSize = bit_size(Bits) - SuffixSize,
    <<Prefix:PrefixSize/bits, Suffix:SuffixSize/bits>> = Bits,
    CopiedPrefix = binary:copy(Prefix),
    <<CopiedPrefix/bytes, Suffix/bits>>.

parser_wrapping_fun(Raw) ->
    maps:get(Raw, #{true => fun tagged_value/2,
                    false => fun just_the_value/2}).

tagged_value(Tag, Value) ->
    {Tag, Value}.

just_the_value(_Tag, Value) ->
    Value.

maybe_wrap(Tag, Value, Opts) ->
    (Opts#parse_opts.wrapping_fun)(Tag, Value).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Validation
%% ------------------------------------------------------------------

validate_positions_in_tree_recur(Aux, CurrentOffset) ->
    try locus_shared_bitarray:get_positions_set_at_cell(Aux#validation_aux.indices_in_tree,
                                                      CurrentOffset) of
        Positions ->
            validate_positions_batch_in_tree_recur(Aux, Positions, CurrentOffset)
    catch
        throw:{index_out_of_bounds, CurrentOffset} ->
            % All done
            ok
    end.

validate_positions_batch_in_tree_recur(Aux, [Position | Next], CurrentOffset) ->
    try validate_position(Aux, Position) of
        ok ->
            validate_positions_batch_in_tree_recur(Aux, Next, CurrentOffset)
    catch
        throw:controlled_validation_error ->
            validate_positions_batch_in_tree_recur(Aux, Next, CurrentOffset)
    end;
validate_positions_batch_in_tree_recur(Aux, [], CurrentOffset) ->
    NextOffset = CurrentOffset + Aux#validation_aux.batch_size,
    validate_positions_in_tree_recur(Aux, NextOffset).

validate_position(Aux, Position) ->
    validate_position_if_unvisited(Aux, Position, _Path = []).

validate_position_if_unvisited(Aux, Position, Path) ->
    try locus_shared_bitarray:is_set(Aux#validation_aux.visited, Position)
         orelse locus_shared_bitarray:is_set(Aux#validation_aux.valid_map_keys, Position)
    of
        true ->
            ok;
        false ->
            validate_position_if_not_in_loop(Aux, Position, Path)
    catch
        throw:{position_out_of_bounds, Position} ->
            locus_mmdb_check_journal:invalid_position_in_data_section(
                Aux#validation_aux.journal, Position, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end.

validate_position_if_not_in_loop(Aux, Position, Path) ->
    case lists:keymember(Position, 1, Path) of
        false ->
            validate_position_if_its_a_sound_location(Aux, Position, Path);
        true ->
            locus_mmdb_check_journal:loop_in_data_section(Aux#validation_aux.journal,
                                                             lists:reverse(Path)),
            throw(controlled_validation_error)
    end.

validate_position_if_its_a_sound_location(Aux, Position, Path) ->
    case Aux#validation_aux.data of
        <<_:Position/bytes, Chunk/bytes>> ->
            validate_chunk_in_position(Aux, Position, Path, Chunk);
        <<_/bytes>> ->
            locus_mmdb_check_journal:invalid_position_in_data_section(
                Aux#validation_aux.journal, Position, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end.

validate_chunk_in_position(Aux, Position, Path, Chunk) ->
    {ok, _RemainingData} = validate_chunk(Aux, Position, Path, Chunk),
    ok.

validate_chunk(Aux, Position, Path, Chunk) ->
    ParseResult = parse_chunk_head(Chunk),
    validate_parsed_chunk(Aux, Position, Path, ParseResult).

validate_parsed_chunk(Aux, Position, Path, ParseResult) ->
    locus_shared_bitarray:set(Aux#validation_aux.visited, Position),

    case ParseResult of
        {pointer, Pointer, RemainingData} ->
            UpdatedPath = [{Position, {pointer, Pointer}} | Path],
            validate_pointer(Aux, Pointer, RemainingData, UpdatedPath);

        {utf8_string, Bytes, RemainingData} ->
            validate_utf8_string(Aux, Position, Bytes, RemainingData, Path);

        {map, Count, RemainingData} ->
            UpdatedPath = [{Position, {map_with, Count, elements}} | Path],
            validate_map(Aux, Count, RemainingData, UpdatedPath);

        {array, Count, RemainingData} ->
            UpdatedPath = [{Position, {array_with, Count, elements}} | Path],
            validate_array(Aux, Count, RemainingData, UpdatedPath);

        {_, _, RemainingData} ->
            {ok, RemainingData};

        {error, Reason} ->
            locus_mmdb_check_journal:bad_chunk_in_data_section(
                Aux#validation_aux.journal, Position, Reason, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end.

validate_pointer(Aux, Pointer, RemainingData, Path) ->
    validate_position_if_unvisited(Aux, _Position = Pointer, Path),
    {ok, RemainingData}.

validate_utf8_string(Aux, Position, Bytes, DataAfter, Path) ->
    case unicode:characters_to_binary(Bytes) of
        <<_/bytes>> ->
            _ = validate_utf8_string_printability(Aux, Position, Bytes, Path),
            {ok, DataAfter};
        Error ->
            locus_mmdb_check_journal:invalid_utf8_string_in_data_section(
                Aux#validation_aux.journal, Position,
                _OriginalData = binary:copy(Bytes),
                Error, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end.

validate_utf8_string_printability(Aux, Position, Bytes, Path) ->
    List = unicode:characters_to_list(Bytes),
    case io_lib:printable_unicode_list(List) of
        true ->
            ok;
        false ->
            locus_mmdb_check_journal:unprintable_utf8_string_in_data_section(
                Aux#validation_aux.journal, Position,
                _Value = binary:copy(Bytes), lists:reverse(Path)
            )
    end.

validate_map(Aux, Count, RemainingData, Path) ->
    validate_map_recur(Aux, Count, RemainingData, Path).

validate_map_recur(Aux, Count, RemainingData, Path)
  when Count > 0 ->
    DataAfterPair = validate_map_pair(Aux, RemainingData, Path),
    validate_map_recur(Aux, Count - 1, DataAfterPair, Path);
validate_map_recur(_Aux, Count, RemainingData, _Path)
  when Count =:= 0 ->
    {ok, RemainingData}.

validate_map_pair(Aux, RemainingData, Path) ->
    {ok, DataAfterKey} = validate_map_key(Aux, RemainingData, Path),
    Position = byte_size(Aux#validation_aux.data) - byte_size(DataAfterKey),
    validate_map_value(Aux, Position, DataAfterKey, Path).

validate_map_key(Aux, RemainingData, Path) ->
    Position = byte_size(Aux#validation_aux.data) - byte_size(RemainingData),
    case parse_chunk_head(RemainingData) of
        {pointer, Pointer, DataAfterKey} ->
            UpdatedPath = [{Position, {pointer, Pointer}} | Path],
            validate_indirect_map_key_if_not_validated_before(Aux, Pointer, DataAfterKey,
                                                              UpdatedPath);
        {utf8_string, Bytes, DataAfterKey} ->
            validate_utf8_string(Aux, Position, Bytes, DataAfterKey, Path);
        {KeyType, KeyValue, _} ->
            SaferKeyValue = locus_util:purge_term_of_very_large_binaries(KeyValue),
            locus_mmdb_check_journal:map_key_of_wrong_type_in_data_section(
                Aux#validation_aux.journal, Position, {KeyType, SaferKeyValue},
                lists:reverse(Path)
            ),
            throw(controlled_validation_error);
        {error, Reason} ->
            locus_mmdb_check_journal:bad_chunk_in_data_section(Aux#validation_aux.journal,
                                                                  Position, Reason,
                                                                  lists:reverse(Path)),
            throw(controlled_validation_error)
    end.

validate_indirect_map_key_if_not_validated_before(Aux, Pointer, DataAfterKey, Path) ->
    try locus_shared_bitarray:is_set(Aux#validation_aux.valid_map_keys, _Position = Pointer)
         orelse validate_indirect_map_key_if_not_in_loop(Aux, Pointer, DataAfterKey, Path)
    of
        true ->
            {ok, DataAfterKey};
        {ok, _} = Success ->
            locus_shared_bitarray:set(Aux#validation_aux.visited, Pointer),
            locus_shared_bitarray:set(Aux#validation_aux.valid_map_keys, Pointer),
            Success
    catch
        throw:{position_out_of_bounds, Position} ->
            locus_mmdb_check_journal:invalid_position_in_data_section(
                Aux#validation_aux.journal, Position, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end.

validate_indirect_map_key_if_not_in_loop(Aux, Pointer, DataAfterKey, Path) ->
    case lists:keymember(Pointer, 1, Path) of
        false ->
            validate_indirect_map_key_if_its_a_sound_location(Aux, Pointer, DataAfterKey, Path);
        true ->
            locus_mmdb_check_journal:loop_in_data_section(Aux#validation_aux.journal,
                                                             lists:reverse(Path)),
            throw(controlled_validation_error)
    end.

validate_indirect_map_key_if_its_a_sound_location(Aux, Pointer, DataAfterKey, Path) ->
    case Aux#validation_aux.data of
        <<_:Pointer/bytes, Chunk/bytes>> ->
            validate_indirect_map_key_chunk(Aux, Pointer, Chunk, DataAfterKey, Path);
        <<_/bytes>> ->
            locus_mmdb_check_journal:invalid_position_in_data_section(
                Aux#validation_aux.journal, _Position = Pointer,
                lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end.

validate_indirect_map_key_chunk(Aux, Pointer, Chunk, DataAfterKey, Path) ->
    case parse_chunk_head(Chunk) of
        {utf8_string, Bytes, _} ->
            Position = byte_size(Aux#validation_aux.data) - byte_size(Chunk),
            validate_utf8_string(Aux, Position, Bytes, DataAfterKey, Path);
        {pointer, NewPointer, _} ->
            UpdatedPath = [{_Position = Pointer, {pointer, NewPointer}} | Path],
            validate_indirect_map_key_if_not_validated_before(Aux, NewPointer,
                                                              DataAfterKey, UpdatedPath);
        {KeyType, KeyValue, _} ->
            Position = byte_size(Aux#validation_aux.data) - byte_size(Chunk),
            SaferKeyValue = locus_util:purge_term_of_very_large_binaries(KeyValue),
            locus_mmdb_check_journal:map_key_of_wrong_type_in_data_section(
                Aux#validation_aux.journal, Position, {KeyType, SaferKeyValue},
                lists:reverse(Path)
            ),
            throw(controlled_validation_error);
        {error, Reason} ->
            Position = byte_size(Aux#validation_aux.data) - byte_size(Chunk),
            locus_mmdb_check_journal:bad_chunk_in_data_section(Aux#validation_aux.journal,
                                                                  Position, Reason,
                                                                  lists:reverse(Path)),
            throw(controlled_validation_error)
    end.

validate_map_value(Aux, Position, DataAfterKey, Path) ->
    case parse_chunk_head(DataAfterKey) of
        {pointer, Pointer, DataAfterValue} ->
            UpdatedPath = [{Position, {pointer, Pointer}} | Path],
            validate_indirect_map_value(Aux, Pointer, DataAfterValue, UpdatedPath);
        ParseResult ->
            validate_direct_map_value(Aux, Position, ParseResult, Path)
    end.

validate_indirect_map_value(Aux, Pointer, DataAfterValue, Path) ->
    ok = validate_position_if_unvisited(Aux, _Position = Pointer, Path),
    DataAfterValue.

validate_direct_map_value(Aux, Position, {Type, _, DataAfterValue} = ParseResult, Path) ->
    CanSkip = not is_map_key(Type, #{array => [], map => []}),

    try (CanSkip andalso locus_shared_bitarray:is_set(Aux#validation_aux.visited, Position))
        orelse validate_parsed_chunk(Aux, Position, Path, ParseResult)
    of
        true ->
            DataAfterValue;
        {ok, RemainingData} ->
            RemainingData
    catch
        throw:{position_out_of_bounds, Position} ->
            locus_mmdb_check_journal:invalid_position_in_data_section(
                Aux#validation_aux.journal, Position, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end;
validate_direct_map_value(Aux, Position, {error, Reason}, Path) ->
    locus_mmdb_check_journal:bad_chunk_in_data_section(Aux#validation_aux.journal,
                                                          Position, Reason,
                                                          lists:reverse(Path)),
    throw(controlled_validation_error).

validate_array(Aux, Count, RemainingData, Path) ->
    validate_array_recur(Aux, Count, RemainingData, Path).

validate_array_recur(Aux, Count, RemainingData, Path)
  when Count > 0 ->
    case validate_array_value(Aux, RemainingData, Path) of
        {ok, DataAfterValue} ->
            validate_array_recur(Aux, Count - 1, DataAfterValue, Path)
    end;
validate_array_recur(_Aux, Count, RemainingData, _Path)
  when Count =:= 0 ->
    {ok, RemainingData}.

validate_array_value(Aux, RemainingData, Path) ->
    Position = byte_size(Aux#validation_aux.data) - byte_size(RemainingData),
    case parse_chunk_head(RemainingData) of
        {pointer, Pointer, DataAfterValue} ->
            UpdatedPath = [{Position, {pointer, Pointer}} | Path],
            validate_indirect_array_value(Aux, Pointer, DataAfterValue, UpdatedPath);
        ParseResult ->
            validate_direct_array_value(Aux, Position, ParseResult, Path)
    end.

validate_indirect_array_value(Aux, Pointer, DataAfterValue, Path) ->
    ok = validate_position_if_unvisited(Aux, _Position = Pointer, Path),
    {ok, DataAfterValue}.

validate_direct_array_value(Aux, Position, {Type, _, DataAfterValue} = ParseResult, Path) ->
    CanSkip = not is_map_key(Type, #{array => [], map => []}),

    try (CanSkip andalso locus_shared_bitarray:is_set(Aux#validation_aux.visited, Position))
         orelse validate_parsed_chunk(Aux, Position, Path, ParseResult)
    of
        true ->
            {ok, DataAfterValue};
        {ok, RemainingData} ->
            {ok, RemainingData}
    catch
        throw:{position_out_of_bounds, Position} ->
            locus_mmdb_check_journal:invalid_position_in_data_section(
                Aux#validation_aux.journal, Position, lists:reverse(Path)
            ),
            throw(controlled_validation_error)
    end;
validate_direct_array_value(Aux, Position, {error, Reason}, Path) ->
    locus_mmdb_check_journal:bad_chunk_in_data_section(Aux#validation_aux.journal,
                                                          Position, Reason,
                                                          lists:reverse(Path)),
    throw(controlled_validation_error).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Debugging
%% ------------------------------------------------------------------

parse_all_recur(<<>>, _FullData, _Opts, Acc) ->
    lists:reverse(Acc);
parse_all_recur(Chunk, FullData, Opts, Acc) ->
    try parse_chunk(Chunk, FullData, Opts, _Path = []) of
        {Value, RemainingData} ->
            UpdatedAcc = [Value | Acc],
            parse_all_recur(RemainingData, FullData, Opts, UpdatedAcc)
    catch
        Class:Reason:Stacktrace ->
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            erlang:raise(Class, SaferReason, SaferStacktrace)
    end.
