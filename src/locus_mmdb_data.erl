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

-module(locus_mmdb_data).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [decode_on_index/2
   ]).

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
-define(extended_data_cache_container, 5).
-define(extended_end_marker, 6).
-define(extended_boolean, 7).
-define(extended_float, 8).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type index() :: non_neg_integer().
-type size() :: pos_integer().
-type type() :: pointer|utf8_string|double|bytes|uint16|uint32|map|int32|uint64|array|boolean|float.
-type value() :: binary() | float() | integer() | boolean().
-export_type([index/0]).

-type decoded_value() :: decoded_composite_value() | decoded_simple_value().
-export_type([decoded_value/0]).

-type decoded_composite_value() :: decoded_map() | decoded_array().
-export_type([decoded_composite_value/0]).

-type decoded_map() :: #{unicode:unicode_binary() => decoded_value()}.
-export_type([decoded_map/0]).

-type decoded_array() :: [decoded_value()].
-export_type([decoded_array/0]).

-type decoded_simple_value() ::
        unicode:unicode_binary() |
        float() |
        binary() |
        int32() |
        uint128() |
        boolean().
-export_type([decoded_simple_value/0]).

-type int32() :: -(1 bsl 32)..((1 bsl 32) - 1).
-export_type([int32/0]).

-type uint128() :: 0..((1 bsl 128) - 1).
-export_type([uint128/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec decode_on_index(index(), binary()) -> {decoded_value(), binary()}.
%% @private
decode_on_index(Index, FullData) ->
    decode_on_index(Index, FullData, []).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

decode_on_index(Index, FullData, Path) ->
    UpdatedPath = [Index | Path],
    case lists:member(Index, Path) of
        true -> error({circular_path, UpdatedPath});
        _ ->
            <<_:Index/bytes, Chunk/bytes>> = FullData,
            decode_chunk(Chunk, FullData, UpdatedPath)
    end.

-spec decode_chunk(binary(), binary(), [index()]) -> {decoded_value(), binary()}.
decode_chunk(Chunk, FullData, Path) ->
    case parse_chunk(Chunk) of
        {_, pointer, Pointer, RemainingData} ->
            {Value, _} = decode_on_index(Pointer, FullData, Path),
            {Value, RemainingData};

        {_, utf8_string, Bytes, RemainingData} ->
            Text = decode_utf8_string(Bytes),
            {Text, RemainingData};

        {_, bytes, Bytes, RemainingData} ->
            Blob = binary:copy(Bytes),
            {Blob, RemainingData};

        {_, map, Count, RemainingData} ->
            decode_map(Count, RemainingData, FullData, Path);

        {_, array, Count, RemainingData} ->
            decode_array(Count, RemainingData, FullData, Path);

        {_, _, Value, RemainingData} ->
            {Value, RemainingData}
    end.

decode_utf8_string(Bytes) ->
    Copy = binary:copy(Bytes),
    case is_utf8_binary(Copy) of
        true -> Copy;
        _ -> error({not_utf8_text,Copy})
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

decode_map(Count, Chunk, FullData, Path) ->
    decode_map_recur(Count, Chunk, FullData, Path, []).

decode_map_recur(0, RemainingData, _, _, KvAcc) ->
    case lists:ukeysort(1, KvAcc) of
        SortedKvAcc when length(SortedKvAcc) =:= length(KvAcc) ->
            Map = maps:from_list(SortedKvAcc),
            {Map, RemainingData}
    end;
decode_map_recur(Count, Chunk, FullData, Path, KvAcc) ->
    {Key, RemainingData} = decode_map_key(Chunk, FullData, Path),
    {Value, RemainingData2} = decode_chunk(RemainingData, FullData, Path),
    UpdatedKvAcc = [{Key,Value} | KvAcc],
    decode_map_recur(Count - 1, RemainingData2, FullData, Path, UpdatedKvAcc).

decode_map_key(Chunk, FullData, Path) ->
    case parse_chunk(Chunk) of
        {_, pointer, Pointer, RemainingData} ->
            {Text, _} = decode_map_key_on_index(Pointer, FullData, Path),
            {Text, RemainingData};
        {_, utf8_string, Bytes, RemainingData} ->
            Text = decode_utf8_string(Bytes),
            {Text, RemainingData}
    end.

decode_map_key_on_index(Index, FullData, Path) ->
    UpdatedPath = [Index | Path],
    case lists:member(Index, Path) of
        true -> error({circular_path, UpdatedPath});
        _ ->
            <<_:Index/bytes, Chunk/bytes>> = FullData,
            decode_map_key(Chunk, FullData, UpdatedPath)
    end.

decode_array(Count, Chunk, FullData, Path) ->
    decode_array_recur(Count, Chunk, FullData, Path, []).

decode_array_recur(0, RemainingData, _, _, Acc) ->
    List = lists:reverse(Acc),
    {List, RemainingData};
decode_array_recur(Count, Chunk, FullData, Path, Acc) ->
    {Value, RemainingData} = decode_chunk(Chunk, FullData, Path),
    UpdatedAcc = [Value | Acc],
    decode_array_recur(Count - 1, RemainingData, FullData, Path, UpdatedAcc).

-spec parse_chunk(binary()) -> {size(), type(), value(), binary()} | finished.
parse_chunk(Data) ->
    case Data of
        <<?pointer:3, 0:2, Pointer:11, RemainingData/bytes>> ->
            {2, pointer, Pointer, RemainingData};
        <<?pointer:3, 1:2, Pointer:19, RemainingData/bytes>> ->
            {3, pointer, Pointer+2048, RemainingData};
        <<?pointer:3, 2:2, Pointer:27, RemainingData/bytes>> ->
            {4, pointer, Pointer+526336, RemainingData};
        <<?pointer:3, _:5, Pointer:32, RemainingData/bytes>> ->
            {5, pointer, Pointer, RemainingData};

        <<?utf8_string:3, Size:5, Bytes:Size/bytes, RemainingData/bytes>> when Size < 29 ->
            {1 + Size, utf8_string, Bytes, RemainingData};
        <<?utf8_string:3, 29:5, BaseSize, Tail/bytes>> ->
            Size = BaseSize + 29,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {2 + Size, utf8_string, Bytes, RemainingData};
        <<?utf8_string:3, 30:5, BaseSize:16, Tail/bytes>> ->
            Size = BaseSize + 285,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {3 + Size, utf8_string, Bytes, RemainingData};
        <<?utf8_string:3, _:5, BaseSize:24, Tail/bytes>> ->
            Size = BaseSize + 65821,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {4 + Size, utf8_string, Bytes, RemainingData};

        <<?double:3, 8:5, Value:64/float, RemainingData/bytes>> ->
            {9, double, Value, RemainingData};

        <<?bytes:3, Size:5, Bytes:Size/bytes, RemainingData/bytes>> when Size < 29 ->
            {1 + Size, bytes, Bytes, RemainingData};
        <<?bytes:3, 29:5, BaseSize, Tail/bytes>> ->
            Size = BaseSize + 29,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {2 + Size, bytes, Bytes, RemainingData};
        <<?bytes:3, 30:5, BaseSize:16, Tail/bytes>> ->
            Size = BaseSize + 285,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {3 + Size, bytes, Bytes, RemainingData};
        <<?bytes:3, _:5, BaseSize:24, Tail/bytes>> ->
            Size = BaseSize + 65821,
            <<Bytes:Size/bytes, RemainingData/bytes>> = Tail,
            {4 + Size, bytes, Bytes, RemainingData};

        <<?uint16:3, Size:5, Value:Size/unsigned-integer-unit:8, RemainingData/bytes>>
          when Size =< 2 ->
            {1 + Size, uint16, Value, RemainingData};
        <<?uint32:3, Size:5, Value:Size/unsigned-integer-unit:8, RemainingData/bytes>>
          when Size =< 4 ->
            {1 + Size, uint32, Value, RemainingData};

        <<?map:3, Count:5, RemainingData/bytes>> when Count < 29 ->
            {1, map, Count, RemainingData};
        <<?map:3, 29:5, BaseCount, RemainingData/bytes>> ->
            Count = BaseCount + 29,
            {2, map, Count, RemainingData};
        <<?map:3, 30:5, BaseCount:16, RemainingData/bytes>> ->
            Count = BaseCount + 285,
            {3, map, Count, RemainingData};
        <<?map:3, _:5, BaseCount:24, RemainingData/bytes>> ->
            Count = BaseCount + 65821,
            {4, map, Count, RemainingData};

        <<0:3, Size:5, ?extended_int32, Value:Size/signed-integer-unit:8, RemainingData/bytes>>
          when Size =< 4 ->
            {2 + Size, int32, Value, RemainingData};
        <<0:3, Size:5, ?extended_uint64, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size =< 8 ->
            {2 + Size, uint64, Value, RemainingData};
        <<0:3, Size:5, ?extended_uint128, Value:Size/integer-unit:8, RemainingData/bytes>>
          when Size =< 16 ->
            {2 + Size, uint128, Value, RemainingData};

        <<0:3, Count:5, ?extended_array, RemainingData/bytes>> when Count < 29 ->
            {2, array, Count, RemainingData};
        <<0:3, 29:5, ?extended_array, BaseCount, RemainingData/bytes>> ->
            Count = BaseCount + 29,
            {3, array, Count, RemainingData};
        <<0:3, 30:5, ?extended_array, BaseCount:16, RemainingData/bytes>> ->
            Count = BaseCount + 285,
            {4, array, Count, RemainingData};
        <<0:3, _:5, ?extended_array, BaseCount:24, RemainingData/bytes>> ->
            Count = BaseCount + 65821,
            {5, array, Count, RemainingData};

        <<0:3, 0:5, ?extended_end_marker>> ->
            finished;

        <<0:3, 0:5, ?extended_boolean, RemainingData/bytes>> ->
            {2, boolean, false, RemainingData};
        <<0:3, 1:5, ?extended_boolean, RemainingData/bytes>> ->
            {2, boolean, true, RemainingData};

        <<0:3, 4:5, ?extended_float, Value:32/float, RemainingData/bytes>> ->
            {6, float, Value, RemainingData};

        <<>> ->
            finished
    end.
