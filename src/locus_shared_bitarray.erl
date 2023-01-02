%% Copyright (c) 2021-2023 Guilherme Andrade
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

%% @private
-module(locus_shared_bitarray).

-include_lib("stdlib/include/assert.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1,
         set/2,
         is_set/2,
         get_positions_set_at_cell/2]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(BITSHIFT, 6).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-opaque t() :: atomics:atomics_ref().
-export_type([t/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec new(pos_integer()) -> t().
new(Length) ->
    Size = ceil(Length / (1 bsl ?BITSHIFT)),
    atomics:new(Size, [{signed, false}]).

-spec set(t(), non_neg_integer()) -> ok.
set(Array, Position) ->
    OneBasedIndex = (Position bsr ?BITSHIFT) + 1,
    Offset = Position band ((1 bsl ?BITSHIFT) - 1),
    UpdateMask = 1 bsl Offset,
    Cell = atomics:get(Array, OneBasedIndex),
    set_recur(Array, Cell, OneBasedIndex, UpdateMask).

-spec is_set(t(), non_neg_integer()) -> boolean().
is_set(Array, Position) ->
    OneBasedIndex = (Position bsr ?BITSHIFT) + 1,
    Offset = Position band ((1 bsl ?BITSHIFT) - 1),
    ReadMask = 1 bsl Offset,
    try atomics:get(Array, OneBasedIndex) of
        Cell ->
            (Cell band ReadMask) =/= 0
    catch
        error:badarg when is_reference(Array), is_integer(Position), Position >= 0 ->
            throw({position_out_of_bounds, Position})
    end.

-spec get_positions_set_at_cell(t(), non_neg_integer()) -> [non_neg_integer(), ...].
get_positions_set_at_cell(Array, Index) ->
    try atomics:get(Array, _OneBasedIndex = Index + 1) of
        Cell ->
            BasePosition = Index bsl ?BITSHIFT,
            positions_set_at_cell(BasePosition, Cell)
    catch
        error:badarg ->
            #{size := Size} = atomics:info(Array),
            ?assertMatch({true, _, _}, {Index >= Size, Index, Size}),
            throw({index_out_of_bounds, Index})
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

set_recur(Array, Cell, OneBasedIndex, UpdateMask) ->
    case Cell bor UpdateMask of
        Cell ->
            ok;
        NewCell ->
            case atomics:exchange(Array, OneBasedIndex, NewCell) of
                Cell ->
                    ok;
                UpdatedCell ->
                    set_recur(Array, UpdatedCell, OneBasedIndex, UpdateMask)
            end
    end.

positions_set_at_cell(BasePosition, Cell)
  when Cell =/= 0 ->
    case Cell band 1 of
        1 ->
            [BasePosition | positions_set_at_cell(BasePosition + 1,
                                                  Cell bsr 1)];
        _ ->
            positions_set_at_cell(BasePosition + 1, Cell bsr 1)
    end;
positions_set_at_cell(_BasePosition, _Cell) ->
    [].
