%% Copyright (c) 2017-2018 Guilherme Andrade
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
-module(locus_maps_compat).
-include("locus_pre_otp19_compat.hrl").
-ifndef(POST_OTP_18).

-export([take/2]).
-export([update_with/3]).
-export([update_with/4]).

-spec take(term(), map()) -> {term(), map()} | error.
take(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            {Value, maps:remove(Key, Map)};
        error ->
            error
    end.

-spec update_with(term(), fun ((term()) -> term()), map()) -> map().
update_with(Key, Fun, Map) ->
    Value = maps:get(Key, Map),
    NewValue = Fun(Value),
    maps:update(Key, NewValue, Map).

-spec update_with(term(), fun ((term()) -> term()), term(), map()) -> map().
update_with(Key, Fun, Default, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            NewValue = Fun(Value),
            maps:update(Key, NewValue, Map);
        error ->
            maps:put(Key, Default, Map)
    end.
-endif.
