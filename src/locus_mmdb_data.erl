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

%% @doc API for working with MMDB - data representation
-module(locus_mmdb_data).

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type value()
    :: map_()
    |  utf8_string()
    |  double()
    |  bytes()
    |  uint16()
    |  uint32()
    |  int32()
    |  uint64()
    |  uint128()
    |  array()
    |  boolean()
    |  float_().

-export_type([value/0]).

-type utf8_string() :: unicode:unicode_binary().
-export_type([utf8_string/0]).

-type double() :: float() | '#Inf' | '#-Inf'.
-export_type([double/0]).

-type bytes() :: binary().
-export_type([bytes/0]).

-type uint16() :: 0..((1 bsl 16) - 1).
-export_type([uint16/0]).

-type uint32() :: 0..((1 bsl 32) - 1).
-export_type([uint32/0]).

-type map_() :: #{unicode:unicode_binary() => value()}.
-export_type([map/0]).

-type int32() :: -(1 bsl 31)..((1 bsl 31) - 1).
-export_type([int32/0]).

-type uint64() :: 0..((1 bsl 64) - 1).
-export_type([uint64/0]).

-type uint128() :: 0..((1 bsl 128) - 1).
-export_type([uint128/0]).

-type array() :: [value()].
-export_type([array/0]).

-type boolean_() :: boolean().
-export_type([boolean_/0]).

-type float_() :: double().
-export_type([float_/0]).
