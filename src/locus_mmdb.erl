%% Copyright (c) 2017-2021 Guilherme Andrade
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

%% @doc API for working with MMDB
-module(locus_mmdb).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([unpack_database/1,
         unpack_tree_data_and_data_section/2,
         lookup_address/2]).

-ignore_xref(
        [unpack_tree_data_and_data_section/2]).

%% ------------------------------------------------------------------
%% Debug API Function Exports
%% ------------------------------------------------------------------

-export([parse_all_data_section_values/1,
         parse_data_section_value/2]).

-ignore_xref(
        [parse_all_data_section_values/1,
         parse_data_section_value/2]).

%% ------------------------------------------------------------------
%% API Type Definitions
%% ------------------------------------------------------------------

-type database()
    :: #{ metadata := locus_mmdb_metadata:t(),
          tree := locus_mmdb_tree:t(),
          data_section := binary()
        }.
-export_type([database/0]).

-type unpack_error()
    :: {bad_metadata, locus_mmdb_metadata:parse_or_validation_error()}

    | failed_to_unpack_tree_data_and_data_section()

    | {bad_tree,
       {{because, locus_mmdb_tree:bad_tree_error()},
        {with_metadata, locus_mmdb_metadata:t()}}}.

-export_type([unpack_error/0]).

-type failed_to_unpack_tree_data_and_data_section()
    :: {intermediate_128bits_of_zero_not_found_after_tree,
        {{not_zeroes, binary()},
         {at_offset, non_neg_integer()},
         {with_metadata, locus_mmdb_metadata:t()}}}

    |  {missing_data_after_tree,
        {{required, {128, bits}},
         {but_got, {0..127, bits}},
         {at_offset, non_neg_integer()},
         {with_metadata, locus_mmdb_metadata:t()}}}

    | {not_enough_data_for_tree,
       {{required, {pos_integer(), bytes}},
        {but_got, {non_neg_integer(), bytes}},
        {with_metadata, locus_mmdb_metadata:t()}}}.

-export_type([failed_to_unpack_tree_data_and_data_section/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Unpacks an `EncodedDatabase' binary into a ready-to-use `Database'.
-spec unpack_database(EncodedDatabase) -> {ok, Database} | {error, ErrorReason}
        when EncodedDatabase :: binary(),
             Database :: database(),
             ErrorReason :: unpack_error().
unpack_database(<<EncodedDatabase/bytes>>) ->
    try
        unpack_database_(EncodedDatabase)
    catch
        Class:Reason:Stacktrace ->
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            erlang:raise(Class, SaferReason, SaferStacktrace)
    end.

%% @doc Unpacks a single `TreeAndDataSection' binary into
%% separate `TreeData' and `DataSection' binaries, leveraging
%% `Metadata' to calculate the size of `TreeData'.
%%
%% You can then use `TreeData' on your own or leverage it
%% to instantiate a new `locus_mmdb_tree:t()',
%% through {@link locus_mmdb_tree:new/5}.
%%
%% As for `DataSection', you can also either use it on your own
%% or pass it to the API defined under {@link locus_mmdb_data_codec}.
%%
%% For how to obtain `Metadata' and `TreeAndDataSection'
%% in the first place, see {@link locus_mmdb_metadata:parse_and_validate/1}.
%%
-spec unpack_tree_data_and_data_section(Metadata, TreeAndDataSection)
        -> {ok, TreeData, DataSection} | {error, Reason}
        when Metadata :: locus_mmdb_metadata:t(),
             TreeAndDataSection :: binary(),
             TreeData :: binary(),
             DataSection :: binary(),
             Reason ::  failed_to_unpack_tree_data_and_data_section().
unpack_tree_data_and_data_section(Metadata, TreeAndDataSection) ->
    #{node_count := NodeCount, record_size := RecordSize} = Metadata,
    TreeSize = ((RecordSize * 2) div 8) * NodeCount,

    case TreeAndDataSection of
        <<TreeData:TreeSize/bytes, 0:128, DataSection/bytes>> ->
            {ok, TreeData, DataSection};
        <<_:TreeSize/bytes, NotZeroes:128/bits, _NotZeroes/bytes>> ->
            {error, {intermediate_128bits_of_zero_not_found_after_tree,
                     {{not_zeroes, NotZeroes},
                      {at_offset, TreeSize},
                      {with_metadata, Metadata}}}};
        <<_:TreeSize/bytes, MissingData/bits>> ->
            {error, {missing_data_after_tree,
                     {{required, {128, bits}},
                      {bot_got, {bit_size(MissingData), bits}},
                      {at_offset, TreeSize},
                      {with_metadata, Metadata}}}};
        <<MissingTree/bytes>> ->
            {error, {not_enough_data_for_tree,
                     {{required, {TreeSize, bytes}},
                      {but_got, {byte_size(MissingTree), bytes}},
                      {with_metadata, Metadata}}}}
    end.

%% @doc Looks up for an entry matching `Address' within `Database'
-spec lookup_address(Address, Database) -> {ok, Entry} | not_found | {error, ErrorReason}
    when Address :: inet:ip_address() | string() | unicode:unicode_binary(),
         Database :: database(),
         Entry :: locus_mmdb_data:value(),
         ErrorReason :: term().
lookup_address(Address, Database) ->
    case locus_util:parse_ip_address(Address) of
        {ok, ParsedAddress} ->
            lookup_parsed_address(ParsedAddress, Database);
        {error, einval} ->
            {error, {invalid_address, Address}}
    end.

%% ------------------------------------------------------------------
%% Debug API Function Definitions
%% ------------------------------------------------------------------

-spec parse_all_data_section_values(file:name_all())
        -> [locus_mmdb_data_raw:value()].
%% @private
parse_all_data_section_values(Filename) ->
    {ok, EncodedDatabase} = file:read_file(Filename),
    {ok, Database} = unpack_database_(EncodedDatabase),
    #{data_section := DataSection} = Database,
    locus_mmdb_data_codec:parse_all(DataSection, _Raw = true).

-spec parse_data_section_value(file:name_all(), locus_mmdb_data_codec:index())
        -> locus_mmdb_data_raw:value().
%% @private
parse_data_section_value(Filename, Index) ->
    {ok, EncodedDatabase} = file:read_file(Filename),
    {ok, Database} = unpack_database_(EncodedDatabase),
    #{data_section := DataSection} = Database,
    {Entry, _RemainingData} = locus_mmdb_data_codec:parse_on_index(Index, DataSection,
                                                                   _Raw = true),
    Entry.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

unpack_database_(EncodedDatabase) ->
    case locus_mmdb_metadata:parse_and_validate(EncodedDatabase) of
        {ok, Metadata, OtherSections} ->
            TreeAndDataSection = OtherSections,
            unpack_tree_and_data_section(Metadata, TreeAndDataSection);
        {error, Reason} ->
            {error, {bad_metadata, Reason}}
    end.

unpack_tree_and_data_section(Metadata, TreeAndDataSection) ->
    case unpack_tree_data_and_data_section(Metadata, TreeAndDataSection) of
        {ok, TreeData, DataSection} ->
            instantiate_database(Metadata, TreeData, DataSection);
        {error, _} = Error ->
            Error
    end.

instantiate_database(Metadata, TreeData, DataSection) ->
    #{node_count := NodeCount, record_size := RecordSize, ip_version := IpVersion} = Metadata,

    case locus_mmdb_tree:new(TreeData, NodeCount, RecordSize, IpVersion,
                             byte_size(DataSection))
    of
        {ok, Tree} ->
            {ok, #{metadata => Metadata,
                   tree => Tree,
                   data_section => DataSection}};
        {error, Reason} ->
            {error, {bad_tree,
                     {{because, Reason},
                      {with_metadata, Metadata}}}}
    end.

lookup_parsed_address(ParsedAddress, Database) ->
    #{tree := Tree, data_section := DataSection} = Database,

    case locus_mmdb_tree:lookup(ParsedAddress, Tree) of
        {ok, DataIndex} ->
            lookup_address_data(DataIndex, DataSection);
        not_found ->
            not_found;
        {error, _} = Error ->
            Error
    end.

lookup_address_data(DataIndex, DataSection) ->
    try locus_mmdb_data_codec:parse_on_index(DataIndex, DataSection, _Raw = false) of
        {Entry, _RemainingData} ->
            {ok, Entry}
    catch
        Class:Reason:Stacktrace ->
            SaferReason = locus_util:purge_term_of_very_large_binaries(Reason),
            SaferStacktrace = locus_util:purge_term_of_very_large_binaries(Stacktrace),
            erlang:raise(Class, SaferReason, SaferStacktrace)
    end.
