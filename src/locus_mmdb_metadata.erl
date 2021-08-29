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

-module(locus_mmdb_metadata).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([parse_and_validate/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(METADATA_MARKER, "\xab\xcd\xefMaxMind.com").

%% ------------------------------------------------------------------
%% Type Definitions
%% ------------------------------------------------------------------

-type t()
    :: #{ binary_format_version := binary_format_version(),
          node_count := locus_mmdb_data:uint32(),
          record_size := locus_mmdb_data:uint16(),
          ip_version := 4 | 6,
          database_type := locus_mmdb_data:utf8_string(),
          languages => [locus_mmdb_data:utf8_string()],
          build_epoch := locus_mmdb_data:uint64(), % UNIX timestamp
          description => #{locus_mmdb_data:utf8_string() => locus_mmdb_data:utf8_string()},
          {'$_unrecognized_properties'} => #{locus_mmdb_data:utf8_string()
                                             := locus_mmdb_data_raw:value()}
        }.
-export_type([t/0]).

-type binary_format_version() :: {2, locus_mmdb_data:uint16()}. % {Major, Minor}
-export_type([binary_format_version/0]).

-type parse_or_validation_error()
    :: {marker_not_found, <<_:(14 * 8)>>}
    |  {atom(), term(), list()} % Stacktrace
    |  {not_a_map, term()}
    |  {incompatible_binary_format_version, {locus_mmdb_data_raw:uint16(),
                                             locus_mmdb_data_raw:uint16()}}
    |  {invalid_binary_format_minor_version, term()}
    |  {invalid_binary_format_major_version, term()}
    |  {missing_metadata_keys, [unicode:unicode_binary(), ...]}
    |  {invalid_node_count, locus_mmdb_data_raw:value()}
    |  {missing_node_count, locus_mmdb_data_raw:map_()}
    |  {invalid_record_size, locus_mmdb_data_raw:value()}
    |  {missing_record_size, locus_mmdb_data_raw:map_()}
    |  {invalid_ip_version, locus_mmdb_data_raw:value()}
    |  {missing_ip_version, locus_mmdb_data_raw:map_()}
    |  {invalid_database_type, locus_mmdb_data_raw:value()}
    |  {missing_database_type, locus_mmdb_data_raw:map_()}
    |  {languages_not_an_array, locus_mmdb_data_raw:value()}
    |  {bad_languages, {language_number, pos_integer(), not_an_utf8_string,
                        locus_mmdb_data_raw:value()}}
    |  {description_not_a_map, locus_mmdb_data_raw:value()}
    |  {bad_description, {for_language_code, unicode:unicode_binary(),
                          {not_an_utf8_string, locus_mmdb_data_raw:value()}}}.

-export_type([parse_or_validation_error/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Parses and validates `Metadata' out of `EncodedDatabase'
-spec parse_and_validate(EncodedDatabase) -> {ok, Metadata, OtherSections}
                                             | {error, Reason}
    when EncodedDatabase :: binary(),
         Metadata :: t(),
         OtherSections :: binary(),
         Reason :: parse_or_validation_error().
parse_and_validate(EncodedDatabase) ->
    case binary:matches(EncodedDatabase, <<?METADATA_MARKER>>) of
        [_|_] = PossibleMetadataMarkers ->
            {MetadataStart, _} = lists:last(PossibleMetadataMarkers),
            <<OtherSections:MetadataStart/bytes,
              ?METADATA_MARKER,
              EncodedMetadata/bytes>> = EncodedDatabase,

            parse_and_validate(EncodedMetadata, OtherSections);

        [] ->
            {error, {marker_not_found, <<?METADATA_MARKER>>}}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

parse_and_validate(EncodedMetadata, OtherSections) ->
    try locus_mmdb_data_codec:parse_on_index(0, EncodedMetadata, _Raw = true) of
        {RawMetadata, _RemainingData} ->
            validate(RawMetadata, OtherSections)
    catch
        Class:Reason:Stacktrace ->
            {error, {Class, Reason, Stacktrace}}
    end.

validate({map, MetadataMap}, OtherSections) ->
    case validate_version(MetadataMap) of
        {ok, Metadata} ->
            {ok, Metadata, OtherSections};
        {error, _} = Error ->
            Error
    end;
validate(NotAMap, _OtherSections) ->
    {error, {not_a_map, NotAMap}}.

validate_version(#{<<"binary_format_major_version">> := MajorVersion,
                   <<"binary_format_minor_version">> := MinorVersion
                  } = MetadataMap)
->
    case {MajorVersion, MinorVersion} of
        {{uint16, 2}, {uint16, MinorVersionValue}} when MinorVersionValue >= 0 ->
            RequiredKeys = [node_count, record_size, ip_version,
                            database_type, languages, build_epoch, description],
            Acc0 = [{binary_format_version, {2, MinorVersionValue}}],
            RemainingMetadataMap = maps:without([<<"binary_format_major_version">>,
                                                 <<"binary_format_minor_version">>],
                                                MetadataMap),
            validate_recur(RequiredKeys, RemainingMetadataMap, Acc0);
        {{uint16, MajorVersionValue}, {uint16, MinorVersionValue}} ->
            {error, {incompatible_binary_format_version, {MajorVersionValue, MinorVersionValue}}};
        {{uint16, _}, _} ->
            {error, {invalid_binary_format_minor_version, MinorVersion}};
        {_, _} ->
            {error, {invalid_binary_format_major_version, MajorVersion}}
    end;
validate_version(MetadataMap) ->
    RequiredKeys = [<<"binary_format_major_version">>, <<"binary_format_minor_version">>],
    MissingKeys = RequiredKeys -- maps:keys(MetadataMap),
    {error, {missing_metadata_keys, MissingKeys}}.

validate_recur([node_count | Next], MetadataMap, Acc) ->
    case maps:take(<<"node_count">>, MetadataMap) of
        {{uint32, NodeCountValue}, RemainingMetadataMap} ->
            UpdatedAcc = [{node_count, NodeCountValue} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {InvalidNodeCount, _} ->
            {error, {invalid_node_count, InvalidNodeCount}};
        error ->
            {error, {missing_node_count, MetadataMap}}
    end;
validate_recur([record_size | Next], MetadataMap, Acc) ->
    case maps:take(<<"record_size">>, MetadataMap) of
        {{uint16, RecordSizeValue}, RemainingMetadataMap} ->
            UpdatedAcc = [{record_size, RecordSizeValue} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {InvalidRecordSize, _} ->
            {error, {invalid_record_size, InvalidRecordSize}};
        error ->
            {error, {missing_record_size, MetadataMap}}
    end;
validate_recur([ip_version | Next], MetadataMap, Acc) ->
    case maps:take(<<"ip_version">>, MetadataMap) of
        {{uint16, IpVersionValue}, RemainingMetadataMap}
          when IpVersionValue =:= 6; IpVersionValue =:= 4 ->
            UpdatedAcc = [{ip_version, IpVersionValue} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {InvalidIpVersion, _} ->
            {error, {invalid_ip_version, InvalidIpVersion}};
        error ->
            {error, {missing_ip_version, MetadataMap}}
    end;
validate_recur([database_type | Next], MetadataMap, Acc) ->
    case maps:take(<<"database_type">>, MetadataMap) of
        {{utf8_string, DatabaseTypeValue}, RemainingMetadataMap} ->
            UpdatedAcc = [{database_type, DatabaseTypeValue} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {InvalidDatabaseType, _} ->
            {error, {invalid_database_type, InvalidDatabaseType}};
        error ->
            {error, {missing_database_type, MetadataMap}}
    end;
validate_recur([languages | Next], MetadataMap, Acc) ->
    case validate_languages(MetadataMap) of
        {ok, LanguageValues, RemainingMetadataMap} ->
            UpdatedAcc = [{languages, LanguageValues} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {error, _} = Error ->
            Error;
        skip ->
            validate_recur(Next, MetadataMap, Acc)
    end;
validate_recur([build_epoch | Next], MetadataMap, Acc) ->
    case maps:take(<<"build_epoch">>, MetadataMap) of
        {{uint64, BuildEpochValue}, RemainingMetadataMap} ->
            UpdatedAcc = [{build_epoch, BuildEpochValue} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {InvalidBuildEpoch, _} ->
            {error, {invalid_build_epoch, InvalidBuildEpoch}};
        error ->
            {error, {missing_build_epoch, MetadataMap}}
    end;
validate_recur([description | Next], MetadataMap, Acc) ->
    case validate_description(MetadataMap) of
        {ok, DescriptionValue, RemainingMetadataMap} ->
            UpdatedAcc = [{description, DescriptionValue} | Acc],
            validate_recur(Next, RemainingMetadataMap, UpdatedAcc);
        {error, _} = Error ->
            Error;
        skip ->
            validate_recur(Next, MetadataMap, Acc)
    end;
validate_recur([], MetadataMap, Acc) ->
    case map_size(MetadataMap) =:= 0 of
        true ->
            Metadata = maps:from_list(Acc),
            {ok, Metadata};
        false ->
            UpdatedAcc = [{{'$_unrecognized_properties'}, MetadataMap}],
            Metadata = maps:from_list(UpdatedAcc),
            {ok, Metadata}
    end.

validate_languages(MetadataMap) ->
    case maps:take(<<"languages">>, MetadataMap) of
        {{array, Elements}, RemainingMetadataMap} ->
            validate_languages_recur(Elements, RemainingMetadataMap, _Acc0 = []);
        {NotAnArray, _} ->
            {error, {languages_not_an_array, NotAnArray}};
        error ->
            % `languages' is optional
            skip
    end.

validate_languages_recur([{utf8_string, LanguageValue} | Next],
                         RemainingMetadataMap, Acc) ->
    UpdatedAcc = [LanguageValue | Acc],
    validate_languages_recur(Next, RemainingMetadataMap, UpdatedAcc);
validate_languages_recur([BadLanguage | _Next],
                         _RemainingMetadataMap, Acc) ->
    Position = length(Acc) + 1,
    {error, {bad_languages, {language_number, Position, not_an_utf8_string, BadLanguage}}};
validate_languages_recur([], RemainingMetadataMap, Acc) ->
    LanguageValues = lists:reverse(Acc),
    {ok, LanguageValues, RemainingMetadataMap}.

validate_description(MetadataMap) ->
    case maps:take(<<"description">>, MetadataMap) of
        {{map, Map}, RemainingMetadataMap} ->
            KvList = maps:to_list(Map),
            validate_description_recur(KvList, RemainingMetadataMap, _Acc0 = []);
        {NotAMap, _} ->
            {error, {description_not_a_map, NotAMap}};
        error ->
            % `description' is optional
            skip
    end.

validate_description_recur([{<<LanguageCode/bytes>>, {utf8_string, LocalizedDescription}}
                            | Next],
                           RemainingMetadataMap, Acc) ->
    UpdatedAcc = [{LanguageCode, LocalizedDescription} | Acc],
    validate_description_recur(Next, RemainingMetadataMap, UpdatedAcc);
validate_description_recur([{<<LanguageCode/bytes>>, BadLocalizedDescription}
                            | _Next],
                           _RemainingMetadataMap, _Acc) ->
    {error, {bad_description, {for_language_code, LanguageCode,
                               {not_an_utf8_string, BadLocalizedDescription}}}};
validate_description_recur([], RemainingMetadataMap, Acc) ->
    Description = maps:from_list(Acc),
    {ok, Description, RemainingMetadataMap}.
