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

-module(locus).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([lookup/1]).
-export([lookup/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec lookup(Address) -> {ok, Entry} | {error, Error}
            when Address :: inet:ip_address() | nonempty_string() | binary(),
                 Entry :: #{ atom() => term() | Entry },
                 Error :: (not_found | invalid_address | no_databases_configured |
                           {database_not_loaded, DatabaseId}),
                 DatabaseId :: atom().
lookup(Address) ->
    lookup_(Address, en).


-spec lookup(Address, Language) -> {ok, Entry} | {error, Error}
            when Address :: inet:ip_address() | nonempty_string() | binary(),
                 Language :: binary(),
                 Entry :: #{ atom() => term() | Entry },
                 Error :: (not_found | invalid_address | no_databases_configured |
                           {database_not_loaded, DatabaseId} | unknown_language),
                 DatabaseId :: atom().
lookup(Address, LanguageBin) ->
    try binary_to_existing_atom(LanguageBin, utf8) of
        Language ->
            lookup_(Address, Language)
    catch
        error:badarg ->
            {error, unknown_language}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

lookup_(Address, Language) ->
    case locus_sup:database_ids() of
        [] ->
            {error, no_databases_configured};
        DatabaseIds ->
            lookup_recur(DatabaseIds, Address, Language, #{})
    end.

lookup_recur([DatabaseId | NextDatabaseIds], Address, Language, Acc) ->
    case locus_mmdb:lookup(DatabaseId, Address) of
        {ok, Metadata, Entry} ->
            DatabaseType = maps:get(database_type, Metadata),
            BuildEpoch = maps:get(build_epoch, Metadata),
            Acc2 =
                maps:update_with(
                  DatabaseType,
                  fun ({OlderBuildEpoch, _OlderEntry}) when OlderBuildEpoch < BuildEpoch ->
                          {BuildEpoch, Entry};
                      (Other) ->
                          Other
                  end,
                  {BuildEpoch, Entry}, Acc),
            lookup_recur(NextDatabaseIds, Address, Language, Acc2);
        {error, not_found} ->
            lookup_recur(NextDatabaseIds, Address, Language, Acc);
        {error, invalid_address} ->
            {error, invalid_address};
        {error, database_not_loaded} ->
            {error, {database_not_loaded, DatabaseId}};
        {error, database_unknown} -> % race condition?
            lookup_recur(NextDatabaseIds, Address, Language, Acc)
    end;
lookup_recur([], _Address, _Language, Acc) when map_size(Acc) =:= 0 ->
    {error, not_found};
lookup_recur([], _Address, Language, Acc) ->
    GeoEntry = select_geo_entry(Acc),
    AsnEntry = select_asn_entry(Acc),
    Entry = maps:merge(GeoEntry, AsnEntry),
    {ok, localize_entry(Entry, Language)}.

select_geo_entry(#{ <<"GeoLite2-City">> := {_BuildEpoch, CityEntry} }) ->
    CityEntry;
select_geo_entry(#{ <<"GeoLite2-Country">> := {_BuildEpoch, CountryEntry} }) ->
    CountryEntry;
select_geo_entry(#{}) ->
    #{}.

select_asn_entry(#{ <<"GeoLite2-ASN">> := {_BuildEpoch, AsnEntry} }) ->
    AsnEntry;
select_asn_entry(#{}) ->
    #{}.

localize_entry(Entry, Language) when is_map(Entry) ->
    case maps:take(names, Entry) of
        {#{ Language := Name }, Entry2} ->
            Entry2#{ name => Name };
        {_MissingLanguage, Entry2} ->
            Entry2;
        error ->
            maps:map(
              fun (_Key, ChildEntry) ->
                      localize_entry(ChildEntry, Language)
              end,
              Entry)
    end;
localize_entry(Entry, Language) when is_list(Entry) ->
    [localize_entry(ChildEntry, Language) || ChildEntry <- Entry];
localize_entry(Entry, _Language) ->
    Entry.
