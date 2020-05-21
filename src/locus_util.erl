%% Copyright (c) 2017-2020 Guilherme Andrade
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
-module(locus_util).

-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [parse_ip_address/1,
    lists_anymap/2,
    lists_take/2,
    bin_to_hex_str/1,
    expect_linked_process_termination/1,
    dialyzer_opaque_atom/1,
    url_query_encode/1,
    filesystem_safe_name/1,
    is_utf8_binary/1,
    is_unicode_string/1,
    is_date/1,
    purge_term_of_very_large_binaries/1,
    resolve_http_location/2,
    censor_url_query/2,
    parse_absolute_http_url/1
   ]).

-ignore_xref(
   [dialyzer_opaque_atom/1
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(is_uint8(V), ((V) band 16#FF =:= (V))).
-define(is_uint16(V), ((V) band 16#FFFF =:= (V))).

-define(TERM_PURGE_LARGE_BINARY_THRESHOLD, (1024 * 1024)).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec parse_ip_address(binary() | string() | inet:ip_address())
        -> {ok, inet:ip_address()} | {error, einval}.
parse_ip_address({A,B,C,D} = Address)
  when ?is_uint8(A), ?is_uint8(B), ?is_uint8(C), ?is_uint8(D) ->
    {ok, Address};
parse_ip_address({A,B,C,D,E,F,G,H} = Address)
  when ?is_uint16(A), ?is_uint16(B), ?is_uint16(C), ?is_uint16(D),
       ?is_uint16(E), ?is_uint16(F), ?is_uint16(G), ?is_uint16(H) ->
    {ok, Address};
parse_ip_address(Binary) when is_binary(Binary) ->
    String = binary_to_list(Binary),
    parse_ip_address(String);
parse_ip_address(String) when length(String) >= 0 ->
    case string:tokens(String, "/") of
        [StrAddress] ->
            inet:parse_address(StrAddress);
        [StrAddress, _PrefixLenStr] ->
            inet:parse_address(StrAddress);
        _ ->
            {error, einval}
    end;
parse_ip_address(_Invalid) ->
    {error, einval}.

-spec lists_anymap(fun ((term()) -> boolean() | {true,term()}), list()) -> {true,term()} | false.
lists_anymap(Fun, [H|T]) ->
    case Fun(H) of
        {true, Mapped} -> {true, Mapped};
        true -> {true, H};
        false -> lists_anymap(Fun, T)
    end;
lists_anymap(_Fun, []) ->
    false.

-spec lists_take(term(), list()) -> {ok, list()} | error.
lists_take(Elem, List) ->
    lists_take_recur(Elem, List, []).

-spec bin_to_hex_str(binary()) -> [48..57 | 97..102].
bin_to_hex_str(Bin) ->
    bin_to_hex_str_recur(Bin, []).

-spec bin_to_hex_str_recur(bitstring(), [48..57 | 97..102]) -> [48..57 | 97..102].
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) when Nibble < 10 ->
    bin_to_hex_str_recur(Rest, [$0 + Nibble | Acc]);
bin_to_hex_str_recur(<<Nibble:4, Rest/bits>>, Acc) ->
    bin_to_hex_str_recur(Rest, [$a + Nibble - 10 | Acc]);
bin_to_hex_str_recur(<<>>, Acc) ->
    lists:reverse(Acc).

-spec expect_linked_process_termination(pid()) -> boolean().
expect_linked_process_termination(Pid) ->
    case flush_link_exit(Pid, 5000) of
        true -> true;
        false ->
            exit(Pid, kill),
            flush_link_exit(Pid, 1000)
    end.

-spec dialyzer_opaque_atom(atom()) -> atom().
dialyzer_opaque_atom(Atom) ->
    list_to_atom( atom_to_list(Atom) ).

-spec url_query_encode(unicode:chardata()) -> binary().
url_query_encode(Chardata) ->
    <<Binary/bytes>> = unicode:characters_to_binary(Chardata),
    << <<(url_query_encode_codepoint(Codepoint))/bytes>> || <<Codepoint/utf8>> <= Binary >>.

-spec filesystem_safe_name(binary()) -> binary().
filesystem_safe_name(Name) ->
    OnlyWordsAndSpaces = re:replace(Name, "[^\\w\\s-]+", "-", [global, unicode, ucp]),
    re:replace(OnlyWordsAndSpaces, "[-\\s]+", "-", [global, unicode, ucp, {return, binary}]).

-spec is_utf8_binary(term()) -> boolean().
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
is_utf8_binary(_) ->
    false.

-spec is_unicode_string(term()) -> boolean().
is_unicode_string(Value)
  when length(Value) >= 0 ->
    try unicode:characters_to_binary(Value) of
        <<_/bytes>> -> true;
        _ -> false
    catch
        _:_ -> false
    end;
is_unicode_string(_) ->
    false.

-spec is_date(term()) -> boolean().
is_date(Date) ->
    try calendar:valid_date(Date) of
        IsValidDate -> IsValidDate
    catch
        _:_ -> false
    end.

-spec purge_term_of_very_large_binaries(term()) -> term().
purge_term_of_very_large_binaries([H|T]) ->
    MappedH = purge_term_of_very_large_binaries(H),
    MappedT = purge_term_of_very_large_binaries(T),
    [MappedH | MappedT];
purge_term_of_very_large_binaries(Tuple)
  when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    MappedList = purge_term_of_very_large_binaries(List),
    list_to_tuple(MappedList);
purge_term_of_very_large_binaries(Map)
  when is_map(Map) ->
    List = maps:to_list(Map),
    MappedList = purge_term_of_very_large_binaries(List),
    maps:from_list(MappedList);
purge_term_of_very_large_binaries(Binary)
  when is_binary(Binary) ->
    Size = byte_size(Binary),
    ReferencedSize = binary:referenced_byte_size(Binary),
    if Size >= ?TERM_PURGE_LARGE_BINARY_THRESHOLD ->
           {'__$VERY_LARGE_BINARY', #{size => Size}};
       ReferencedSize >= ?TERM_PURGE_LARGE_BINARY_THRESHOLD ->
           binary:copy(Binary);
       true ->
           Binary
    end;
purge_term_of_very_large_binaries(Other) ->
    Other.

-spec resolve_http_location(string(), string()) -> {ok, string()} | {error, term()}.
resolve_http_location(BaseURL, Location) ->
    % See:
    % * https://tools.ietf.org/html/rfc7231#page-68
    % * https://tools.ietf.org/html/rfc3986#section-4.2

    case parse_absolute_http_url(Location) of
        {ok, _} ->
            % location is an absolute URL
            {ok, Location};
        {error, not_absolute_http_url} ->
            % location is a relative URL
            BaseURLParseSuccess = parse_absolute_http_url(BaseURL),
            % we ignore URL fragments as they wouldn't be sent to the server anyway.
            {ok, {Scheme, UserInfo, Host, Port, _Path, _Query, _Fragment}} = BaseURLParseSuccess,
            case Location of
                "//" ++ _ ->
                    % network-path reference
                    NewURL = format_string("~s:~s", [Scheme, Location]),
                    {ok, NewURL};
                "/" ++ _ ->
                    % we should consider the colon character rule here
                    NewURL = build_url(Scheme, UserInfo, Host, Port, Location, ""),
                    {ok, NewURL};
                _ ->
                    {error, bad_relative_location}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec censor_url_query(string(), [unicode:charlist()]) -> string().
censor_url_query(URL, KeysToCensor) ->
    % we ignore URL fragments as they wouldn't have been sent to the server anyway.
    {ok, {Scheme, UserInfo, Host, Port, Path,
          Query, _Fragment}} = parse_absolute_http_url(URL),

    case Query of
        "?" ++ QueryBody ->
            BinEncodedKeysToCensor = [url_query_encode(Key) || Key <- KeysToCensor],
            PrefixesForWhichToCensorSuffix = [binary_to_list(BinEncodedKey) ++ "="
                                              || BinEncodedKey <- BinEncodedKeysToCensor],

            QueryPairs = string:tokens(QueryBody, [$&]),
            CensoredQueryPairs = [maybe_censor_url_query_pair(Pair, PrefixesForWhichToCensorSuffix)
                                  || Pair <- QueryPairs],
            CensoredQueryBody = lists:join($&, CensoredQueryPairs),
            CensoredQuery = [$? | CensoredQueryBody],
            build_url(Scheme, UserInfo, Host, Port, Path, CensoredQuery);
        "" ->
            URL
    end.

-ifdef(HTTP_URI_PARSE_DEPRECATED).
-spec parse_absolute_http_url(string())
        -> {ok, {atom(), string(), string(), inet:port_number(),
                 string(), string(), string()}}
           | {error, not_absolute_http_url}
           | {error, {atom(), term()}}.
parse_absolute_http_url(URI) ->
    case uri_string:parse(URI) of
        #{scheme := SchemeStr, host := Host} = ParsedURI
          when (SchemeStr =:= "http" orelse SchemeStr =:= "https"),
               length(Host) > 0 ->

            Scheme = list_to_existing_atom(SchemeStr),
            DefaultPort =
                case Scheme of
                    http -> 80;
                    https -> 443
                end,
            Query =
                case ParsedURI of
                    #{query := QueryStr} when length(QueryStr) > 0 ->
                        [$? | QueryStr]; % simulate http_uri:parse/2 behaviour
                    #{} ->
                        ""
                end,

            {ok, {Scheme,
                  maps:get(userinfo, ParsedURI, ""),
                  Host,
                  maps:get(port, ParsedURI, DefaultPort),
                  maps:get(path, ParsedURI, "/"),
                  Query,
                  maps:get(fragment, ParsedURI, "")
                 }};
        #{} ->
            {error, not_absolute_http_url};
        {error, Reason, Context} ->
            {error, {Reason, Context}}
    end.
-else.
-spec parse_absolute_http_url(string())
        -> {ok, {atom(), string(), string(), inet:port_number(),
                 string(), string(), string()}}
           | {error, term()}.
parse_absolute_http_url(URI) ->
    case http_uri:parse(URI, [{fragment, true}]) of
        {ok, {Scheme, _, Host, _, _, _, _}} = Success
          when (Scheme =:= http orelse Scheme =:= https),
               length(Host) > 0 ->
            Success;
        {ok, _} ->
            {error, not_absolute_http_url};
        {error, no_scheme} ->
            {error, not_absolute_http_url};
        {error, _} = Error ->
            Error
    end.
-endif.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec lists_take_recur(term(), list(), list()) -> {ok, list()} | error.
lists_take_recur(Elem, [H|T], Acc) when Elem =:= H ->
    {ok, lists:reverse(Acc, T)};
lists_take_recur(Elem, [H|T], Acc) ->
    lists_take_recur(Elem, T, [H|Acc]);
lists_take_recur(_, [], _) ->
    error.

-spec flush_link_exit(pid(), timeout()) -> boolean().
flush_link_exit(Pid, Timeout) ->
    receive
        {'EXIT', Pid, _} -> true
    after
        Timeout -> false
    end.

url_query_encode_codepoint(Codepoint)
  when Codepoint >= $A, Codepoint =< $Z;
       Codepoint >= $a, Codepoint =< $z;
       Codepoint >= $0, Codepoint =< $9;
       Codepoint =:= $*;
       Codepoint =:= $-;
       Codepoint =:= $.;
       Codepoint =:= $_ ->
    <<Codepoint>>;
url_query_encode_codepoint($\ ) ->
    <<$+>>;
url_query_encode_codepoint(Codepoint) ->
    UTF8Bytes = <<Codepoint/utf8>>,
    << <<$%, (integer_to_binary(Byte, 16))/bytes>> || <<Byte>> <= UTF8Bytes >>.

-spec build_url(atom(), string(), iodata(), inet:port_number(), iodata(), iodata()) -> string().
build_url(Scheme, UserInfo, Host, Port, Path, Query)
  when UserInfo =:= "",
       ((Scheme =:= http andalso Port =:= 80) orelse
        (Scheme =:= https andalso Port =:= 443))
       ->
    format_string("~s://~s~s~s", [Scheme, Host, Path, Query]);
build_url(Scheme, UserInfo, Host, Port, Path, Query)
  when UserInfo =:= "" ->
    format_string("~s://~s:~b~s", [Scheme, Host, Port, Path, Query]);
build_url(Scheme, UserInfo, Host, Port, Path, Query) ->
    format_string("~s://~s@~s:~b~s", [Scheme, UserInfo, Host, Port, Path, Query]).

%-spec format_string(string(), list()) -> string().
format_string(Fmt, Args) ->
    IoData = io_lib:format(Fmt, Args),
    Binary = iolist_to_binary(IoData),
    binary_to_list(Binary).

maybe_censor_url_query_pair(Pair, PrefixesForWhichToCensorSuffix) ->
    case lists_anymap(
           fun (Prefix) -> lists:prefix(Prefix, Pair) end,
           PrefixesForWhichToCensorSuffix)
    of
        {true, Prefix} ->
            Prefix ++ "XXXXXXXXXXXXXXXX";
        false ->
            Pair
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions - Unit Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

resolve_http_location_test() ->
    BaseURL = "http://www.foo.bar",
    AbsoluteLocation = "http://www.example.com/whatnot",
    NetworkPathReference = "//google.com/thing",
    RelativeReference = "/etc",

    ?assertEqual({ok, AbsoluteLocation},
                 resolve_http_location(BaseURL, AbsoluteLocation)),
    ?assertEqual({ok, "http:" ++ NetworkPathReference},
                 resolve_http_location(BaseURL, NetworkPathReference)),
    ?assertEqual({ok, BaseURL ++ RelativeReference},
                 resolve_http_location(BaseURL, RelativeReference)).

-endif.
