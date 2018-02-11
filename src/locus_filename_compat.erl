%% @private
-module(locus_filename_compat).
-include("locus_pre_otp19_compat.hrl").
-ifndef(POST_OTP_18).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-export([basedir/2]).

%% Application Base Directories
%% basedir
%% http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html

-type basedir_type() :: 'user_cache' | 'user_config' | 'user_data'
                      | 'user_log'
                      | 'site_config' | 'site_data'.

-spec basedir(Type,Application) -> file:filename_all() when
      Type :: basedir_type(),
      Application :: string() | binary().

basedir(Type,Application) when is_atom(Type), is_list(Application) orelse
                                              is_binary(Application) ->
    basedir(Type, Application, #{}).

-spec basedir(Type,Application,Opts) -> file:filename_all() when
      Type :: basedir_type(),
      Application :: string() | binary(),
      Opts :: #{author => string() | binary(),
                os => 'windows' | 'darwin' | 'linux',
                version => string() | binary()}.

basedir(Type,Application,Opts) when is_atom(Type), is_map(Opts),
                                    is_list(Application) orelse
                                    is_binary(Application) ->
    Os   = basedir_os_from_opts(Opts),
    Name = basedir_name_from_opts(Os,Application,Opts),
    Base = basedir_from_os(Type,Os),
    case {Type,Os} of
        {user_log,linux} ->
            filename:join([Base,Name,"log"]);
        {user_log,windows} ->
            filename:join([Base,Name,"Logs"]);
        {user_cache,windows} ->
            filename:join([Base,Name,"Cache"]);
        {Type,_} when Type =:= site_config orelse Type =:= site_data ->
            [filename:join([B,Name]) || B <- Base];
        _ ->
            filename:join([Base,Name])
    end.

basedir_os_from_opts(#{os := linux}) -> linux;
basedir_os_from_opts(#{os := windows}) -> windows;
basedir_os_from_opts(#{os := darwin}) -> darwin;
basedir_os_from_opts(#{}) -> basedir_os_type().

basedir_name_from_opts(windows,App,#{author:=Author,version:=Vsn}) ->
    filename:join([Author,App,Vsn]);
basedir_name_from_opts(windows,App,#{author:=Author}) ->
    filename:join([Author,App]);
basedir_name_from_opts(_,App,#{version:=Vsn}) ->
    filename:join([App,Vsn]);
basedir_name_from_opts(_,App,_) ->
    App.

basedir_from_os(Type,Os) ->
    case Os of
        linux   -> basedir_linux(Type);
        darwin  -> basedir_darwin(Type);
        windows -> basedir_windows(Type)
    end.

-define(basedir_linux_user_data,   ".local/share").
-define(basedir_linux_user_config, ".config").
-define(basedir_linux_user_cache,  ".cache").
-define(basedir_linux_user_log,    ".cache"). %% .cache/App/log
-define(basedir_linux_site_data,   "/usr/local/share/:/usr/share/").
-define(basedir_linux_site_config, "/etc/xdg").

basedir_linux(Type) ->
    case Type of
        user_data   -> getenv("XDG_DATA_HOME",  ?basedir_linux_user_data,  true);
        user_config -> getenv("XDG_CONFIG_HOME",?basedir_linux_user_config,true);
        user_cache  -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_cache, true);
        user_log    -> getenv("XDG_CACHE_HOME", ?basedir_linux_user_log,   true);
        site_data   ->
            Base = getenv("XDG_DATA_DIRS",?basedir_linux_site_data,false),
            string:tokens(Base,":");
        site_config ->
            Base = getenv("XDG_CONFIG_DIRS",?basedir_linux_site_config,false),
            string:tokens(Base,":")
    end.

-define(basedir_darwin_user_data,   "Library/Application Support").
-define(basedir_darwin_user_config, "Library/Application Support").
-define(basedir_darwin_user_cache,  "Library/Caches").
-define(basedir_darwin_user_log,    "Library/Logs").
-define(basedir_darwin_site_data,   "/Library/Application Support").
-define(basedir_darwin_site_config, "/Library/Application Support").

basedir_darwin(Type) ->
    case Type of
        user_data   -> basedir_join_home(?basedir_darwin_user_data);
        user_config -> basedir_join_home(?basedir_darwin_user_config);
        user_cache  -> basedir_join_home(?basedir_darwin_user_cache);
        user_log    -> basedir_join_home(?basedir_darwin_user_log);
        site_data   -> [?basedir_darwin_site_data];
        site_config -> [?basedir_darwin_site_config]
    end.

%% On Windows:
%% ex. C:\Users\egil\AppData\Local\Ericsson\Erlang
%% %LOCALAPPDATA% is defined on Windows 7 and onwards
%% %APPDATA% is used instead of %LOCALAPPDATA% if it's not defined.
%% %APPDATA% is used for roaming, i.e. for user_config on Windows 7 and beyond.
%%
%% user_data    %LOCALAPPDATA%[/$author]/$appname[/$version]
%% user_config  %APPDATA%[/$author]/$appname[/$version]
%% user_cache   %LOCALAPPDATA%[/$author]/$appname[/$version]/Cache
%% user_log     %LOCALAPPDATA%[/$author]/$appname[/$version]/Logs

-define(basedir_windows_user_data,   "Local").
-define(basedir_windows_user_config, "Roaming").
-define(basedir_windows_user_cache,  "Local").    %% Cache is added later
-define(basedir_windows_user_log,    "Local").    %% Logs is added later

basedir_windows(Type) ->
    %% If LOCALAPPDATA is not defined we are likely on an
    %% XP machine. Use APPDATA instead.
    case basedir_windows_appdata() of
        noappdata ->
            %% No AppData is set
            %% Probably running MSYS
            case Type of
                user_data   -> basedir_join_home(?basedir_windows_user_data);
                user_config -> basedir_join_home(?basedir_windows_user_config);
                user_cache  -> basedir_join_home(?basedir_windows_user_cache);
                user_log    -> basedir_join_home(?basedir_windows_user_log);
                site_data   -> [];
                site_config -> []
            end;
        {ok, AppData} ->
            case Type of
                user_data   -> getenv("LOCALAPPDATA", AppData);
                user_config -> AppData;
                user_cache  -> getenv("LOCALAPPDATA", AppData);
                user_log    -> getenv("LOCALAPPDATA", AppData);
                site_data   -> [];
                site_config -> []
            end
    end.

basedir_windows_appdata() ->
    case os:getenv("APPDATA") of
        Invalid when Invalid =:= false orelse Invalid =:= [] ->
            noappdata;
        Val ->
            {ok, Val}
    end.

%% basedir aux

getenv(K,Def,false) -> getenv(K,Def);
getenv(K,Def,true)  -> getenv(K,basedir_join_home(Def)).

getenv(K,Def) ->
    case os:getenv(K) of
        []    -> Def;
        false -> Def;
        Val   -> Val
    end.

basedir_join_home(Dir) ->
    case os:getenv("HOME") of
        false ->
            {ok,[[Home]]} = init:get_argument(home),
            filename:join(Home,Dir);
        Home  -> filename:join(Home,Dir)
    end.

basedir_os_type() ->
    case os:type() of
        {unix,darwin} -> darwin;
        {win32,_}     -> windows;
        _             -> linux
    end.
-endif.
