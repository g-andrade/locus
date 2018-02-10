%% @private
-module(locus_gen_compat).
-include("locus_pre_otp19_compat.hrl").
-ifndef(POST_OTP_18).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
%%
-export([get_parent/0]).
-export([get_proc_name/1]).
-export([debug_options/2]).
-export([unregister_name/1]).

-type proc_name() :: pid() | {local,atom()} | {global,atom()} | {via,module(),term()}.
-export_type([proc_name/0]).

-spec get_parent() -> pid() | no_return().
get_parent() ->
	case get('$ancestors') of
		[Parent | _] when is_pid(Parent) ->
			Parent;
		[Parent | _] when is_atom(Parent) ->
			name_to_pid(Parent);
		_ ->
			exit(process_was_not_started_by_proc_lib)
	end.

-spec name_to_pid(atom()) -> pid() | no_return().
name_to_pid(Name) ->
	case whereis(Name) of
		undefined ->
			case global:whereis_name(Name) of
				undefined ->
					exit(could_not_find_registered_name);
				Pid ->
					Pid
			end;
		Pid ->
			Pid
	end.

-spec get_proc_name(proc_name()) -> term() | no_return().
get_proc_name(Pid) when is_pid(Pid) ->
	Pid;
get_proc_name({local, Name}) ->
	case process_info(self(), registered_name) of
		{registered_name, Name} ->
			Name;
		{registered_name, _Name} ->
			exit(process_not_registered);
		[] ->
			exit(process_not_registered)
	end;
get_proc_name({global, Name}) ->
	case global:whereis_name(Name) of
		undefined ->
			exit(process_not_registered_globally);
		Pid when Pid =:= self() ->
			Name;
		_Pid ->
			exit(process_not_registered_globally)
	end;
get_proc_name({via, Mod, Name}) ->
	case Mod:whereis_name(Name) of
		undefined ->
			exit({process_not_registered_via, Mod});
		Pid when Pid =:= self() ->
			Name;
		_Pid ->
			exit({process_not_registered_via, Mod})
	end.

-spec debug_options(term(), [{debug,term()}]) -> term().
debug_options(Name, Opts) ->
	case lists:keyfind(debug, 1, Opts) of
		{_,Options} ->
			try sys:debug_options(Options)
			catch _:_ ->
					  error_logger:format(
						"~p: ignoring erroneous debug options - ~p~n",
						[Name,Options]),
					  []
			end;
		false ->
			[]
	end.

-spec unregister_name(proc_name()) -> ok.
unregister_name({local,Name}) ->
	try unregister(Name) of
		_ -> ok
	catch
		_:_ -> ok
	end;
unregister_name({global,Name}) ->
	_ = global:unregister_name(Name),
	ok;
unregister_name({via, Mod, Name}) ->
	_ = Mod:unregister_name(Name),
	ok;
unregister_name(Pid) when is_pid(Pid) ->
	ok.
-endif.
