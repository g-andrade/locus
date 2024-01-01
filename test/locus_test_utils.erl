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

-module(locus_test_utils).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([test_cases/1,
         test_cases/2,
         path_with_test_tarballs/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec test_cases(module()) -> [atom()].
test_cases(Module) ->
    test_cases(Module, ["_test"]).

-spec test_cases(module(), [nonempty_string(), ...]) -> [atom()].
test_cases(Module, Suffixes) ->
    exported_functions_with_suffixes(Module, Suffixes).

-spec path_with_test_tarballs() -> nonempty_string().
path_with_test_tarballs() ->
    PrivDir = priv_dir(),
    BuildRoot = filename:dirname(PrivDir),
    filename:join([BuildRoot, "test", "priv"]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

exported_functions_with_suffixes(Module, Suffixes) ->
    [Name || {Name, 1} <- exported_functions(Module),
             lists:any(fun (Suffix) -> lists:suffix(Suffix, atom_to_list(Name)) end,
                       Suffixes)].

exported_functions(Module) ->
    ModuleInfo = Module:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    Exports.

priv_dir() ->
    % from: http://erlang.org/pipermail/erlang-questions/2011-October/062026.html
    case code:priv_dir(locus) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, "priv");
        Priv ->
            Priv
    end.

