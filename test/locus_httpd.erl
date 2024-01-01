%% Copyright (c) 2018-2024 Guilherme Andrade
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

-module(locus_httpd).

-export([start/1]).
-export([stop/1]).

start(DocumentRoot) ->
    ServerName = "localhost",

    % https://github.com/simplegeo/erlang/blob/master/lib/inets/examples/server_root/conf/httpd.conf
    Modules = [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_responsecontrol, mod_trace,
               mod_range, mod_head, %mod_include,
               mod_dir, mod_get, mod_log, mod_disk_log
              ],

    {ok, Pid} = inets:start(httpd, [{server_name, ServerName},
                                    {port, 0},
                                    {server_root, "."},
                                    {document_root, DocumentRoot},
                                    {modules, Modules}]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    BaseURL = iolist_to_binary(io_lib:format("http://~s:~p", [ServerName, Port])),
    {ok, Pid, binary_to_list(BaseURL)}.

stop(Pid) ->
    inets:stop(httpd, Pid).
