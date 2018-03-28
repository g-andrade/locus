-module(locus_httpd).

-export([start/1]).
-export([stop/1]).

start(DocumentRoot) ->
    ServerName = "localhost",
    {ok, Pid} = inets:start(httpd, [{server_name, ServerName},
                                    {port, 0},
                                    {server_root, "."},
                                    {document_root, DocumentRoot},
                                    {modules,
                                     [% https://github.com/simplegeo/erlang/blob/master/lib/inets/examples/server_root/conf/httpd.conf
                                      mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_responsecontrol, mod_trace,
                                      mod_range, mod_head, %mod_include,
                                      mod_dir, mod_get, mod_log, mod_disk_log
                                     ]}
                                   ]),
    Info = httpd:info(Pid),
    Port = proplists:get_value(port, Info),
    BaseURL = iolist_to_binary(io_lib:format("http://~s:~p", [ServerName, Port])),
    {ok, Pid, binary_to_list(BaseURL)}.

stop(Pid) ->
    inets:stop(httpd, Pid).
