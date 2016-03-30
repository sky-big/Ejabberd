-module(win_service).
-author("Christophe Romain <christophe.romain@process-one.net>").

-export([start/0,stop/0]).

start() ->
    c:cd(filename:join(code:root_dir(), "bin")),
    start_ejabberd(),
    net_kernel:monitor_nodes(true),
    net_kernel:connect_node(configured_node()),
    loop().

stop() ->
    win_service_loop ! {quit, self()},
    receive
    stopped -> ok
    after 10000 -> timeout
    end.

loop() ->
    case whereis(win_service_loop) of
    undefined -> register(win_service_loop, self());
    Pid -> true
    end,
    receive
    {nodeup, Node} ->
	io:format("node ~p started~n",[Node]),
	loop();
    {nodedown, Node} ->
	io:format("node ~p stopped~n",[Node]);
    {quit, From} ->
	io:format("stopping service~n",[]),
	stop_ejabberd(),
	From ! stopped
    end,
    unregister(win_service_loop),
    halt(0).

configured_node() ->
    Node = os:cmd("sed -e \"/^ERLANG_NODE/!d;s/.*=//\" ../conf/ejabberdctl.cfg"),
    list_to_atom(hd(string:tokens(Node, "\r\n\t "))).

start_ejabberd() ->
    os:cmd("bash ejabberdctl start"),
    os:cmd("bash ejabberdctl started").

stop_ejabberd() ->
    os:cmd("bash ejabberdctl stop"),
    os:cmd("bash ejabberdctl stopped").

