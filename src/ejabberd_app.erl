%%%----------------------------------------------------------------------
%%% File    : ejabberd_app.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : ejabberd's application callback module
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2016   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_app).

-behaviour(ejabberd_config).
-author('alexey@process-one.net').

-behaviour(application).

-export([start_modules/0, start/2, prep_stop/1, stop/1,
	 init/0, opt_type/1]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%
%%% Application API
%%%
%% Ejabberd应用的启动入口函数
start(normal, _Args) ->
	%% 启动当前节点Ejabberd系统的日志(当前版本用的lager日志系统)
	ejabberd_logger:start(),
	%% 将当前进程标识写到环境变量EJABBERD_PID_PATH定义的文件中
	write_pid_file(),
	%% 当前Ejabberd节点上创建jlib受保护的ETS表
	jid:start(),
	%% 当前Ejabberd节点上启动crypto，sasl，ssl，fast_yaml，fast_tls，fast_xml，stringprep，ezlib，cache_tab这些应用
	start_apps(),
	%% 检查ejabberd应用配置的模块是否存在
	ejabberd:check_app(ejabberd),
	%% Ejabberd系统随机模块的启动（当前版本随机模块的启动不做任何操作）
	randoms:start(),
	%% 初始化并启动本地的mnesia数据库，并等待集群数据的同步到本地Ejabberd节点上的Mnesia数据库直到所有表可用或超时
	db_init(),
	%% 单独启动一个init进程，注册该进程自身为ejabberd
	start(),
	%% 从环境变量EJABBERD_MSGS_PATH定义的位置找*.msg 并加载到ets:translations 表中(国际化)
	translate:start(),
	%% 创建两张表ejabberd_ctl_cmds,ejabberd_ctl_host_cmd
	ejabberd_ctl:init(),
	%% 创建表ejabberd_commands
	ejabberd_commands:init(),
	%% 注册常用的ejabberd_command到表ejabberd_commands
	ejabberd_admin:start(),
	%% 创建表ejabberd_modules ETS表
	gen_mod:start(),
	ext_mod:start(),
	%% 创建config,local_config两张表，并加载配置文件中的配置到表中
	ejabberd_config:start(),
	%% 根据配置文件设置日志等级和设置集群中节点间心跳检查的间隔时间
	set_settings_from_config(),
	%% 访问控制
	acl:start(),
	shaper:start(),
	%% 建立到集群中其他节点的链接,其他节点在配置中定义
	connect_nodes(),
	%% 启动一个ejabberd_sup监督进程,并启动和监控定义的子进程
	Sup = ejabberd_sup:start_link(),
	%% 启动数据库相关模块 
	ejabberd_rdbms:start(),
	ejabberd_riak_sup:start(),
	ejabberd_sm:start(),
	%% 启动SASL安全认证模块
	cyrsasl:start(),
	% Profiling
	%ejabberd_debug:eprof_start(),
	%ejabberd_debug:fprof_start(),
	%% 如果运行在windows系统，则添加域名服务器(DNS)地址到erlang系统中
	maybe_add_nameservers(),
	%% 启动所有鉴权模块
	ejabberd_auth:start(),
	ejabberd_oauth:start(),
	%% 启动所有节点上的定义在local_config中的模块
	start_modules(),
	%% 启动配置文件中的监听器模块
	ejabberd_listener:start_listeners(),
	?INFO_MSG("ejabberd ~s is started in the node ~p", [?VERSION, node()]),
	Sup;

start(_, _) ->
	{error, badarg}.

%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
	ejabberd_listener:stop_listeners(),
	stop_modules(),
	ejabberd_admin:stop(),
	broadcast_c2s_shutdown(),
	timer:sleep(5000),
	State.


%% All the processes were killed when this function is called
stop(_State) ->
	?INFO_MSG("ejabberd ~s is stopped in the node ~p", [?VERSION, node()]),
	delete_pid_file(),
	%%ejabberd_debug:stop(),
	ok.

%%%
%%% Internal functions
%%%
%% 单独启动一个init进程，注册该进程自身为ejabberd
start() ->
	spawn_link(?MODULE, init, []).


%% 启动的进程要执行的init操作接口
init() ->
	%% 注册本进程的名字为ejabberd
	register(ejabberd, self()),
	loop().


loop() ->
	receive
		_ ->
			loop()
	end.


%% 当前Ejabberd节点上数据库初始化
db_init() ->
	%% 设置Mnesia数据库的路径
	ejabberd_config:env_binary_to_list(mnesia, dir),
	MyNode = node(),
	DbNodes = mnesia:system_info(db_nodes),
	case lists:member(MyNode, DbNodes) of
		true ->
			ok;
		false ->
			?CRITICAL_MSG("Node name mismatch: I'm [~s], "
							  "the database is owned by ~p", [MyNode, DbNodes]),
			?CRITICAL_MSG("Either set ERLANG_NODE in ejabberdctl.cfg "
							  "or change node name in Mnesia", []),
			erlang:error(node_name_mismatch)
	end,
	case mnesia:system_info(extra_db_nodes) of
		[] ->
			mnesia:create_schema([node()]);
		_ ->
			ok
	end,
	%% 当前Ejabberd节点启动Mnesia数据库
	ejabberd:start_app(mnesia, permanent),
	%% 当前Ejabberd节点的Mnesia数据库等待同步集群的Mnesia数据
	mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).


%% Start all the modules in all the hosts
%% 启动所有节点上的定义在local_config中的模块
start_modules() ->
	lists:foreach(
	  fun(Host) ->
			  Modules = ejabberd_config:get_option(
						  {modules, Host},
						  fun(Mods) ->
								  lists:map(
									fun({M, A}) when is_atom(M), is_list(A) ->
											{M, A}
									end, Mods)
						  end, []),
			  lists:foreach(
				fun({Module, Args}) ->
						gen_mod:start_module(Host, Module, Args)
				end, Modules)
	  end, ?MYHOSTS).


%% Stop all the modules in all the hosts
stop_modules() ->
	lists:foreach(
	  fun(Host) ->
			  Modules = ejabberd_config:get_option(
						  {modules, Host},
						  fun(Mods) ->
								  lists:map(
									fun({M, A}) when is_atom(M), is_list(A) ->
											{M, A}
									end, Mods)
						  end, []),
			  lists:foreach(
				fun({Module, _Args}) ->
						gen_mod:stop_module_keep_config(Host, Module)
				end, Modules)
	  end, ?MYHOSTS).


%% 建立到集群中其他节点的链接,其他节点在配置中定义
connect_nodes() ->
	Nodes = ejabberd_config:get_option(
			  cluster_nodes,
			  fun(Ns) ->
					  true = lists:all(fun is_atom/1, Ns),
					  Ns
			  end, []),
	%% 将配置文件中的集群其他节点连接起来
	lists:foreach(fun(Node) ->
						  net_kernel:connect_node(Node)
				  end, Nodes).

%% If ejabberd is running on some Windows machine, get nameservers and add to Erlang
%% 如果运行在windows系统，则添加域名服务器(DNS)地址到erlang系统中
maybe_add_nameservers() ->
	case os:type() of
		{win32, _} -> add_windows_nameservers();
		_ -> ok
	end.


add_windows_nameservers() ->
	IPTs = win32_dns:get_nameservers(),
	?INFO_MSG("Adding machine's DNS IPs to Erlang system:~n~p", [IPTs]),
	lists:foreach(fun(IPT) -> inet_db:add_ns(IPT) end, IPTs).


broadcast_c2s_shutdown() ->
	Children = ejabberd_sm:get_all_pids(),
	lists:foreach(
	  fun(C2SPid) when node(C2SPid) == node() ->
			  C2SPid ! system_shutdown;
		 (_) ->
			  ok
	  end, Children).

%%%
%%% PID file
%%%
%% 将当前进程标识写到环境变量EJABBERD_PID_PATH定义的文件中
write_pid_file() ->
	case ejabberd:get_pid_file() of
		false ->
			ok;
		PidFilename ->
			write_pid_file(os:getpid(), PidFilename)
	end.


write_pid_file(Pid, PidFilename) ->
	case file:open(PidFilename, [write]) of
		{ok, Fd} ->
			io:format(Fd, "~s~n", [Pid]),
			file:close(Fd);
		{error, Reason} ->
			?ERROR_MSG("Cannot write PID file ~s~nReason: ~p", [PidFilename, Reason]),
			throw({cannot_write_pid_file, PidFilename, Reason})
	end.


delete_pid_file() ->
	case ejabberd:get_pid_file() of
		false ->
			ok;
		PidFilename ->
			file:delete(PidFilename)
	end.


%% 根据配置文件设置日志等级和设置集群中节点间心跳检查的间隔时间
set_settings_from_config() ->
	%% 从当前Ejabberd配置模块中拿到当前配置的日志等级
	Level = ejabberd_config:get_option(
			  loglevel,
			  fun(P) when P>=0, P=<5 -> P end,
			  4),
	%% 根据配置文件中的日志等级设置当期日志等级
	ejabberd_logger:set(Level),
	Ticktime = ejabberd_config:get_option(
				 net_ticktime,
				 opt_type(net_ticktime),
				 60),
	%% 当节点群互连时，会通过心跳包检查所连接节点是不是连接正常，这个心跳时间默认为60s，此处将集群节点中的心跳包检测时间设置为Ticktime
	net_kernel:set_net_ticktime(Ticktime).


%% 当前Ejabberd节点上启动crypto，sasl，ssl，fast_yaml，fast_tls，fast_xml，stringprep，ezlib，cache_tab这些应用
start_apps() ->
	crypto:start(),
	ejabberd:start_app(sasl),
	ejabberd:start_app(ssl),
	ejabberd:start_app(fast_yaml),
	ejabberd:start_app(fast_tls),
	ejabberd:start_app(fast_xml),
	ejabberd:start_app(stringprep),
	ejabberd:start_app(ezlib),
	ejabberd:start_app(cache_tab).


opt_type(net_ticktime) ->
	fun (P) when is_integer(P), P > 0 -> P end;

opt_type(cluster_nodes) ->
	fun (Ns) -> true = lists:all(fun is_atom/1, Ns), Ns end;

opt_type(loglevel) ->
	fun (P) when P >= 0, P =< 5 -> P end;

opt_type(modules) ->
	fun (Mods) ->
			 lists:map(fun ({M, A}) when is_atom(M), is_list(A) ->
								{M, A}
					   end,
					   Mods)
	end;

opt_type(_) -> [cluster_nodes, loglevel, modules, net_ticktime].
