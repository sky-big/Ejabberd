%% Author: xxw
%% Created: 2015-11-28
%% Description: 启动Ejabberd节点
-module(run_ejabberd).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).

%%
%% API Functions
%%
%% ejabberd节点启动入口
start() ->
	case run_option:get_options("../options/run.option") of
		[] ->
			io:format("run_ejabberd start find run.option is empty~n");
		Options ->
			Prefix = run_option:get_prefix(Options),
			Nodes = run_option:get_nodes(Options),
			BeamDir = run_option:get_beam_dir(Options),
			file:set_cwd(BeamDir),
			%% 启动配置文件中所有配置的RabbitMQ节点
			lists:foreach(fun(RunOpt) ->
								  run_node(Options, Prefix, RunOpt, nowait)
						  end, Nodes)
	end.


%% 停止RabbitMQ集群系统
stop_cluster() ->
	OptionFile = "../options/run.option",
	case run_option:get_options(OptionFile) of
		[] ->
			io:format("start_cluster start find run.option is empty~n");
		Options ->
			PreFix = proplists:get_value(prefix, Options),
			
			IpList = run_option:get_ip_list(Options),
			MyIpList = os_util:get_localips(),
			
			MyIps = lists:filter(fun(IpStr) ->
										 lists:member(IpStr, IpList)
								 end, MyIpList),
			case MyIps of
				[] ->
					io:format("get ip error ~n");
				[Ip | _] ->
					NodeStr = str_util:datetime_to_short_string(erlang:localtime()),
					AddOption = " -pa ../ebin -s start_cluster stop " ++ OptionFile ++ " -s init stop",
					case os:type() of
						{win32, nt} ->
							os_util:run_erl(normal, PreFix ++ NodeStr, Ip, [], smp, wait, AddOption);
						_ ->
							os_util:run_erl(hiden, PreFix ++ NodeStr, Ip, [], smp, wait, AddOption)
					end
			end
	end.

%%
%% Local Functions
%%
%% 单个节点的启动入口函数
run_node(Options, Prefix, RunOpt, Wait) ->
	{SNode, Host, _ClientPort, Smp, _NodeType, Addition} = RunOpt,
	NodeName = str_util:sprintf("~s@~s", [Prefix ++ SNode, Host]),
	%% 设置环境变量
	set_env(Options),
	NewAddition = get_ejabberd_node_start_args(Options, NodeName) ++ " " ++ Addition,
	CommandLine = os_util:get_erl_cmd(normal, Prefix ++ SNode, Host, [], Smp, Wait, NewAddition),
	case Wait of
		wait ->
			os_util:wait_exe(CommandLine);
		nowait ->
			os_util:run_exe(CommandLine)
	end.


%% 获得Ejabberd节点相关的启动参数
get_ejabberd_node_start_args(Options, NodeName) ->
	%% 最大的Erlang进程数量
	EjabberdErlangMaxProcess = proplists:get_value(ejabberd_max_process, Options),
	%% Mnesia数据存储目录
	EjabberdMnesiaDbPath = proplists:get_value(ejabberd_db_path, Options) ++ "/" ++ NodeName,
	
	%% 获得代码beam路径
	CodeBeamPath = " -pa " ++ proplists:get_value(beam_dir, Options) ++ " ",
	%% 启动后执行的模块和函数名
	StartModuleAndFun = " -s ejabberd_application start" ++ " ",
	%% Mnesia配置
	MnesiaOptionStr = " -mnesia dir \\\"" ++ EjabberdMnesiaDbPath ++ "\\\"",
	%% Erlang配置
	ErlangOptionStr = " -smp enable +P " ++ integer_to_list(EjabberdErlangMaxProcess) ++ " ",
	
	%% 组合所有的启动参数
	lists:append([CodeBeamPath, StartModuleAndFun, MnesiaOptionStr, ErlangOptionStr]).


%% RabbitMQ集群系统的停止接口
stop(Args) ->
	[OptionPath | _] = Args,
	case run_option:get_options(OptionPath) of
		[] ->
			io:format("start_cluster start find run.option is empty~n");
		Options ->
			%% 统一节点的cookie
			Cookie = proplists:get_value(cookie, Options),
			erlang:set_cookie(node(), Cookie),
			Prefix = run_option:get_prefix(Options),
			Nodes = run_option:get_nodes(Options),
			lists:foreach(fun({SNode, Ip, _, _, _, _}) ->
								  NodeName = str_util:sprintf("~s@~s", [Prefix ++ SNode, Ip]),
								  stop_node(NodeName)
						  end, Nodes)
	end.


%% RabbitMQ集群单个节点的停止
stop_node(NodeName) ->
	rabbit_cli:rpc_call(list_to_atom(NodeName), ?MODULE, stop_node1, []),
	io:format("stopping the node ~p ~n", [NodeName]),
	NodeName.


%% 停止节点相关操作
stop_node1() ->
	rabbit:stop_and_halt(),
	os:cmd("exit()").


%% 设置Ejabberd相关环境变量
set_env(Options) ->
	%% 获取Ejabberd单个节点的配置文件路径
	EjabberdOptionsPath = proplists:get_value(ejabberd_options_path, Options),
	%% EjabberdPriv路径
	EjabberdPrivPath = proplists:get_value(ejabberd_priv_path, Options),
	%% Ejabberd文档路径
	EjabberdDocPath = proplists:get_value(ejabberd_doc_path, Options),
	
	%% 设置相关环境变量
	%% 设置配置文件的环境变量
	os:putenv("EJABBERD_CONFIG_PATH", EjabberdOptionsPath ++ "/ejabberd.yml"),
	%% 设置ejabberdctl.cfg文件路径
	os:putenv("EJABBERDCTL_CONFIG_PATH", EjabberdOptionsPath ++ "/ejabberdctl.cfg"),
	%% Ejabberd节点翻译路径的设置
	os:putenv("EJABBERD_MSGS_PATH", EjabberdPrivPath ++ "/msgs"),
	%% Ejabberd Bin路径环境变量设置
	os:putenv("EJABBERD_BIN_PATH", EjabberdPrivPath ++ "/bin"),
	%% Ejabberd文档路径环境变量的设置
	os:putenv("EJABBERD_DOC_PATH", EjabberdDocPath),
	
	%% 设置ERL_INETRC环境变量
	os:putenv("ERL_INETRC", EjabberdOptionsPath ++ "/inetrc"),
	%% 设置单个Ejabberd节点最大的port数量
	os:putenv("ERL_MAX_PORTS", integer_to_list(proplists:get_value(ejabberd_max_port, Options))),
	%% 设置单个节点Ejabberd节点上的最大ETS数量
	os:putenv("ERL_MAX_ETS_TABLES", integer_to_list(proplists:get_value(ejabberd_max_ets, Options))).
