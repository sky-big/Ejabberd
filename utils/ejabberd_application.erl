%%% -------------------------------------------------------------------
%%% Author  : xxw
%%% Description :
%%%
%%% Created : 2016-03-25
%%% -------------------------------------------------------------------
-module(ejabberd_application).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([
		 start/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start() ->
	case erlang:whereis(?MODULE) of
		undefined ->
			gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
		_ ->
			nothing
	end.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	case run_option:get_options("../options/run.option") of
		[] ->
			io:format("start_cluster start find run.option is empty~n");
		Options ->
			%% 设置当前节点的cookie
			erlang:set_cookie(node(), proplists:get_value(cookie, Options)),
			%% 设置单个Ejabberd节点的环境变量
			set_env(Options),
			%% 启动Ejabberd系统
			ejabberd:start()
	end,
	%% 代码更新相关初始化
%% 	version_up:init(),
	{ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% 设置单个Ejabberd节点的环境变量
set_env(Options) ->
	%% 统一节点的cookie
	Cookie = proplists:get_value(cookie, Options),
	erlang:set_cookie(node(), Cookie),
	%% 如果是windows则修改cmd窗口的标题等相关东西
	case os:type() of
		{win32, nt} ->
			SNodeName = node_util:get_node_name(node()),
			TitleCmdLine = "title " ++ SNodeName,
			os:cmd(TitleCmdLine);
		_ ->
			nothing
	end,
	%% 获取Ejabberd日志存储路径
	EjabberdLogPath = proplists:get_value(ejabberd_log_path, Options),
	%% 设置当前Ejabberd节点的日志路径
	os:putenv("EJABBERD_LOG_PATH", EjabberdLogPath ++ "/" ++ atom_to_list(node()) ++ "/ejabberd.log"),
	%% 设置Erlang崩溃文件路径
	os:putenv("ERL_CRASH_DUMP", EjabberdLogPath ++ "/" ++ atom_to_list(node()) ++ "/erl_crash.log").