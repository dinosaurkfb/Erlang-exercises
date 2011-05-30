%%%-------------------------------------------------------------------
%%% @author Kong Fanbin  <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Free
%%% @doc RPC over TCP server. This module defines a server processe that
%%%      listens for incoming TCP connections and allows the user to 
%%%      execute RPC commands via that TCP stream.
%%% @end
%%% Created : 26 May 2011 by  <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(mytr_server).

-behaviour(gen_server).

%% API
-export([start_link/1, get_count/0, start/0, stop/0]).

%% Debug
-export([do_rpc/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(DEFAULT_PORT, 1055).
-record(state, {port, lsock, request_count = 0}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server with a port number
%%
%% @spec start_link(Port) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server using the default port number.
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc
%% Get times the server is requested.
%% @spec get_count() -> {ok, Cnt}
%% @end
%%--------------------------------------------------------------------
get_count() ->
    {reply, Cnt, _State} = gen_server:call(?SERVER, get_count),
    {ok, Cnt}.

%%--------------------------------------------------------------------
%% @doc Stops the server
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    %% {active, true}表示收到从socket收到的任何包都作为消息发送给接收
    %% 进程
    {ok, Lsock} = gen_tcp:listen(Port, [{active, true}]),
    %% 返回值中最后的0告诉mytr_server，init调用结束后立即触发一个 
    %% timeout，这个timeout会产生一条timeout消息，并调用handle_info
    %% 来处理
    {ok, #state{port=Port, lsock=Lsock}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_count, _From, State) ->
    {reply, State#state.request_count, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    io:format("handle_cast:stop~n"),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    io:format("handle_info:timeout~n"),
    %% init之后，mytr_server会阻塞在下面的accept处，所以刚初始化之后
    %% 立即调用get_count()无法得到mytr_server进程的回复，并且会引起
    %% 一个异常，如下：
    %% 2> tr_server:get_count().
    %% ** exception exit: {timeout,{gen_server,call,[tr_server,get_count]}}
    %% in function  gen_server:call/2
    %% 这也解释了为什么start_link()之后，马上调用stop(),虽然返回了ok，
    %% 但是其实tr_server并没有停止，因为gen_server进程此时同样无法处理
    %% stop()，但之所以没有像get_count()那样产生异常，是因为stop()用的
    %% 是gen_server:cast,是异步调用。
    {ok, ASock} = gen_tcp:accept(State#state.lsock),
    %% accept返回的socket不用记录的原因是根据gen_tcp的最后一个参数:
    %% {active, true}，该socket上接收的所有包将作为消息发送给此进程
    %% 消息格式为{tcp, Socket, Data}, 所以只需要在handle_info回调
    %% 函数中处理{tcp, Socket, Data}消息即可。
    io:format("handle_info:gen_tcp:accept=~p~n", [ASock]),
    {noreply, State};
handle_info({tcp, Socket, Data}, State) ->
    io:format("Socket:~p recved:~p~n", [Socket, Data]),
    do_rpc(Data),
    %gen_tcp:send(Socket, Data ++ "recved!"),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Do the real RPC work.
%% @spec do_rpc(Data) -> {ok}
%% where
%%  Data = string()
%% @end
%%--------------------------------------------------------------------
do_rpc(Data) ->
    Data1 = re:replace(Data, "\r\n", ""),
    io:format("do_rpc:Data1:~p~n", [Data1]),
    Data2 = re:replace(Data, "\r\n", "", [{return, list}]),
    io:format("do_rpc:Data2:~p~n", [Data2]),
    {match, Captured} = re:run(Data1, "(.*):(.*)\s*\\((.*)\\)\.$",
				[{capture, [1,2,3], list}, ungreedy]),
    io:format("do_rpc:Captured:~p~n", [Captured]).

