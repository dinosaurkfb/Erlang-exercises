%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(rd_watch).

-behaviour(gen_server).

%% API
-export([start_link/0, rm_watch/1]).

-export([print/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(PERIOD, 30*1000).

-record(state, {node_list}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
rm_watch(Node) ->
    gen_server:cast(?SERVER, {rm_watch, Node}).
print() ->
    gen_server:cast(?SERVER, print).
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
init([]) ->
    {ok, #state{node_list = []}, ?PERIOD}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({rm_watch, Node}, State) ->
    OldList = State#state.node_list,
    NewList = lists:keydelete(Node, 1, OldList),
     {noreply, State#state{node_list = NewList}};
handle_cast(print, State) ->
    NewList = check_node(State#state.node_list),
    io:format("nodes:~p~n", [NewList]).

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
    NewList = check_node(State#state.node_list),
%%    io:format("nodes:~p~n", [NewList]),
    resource_discovery:trade_resources(),
%%    resource_discovery:print(),
    {noreply, State#state{node_list = NewList}, ?PERIOD}.

check_node(List) ->
    Fun = fun(Node, Acc) ->
		  case lists:keyfind(Node, 1, List) of
		      false ->
			  [{Node, offline} | Acc];
		      _NodeState ->
			  Acc
		  end
	  end,
    List2 = lists:foldl(Fun, List, nodes()),
    Fun2 = fun({Node, _}, Acc) ->
		   case net_adm:ping(Node) of
		       pong ->
			   Now = erlang:now(),
			   [{Node, Now} | Acc];
		       pang ->
			   [{Node, offline} | Acc]
		   end
	   end,
    lists:foldl(Fun2, [], List2).

    
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
