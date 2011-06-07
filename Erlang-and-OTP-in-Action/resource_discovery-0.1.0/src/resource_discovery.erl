%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(resource_discovery).

-behaviour(gen_server).

%% API
-export([
	 start_link/0,
	 add_target_resource_type/1,
	 add_local_resource/2,
	 fetch_resources/1,
	 trade_resources/0,
	 print/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {target_resource_types,
		local_resource_tuples,
		found_resource_tuples
	       }).
print() ->
    gen_server:cast(?SERVER, print).

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

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

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
    {ok, #state{target_resource_types = [],
		local_resource_tuples = dict:new(),
		found_resource_tuples = dict:new()
	       }}.

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
handle_call({fetch_resources, Type}, _From, State) ->
    FoundTuples = State#state.found_resource_tuples,
    {reply, dict:find(Type, FoundTuples), State}.

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
handle_cast(print, State) ->
    io:format("target_resource_types:~p~n", [State#state.target_resource_types]),
    io:format("local_resource_tuples:~p~n", [State#state.local_resource_tuples]),
    io:format("found_resource_tuples:~p~n", [State#state.found_resource_tuples]),
    {noreply, State};

handle_cast({add_target_resource_type, Type}, State) ->
    #state{target_resource_types = OldTarget} = State,
    NewTarget = [Type | lists:delete(Type, OldTarget)],
    {noreply, State#state{target_resource_types = NewTarget}};

handle_cast(trade_resources, State) ->
    LocalTuples = State#state.local_resource_tuples,
    Nodes = [node() | nodes()],
    lists:foreach(fun(Node) ->
			  gen_server:cast({?SERVER, Node}, {trade_resources, {node(), LocalTuples}})
		  end, Nodes),
    {noreply, State};

handle_cast({trade_resources, {From, Remotes}}, 
	    #state{target_resource_types = Targets,
		   local_resource_tuples = Locals,
		  found_resource_tuples = OldFounds} = State) ->
    NewFoundInRemotes = resources_for_types(Targets, Remotes),
    %% NewFoundInRemotes is a list of {Type, Resouce} pairs.
    %% 同一个 Type 下的 Resouces 需要展开成多个{Type, Resouce}对
    NewFounds = add_resource_tuples(NewFoundInRemotes, OldFounds),
    case From of
	noreply ->
	    ok;
	_ ->
	    gen_server:cast({?SERVER, From}, {trade_resources, {noreply, Locals}})
    end,
    {noreply, State#state{found_resource_tuples = NewFounds}};

handle_cast({add_local_resource, {Type, Instance}}, State) ->
    #state{local_resource_tuples = OldLocal} = State,
    NewLocal = add_resource_tuple(Type, Instance, OldLocal),
    {noreply, State#state{local_resource_tuples = NewLocal}}.


add_resource_tuple(Type, Resource, ResourceTuples) ->
    case dict:find(Type, ResourceTuples) of
	{ok, ResourceLists} ->
	    NewLists = [Resource | lists:delete(Resource, ResourceLists)],
	    dict:store(Type, NewLists, ResourceTuples);
	error ->
	    dict:store(Type, [Resource], ResourceTuples)
    end.
add_resource_tuples([{Type, Resource} | T], Old) ->
    add_resource_tuples(T, add_resource_tuple(Type, Resource, Old));
add_resource_tuples([], T) ->
    T.
    
resources_for_types(Types, ResourceTuples) ->
    Fun = fun(Type, Acc) ->
		  case dict:find(Type, ResourceTuples) of
		      {ok, Resources} ->
			  Fun2 = fun(R, Acc2) ->
					 [{Type, R} | Acc2]
				 end,
			  lists:foldl(Fun2, Acc, Resources);
		      error ->
			  Acc
		  end
	  end,
    lists:foldl(Fun, [], Types).

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
handle_info(_Info, State) ->
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
