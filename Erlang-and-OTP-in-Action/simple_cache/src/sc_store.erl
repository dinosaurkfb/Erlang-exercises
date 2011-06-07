%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  1 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(sc_store).

-export([
	 init/0, 
	 insert/2, 
	 lookup/1,
	 lookup/2,
	 delete/1
	]).

-define(TABLE_ID, ?MODULE).
-define(WAIT_FOR_TABLES, 5000).
-record(key_to_pid, {key, pid}).
%%--------------------------------------------------------------------
%% @doc Initialize the sc_store module
%% @spec init() -> ok
%% @end
%%--------------------------------------------------------------------
init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    {ok, CacheNodes} = resource_discovery:fetch_resources(simple_cache),
    dynamic_db_init(lists:delete(node(), CacheNodes)).

dynamic_db_init([]) ->
    {atomic, ok} = mnesia:create_table(key_to_pid,
			[{index, [pid]},
			 {attributes, record_info(fields, key_to_pid)}
			]);
dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).

add_extra_nodes([Node|T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok, [Node]} ->
	    mnesia:add_table_copy(schema, node(), ram_copies),
	    mnesia:add_table_copy(key_to_pid, node(), ram_copies),
	    Tables = mnesia:system_info(tables),
	    mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
	_ ->
	    add_extra_nodes(T)
    end.

%%--------------------------------------------------------------------
%% @doc Insert a {Key, Pid} pair into the simmple cache storage
%% @spec insert(Key, Pid) -> true.
%% @end
%%--------------------------------------------------------------------
insert(Key, Pid) ->
    mnesia:dirty_write(#key_to_pid{key=Key, pid=Pid}).

%%--------------------------------------------------------------------
%% @doc Lookup a {key, pid} pair in which key = Key, and return the 
%% value of pid.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    case mnesia:dirty_read(key_to_pid, Key) of
	[{key_to_pid, Key, Pid}] ->
	    case is_pid_alive(Pid) of
		true -> {ok, Pid};
		false -> {error, not_found}
	    end;
	[] -> {error, not_found}
    end.

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    lists:member(node(Pid), nodes()) andalso
	(rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).

    
%%--------------------------------------------------------------------
%% @doc Lookup a {key, pid} pair in which pid = Pid, and return the 
%% value of pid.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(pid, Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
	[#key_to_pid{} = Record] ->
	    {ok, Record#key_to_pid.pid};
	_ ->
	    {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Delete a {key, pid} pair in which pid = Pid
%% @spec delete(Pid) -> true
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
	[#key_to_pid{} = Record] ->
	    mnesia:dirty_delete_object(Record);
	_ ->
	    ok
    end.

