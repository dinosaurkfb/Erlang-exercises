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

%%--------------------------------------------------------------------
%% @doc Initialize the sc_store module
%% @spec init() -> ok
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @doc Insert a {Key, Pid} pair into the simmple cache storage
%% @spec insert(Key, Pid) -> true.
%% @end
%%--------------------------------------------------------------------
insert(Key, Pid) ->
    ets:insert(?TABLE_ID, {Key, Pid}).

%%--------------------------------------------------------------------
%% @doc Lookup a {key, pid} pair in which key = Key, and return the 
%% value of pid.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
	[{Key, Pid}] -> {ok, Pid};
	[] -> {error, not_found}
    end.
%%--------------------------------------------------------------------
%% @doc Lookup a {key, pid} pair in which pid = Pid, and return the 
%% value of pid.
%% @spec lookup(Key) -> {ok, Pid} | {error, not_found}
%% @end
%%--------------------------------------------------------------------
lookup(pid, Pid) ->
    case ets:match(?TABLE_ID, {'$1', Pid}) of
	[Key] -> {ok, Key};
	[] -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Delete a {key, pid} pair in which pid = Pid
%% @spec delete(Pid) -> true
%% @end
%%--------------------------------------------------------------------
delete(Pid) ->
    ets:match_delete(?TABLE_ID, {'_', Pid}).

