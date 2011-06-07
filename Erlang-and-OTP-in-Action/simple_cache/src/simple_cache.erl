%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  1 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(simple_cache).

-export([insert/2, lookup/1, delete/1, timeout/2]).

insert(Key, Value) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:replace(Pid, Value);
	{error, _} ->
	    {ok, Pid} = sc_element:create(Value, 60),
	    sc_store:insert(Key, Pid),
	    sc_event:create(Key, Value)
    end.

lookup(Key) ->
    try
	sc_event:lookup(Key),
	{ok, Pid} = sc_store:lookup(Key),
	{ok, Value} = sc_element:fetch(Pid),
	{ok, Value}
    catch
	_Class:_Exception ->
	    {error, not_found}
    end.

delete(Key) ->
    case sc_store:lookup(Key) of
	{ok, Pid} ->
	    sc_element:delete(Pid),
	    sc_event:delete(Key);
	{error, _Reason} ->
	    ok
    end.

timeout(Pid, Value) ->
    case sc_store:lookup(pid, Pid) of
	{ok, Key} ->
	    sc_event:timeout(Key, Value);
	{error, _Reason} ->
	    ok
    end.
