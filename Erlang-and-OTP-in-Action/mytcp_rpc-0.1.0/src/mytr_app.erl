%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, KONGVM
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(mytr_app).
-behaviour(application).

-export([
	 start/2,
	 stop/1
	 ]).

start(_Type, _StartArgs) ->
    case mytr_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Other  ->
	    io:format("kfb: mytr_app:call mytr_sup:start_link() failed:~n"),
	    {error, Other}
    end.

stop(_State) ->
    ok.
