%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  2 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(sc_event).


%% API
-export([
	start_link/0,
	add_handler/2,
	delete_handler/2,
	lookup/1,
	create/2,
	replace/2,
	delete/1,
	 timeout/2
	]).

-define(SERVER, ?MODULE). 


%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    error_logger:info_msg("sc_event start"),
    gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Deletes an event handler
%%
%% @spec delete_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

lookup(Key) ->
    gen_event:notify(?SERVER, {lookup, Key}).
create(Key, Value) ->
    gen_event:notify(?SERVER, {create, {Key, Value}}).
replace(Key, Value) ->
    gen_event:notify(?SERVER, {replace, {Key, Value}}).
delete(Key) ->
    gen_event:notify(?SERVER, {delete, Key}).
timeout(Key, Value) ->
    gen_event:notify(?SERVER, {timeout, {Key, Value}}).
