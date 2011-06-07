%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  3 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(rd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RdMain = {resource_discovery, {resource_discovery, start_link, []},
                  permanent, 2000, worker, [resource_discovery]},
    RdWatch = {rd_watch, {rd_watch, start_link, []},
                  permanent, 2000, worker, [rd_watch]},
%%    EventManager = {rd_event, {rd_event, start_link, []},    
%%			permanent, 2000, worker, [rd_event]},
%%    Children = [RdMain, EventManager, RdWatch],
    Children = [RdMain, RdWatch],
    RestartStrategy = {one_for_one, 2, 60},
         
    {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
