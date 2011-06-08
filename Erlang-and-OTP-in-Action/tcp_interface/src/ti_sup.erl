%%%-------------------------------------------------------------------
%%% @author Kong Fanbin <kfbuniversity@gmail.com>
%%% @copyright (C) 2011, Kong Fanbin
%%% @doc
%%%
%%% @end
%%% Created :  7 Jun 2011 by Kong Fanbin <kfbuniversity@gmail.com>
%%%-------------------------------------------------------------------
-module(ti_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% gen_server callbacks
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
start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?SERVER, []).
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
init([LSock]) ->
RestartStrategy = simple_one_for_one,
MaxRestarts = 0,
MaxSecondsBetweenRestarts = 1,

SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

Restart = temporary,
Shutdown = brutal_kill,
Type = worker,

Server = {ti_server, {ti_server, start_link, [LSock]},
Restart, Shutdown, Type, [ti_server]},

{ok, {SupFlags, [Server]}}.

