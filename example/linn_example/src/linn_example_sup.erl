%%%-------------------------------------------------------------------
%% @doc linn_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(linn_example_sup).

-behaviour(supervisor).

%% API
-export([start_link/0 ,get_pid/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    linn:add_pool(linn_test , linn_example_test , start_link , 4),
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
get_pid()->
    linn:get_process(linn_test).