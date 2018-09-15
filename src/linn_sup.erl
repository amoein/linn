%%%-------------------------------------------------------------------
%% @doc linn top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(linn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_pool/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_pool(Id, M ,F, Count) ->
    supervisor:start_child(?MODULE, #{id => Id,
                                      start =>  {linn_pool, start_link, [Id, M ,F, Count]}}).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
