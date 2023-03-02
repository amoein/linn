-module(linn_sup).

-author("amoein").

-behaviour(supervisor).

-include("linn.hrl").

%% API
-export([start_link/0, add_pool/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_pool(#option{pool_id = Id} = Opts) ->
    supervisor:start_child(?MODULE, #{
        id => Id,
        start => {linn_pool, start_link, [Opts]}
    }).
%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.
