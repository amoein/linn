-module(linn_app).

-author("amoein").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    linn_sup:start_link().

stop(_State) ->
    ok.
