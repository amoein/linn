-module(linn).

-author("amoein").

-include("linn.hrl").

%% API
-export([add_pool/1, get_process/1]).

-spec add_pool(Option :: option()) -> ok.
add_pool(Option) ->
    linn_sup:add_pool(Option).

-spec get_process(Id :: atom()) -> {ok, Pid :: pid()} | {error, not_found}.
get_process(Id) ->
    linn_pool:get(Id).
