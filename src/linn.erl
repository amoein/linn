-module(linn).
-author("info@moein").

%% API
-export([add_pool/4, get_process/1]).

add_pool(Id, M, F, Count) ->
    linn_sup:add_pool(Id, M, F, Count).

get_process(Id) ->
    linn_pool:get(Id).
