-module(linn_pool_SUITE).
-author("aleyandro").

-include_lib("common_test/include/ct.hrl").
%% Test server callbacks
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([pool/1]).

suite() ->
    [].


init_per_suite(Config) ->
    application:start(linn),

    Config.


end_per_suite(_Config) ->
    ok.

all() ->
    [pool].

pool(_Config) ->
    linn:add_pool(test, linn_test_server, start_link, 10),
    Res = linn:get_process(test),
    erlang:is_pid(Res).