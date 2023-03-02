-module(linn_pool_SUITE).
-author("aleyandro").

-include_lib("common_test/include/ct.hrl").
%% Test server callbacks
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([pool/1, multi/1]).

suite() ->
    [].

init_per_suite(Config) ->
    application:start(linn),

    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [
        pool,
        multi
    ].

pool(_Config) ->
    linn:add_pool(test, linn_test_server, start_link, 10),
    Res = linn:get_process(test),
    erlang:is_pid(Res).

multi(_Config) ->
    linn:add_pool(test2, linn_test_server, start_link, 10),
    T1 = erlang:system_time(millisecond),

    L = [
        begin
            Res = linn:get_process(test2),
            erlang:is_pid(Res)
        end
     || _ <- lists:seq(1, 1000000)
    ],

    T2 = erlang:system_time(millisecond),
    [ct:print("Result ~p", [(T2 - T1)])],

    not lists:member(false, L).
