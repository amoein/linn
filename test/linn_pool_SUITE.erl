-module(linn_pool_SUITE).

-author("amoein").

-include_lib("common_test/include/ct.hrl").

-include("linn.hrl").

%% Test server callbacks
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([pool/1, multi/1]).

suite() -> [].

init_per_suite(Config) ->
    application:start(linn),
    Option1 = #option{
        pool_id = test1,
        handler_module = linn_test_server,
        handler_func = start_link,
        handler_func_arity = 0,
        process_count = 10
    },
    Option2 = #option{
        pool_id = test2,
        handler_module = linn_test_server,
        handler_func = start_link,
        handler_func_arity = 0,
        process_count = 100
    },
    linn:add_pool(Option1),
    [{option1, Option1}, {option2, Option2} | Config].

end_per_suite(_Config) -> ok.

all() -> [pool, multi].

pool(_Config) ->
    Res = linn:get_process(test1),
    erlang:is_pid(Res).

multi(_Config) ->
    T1 = erlang:system_time(millisecond),

    L = [
        begin
            Res = linn:get_process(test2),
            erlang:is_pid(Res)
        end
     || _ <- lists:seq(1, 1_000_000)
    ],

    T2 = erlang:system_time(millisecond),
    [?LOG_INFO("Result ~p", [(T2 - T1)])],

    not lists:member(false, L).
