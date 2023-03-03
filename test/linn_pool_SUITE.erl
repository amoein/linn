-module(linn_pool_SUITE).

-author("amoein").

-include_lib("eunit/include/eunit.hrl").

-include("linn.hrl").

%% Test server callbacks
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([pool/1, multi/1, fail_recover/1]).

all() ->
    [pool, multi, fail_recover].

suite() -> [].

init_per_suite(Config) ->
    application:start(linn),
    linn:add_pool(#option{
        pool_id = test1,
        handler_module = linn_test_server,
        handler_func = start_link,
        handler_func_arity = [],
        process_count = 10
    }),
    linn:add_pool(#option{
        pool_id = test2,
        handler_module = linn_test_server,
        handler_func = start_link,
        handler_func_arity = [],
        process_count = 100
    }),
    linn:add_pool(#option{
        pool_id = test3,
        handler_module = linn_test_server,
        handler_func = start_link,
        handler_func_arity = [],
        process_count = 1
    }),
    Config.

end_per_suite(_Config) -> ok.

pool(_Config) ->
    {ok, Res} = linn:get_process(test1),
    erlang:is_pid(Res).

multi(_Config) ->
    T1 = erlang:system_time(millisecond),

    Result = lists:foldr(
        fun(_, Acc) ->
            case linn:get_process(test2) of
                {ok, Res} -> erlang:is_pid(Res) and Acc;
                _ -> false
            end
        end,
        true,
        lists:seq(1, 100_000)
    ),

    T2 = erlang:system_time(millisecond),
    ?LOG_INFO("Result time out for 100K request: ~p ms", [(T2 - T1)]),
    ?assert(Result).

fail_recover(_Config) ->
    {ok, Res1} = linn:get_process(test3),
    ?LOG_INFO("Res: ~p", [Res1]),
    % terminate normally
    Res1 ! kill,
    timer:sleep(1),
    {ok, Res2} = linn:get_process(test3),
    ?LOG_INFO("Res1: ~p Res2: ~p", [Res1, Res2]),
    ?assertNotEqual(Res1, Res2),
    % terminate abnormally
    exit(Res2, "check"),
    timer:sleep(1),
    {ok, Res3} = linn:get_process(test3),

    ?LOG_INFO("Res2: ~p Res3: ~p", [Res2, Res3]),
    ?assertNotEqual(Res2, Res3).
