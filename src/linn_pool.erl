-module(linn_pool).
-author("moein").

-behaviour(gen_server).

%% API
-export([start_link/4]).
-export([add/1]).
-export([get/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(ETS_NAME, linn_pool_sup).

-record(state, {module :: atom(),
                func :: atom(),
                id :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, M, F, Count) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, M, F, Count], []).

-spec add(atom()) -> ok.
add(Id) ->
    gen_server:cast(Id, new).


-spec get(atom()) -> {ok, pid()} | error.
get(Id) ->
    case ets:lookup(?ETS_NAME, Id) of
        [{_, Pids}] ->
            Time = erlang:system_time(),
            case lists:sublist(Pids, ((Time rem length(Pids)) + 1), 1) of
                [] -> error;
                [Pid] -> {ok, Pid}
            end;
        _ -> error
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id, M, F, Count]) ->
    Process = [erlang:apply(M, F, []) || _ <- lists:seq(1, Count)],
    Pids = [Pid || {ok, Pid} <- Process],

    [erlang:monitor(process, Pid) || {ok, Pid} <- Process],

    ets:insert_new(?ETS_NAME, {Id, Pids}),

    {ok, #state{id = Id, module = M, func = F}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(new, #state{id = Id,
                        module = M,
                        func = F} = State) ->

    [{_, Pids}] = ets:lookup(?ETS_NAME, Id),

    {ok, NewPid} = erlang:apply(M, F, []),
    erlang:monitor(process, NewPid),

    ets:delete(?ETS_NAME, Id),
    ets:insert_new(?ETS_NAME, [NewPid | Pids]),

    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{id = Id} = State) ->
    [{_, Pids}] = ets:lookup(?ETS_NAME, Id),

    NewPids = lists:delete(Pid, Pids),

    ets:delete(?ETS_NAME, Id),
    ets:insert_new(?ETS_NAME, NewPids),

    gen_server:cast(self(), new),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
