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

-define(ETS_NAME(P), list_to_atom(atom_to_list(linn_pool_sup_) ++ atom_to_list(P))).

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
    ID = ?ETS_NAME(Id),
    case ets:info(ID) of
        undefined -> error;

        E ->
            case lists:keyfind(size, 1, E) of
                {size, 0} -> error;

                {size, N} ->
                    Indexes = round(N / 2),
                    Index = rand:uniform_s(Indexes,rand:seed(exrop)),

                    case ets:lookup(ID, Index) of
                        [{Index, Pid}] -> Pid;

                        _ -> error

                    end

            end
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id, M, F, Count]) ->
    ID = ?ETS_NAME(Id),

    ID = ets:new(ID, [public, named_table, {read_concurrency, true}]),

    [begin
         {ok, Pid} = erlang:apply(M, F, []),

         erlang:monitor(process, Pid),

         ets:insert_new(ID, {Index, Pid}),

         ets:insert_new(ID, {Pid, Index})

     end || Index <- lists:seq(1, Count)],


    {ok, #state{id = Id, module = M, func = F}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(new, #state{id = Id, module = M, func = F} = State) ->
    ID = ?ETS_NAME(Id),

    case ets:info(ID) of
        undefined -> {noreply, State};

        E ->
            {size, N} = lists:keyfind(size, 1, E),

            Indexes = round(N / 2),
            Index = Indexes + 1,

            {ok, NewPid} = erlang:apply(M, F, []),

            erlang:monitor(process, NewPid),

            ets:insert_new(ID, {Index, NewPid}),

            ets:insert_new(ID, {NewPid, Index}),

            {noreply, State}
    end;

handle_cast({new, Index}, #state{id = Id, module = M, func = F} = State) ->
    ID = ?ETS_NAME(Id),

    {ok, NewPid} = erlang:apply(M, F, []),

    erlang:monitor(process, NewPid),

    ets:insert_new(ID, {Index, NewPid}),
    ets:insert_new(ID, {NewPid, Index}),

    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{id = Id} = State) ->
    ID = ?ETS_NAME(Id),

    [{_, Index}] = ets:lookup(ID, Pid),

    ets:delete(ID, Pid),
    ets:delete(ID, Index),

    gen_server:cast(self(), {new, Index}),

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
