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

-define(SERVER, ?MODULE).

-record(state, {ets :: string(),
                m :: atom(), f :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, M, F, Count) ->
    io:format("~n ~p T ~n", [{M, F, Count}]),
    gen_server:start_link({local, Id}, ?MODULE, [Id, M, F, Count], []).

add(Id) ->
    gen_server:cast(Id, new).

get(Id) ->
    Name0 = erlang:atom_to_list(?MODULE) ++ "_" ++ erlang:atom_to_list(Id),

    Name = erlang:list_to_atom(Name0),

    [{_, Count}] = ets:lookup(Name, count),
    Time = erlang:system_time(),

    [{_, Pid}] = ets:lookup(Name, ((Time rem Count) + 1)),

    Pid.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Id, M, F, Count]) ->
    Name0 = erlang:atom_to_list(?MODULE) ++ "_" ++ erlang:atom_to_list(Id),
    Name = erlang:list_to_atom(Name0),

    Process = [{Item, erlang:apply(M, F, [])} || Item <- lists:seq(1, Count)],
    [erlang:monitor(process, Pid) || {_, {ok, Pid}} <- Process],

    ets:new(Name, [named_table,{read_concurrency, true}]),

    [begin
         ets:insert_new(Name, {Item, Pid}),
         ets:insert_new(Name, {Pid, Item})

     end || {Item, {ok, Pid}} <- Process],

    ets:insert_new(Name, {count, Count}),

    {ok, #state{ets = Name, m = M, f = F}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(new, #state{ets = ETS,
                        m = M,
                        f = F} = State) ->

    [{_, Count}] = ets:lookup(ETS, count),

    Head = Count + 1,
    {ok, NewPid} = erlang:apply(M, F, []),
    erlang:monitor(process, NewPid),

    ets:delete(ETS, count),
    ets:delete(ETS, {count, Head}),
    ets:insert_new(ETS, {Head, NewPid}),
    ets:insert_new(ETS, {NewPid, Head}),

    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{ets = ETS,
                                                          m = M,
                                                          f = F} = State) ->
    [{Pid, Head}] = ets:lookup(ETS, Pid),
    ets:delete(ETS, Head),
    ets:delete(ETS, Pid),

    {ok, NewPid} = erlang:apply(M, F, []),

    ets:insert_new(ETS, {Head, NewPid}),
    ets:insert_new(ETS, {NewPid, Head}),

    erlang:monitor(process, NewPid),

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
