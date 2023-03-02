-module(linn_pool).

-author("amoein").

-behaviour(gen_server).

-include("linn.hrl").

%% API
-export([start_link/1, add/1, get/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(ETS_NAME(P), list_to_atom(atom_to_list(linn_pool_sup_) ++ atom_to_list(P))).

-record(state, {option :: option(), id :: atom()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add(atom()) -> ok.
add(Id) ->
    gen_server:cast(Id, new).

-spec get(atom()) -> {ok, pid()} | error.
get(Id) ->
    ID = ?ETS_NAME(Id),
    Info = ets:info(ID),
    if
        Info == undefined ->
            error;
        true ->
            case lists:keyfind(size, 1, Info) of
                {size, 0} ->
                    error;
                {size, N} ->
                    {Index, _} = rand:uniform_s(N, rand:seed(exrop)),
                    % ?LOG_ERROR("REsult ~p", [ets:match(ID, {'$1',Index,'$2'})])
                    case ets:match(ID, {'$1',Index,'$2'}) of
                        [{Pid, _}] -> {ok, Pid};
                        _ -> error
                    end
            end
    end.

-spec start_link(option()) -> {ok, pid()}.
start_link(#option{pool_id = Id} = Opts) ->
    ?LOG_DEBUG("Chiled Resive", []),
    gen_server:start_link({local, Id}, ?MODULE, [Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([option()]) -> {ok, #state{}}.
init([Opts]) when is_record(Opts, option) ->
    Id = ?ETS_NAME(Opts#option.pool_id),

    Id = ets:new(Id, [ordered_set, public, named_table, {read_concurrency, true}]),

    [gen_server:cast(self(), {new, 0}) || _ <- lists:seq(1, Opts#option.process_count)],

    {ok, #state{option = Opts, id = Id}}.

-spec handle_call(any(), any(), #state{}) -> {reply, any(), #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({new, Index}, #state{id = Id, option = Opts} = State) ->
    Info = ets:info(Id),
    if
        Info == undefined ->
            {noreply, State};
        true ->
            InsertIndex =
                if
                    Index == 0 ->
                        {size, ItemsCount} = lists:keyfind(size, 1, Info),
                        ItemsCount + 1;
                    true ->
                        Index
                end,

            {ok, NewPid} = erlang:apply(
                Opts#option.handler_module,
                Opts#option.handler_func,
                [Opts#option.handler_func_arity]
            ),

            Ref = erlang:monitor(process, NewPid),

            ets:insert_new(Id, {NewPid, InsertIndex, Ref}),

            {noreply, State}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{id = Id} = State) ->
    [{Pid, Index, _Ref_1}] = ets:take(Id, Pid),
    ?LOG_INFO("Server down: ~p", [{process_down, Pid, _Reason}]),
    ets:delete(Id, Pid),

    gen_server:cast(self(), {new, Index}),

    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
