%%%-------------------------------------------------------------------
%%% @author moein
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2018 1:26 PM
%%%-------------------------------------------------------------------
-module(linn).
-author("moein").

%% API
-export([add_pool/4]).

add_pool(Id, M, F, Count) ->
    linn_sup:add_pool(Id, M, F, Count).
