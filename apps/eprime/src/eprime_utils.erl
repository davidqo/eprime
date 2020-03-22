%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Март 2020 14:23
%%%-------------------------------------------------------------------
-module(eprime_utils).
-author("davidqo").

%% API
-export([is_prime/1]).

-spec is_prime(N :: pos_integer()) -> boolean().
is_prime(N) ->
    do_test_prime(N, 2, math:sqrt(N)).

do_test_prime(_, Divider, MaxDivider) when Divider > MaxDivider ->
    true;
do_test_prime(N, Divider, MaxDivider) ->
    case N rem Divider of
        0 ->
            false;
        _ ->
            do_test_prime(N, Divider + 1, MaxDivider)
    end.

