%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Март 2020 14:15
%%%-------------------------------------------------------------------
-module(eprime_prime_test).
-author("davidqo").

-include_lib("eunit/include/eunit.hrl").

-define(PRETTY_BIG_PRIME_NUMBER, 999883).

prime_number_test() ->
    ?assertEqual(true, eprime_utils:is_prime(1)),
    ?assertEqual(true, eprime_utils:is_prime(2)),
    ?assertEqual(true, eprime_utils:is_prime(3)),
    ?assertEqual(false, eprime_utils:is_prime(4)),
    ?assertEqual(true, eprime_utils:is_prime(5)),
    ?assertEqual(false, eprime_utils:is_prime(6)),
    ?assertEqual(true, eprime_utils:is_prime(7)),
    ?assertEqual(false, eprime_utils:is_prime(8)),
    ?assertEqual(false, eprime_utils:is_prime(9)),
    ?assertEqual(false, eprime_utils:is_prime(10)),
    ?assertEqual(true, eprime_utils:is_prime(7211)),
    ?assertEqual(false, eprime_utils:is_prime(7212)),
    ?assertEqual(true, eprime_utils:is_prime(7213)),
    ?assertEqual(true, eprime_utils:is_prime(7919)),
    ?assertEqual(false, eprime_utils:is_prime(7920)).

big_prime_number_performance_test() ->
    N = 1000,
    erlang:garbage_collect(),
    erlang:yield(),
    Time1 = erlang:system_time(microsecond),
    IsPrime = calc_prime_n_times(?PRETTY_BIG_PRIME_NUMBER, N, undefined),
    Time2 = erlang:system_time(microsecond),
    ?assertEqual(true, IsPrime),
    %% Single pretty big prime calculation time less than 200 uSeconds.
    %% This is enough for my ancient laptop =)
    ?assert(((Time2 - Time1) / N) < 200.0),
    ok.

calc_prime_n_times(_, 0, Res) ->
    Res;
calc_prime_n_times(X, N, _Res) ->
    Res2 = eprime_utils:is_prime(X),
    calc_prime_n_times(X, N - 1, Res2).


