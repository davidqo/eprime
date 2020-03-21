%%%-------------------------------------------------------------------
%% @doc eprime public API
%% @end
%%%-------------------------------------------------------------------

-module(eprime_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eprime_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
