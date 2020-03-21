%%%-------------------------------------------------------------------
%% @doc eprime top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eprime_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(DEFAULT_REDIS_HOST, "127.0.0.1").
-define(DEFAULT_REDIS_PORT, 6379).
-define(DEFAULT_REDIS_DB, undefined).
-define(DEFAULT_QUEUE_KEY, "generator_queue").
-define(DEFAULT_RESULT_KEY, "result_set").
-define(DEFAULT_N, 1000).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Env = application:get_all_env(),
    N = proplists:get_value(n, Env, ?DEFAULT_N),
    RedisHost = proplists:get_value(redis_host, Env, ?DEFAULT_REDIS_HOST),
    RedisPort = proplists:get_value(redis_port, Env, ?DEFAULT_REDIS_PORT),
    RedisDb = proplists:get_value(redis_db, Env, ?DEFAULT_REDIS_DB),
    QueueKey = proplists:get_value(queue_key, Env, ?DEFAULT_QUEUE_KEY),
    ResultKey = proplists:get_value(result_key, Env, ?DEFAULT_RESULT_KEY),
    GeneratorArgs = [
        {redis_host, RedisHost},
        {redis_port, RedisPort},
        {redis_db, RedisDb},
        {queue_key, QueueKey},
        {n, N}
    ],
    GeneratorSpec = #{
        id => eprime_generator_server,
        start =>
        {eprime_generator_server, start_link, [GeneratorArgs]},
        restart => permanent, type => worker
    },
    FilteringArgs = [
        {redis_host, RedisHost},
        {redis_port, RedisPort},
        {redis_db, RedisDb},
        {queue_key, QueueKey},
        {result_key, ResultKey}
    ],
    FilteringSpec = #{
        id => eprime_filtering_server,
        start =>
        {eprime_filtering_server, start_link, [FilteringArgs]},
        restart => permanent, type => worker
    },
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1},
    ChildSpecs = [GeneratorSpec, FilteringSpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

