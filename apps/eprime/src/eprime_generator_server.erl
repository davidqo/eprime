%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Март 2020 17:04
%%%-------------------------------------------------------------------
-module(eprime_generator_server).
-author("davidqo").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("./log.hrl").

-define(SERVER, ?MODULE).

-define(RATE, 3000).

-record(state, {
    redis_host :: string(),
    redis_port :: pos_integer(),
    redis_db :: non_neg_integer(),
    n :: pos_integer(),
    queue_key :: string(),
    rate :: pos_integer(),
    diff :: float(),
    error = 0.0 :: float(),
    next_write :: pos_integer(),
    writes_number = 0 :: non_neg_integer(),
    redis_handler :: term(),
    next_number :: pos_integer() %% 2..n
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
%% TODO: Проверять здесь передаваемые аргументы
init(Args) ->
    self() ! init,
    RedisHost = proplists:get_value(redis_host, Args),
    RedisPort = proplists:get_value(redis_port, Args),
    RedisDb = proplists:get_value(redis_db, Args),
    QueueKey = proplists:get_value(queue_key, Args),
    N = proplists:get_value(n, Args),
    if not is_integer(N) andalso N < 2 ->
       erlang:error(badarg, {n, {less_than_2, N}});
    true ->
       ok
    end,
    {ok, #state{
        redis_host = RedisHost,
        redis_port = RedisPort,
        redis_db = RedisDb,
        queue_key = QueueKey,
        n = N,
        rate = ?RATE,
        diff = 1000000 / ?RATE
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%
%% TODO: Reindent by VS Code
%% TODO: Add specs to internal functions
%%
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(awake, State = #state{
    next_write = DestinationTime,
    diff = Diff,
    error = Error,
    writes_number = WritesNumber,
    redis_handler = RedisHandler,
    queue_key = QueueKey,
    next_number = NextNumber,
    n = N}) ->
    high_resolution_wait(DestinationTime),
    WritesNumber2 =
        case write(RedisHandler, QueueKey, NextNumber) of
            {error, _} ->
                WritesNumber;
            _ ->
                WritesNumber + 1
        end,
    NextNumber2 = generate_random_number(N),
    {DestinationTime2, Error2} = calculate_next_write(DestinationTime, Diff, Error),
    self() ! awake,
    {noreply, State#state{next_write = DestinationTime2, error = Error2, writes_number = WritesNumber2, next_number = NextNumber2}};
handle_info({timeout, _, statistics}, State = #state{writes_number = WritesNumber, rate = Rate, diff = Diff, error = Error}) ->
    ?LOG("Writes number: ~p. Rate: ~p. Diff usec: ~p. Error: ~p", [WritesNumber, Rate, Diff, Error]),
    erlang:start_timer(5000, self(), statistics),
    {noreply, State};
handle_info(init, State = #state{redis_host = RedisHost, redis_port = RedisPort, redis_db = RedisDb, n = N}) ->
    Self = self(),
    ?LOG("Generator process started: ~p", [Self]),
    erlang:start_timer(5000, Self, statistics),
    {ok, RedisHandler} = eredis:start_link(RedisHost, RedisPort, RedisDb),
    NextNumber = generate_random_number(N),
    Self ! awake,
    {noreply, State#state{next_write = erlang:system_time(microsecond), redis_handler = RedisHandler, next_number = NextNumber}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

high_resolution_wait(DestinationTime) ->
    Time = erlang:system_time(microsecond),
    Diff = DestinationTime - Time,
    if
        Diff < (1000 * 1000 * -1) ->
            throw({too_big_latency, Diff});
        Diff =< 0 ->
            done;
    %% Too long waiting (1 second)
        Diff > (1000 * 1000 * 1) ->
            throw({too_long, Diff});
        true ->
            erlang:yield(),
            high_resolution_wait(DestinationTime)
    end.

calculate_next_write(DestinationTime, Diff, Error) when Error >= 1 ->
    calculate_next_write(DestinationTime, Diff - 1, Error - 1);
calculate_next_write(DestinationTime, Diff, Error) ->
    TruncatedDiff = trunc(Diff),
    DestinationTime2 = DestinationTime + TruncatedDiff,
    Error2 = Error + Diff - TruncatedDiff,
    {DestinationTime2, Error2}.

generate_random_number(N) ->
    %% from 2 to N
    1 + rand:uniform(N - 1).

write(RedisHandler, QueueKey, X) ->
    eredis:q(RedisHandler, ["LPUSH", QueueKey, integer_to_list(X)]).