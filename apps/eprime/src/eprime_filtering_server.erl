%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Март 2020 17:33
%%%-------------------------------------------------------------------
-module(eprime_filtering_server).
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

-record(state, {
    redis_host :: string(),
    redis_port :: pos_integer(),
    redis_db :: non_neg_integer(),
    queue_key :: string(),
    result_key :: string(),
    redis_handler :: term(),
    numbers_processed = 0 :: integer(),
    prime_numbers_selected = 0 :: integer(),
    prime_numbers_saved = 0 :: integer()
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
    ResultKey = proplists:get_value(result_key, Args),
    {ok, #state{redis_host = RedisHost, redis_port = RedisPort, redis_db = RedisDb, result_key = ResultKey, queue_key = QueueKey}}.

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
    redis_handler = RedisHandler,
    queue_key = QueueKey,
    result_key = ResultKey,
    numbers_processed = NumbersProcessed,
    prime_numbers_selected = PrimeNumbersSelected,
    prime_numbers_saved = PrimeNumbersSaved}) ->
    self() ! awake,
    case read(RedisHandler, QueueKey) of
        {ok, X} when is_integer(X) ->
            State2 = State#state{numbers_processed = NumbersProcessed + 1},
            case eprime_utils:is_prime(X) of
                true ->
                    State3 = State2#state{prime_numbers_selected = PrimeNumbersSelected + 1},
                    case save(RedisHandler, ResultKey, X) of
                        {ok, _} ->
                            {noreply, State3#state{prime_numbers_saved = PrimeNumbersSaved + 1}};
                        _ ->
                            {noreply, State3}
                    end;
                false ->
                    {noreply, State2}
            end;
        {ok, undefined} ->
            {noreply, State};
        {error, _} ->
            {noreply, State}
    end;
handle_info({timeout, _, statistics}, State = #state{
    numbers_processed = NumbersProcessed,
    prime_numbers_selected = PrimeNumbersSelected,
    prime_numbers_saved = PrimeNumbersSaved}) ->
    ?LOG("Numbers processed: ~p. Prime numbers selected: ~p. Prime numbers saved: ~p", [NumbersProcessed, PrimeNumbersSelected, PrimeNumbersSaved]),
    erlang:start_timer(5000, self(), statistics),
    {noreply, State};
handle_info(init, State = #state{redis_host = RedisHost, redis_port = RedisPort, redis_db = RedisDb}) ->
    Self = self(),
    ?LOG("Filtering process started: ~p", [Self]),
    erlang:start_timer(5000, Self, statistics),
    {ok, RedisHandler} = eredis:start_link(RedisHost, RedisPort, RedisDb),
    Self ! awake,
    {noreply, State#state{redis_handler = RedisHandler}}.

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

-spec read(RedisHandler :: term(), QueueKey :: string()) -> {ok, X :: integer() | undefined} | {error, term()}.
read(RedisHandler, QueueKey) ->
    case eredis:q(RedisHandler, ["LPOP", QueueKey]) of
        {ok, X} when is_binary(X) ->
            {ok, binary_to_integer(X)};
        Res ->
            Res
    end.

-spec save(RedisHandler :: term(), ResultKey :: string(), X :: pos_integer()) -> {ok, binary()} | {error, term()}.
save(RedisHandler, ResultKey, X) ->
    eredis:q(RedisHandler, ["SADD", ResultKey, integer_to_list(X)]).