%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Март 2020 17:27
%%%-------------------------------------------------------------------
-author("davidqo").

%% TODO: Добавить тут точность до микросекунд
-define(LOG(Fmt), ?LOG(Fmt, [])).
-define(LOG(Fmt, Args), io:format("~p:[~p/~p]:~p: " ++ Fmt ++ "~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE | Args])).
