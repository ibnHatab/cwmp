%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------

%% Trace macro
-ifdef(TEST).
-define(DBG(ARG), io:format(user, "~n>> ~p: ~p~n", [??ARG, ARG])).
-else.
-define(DBG(ARG), true).
-endif.

-define(DEBUG(Msg),
        ok = lager:debug(Msg)).
-define(DEBUG(Msg, Args),
        ok = lager:debug(Msg, Args)).

-define(INFO(Msg),
        ok = lager:info(Msg)).
-define(INFO(Msg, Args),
        ok = lager:info(Msg, Args)).

-define(WARNING(Msg),
        ok = lager:warning(Msg)).
-define(WARNING(Msg, Args),
        ok = lager:warning(Msg, Args)).

-define(ERROR(Msg),
        ok = lager:error(Msg)).
-define(ERROR(Msg, Args),
        ok = lager:error(Msg, Args)).


-ifndef(TIMEON).
%% Yes, these need to be on a single line to work...
%% ?TIMEON,
%% ...some code...
%% ?TIMEOFF(my_code_block).

-define(TIMEON, erlang:put(debug_timer, [now()|case erlang:get(debug_timer) == undefined of true -> []; false -> erlang:get(debug_timer) end])).
-define(TIMEOFF(Var), io:format("~s :: ~10.2f ms : ~p~n", [string:copies(" ", length(erlang:get(debug_timer))), (timer:now_diff(now(), hd(erlang:get(debug_timer)))/1000), Var]), erlang:put(debug_timer, tl(erlang:get(debug_timer)))).

-define(EXPECT(Cond, Msg),
	if
	    Cond ->
		ok;
	    true ->
		%io:format(user, "Expecting ~p: ~p at ~p:~p~n", [Msg, ??Cond, ?FILE, ?LINE]),
		ok = lager:warning("Expecting ~p: ~p at ~p:~p~n", [Msg, ??Cond, ?FILE, ?LINE])
	end).
-endif.
