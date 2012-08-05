%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------

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
