%%%-------------------------------------------------------------------
%% @doc etheatr public API
%% @end
%%%-------------------------------------------------------------------

-module(etheatr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    etheatr_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
