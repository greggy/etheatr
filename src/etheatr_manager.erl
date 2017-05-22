%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2017, greg
%%% @doc
%%%
%%% @end
%%% Created : 18 May 2017 by greg <>
%%%-------------------------------------------------------------------
-module(etheatr_manager).

-behaviour(supervisor).

-include("etheatr.hrl").

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ScreenId, ImdbId, Limit) ->
    supervisor:start_child(?SERVER, child_spec([ScreenId, ImdbId, Limit])).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = {one_for_one, 3, 30},

    {_Type, Connection} = etheatr_util:take_or_new(),
    WorkerSpecs =
        case mc_worker_api:find(Connection, ?MOVIE_COLLECTION, {}) of
            [] ->
                [];
            {ok, Cursor} ->
                Fun = fun(Map) ->
                              Name = maps:get(<<"screenId">>, Map),
                              ImdbId = maps:get(<<"imdbId">>, Map),
                              child_spec([Name, ImdbId])
                      end,
                lists:map(Fun, mc_cursor:rest(Cursor))
        end,

    {ok, {SupFlags, WorkerSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec([ScreenId, ImdbId|_]=Args) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    {Name, {etheatr_worker, start_link, Args},
     Restart, Shutdown, Type, [etheatr_worker]}.











