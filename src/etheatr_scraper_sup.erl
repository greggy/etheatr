%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2017, greg
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2017 by greg <>
%%%-------------------------------------------------------------------
-module(etheatr_scraper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_scraper/2]).

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

start_scraper(ScreenId, ImdbId) ->
    supervisor:start_child(?SERVER, [ScreenId, ImdbId]).

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
    Restart = transient,
    Shutdown = 200,
    Type = worker,

    ScraperSpec =
        {etheatr_scraper, {etheatr_scraper, start_link, []},
         Restart, Shutdown, Type, [etheatr_scraper]},

    {ok, {{simple_one_for_one, 10, 30}, [ScraperSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
