%%%-------------------------------------------------------------------
%% @doc etheatr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(etheatr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%Child :: {Id, StartFunc, Restart, Shutdown, Type, Modules}
init([]) ->
    ManagerSpec =
        {etheatr_manager, {etheatr_manager, start_link, []},
         permanent, 1000, supervisor, [etheatr_worker]},
    ScraperSupSpec =
        {etheatr_scraper_sup, {etheatr_scraper_sup, start_link, []},
         permanent, 1000, supervisor, [etheatr_scraper_sup]},
    PoolSpec = etheatr_util:pool_spec(),
    {ok, { {one_for_one, 3, 60}, [PoolSpec, ManagerSpec, ScraperSupSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
