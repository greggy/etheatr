%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2017, greg
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2017 by greg <>
%%%-------------------------------------------------------------------
-module(etheatr_util).

%% API
-export([pool_spec/0, take_or_new/0, connect_params/0]).

-define(POOL_NAME, etheatr_db).
-define(POOL_SIZE, 10).
-define(POOL_MAX_SIZE, 50).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Construct a spec for pool of connections to mongodb
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
pool_spec() ->
    PoolConfig = [{name, ?POOL_NAME},
                  {max_count, ?POOL_MAX_SIZE},
                  {init_count, ?POOL_SIZE},
                  {start_mfa, {mc_worker, start_link, [connect_params()]}}],
    pooler:pool_child_spec(PoolConfig).

%%--------------------------------------------------------------------
%% @doc
%% Take from pool or create new connection
%% 
%% @spec take_or_new() -> {'new' | 'pool', pid()}
%% @end
%%--------------------------------------------------------------------
take_or_new() ->
    case pooler:take_member(?POOL_NAME) of
        error_no_members ->
            {ok, Connect} = mc_worker_api:connect(connect_params()),
            {new, Connect};
        Connect when is_pid(Connect) ->
            {pool, Connect}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Construct connection structure
%%
%% @spec connect_params() -> [{atom(), list() | binary()}]
%% @end
%%--------------------------------------------------------------------
connect_params() ->
    Host = application:get_env(etheatr, mongodb_host, ""),
    Database = application:get_env(etheatr, mongodb_database, <<"">>),
    [{host, Host},{database, Database}].


%%%===================================================================
%%% Internal functions
%%%===================================================================
