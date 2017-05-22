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
-export([pool_spec/0, take_or_new/0, connect_params/0, generate_name/2]).
-export([handle_request/3, response/4, get_parameter/2, get_parameter/3]).
-export([url_encode/1, url_decode/1]).

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

%%--------------------------------------------------------------------
%% @doc
%% Handle and parse web requests
%%
%% @spec handle_request(tuple(), proplist(), atom()) ->
%%                                           {ok, tuple(), proplist()}
%% @end
%%--------------------------------------------------------------------
handle_request(Req, Opts, Handle) ->
    {RawPath, Req1} = cowboy_req:path(Req),
    [<<>>|Path] = binary:split(RawPath, <<"/">>, [global]),
    {GET, Req2} = cowboy_req:qs_vals(Req1),
    {ok, POST, Req3} = cowboy_req:body_qs(Req2),
    {Method, Req4} = cowboy_req:method(Req3),
    try
        Params = case Method of
                     <<"GET">> ->
                         GET;
                     <<"POST">> ->
                         {POST0, _} = hd(POST),
                         jsx:decode(POST0)
                 end,
        Handle({to_atom(Method), Path, Req4}, Params, Opts)
    catch
        Exception:{error, {method_unsupported = Reason, ReasonMsg}} ->
            Response = message(ReasonMsg),
            error_response(405, Exception, Reason, Response, Req3, Opts);
        Exception:{error, Reason} ->
            Response = message(Reason),
            error_response(400, Exception, Reason, Response, Req3, Opts);
        Exception:Reason ->
            Response = message(request),
            error_response(500, Exception, Reason, Response, Req3, Opts)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Construct response for web calls
%%
%% @spec response(integer(), proplst() | map(), tuple(), proplist()) ->
%%                                            {ok, tuple(), proplist()}
%% @end
%%--------------------------------------------------------------------
response(Status, Response, Req, Env) ->
    Json = jsx:encode(Response),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req1} = cowboy_req:reply(Status, Headers, Json, Req),
    {ok, Req1, Env}.

get_parameter(Param, Params) ->
    get_parameter(Param, Params, undefined).

get_parameter(Param, Params, Default) ->
    case lists:keyfind(Param, 1, Params) of
        {_, <<>>} -> Default;
        false -> Default;
        {_, Val} -> Val
    end.

generate_name(ScreenId, ImdbId) ->
    to_atom(iolist_to_binary([ScreenId, "_", ImdbId])).

url_encode(Value) when is_binary(Value) ->
    url_encode(binary_to_list(Value));
url_encode(Value) when is_list(Value) ->
    list_to_binary(http_uri:encode(Value));
url_encode(Value) ->
    Value.

url_decode(Value) when is_binary(Value) ->
    url_decode(binary_to_list(Value));
url_decode(Value) when is_list(Value) ->
    list_to_binary(http_uri:decode(Value));
url_decode(Value) ->
    Value.

%%%===================================================================
%%% Internal functions
%%%===================================================================

error_response(Status, Exception, Reason, Response, Req, Opts) ->
    {Path, Req1} = cowboy_req:path(Req),
    {UserIP, Req2} = cowboy_req:meta(user_ip, Req1),
    {Method, Req3} = cowboy_req:method(Req2),
    lager:error("Error response ~p", [{Exception, Reason, Path, UserIP, Method}]),
    response(Status, [{<<"error">>, Response}], Req3, Opts).

message(callback) ->
	<<"The requested resource requires a callback parameter.">>;
message(request) ->
	<<"The requested resource is not available at this time.">>;
message(missing_params) ->
	<<"The following required parameters are missing: ">>;
message(empty_params) ->
	<<"The following required parameters are empty: ">>.

to_atom(Value) when is_binary(Value) ->
    binary_to_atom(Value, utf8);
to_atom(Value) when is_list(Value) ->
    list_to_atom(Value);
to_atom(Value) ->
    Value.
