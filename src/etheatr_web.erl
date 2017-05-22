-module(etheatr_web).

-export([init/3]).
-export([handle/2, handle/3]).
-export([terminate/3]).

-include("etheatr.hrl").

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    etheatr_util:handle_request(Req, State, fun handle/3).

handle({'GET', [<<"one">>, <<"ping">>], Req}, _Params, State) ->
    {Cookies, Req1} = cowboy_req:cookies(Req),
    {UserIP, Req2} = cowboy_req:meta(user_ip, Req1),
    lager:info("UserIP ~p Cookie ~p", [UserIP, Cookies]),
    etheatr_util:response(200, [{<<"result">>, <<"pong">>}], Req2, State);

handle({'GET', [<<"movie">>, <<"list">>], Req}, _Params, State) ->
    {_Type, Connection} = etheatr_util:take_or_new(),
    Response =
        case mc_worker_api:find(Connection, ?MOVIE_COLLECTION, {}) of
            [] ->
                [];
            {ok, Cursor} ->
                [maps:remove(<<"_id">>, Map) || Map <- mc_cursor:rest(Cursor)]
        end,
    lager:info("Response ~p", [Response]),
    etheatr_util:response(200, Response, Req, State);

handle({'GET', [<<"movie">>, MovieId], Req}, _Params, State) ->
    {_Type, Connection} = etheatr_util:take_or_new(),
    case mc_worker_api:find_one(Connection, ?MOVIE_COLLECTION,
                                #{<<"screenId">> => MovieId}) of
        undefined ->
            etheatr_util:response(404, [], Req, State);
        Response ->
            Response1 = maps:remove(<<"_id">>, Response),
            lager:info("Response ~p", [Response]),
            etheatr_util:response(200, Response1, Req, State)
    end;

handle({'POST', [<<"movie">>, <<"add">>], Req}, Params, State) ->
    ScreenId = etheatr_util:get_parameter(<<"screen_id">>, Params),
    ImdbId = etheatr_util:get_parameter(<<"imdb_id">>, Params),
    Limit = binary_to_integer(etheatr_util:get_parameter(<<"limit">>, Params, <<"3">>)),
    lager:info("Params ~p", [{Params, ScreenId, ImdbId, Limit}]),
    case etheatr_manager:start_child(ScreenId, ImdbId, Limit) of
        {ok, _Pid} ->
            etheatr_util:response(200, [{<<"response">>, <<"ok">>}], Req, State);
        {error, {Reason, _Pid}} ->
            etheatr_util:response(200, [{<<"response">>, <<"error">>},{<<"reason">>, Reason}], Req, State);
        ignore ->
            etheatr_util:response(200, [{<<"response">>, <<"error">>},{<<"reason">>, ignore}], Req, State)
        end;

handle({'POST', [<<"movie">>, <<"get_seat">>], Req}, Params, State) ->
    ScreenId = etheatr_util:get_parameter(<<"screen_id">>, Params),
    ImdbId = etheatr_util:get_parameter(<<"imdb_id">>, Params),
    lager:info("Params ~p", [{ScreenId, ImdbId}]),
    case (catch etheatr_worker:get_seat(ScreenId, ImdbId)) of
        {ok, Num} ->
            etheatr_util:response(200, [{<<"response">>, <<"ok">>},{<<"seats">>, Num}], Req, State);
        {error, Reason} ->
            etheatr_util:response(200, [{<<"response">>, <<"error">>},{<<"reason">>, Reason}], Req, State);
        {'EXIT', _} ->
            etheatr_util:response(200, [{<<"response">>, <<"error">>},{<<"reason">>, <<"not_found">>}], Req, State)
        end;

handle(MethodPath, _Params, _State) ->
    lager:info("MethodPath ~p", [MethodPath]),
    throw({error, resource_notfound}).

terminate(_Reason, _Req, _State) ->
    ok.













