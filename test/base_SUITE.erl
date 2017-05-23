%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2017, greg
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2017 by greg <>
%%%-------------------------------------------------------------------
-module(base_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    ok = application:set_env(etheatr, mongodb_host, "192.168.1.123"),
    ok = application:set_env(etheatr, mongodb_database, <<"etheatr_test">>),

    ok = lager:start(),
    ok = pooler:start(),
    {ok, _} = application:ensure_all_started(etheatr),
    [{test1, {<<"Test1">>, <<"tt1411697">>}},{test2, {<<"Test2">>,<<"tt0088944">>}} | Config].

end_per_suite(_Config) ->
    {_Type, Connection} = etheatr_util:take_or_new(),
    mc_worker_api:delete(Connection, <<"movie">>, {}),
    ok = etheatr_app:stop(etheatr),
    ok.

all() -> 
    [
     insert_movies,
     get_seats,
     list_api_requests,
     get_api_requests,
     put_api_requests,
     seats_api_requests
    ].

insert_movies(Config) ->
    {ScreenId1, ImdbId1} = ?config(test1, Config),
    {ok, _Pid1} = etheatr_manager:start_child(ScreenId1, ImdbId1, 3),
    {ScreenId2, ImdbId2} = ?config(test2, Config),
    {ok, Pid2} = etheatr_manager:start_child(ScreenId2, ImdbId2, 3),
    {error, {already_started, Pid2}} = etheatr_manager:start_child(ScreenId2, ImdbId2, 3).

get_seats(Config) ->
    {ScreenId1, ImdbId1} = ?config(test1, Config),
    {ok, 1} = etheatr_worker:get_seat(ScreenId1, ImdbId1),
    {ok, 2} = etheatr_worker:get_seat(ScreenId1, ImdbId1),
    {ok, 3} = etheatr_worker:get_seat(ScreenId1, ImdbId1),
    {error, cap_full} = etheatr_worker:get_seat(ScreenId1, ImdbId1),
    {ScreenId2, ImdbId2} = ?config(test2, Config),
    {ok, 1} = etheatr_worker:get_seat(ScreenId2, ImdbId2).

list_api_requests(Config) ->
    URL = "http://localhost:8080/movie/",
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, 200, _H, Ref} = hackney:get(URL ++ "list", Headers, <<>>, []),
    {ok, Body} = hackney:body(Ref),
    ct:pal("Body ~p", [Body]),
    JsonBody = jsx:decode(Body),
    [begin
         B = lists:nth(N, JsonBody),
         {ScreenId, _ImdbId} = ?config(list_to_atom("test" ++ integer_to_list(N)), Config),
         {<<"screenId">>, ScreenId} = lists:keyfind(<<"screenId">>, 1, B)
     end || N <- lists:seq(1, length(JsonBody))].

get_api_requests(Config) ->
    URL = "http://localhost:8080/movie/",
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ScreenId1, ImdbId1} = ?config(test1, Config),
    {ok, 200, _H1, Ref1} = hackney:get(URL ++ ScreenId1, Headers, <<>>, []),
    {ok, Body1} = hackney:body(Ref1),
    ct:pal("Body1 ~p", [Body1]),
    JsonBody1 = jsx:decode(Body1),
    {<<"imdbId">>, ImdbId1} = lists:keyfind(<<"imdbId">>, 1, JsonBody1),
    {ScreenId2, ImdbId2} = ?config(test2, Config),
    {ok, 200, _H2, Ref2} = hackney:get(URL ++ ScreenId2, Headers, <<>>, []),
    {ok, Body2} = hackney:body(Ref2),
    ct:pal("Body2 ~p", [Body2]),
    JsonBody2 = jsx:decode(Body2),
    {<<"imdbId">>, ImdbId2} = lists:keyfind(<<"imdbId">>, 1, JsonBody2),
    {ok, 404, _H3, _Ref3} = hackney:get(URL ++ "Wrong%20Screen", Headers, <<>>, []).

put_api_requests(_Config) ->
    URL = "http://localhost:8080/movie/",
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ScreenId, ImdbId} = {<<"Test4">>, <<"tt0103064">>},
    ReqBody = jsx:encode([{<<"screen_id">>, ScreenId},{<<"imdb_id">>, ImdbId},{<<"limit">>, <<"2">>}]),
    {ok, 200, _H, Ref} = hackney:request(post, URL ++ "add", Headers, ReqBody, []),
    {ok, Body} = hackney:body(Ref),
    ct:pal("Body ~p", [Body]),
    JsonBody = jsx:decode(Body),
    {<<"response">>, <<"ok">>} = lists:keyfind(<<"response">>, 1, JsonBody),
    timer:sleep(100),
    {ok, 200, _H2, Ref2} = hackney:get(URL ++ ScreenId, Headers, <<>>, []),
    {ok, Body2} = hackney:body(Ref2),
    ct:pal("Body2 ~p", [Body2]),
    JsonBody2 = jsx:decode(Body2),
    {<<"imdbId">>, ImdbId} = lists:keyfind(<<"imdbId">>, 1, JsonBody2).
    
seats_api_requests(Config) ->
    URL = "http://localhost:8080/movie/",
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ScreenId1, ImdbId1} = ?config(test2, Config),
    ReqBody1 = jsx:encode([{<<"screen_id">>, ScreenId1},{<<"imdb_id">>, ImdbId1}]),
    {ok, 200, _H1, Ref1} = hackney:request(post, URL ++ "get_seat", Headers, ReqBody1, []),
    {ok, Body1} = hackney:body(Ref1),
    ct:pal("Body1 ~p", [Body1]),
    JsonBody1 = jsx:decode(Body1),
    {<<"response">>, <<"ok">>} = lists:keyfind(<<"response">>, 1, JsonBody1),
    {<<"seats">>, 2} = lists:keyfind(<<"seats">>, 1, JsonBody1).
