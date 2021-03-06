%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2017, greg
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2017 by greg <>
%%%-------------------------------------------------------------------
-module(etheatr_scraper).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          screen_id :: atom(),
          imdb_id :: binary()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(binary(), binary()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ScreenId, ImdbId) ->
    Name = binary_to_atom(iolist_to_binary(["scraper_", ScreenId]), utf8),
    gen_server:start_link({local, Name}, ?MODULE, [ScreenId, ImdbId], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ScreenId, ImdbId]) ->
    process_flag(trap_exit, true),
    lager:info("Start scraper info for screen ~p imdb ~p", [ScreenId, ImdbId]),
    gen_server:cast(self(), fetch_data),
    {ok, #state{screen_id=ScreenId, imdb_id=ImdbId}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(fetch_data, #state{imdb_id=ImdbId}=State) ->
    ImdbApi = application:get_env(etheatr, imdb_api, ""),
    ImdbApiKey = application:get_env(etheatr, imdb_api_key, ""),
    Url = ImdbApi ++ binary_to_list(ImdbId) ++ "?external_source=imdb_id&language=en-US&api_key=" ++ ImdbApiKey,
    hackney:get(Url, [], <<>>, [async, {stream_to, self()}]),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:info("Catch unhendled cast message ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({hackney_response, _Port, Body}, State) when is_binary(Body) ->
    BodyJson = jsx:decode(Body),
    case lists:keyfind(<<"movie_results">>, 1, BodyJson) of
        {<<"movie_results">>, [Results]} ->
            {<<"original_title">>, Title} = lists:keyfind(<<"original_title">>, 1, Results),
            ok = etheatr_worker:set_title(State#state.screen_id, State#state.imdb_id, Title);
        _ ->
            lager:warning("Can't find movie results in ~p", [BodyJson])
    end,
    {stop, normal, State};
handle_info(Info, State) ->
    lager:info("Catch unhendled info message ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    lager:info("Scraper worker has stoped with reason ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
