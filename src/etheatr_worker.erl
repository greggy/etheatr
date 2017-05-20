%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2017, greg
%%% @doc
%%%
%%% @end
%%% Created : 18 May 2017 by greg <>
%%%-------------------------------------------------------------------
-module(etheatr_worker).

-behaviour(gen_server).

-include("etheatr.hrl").

%% API
-export([start_link/2, start_link/3, stop/1, get_seat/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          screen_id :: atom() | list(),
          imdb_id :: binary(),
          limit :: pos_integer(),
          seat_cap=0 :: non_neg_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(atom() | list(), binary(), pos_integer()) ->
%%                                 {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ScreenID, ImdbId, Limit) ->
    gen_server:start_link({local, ScreenID}, ?MODULE, [ScreenID, ImdbId, Limit], []).

start_link(ScreenID, ImdbId) ->
    gen_server:start_link({local, ScreenID}, ?MODULE, [ScreenID, ImdbId], []).

stop(ScreenID) ->
    gen_server:call(ScreenID, stop).

%%--------------------------------------------------------------------
%% @doc
%% Get seat for movie
%%
%% @spec get_seat(list()) -> {ok, SeatCap} | {error, cap_full}.
%% @end
%%--------------------------------------------------------------------
get_seat(ScreenID) ->
    gen_server:call(ScreenID, get_seat).

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
    lager:info("Start OLD screen ~p and imdb ~p", [ScreenId, ImdbId]),
    gen_server:cast(ScreenId, update),
    {ok, #state{screen_id=ScreenId, imdb_id=ImdbId}};
init([ScreenId, ImdbId, Limit]) ->
    process_flag(trap_exit, true),
    lager:info("Start NEW screen ~p and imdb ~p", [ScreenId, ImdbId]),
    gen_server:cast(ScreenId, create),
    {ok, #state{screen_id=ScreenId, imdb_id=ImdbId, limit=Limit}}.

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
handle_call(get_seat, _From, #state{screen_id=ScreenId, imdb_id=ImdbId,
                                    limit=Limit, seat_cap=SeatCap}=State) ->
    case SeatCap < Limit of
        true ->
            {_Type, Connection} = etheatr_util:take_or_new(),
            Command = #{<<"$set">> => #{
                            <<"reservedSeats">> => SeatCap+1
                           }},
            mc_worker_api:update(Connection, ?MOVIE_COLLECTION,
                                 #{<<"screen_id">> => ScreenId,
                                   <<"imdb_d">> => ImdbId},
                                 Command),
            {reply, {ok, SeatCap+1}, State#state{seat_cap=SeatCap+1}};
        false ->
            {reply, {error, cap_full}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(Request, _From, State) ->
    Reply = ok,
    lager:info("Catch unhadled call message ~p", [Request]),
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
handle_cast(create, #state{screen_id=ScreenId, imdb_id=ImdbId, limit=Limit}=State) ->
    {_Type, Connection} = etheatr_util:take_or_new(),
    mc_worker_api:insert(Connection, ?MOVIE_COLLECTION,
                         #{<<"imdbId">> => ImdbId,
                           <<"screenId">> => ScreenId,
                           <<"movieTitle">> => <<"In Progress ...">>,
                           <<"availableSeats">> => Limit,
                           <<"reservedSeats">> => 0}
                        ),
    {noreply, State};
handle_cast(update, #state{screen_id=ScreenId, imdb_id=ImdbId}=State) ->
    {_Type, Connection} = etheatr_util:take_or_new(),
    Selector = #{<<"imdbId">> => ImdbId, <<"screenId">> => ScreenId},
    case mc_worker_api:find_one(Connection, ?MOVIE_COLLECTION, Selector) of
        undefined ->
            {stop, normal, State};
        Result ->
            Limit = maps:get(<<"availableSeats">>, Result),
            SeatCap = maps:get(<<"reservedSeats">>, Result),
            {noreply, State#state{limit=Limit, seat_cap=SeatCap}}
    end;
handle_cast(Msg, State) ->
    lager:info("Catch unhadled cast message ~p", [Msg]),
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
handle_info(Info, State) ->
    lager:info("Catch unhadled info message ~p", [Info]),
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
    lager:info("Worker was stoped with reason ~p", [Reason]),
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
