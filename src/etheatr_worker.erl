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
-export([start_link/2, start_link/3, stop/2, get_seat/2,
         set_title/3]).

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
%% @spec start_link(atom(), binary(), pos_integer()) ->
%%                                 {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ScreenId, ImdbId, Limit) ->
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    lager:info("New Name ~p", [Name]),
    gen_server:start_link({local, Name}, ?MODULE, [ScreenId, ImdbId, Limit], []).

start_link(ScreenId, ImdbId) ->
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    lager:info("Old Name ~p", [Name]),
    gen_server:start_link({local, Name}, ?MODULE, [ScreenId, ImdbId], []).

stop(ScreenId, ImdbId) ->
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    gen_server:call(Name, stop).

%%--------------------------------------------------------------------
%% @doc
%% Get seat for movie
%%
%% @spec get_seat(binary(), binary) -> {ok, SeatCap} |
%%                                     {error, cap_full}
%% @end
%%--------------------------------------------------------------------
get_seat(ScreenId, ImdbId) ->
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    gen_server:call(Name, get_seat).

%%--------------------------------------------------------------------
%% @doc
%% Set a title for movie
%%
%% @spec set_title(binary(), binary()) -> ok
%% @end
%%--------------------------------------------------------------------
set_title(ScreenId, ImdbId, Title) ->
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    gen_server:cast(Name, {update, Title}).

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
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    lager:info("Start OLD screen with name ~p", [Name]),
    gen_server:cast(Name, update),
    {ok, #state{screen_id=ScreenId, imdb_id=ImdbId}};
init([ScreenId, ImdbId, Limit]) ->
    process_flag(trap_exit, true),
    Name = etheatr_util:generate_name(ScreenId, ImdbId),
    lager:info("Start NEW screen with name ~p", [Name]),
    gen_server:cast(Name, create),
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
                                 #{<<"screenId">> => ScreenId,
                                   <<"imdbId">> => ImdbId},
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
    etheatr_scraper_sup:start_scraper(ScreenId, ImdbId),
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
handle_cast({update, Title}, #state{screen_id=ScreenId, imdb_id=ImdbId}=State) ->
    {_Type, Connection} = etheatr_util:take_or_new(),
    Selector = #{<<"imdbId">> => ImdbId, <<"screenId">> => ScreenId},
    Command = #{<<"$set">> => #{
                    <<"movieTitle">> => Title
                   }},
    mc_worker_api:update(Connection, ?MOVIE_COLLECTION, Selector, Command),
    {noreply, State};
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
