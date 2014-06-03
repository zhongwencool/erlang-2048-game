%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc client node connect to center node
%%%  Be responsible for communicate (send/receive msg) with center server
%%% @end
%%% Created : 23. May 2014 5:51 PM
%%%-------------------------------------------------------------------

-module(game2048_client).

-behaviour(gen_server).

-include("game2048.hrl").
%% API
-export([start_link/0,send_chat_msg_to_center/1,i/0,start/0,stop/0,sign_up/2,msg_box/2,bc_signup_info/2,send_challenge/1,
    send_chat_msg_to_client/2,center_respond_ok/2,start_challenge_game/2,update_board_to_other/2
]).

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state,{center_state,name="NotSignUp"::list(),other_client::tuple()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc for debugger :look state
-spec i() -> #state{}.
i() ->
    gen_server:call(get_svrname(),i,infinity).

%%--------------------------------------------------------------------
%% @doc start client server
-spec start() -> gen_server:start(3) .
start() ->
    gen_server:start({local,?MODULE},?MODULE,[?MODULE],[]).

%% @doc start client server
-spec start_link() -> gen_server:start_link(3) .
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [?MODULE], []).

%% @doc chat board send chat message to client ,so that can transfer to center server
-spec send_chat_msg_to_center(Msg::list()) -> ok.
send_chat_msg_to_center(Msg) ->
    erlang:send(?MODULE,{send_chat_msg_to_center,node(),Msg}).

%% @doc click signup button on gui for signup
-spec sign_up(Board::pid(),Name::list()) ->ok.
sign_up(Board,Name) ->
    game2048_single_board:set_new_name(Board,Name),
    erlang:send(?MODULE,{update_new_name,Name}).

%% @doc popup a message box is client
-spec msg_box(Pid::pid(),Msg::list()) -> ok.
msg_box(PID,Msg)when is_list(Msg) ->
    erlang:send(PID,{msg_box,Msg}).

%% @doc center server update newest signup info to client
-spec bc_signup_info(ClientPId::pid(),Info::list()) -> ok.
bc_signup_info(ClientPID,Info) ->
    erlang:send(ClientPID,{update_signup_info,Info}).

%% @doc client compare board send challenge info to client server for  transponding to center server
-spec send_challenge(list()) -> ok.
send_challenge(Info) ->
    erlang:send(?MODULE,{challenge_info,Info}).

%% @doc center server boardcast chat message to all client server for transponding to client chat board
-spec send_chat_msg_to_client(ClientPid::pid(),Msg::list()) ->ok.
send_chat_msg_to_client(ClientPID,Msg) ->
    erlang:send(ClientPID,{send_chat_msg_to_client,Msg}).

%% @doc center respond client that center server is ready for useing.
-spec center_respond_ok(pid(),Msg::list()) -> ok.
center_respond_ok(PID,Msg) ->
    erlang:send(PID,{center_respond_client_ok,Msg}).

%% @doc notify two players that game is begining
-spec start_challenge_game(#player{},#player{}) -> ok.
start_challenge_game(Self,Player) ->
    erlang:send(Self#player.pid,{start_challenge_game,Self,Player}).

%% @doc update boardlist and score to challenge when playing online game
-spec update_board_to_other(list(),integer()) -> ok.
update_board_to_other(BoardList,Score) ->
    erlang:send(?MODULE,{update_board_list,BoardList,Score}).
%%--------------------------------------------------------------------
%% @doc stop client server
-spec stop() -> ok.
stop() ->
    ServerName = get_svrname(),
    gen_server:cast(ServerName, stop).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([_]) ->
    erlang:process_flag(trap_exit,true),
    erlang:send_after(1000,self(),loop_second),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(i,_From,State) ->
    {reply,{ok,State},State};
handle_call(Request, _From, BookList) ->
    io:format("unknow_call_msg:Request:~p",[Request]),
    {reply, ok, BookList}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(stop,State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(loop_second,State) ->
    erlang:send_after(1000,self(),loop_second),
    NewState = do_loop(State),
    {noreply,NewState};

handle_info(connect_center_ok,State) ->
    {noreply,State#state{center_state= node_connected}};

handle_info({center_respond_client_ok,CenterPID},State) ->
    {noreply,State#state{center_state={available,CenterPID}}};

handle_info({send_chat_msg_to_center,Node,Msg},State = #state{center_state = {_,CenterPID}}) ->
    game2048_center:client_to_center_chat(CenterPID,Node,Msg),
    {noreply,State};

handle_info({send_chat_msg_to_client,Msg},State) ->
    game2048_chat_board:print_chat_msg(Msg),
    {noreply,State};

handle_info({update_new_name,Name},State = #state{center_state = {_,CenterPID}}) ->
    try
        game2048_center:update_player(CenterPID,#player{node=node(),pid=self(),name=Name})
    catch _E:_Reason -> io:format("error:~p:~p~n",[_E,_Reason])
    end,
    {noreply,State#state{name = Name}};
handle_info({msg_box,Msg},State) ->
    game2048_gui:msg_box(Msg),
    {noreply,State};

handle_info({update_signup_info,SignupList},State) ->
    game2048_gui:update_signup_info(SignupList),
    {noreply,State};

handle_info({challenge_info,OtherInfo},State = #state{center_state = {_,CenterPID},name = Name}) ->
    SelfInfo = #player{node = node(),name = Name,pid = self()},
    case game2048_center:client_to_center_challenge(CenterPID,{SelfInfo,OtherInfo}) of
        ok -> ok;
        {error,Reason,NewSignupList} ->
            game2048_gui:update_signup_info(NewSignupList),
            game2048_gui:msg_box(Reason)
    end,
    {noreply,State};

handle_info({start_challenge_game,Self,Player},State) ->
    game2048_gui:start_challenge_game(Self,Player),
    {noreply,State#state{other_client = Player#player.pid}};

handle_info({update_board_list,BoardList,Score},State=#state{other_client = ClientPID}) ->
    catch erlang:send(ClientPID,{board_list_from_other,BoardList,Score}),
    {noreply,State};

handle_info({board_list_from_other,BoardList,Score},State=#state{name = Name}) ->
    game2048_gui:board_list_from_other(BoardList,{Name,Score}),
    {noreply,State};

handle_info(_Info, State) ->
    io:format("unknow msg info :~p~n",[_Info]),
    {noreply,State}.
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================
%% @private
%% @doc maintain node connection with center node
%% not connect center node yet
do_loop( State = #state{center_state=undefined}) ->
    io:format("connecting center server:~p,node~n ",[?CENTER_NODE]),
    SelfPID = erlang:self(),
    erlang:spawn(fun() -> do_connect_center(SelfPID) end),
    State;
%% node connected but not sure center server is alive
do_loop( State = #state{center_state= node_connected}) ->
    io:format("confriming ~p,center server is ok:~n",[?CENTER_NODE]),
    connect_to_center_server(),
    State;
%% center server working ok
do_loop( State = #state{center_state={available,_centerPID}}) ->
    %%io:format("~p:center server is up:~n",[?CENTER_NODE]),
    State.

%% @private
%% @doc client node connect to center node is asynchronous
do_connect_center(ParentPID) ->
    true = erlang:set_cookie(?CENTER_NODE, 'game2048'),
    case net_kernel:connect_node(?CENTER_NODE) of
        true -> erlang:send(ParentPID,connect_center_ok);
        _ -> ignore
    end.

%% @private
%% @doc connect to center_server app
connect_to_center_server() ->
    case rpc:call(?CENTER_NODE, erlang, whereis, [game2048_center]) of
        CenterPID when erlang:is_pid(CenterPID)  ->
            game2048_center:client_to_center_app(CenterPID,#player{pid = self(),node = node()});
        _Err ->
            io:format("rpc call error~p~n",[_Err]),
            net_kernel:disconnect(?CENTER_NODE)
    end.

%% @private
%% @doc client register name ,only user is own client node
get_svrname() ->
    ?MODULE.
