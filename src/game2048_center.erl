%%%-------------------------------------------------------------------
%%% @author  <zhongwencool@gmail.com>
%%% @doc
%%% All player play online games will connect to this center for sign up
%%% @end
%%% Created : 23. May 2014 5:51 PM
%%%-------------------------------------------------------------------
-module(game2048_center).

-behaviour(gen_server).

-include("game2048.hrl").

%% API
-export([start/0,i/0]).

-export([update_player/2,client_to_center_app/2,client_to_center_challenge/2,client_to_center_chat/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-record(state, {signup=[],nodes = []}).

-define(MAX_SIGNUP_NUM,8).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Starts the game 2048 center server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @doc for debugger look state
-spec i() -> #state{}.
i() ->
    call(i).

-spec update_player(pid(),player()) ->
    ok.
%% @doc client update player to center such as sign up in center
update_player(CenterPID,Info) ->
    erlang:send(CenterPID,{update_player,Info}).

-spec client_to_center_app(pid(),player()) ->
    ok.
%% @doc client send to center to check center node has already set center server up
client_to_center_app(CenterPID,PlayInfo) ->
    erlang:send(CenterPID,{client_to_center_app,PlayInfo}).

-spec client_to_center_challenge(pid(),player()) ->
    ok|{error,term(),[player()]}.
%% @doc client send to center server to challenge other players who already signup
client_to_center_challenge(CenterPID,PlayInfo) ->
    gen_server:call(CenterPID,{client_to_center_challenge,PlayInfo}).

-spec client_to_center_chat(pid(),node(),tuple()) ->
    ok.
%% @doc client send to center server to boardcast chat msg to all client export self
client_to_center_chat(CenterPID,Node,ChatMsg) ->
    erlang:send(CenterPID,{client_to_center_chat,Node,ChatMsg}).

%% @private gen_server call for test
call(Msg) ->
    gen_server:call(?MODULE, Msg, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
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
-spec(init(Args :: []) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ok = net_kernel:monitor_nodes(true),
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

handle_call({client_to_center_challenge,{Self,Other}},_From,State) ->
    case do_challenge(Self,Other,State) of
        {ok,NewState} ->
            {reply,ok,NewState};
        {error,_Reason,_NewSignup} = Err ->
            {reply,Err,State}
    end;
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
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
handle_info({nodedown, NodeName},State)  ->
    NewState = do_node_down(NodeName,State),
    {noreply,NewState};

handle_info({client_to_center_app,Player = #player{node = Node,pid = PID}},State = #state{nodes = Nodes}) ->
    NewNodes = case lists:keytake(Node,#player.node,Nodes) of
                   false -> [Player|Nodes];
                   {value, _, Left} -> [Player|Left]
               end,
    game2048_client:center_respond_ok(PID,self()),
    {noreply,State#state{nodes = NewNodes}};

handle_info({client_to_center_chat,SelfNode,Msg},State = #state{nodes=Nodes}) ->
    [begin
         game2048_client:send_chat_msg_to_client(Node#player.pid,Msg)
     end||Node <- Nodes ,Node#player.node=/=SelfNode],
    {noreply,State};
handle_info({update_player,Player},State) ->
    NewState = update_player2(Player,State),
    {noreply, NewState};
handle_info(_Info, State) ->
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
%% @doc center server deal with nodedown message when client node down
do_node_down(NodeName,State = #state{signup = Signup,nodes = Nodes}) ->
    NewNodes = case lists:keytake(NodeName,#player.node,Nodes) of
                   false  ->  Nodes;
                   {value, _, L} -> L
               end,
    NewSigup = case lists:keytake(NodeName,#player.node,Signup) of
                   false -> Signup;
                   {value,_,L1} ->
                       boardcast_signup_info_to_all(Nodes,L1),
                       L1
               end,
    State#state{signup = NewSigup, nodes = NewNodes}.

%% @private
update_player2(Player = #player{node = NodeName},State = #state{signup = Signup,nodes = Nodes}) ->
    NewNodes = case lists:keytake(NodeName,#player.node,Nodes) of
                   false  ->  [Player|Nodes];
                   {value, _, L} -> [Player|L]
               end,
    NewSigup = update_signup(Player,Signup,Nodes),
    State#state{signup = NewSigup, nodes = NewNodes}.

%% @private
update_signup(Player,Signups,Nodes) ->
    case lists:keymember(Player#player.node,#player.node,Signups) of
        true ->
            game2048_client:msg_box(Player#player.pid,"alreay sign up plz click compare to begin"),
            Signups;
        false ->
            case erlang:length(Signups) >= ?MAX_SIGNUP_NUM of
                true ->
                    game2048_client:msg_box(Player#player.pid,"sign up number is full try later"),
                    Signups;
                false ->
                    game2048_client:msg_box(Player#player.pid,"sign up ok, click compare button to start"),
                    NewSignups = [Player|Signups],
                    boardcast_signup_info_to_all(Nodes,NewSignups),
                    NewSignups
            end
    end.

%% @private
do_challenge_check(Self,Other,AllPlayers) ->
    case lists:keytake(Self#player.node,#player.node,AllPlayers) of
        false -> {error,"self_no_sign_up"};
        {value,Other,_} -> {error,"cannot challenge yourself"};
        {value,_,LeftPlayersT} ->
            case lists:keytake(Other#player.node,#player.node,LeftPlayersT) of
                false -> {error,"other_alreay_begin_plz_try_another"};
                {value,_,LeftPlayers} ->
                    {ok,LeftPlayers}
            end
    end.

%% @private
do_challenge(Self,Other,State = #state{signup = SignupPlayers,nodes = AllPlayers}) ->
    case do_challenge_check(Self,Other,SignupPlayers) of
        {error,ErrReason} ->
            {error,ErrReason,SignupPlayers};
        {ok,LeftPlayers} ->
            boardcast_signup_info_to_all(AllPlayers,LeftPlayers),
            set_game_begin(Self,Other),
            {ok,State#state{signup = LeftPlayers}}
    end.

%% @private
%% @doc make two player begin the online game
set_game_begin(Self,Player) ->
    game2048_client:start_challenge_game(Self,Player),
    game2048_client:start_challenge_game(Player,Self),
    ok.
%% @private
%% @doc boardcast newest signup list to all client
boardcast_signup_info_to_all(Players,SignupPlayers) when is_list(Players)->
    [begin
         game2048_client:bc_signup_info(Player#player.pid,SignupPlayers)
     end||Player<-Players],
    ok.