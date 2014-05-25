%%%-------------------------------------------------------------------
%%% @author  <zhongwencool@gmail.com>
%%% @doc
%%% Created : 23. May 2014 5:51 PM
%%%-------------------------------------------------------------------
-module(game2048_center).

-behaviour(gen_server).

-include("game2048.hrl").

%% API
-export([start/0,i/0]).

-export([sign_up/1,compare/2,update_player/2,client_to_center_app/2,client_to_center_challenge/2,client_to_center_chat/3,send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-record(state, {signup=[],nodes = []}).

-define(MAX_SIGNUP_NUM,3).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% for debugger look state
i() ->
    call(i).

sign_up(Player) ->
    call({sign_up,Player}).
compare(Self,Player) ->
    call({compare,Self,Player}).

update_player(CenterPID,Info) ->
    erlang:send(CenterPID,{update_player,Info}).

client_to_center_app(CenterPID,PlayInfo) ->
    erlang:send(CenterPID,{client_to_center_app,PlayInfo}).

client_to_center_challenge(CenterPID,PlayInfo) ->
    gen_server:call(CenterPID,{client_to_center_challenge,PlayInfo}).

client_to_center_chat(CenterPID,Node,ChatMsg) ->
    erlang:send(CenterPID,{client_to_center_chat,Node,ChatMsg}).

send_msg(CenterPID,Info) ->
    erlang:send(CenterPID,Info).

call(Msg) ->
    gen_server:call(?MODULE, Msg, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    ok = net_kernel:monitor_nodes(true),
    {ok, #state{}}.
%%--------------------------------------------------------------------
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
handle_cast(_Msg, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------

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
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================
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

update_player2(Player = #player{node = NodeName},State = #state{signup = Signup,nodes = Nodes}) ->
    NewNodes = case lists:keytake(NodeName,#player.node,Nodes) of
                   false  ->  [Player|Nodes];
                   {value, _, L} -> [Player|L]
               end,
    NewSigup = update_signup(Player,Signup,Nodes),
    State#state{signup = NewSigup, nodes = NewNodes}.

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
do_challenge(Self,Other,State = #state{signup = SignupPlayers,nodes = AllPlayers}) ->
    case do_challenge_check(Self,Other,SignupPlayers) of
        {error,ErrReason} ->
            {error,ErrReason,SignupPlayers};
        {ok,LeftPlayers} ->
            boardcast_signup_info_to_all(AllPlayers,LeftPlayers),
            set_game_begin(Self,Other),
            {ok,State#state{signup = LeftPlayers}}
    end.

set_game_begin(Self,Player) ->
    game2048_client:start_challenge_game(Self,Player),
    game2048_client:start_challenge_game(Player,Self),
    ok.
boardcast_signup_info_to_all(Players,SignupPlayers) when is_list(Players)->
    [begin
         game2048_client:bc_signup_info(Player#player.pid,SignupPlayers)
     end||Player<-Players],
    ok.