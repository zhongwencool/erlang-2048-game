%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc client  link to center node
%%%
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

-record(state,{center_state,name="NotSignUp",other_client}).

-define(CENTER_NODE,'game2048_center_server@127.0.0.1').
%%%===================================================================
%%% API
%%%===================================================================

call(PID,Msg)  ->
    gen_server:call(PID,Msg,infinity).

%% @doc for debugger :look state
i() ->
    call( get_svrname(), i).

%%--------------------------------------------------------------------
%%
start() ->
    gen_server:start({local,?MODULE},?MODULE,[?MODULE],[]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [?MODULE], []).

send_chat_msg_to_center(Msg) ->
    erlang:send(?MODULE,{send_chat_msg_to_center,node(),Msg}).

update_new_name(Name) ->
    erlang:send(?MODULE,{update_new_name,Name}).

msg_box(PID,Msg)when is_list(Msg) ->
    erlang:send(PID,{msg_box,Msg}).

bc_signup_info(PID,Info) ->
    erlang:send(PID,{update_signup_info,Info}).

send_challenge(Info) ->
    erlang:send(?MODULE,{challenge_info,Info}).

send_chat_msg_to_client(PID,Msg) ->
    erlang:send(PID,{send_chat_msg_to_client,Msg}).

center_respond_ok(PID,Msg) ->
    erlang:send(PID,{center_respond_client_ok,Msg}).

start_challenge_game(Self,Player) ->
    erlang:send(Self#player.pid,{start_challenge_game,Self,Player}).

update_board_to_other(BoardList,Score) ->
    erlang:send(?MODULE,{update_board_list,BoardList,Score}).
%%--------------------------------------------------------------------
%%
stop() ->
    ServerName = get_svrname(),
    gen_server:cast(ServerName, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([_]) ->
    erlang:process_flag(trap_exit,true),
    erlang:send_after(1000,self(),loop_second),
    {ok, #state{}}.

%%--------------------------------------------------------------------
handle_call(i,_From,State) ->
    {reply,{ok,State},State};

handle_call(Request, _From, BookList) ->
    io:format("unknow_call_msg:Request:~p",[Request]),
    {reply, ok, BookList}.

%%--------------------------------------------------------------------
handle_cast(stop,State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(loop_second,State) ->
    erlang:send_after(1000,self(),loop_second),
    NewState = do_loop(State),
    {noreply,NewState};

handle_info(connect_center_ok,State) ->
    {noreply,State#state{center_state= node_connected}};

handle_info({center_respond_client_ok,CloudPID},State) ->
    {noreply,State#state{center_state={available,CloudPID}}};

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
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================
%% not connect center node yet
do_loop( State = #state{center_state=undefined}) ->
    io:format("connecting center server node~n "),
    SelfPID = erlang:self(),
    erlang:spawn(fun() -> do_connect_cloud(SelfPID) end),
    State;
%% node connected but not sure center server is alive
do_loop( State = #state{center_state= node_connected}) ->
    io:format("confriming center server is ok:~n"),
    connect_to_cloud_server(),
    State;
%% center server working ok
do_loop( State = #state{center_state={available,_CloudPID}}) ->
    State.

do_connect_cloud(ParentPID) ->
    true = erlang:set_cookie(?CENTER_NODE, 'game2048'),
    case net_kernel:connect_node(?CENTER_NODE) of
        true -> erlang:send(ParentPID,connect_center_ok);
        _ -> ignore
    end.

%% connect to cloud_server app
connect_to_cloud_server() ->
    case rpc:call(?CENTER_NODE, erlang, whereis, [game2048_center]) of
        CloudPID when erlang:is_pid(CloudPID)  ->
            game2048_center:client_to_center_app(CloudPID,#player{pid = self(),node = node()});
        _Err ->
            io:format("rpc call error~p~n",[_Err]),
            net_kernel:disconnect(?CENTER_NODE)
    end.

%%  todo
get_svrname() ->
    ?MODULE.

sign_up(Board,Name) ->
    %% change new name
    game2048_single_board:set_new_name(Board,Name),
    update_new_name(Name),
    ok.