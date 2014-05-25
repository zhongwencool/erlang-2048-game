%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%   another player board
%%% @end
%%% Created : 18. May 2014 3:14 PM
%%%-------------------------------------------------------------------
-module(game2048_compare_board).
-author("zhongwencool@gmail.com").

%% API
-export([new/1, setup_board/2,get_board_score_data/1,set_board_score_data/2,update_signup_info/2,start_challenge_game/3,
    board_list_from_other/3
]).

%% Callbacks
-export([init/1, handle_sync_event/3,handle_event/2, handle_info/2, handle_call/3, handle_cast/2,code_change/3, terminate/2]).

-include("game2048.hrl").

-record(state, {win, parent, board=[], pen, fonts=[],score=0,list = []}).

-behaviour(wx_object).

%% API
new(ParentObj) ->
    wx_object:start_link(?MODULE, [ParentObj, self()], []).

setup_board(Board, Init) ->
    wx_object:call(Board, {setup_board, Init}).

get_board_score_data(Board) ->
    wx_object:call(Board, get_board_score_data).

set_board_score_data(Board,Data) ->
    wx_object:call(Board, {set_board_score_data,Data}).

update_signup_info(Board,SignupList) ->
    wx_object:call(Board, {update_signup_info,SignupList}).

start_challenge_game(Board,Self,Other) ->
    wx_object:call(Board, {start_challenge_game,Self,Other}).

board_list_from_other(Board,BoardList,Info) ->
    wx_object:call(Board, {board_list_from_other,BoardList,Info}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentObj, ParentPid]) ->
    random:seed(erlang:now()),

    Win = wxPanel:new(ParentObj, [{style,?wxFULL_REPAINT_ON_RESIZE}]),%%
    wxWindow:setSizeHints(Win, {250*2,250*2}),
    wxWindow:connect(Win, paint,  [callback]),%% callback paint event init 4*4line
    wxWindow:connect(Win, size,  []),
    wxWindow:connect(Win, command_button_clicked),

    %% Init pens and fonts
    Pen = wxPen:new({0,0,0}, [{width, 3}]),
    Fs0  = [{1 ,wxFont:new(80 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])},
        {2 ,wxFont:new(65 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])},
        {3 ,wxFont:new(45 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])},
        {4 ,wxFont:new(35 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])}],

    TestDC  = wxMemoryDC:new(),
    Bitmap = wxBitmap:new(256,256),
    wxMemoryDC:selectObject(TestDC, Bitmap),
    true = wxDC:isOk(TestDC),
    CW = fun({Sz,Font},Acc) ->
        case wxFont:ok(Font) of
            true ->
                wxDC:setFont(TestDC, Font),
                CH = wxDC:getCharHeight(TestDC),
                [{Sz,CH,Font} | Acc];
            false ->
                Acc
        end
    end,

    Fs = lists:foldl(CW, [], Fs0),
    wxMemoryDC:destroy(TestDC),
    wxStaticBoxSizer:new(?wxVERTICAL, Win,[{label, "Online-->Signup-->challenge"}]),
    {Win, #state{win=Win, board=game2048_lib:init_board(), pen=Pen, fonts=Fs,parent=ParentPid}}.

%% init will be call by pain callback :pain 4*4 lines or 1*8 lines
handle_sync_event(#wx{event=#wxPaint{}}, _Obj, State = #state{win=Win,board = Board}) ->
    Size = wxWindow:getSize(Win),
    DC = wxPaintDC:new(Win),
    wxDC:destroyClippingRegion(DC),
    wx:batch(fun() -> wxDC:setBackground(DC, ?wxWHITE_BRUSH),
        wxDC:clear(DC),
        case Board =:= game2048_lib:init_board() of
            true ->
                draw_board_for_sigup(DC,Size,State);
            false ->
                draw_board_for_play(DC,Size,State)
        end
    end),
    wxPaintDC:destroy(DC),
    ok.

handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{win=Win}) ->
    wxWindow:setFocus(Win), %% Get keyboard focus
    {noreply,State};
handle_event(#wx{id= ButtonID}, State)when ButtonID>= ?COMPARE1 andalso ButtonID =< ?COMPARE8 ->
   case make_challenge_info(ButtonID,State) of
       {ok,Info} -> game2048_client:send_challenge(Info);
       error -> game2048_gui:msg_box("nobody there,pick another")
   end,
    {noreply,State};
handle_event(_Ev, State) ->
    io:format("~p::::unknown event:::~p",[?MODULE,_Ev]),
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%
handle_call({setup_board, Init},_From, State=#state{board=Board}) ->
    NewBoard = lists:foldl(fun(New=#pos{xy=XY},Acc) ->
        lists:keyreplace(XY, #pos.xy, Acc, New)
    end,Board,Init),
    S = State#state{board=NewBoard},
    redraw(S),
    {reply, ok, S};
handle_call(get_board_score_data,_From, State=#state{board=Board,score=Score}) ->
    {reply, {ok,[Score|Board]}, State};
handle_call({set_board_score_data,[Score|BoardData]},_From, State) ->
    NewState = State#state{board=BoardData,score=Score},
    redraw(NewState),
    {reply, ok, NewState};
handle_call({update_signup_info,SignupList},_Form,State) ->
    do_update_signup_info(SignupList,State),
    {reply, ok, State#state{list = SignupList}};
handle_call({start_challenge_game,Self,Other},_From,State) ->
    do_start_challenge_game(Self,Other,State),
    {reply, ok, State};
handle_call({board_list_from_other,BoardList,{Name,Score}},_From,State) ->
    NewState = State#state{board = BoardList},
    redraw(NewState),
    game2048_gui:update_score_from_other(Name,Score),
    {reply, ok,NewState};
handle_call(Msg,_From,S) ->
    io:format("~p:::::unknow call ~p~n",[?MODULE,Msg]),
    {reply, ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(Msg, State) ->
    io:format("~p:Got cast ~p~n",[?MODULE,Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_info(Msg, State) ->
    {stop, {info, Msg}, State}.

terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

redraw(S = #state{win=Win}) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, S),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

redraw(DC, Size, S) ->
    wx:batch(fun() ->
        wxDC:setBackground(DC, ?wxWHITE_BRUSH),
        wxDC:clear(DC),
        draw_board_for_play(DC,Size,S),
        Fonts = S#state.fonts,
        [draw_number(DC,Fonts,Sq) || Sq = #pos{val= Val} <- S#state.board,Val =/=0]
    end).

draw_number(DC,Fonts,#pos{xy={PosX, PosY}, val = Num}) ->
    NumList = integer_to_list(Num),
    Length = erlang:length(NumList),
    F = game2048_lib:select_font(Length,Fonts),
    {XInit,YInit,Color} = game2048_lib:select_dis_color(Length),
    wxFont:setWeight(F,?wxNORMAL),
    wxDC:setTextForeground(DC,Color),
    wxDC:setFont(DC,F),
    wxDC:drawText(DC, NumList, {(PosX-1)*120+XInit,YInit+(PosY-1)*120}),
    ok.

draw_board_for_play(DC,{W0,H0},#state{pen=Pen}) ->
    BoxSz = game2048_lib:getGeomSz(W0,H0),
    BS = ?BRD+4*BoxSz,
    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0,0,0}),
    wxDC:setPen(DC,Pen),
    wxDC:drawRoundedRectangle(DC, {?BRD,?BRD,4*BoxSz+1,4*BoxSz+1},
        float(?ARC_R)),
    %% Draw 4*4 Line
    wxDC:drawLines(DC, [{?BRD+BoxSz, ?BRD}, {?BRD+BoxSz, BS}]),
    wxDC:drawLine(DC, {?BRD+BoxSz*2, ?BRD}, {?BRD+BoxSz*2, BS}),
    wxDC:drawLines(DC, [{?BRD+BoxSz*3, ?BRD}, {?BRD+BoxSz*3, BS}]),
    wxDC:drawLine(DC, {?BRD+BoxSz*4, ?BRD}, {?BRD+BoxSz*4, BS}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz}, {BS, ?BRD+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*2}, {BS, ?BRD+BoxSz*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*3}, {BS, ?BRD+BoxSz*3}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*4}, {BS, ?BRD+BoxSz*4}),
    BoxSz.

draw_board_for_sigup(DC,{W0,H0},#state{pen=Pen,win = Win}) ->
    BoxSz = (game2048_lib:getGeomSz(W0,H0)) div 2,
    BS = ?BRD+4*BoxSz+200,
    wxPen:setWidth(Pen, 3),
    wxPen:setColour(Pen, {0,0,0}),
    wxDC:setPen(DC,Pen),
    wxDC:drawRoundedRectangle(DC, {?BRD,?BRD,8*BoxSz+1,8*BoxSz+1},
        float(?ARC_R)),
    %% Draw 8 Line
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz}, {BS, ?BRD+BoxSz}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*2}, {BS, ?BRD+BoxSz*2}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*3}, {BS, ?BRD+BoxSz*3}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*4}, {BS, ?BRD+BoxSz*4}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*5}, {BS, ?BRD+BoxSz*5}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*6}, {BS, ?BRD+BoxSz*6}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*7}, {BS, ?BRD+BoxSz*7}),
    wxDC:drawLine(DC, {?BRD, ?BRD+BoxSz*8}, {BS, ?BRD+BoxSz*8}),
    %% new compare button
    lists:foldl(fun(X,Acc) ->
        wxButton:new(Win, ?COMPARE1+X, [{label,"←_←"},{size, {-1, 60}},{pos,{400+70,Acc}}]),
        Acc+60
    end, 8, [0,1,2,3,4,5,6,7]).


do_start_challenge_game(_Self,_Other,State = #state{win = Win}) ->
    redraw(State),
    [begin Button = wxWindow:findWindow(Win,ID),
    wxButton:disable(Button)
     end||ID<- lists:seq(?COMPARE1,?COMPARE8)],
    ok.

do_update_signup_info(SignupList,#state{win = Win,fonts = Fonts} = State) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    wx:batch(fun() ->
        wxDC:setBackground(DC, ?wxWHITE_BRUSH),
        wxDC:clear(DC),
        draw_board_for_sigup(DC,Size,State),
        draw_signup_info(SignupList,DC,Fonts,1)
    end),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

draw_signup_info([],_,_,_) ->
    ok;
draw_signup_info([Signup|Left],DC,Fonts,Num) ->
    draw_name_for_sigup(DC,Fonts,Signup#player.name,get_name_pos(Num)),
    draw_signup_info(Left,DC,Fonts,Num+1).

draw_name_for_sigup(DC,Fonts,Name,XY)when is_list(Name) ->
    F = game2048_lib:select_font(4,Fonts),
    {_,_,Color} = game2048_lib:select_dis_color(random:uniform(4)),
    wxFont:setWeight(F,?wxITALIC),
    wxDC:setTextForeground(DC,Color),
    wxDC:setFont(DC,F),
    wxDC:drawText(DC, Name, XY),
    ok.

%%[{X,Y},{X,Y+60*1},{X,Y+60*2},{X,Y+60*3},{X,Y+60*4},{X,Y+60*5},{X,Y+60*6},{X,Y+60*7}].
get_name_pos(Num) ->
    {10,10+60*(Num-1)}.

make_challenge_info(ButtonID,#state{list = List}) ->
    Pos = ButtonID-?COMPARE1+1,
    case Pos > erlang:length(List)  of
        true -> error;
        false -> {ok,lists:nth(Pos,List)}
    end.
