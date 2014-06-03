%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2014 3:14 PM
%%%-------------------------------------------------------------------
-module(game2048_single_board).

%% API
-export([new/1,setup_board/2,get_board_score_data/1,set_board_score_data/2,set_new_name/2]).

%% Callbacks
-export([init/1, handle_sync_event/3,handle_event/2, handle_info/2, handle_call/3, handle_cast/2,code_change/3, terminate/2]).

-include("game2048.hrl").

-record(state, {win::pid(),%% self board pid
    parent::pid(),%%gui pid
    board=[]::list(),%% [#pos{}]
    pen,%% draw pen object
    fonts=[],%% draw fonts list
    score=0::integer(),
    name="NotSignUp"::list() %%signup name
}).

-behaviour(wx_object).

%%%%%%%%%%%%%%%%%%%%%%%%%% API
%% @doc new a single board
new(ParentObj) ->
    wx_object:start_link(?MODULE, [ParentObj, self()], []).

-spec setup_board(Board::pid(),Init::list()) ->
    ok.
setup_board(Board, Init) ->
    wx_object:call(Board, {setup_board, Init}).

-spec get_board_score_data(Board::pid()) ->
    {ok,[integer()|list()]}.
get_board_score_data(Board) ->
    wx_object:call(Board, get_board_score_data).

-spec set_board_score_data(Board::pid(),[integer()|list()]) ->
    ok.
set_board_score_data(Board,Data) ->
    wx_object:call(Board, {set_board_score_data,Data}).

%% @doc  begin a new game
-spec set_new_name(Board::pid(),Name::list()) ->
    ok.
set_new_name(Board,Name) ->
    wx_object:call(Board, {set_new_name,Name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init([ParentObj, ParentPid]) ->
    random:seed(erlang:now()),

    Win = wxPanel:new(ParentObj, [{style,?wxFULL_REPAINT_ON_RESIZE}]),%%
    wxWindow:setFocus(Win), % Get keyboard focus
    wxWindow:setSizeHints(Win, {250*2-100,250*2-100}),
    wxWindow:connect(Win, paint,  [callback]),%% callback paint event init 4*4line
    wxWindow:connect(Win, size,  []),
    wxWindow:connect(Win, key_up, [{skip, true}]),

    %% Init pens and fonts
    {Pen,Fs0} = game2048_lib:init_pen_fs(),

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
    {Win, #state{win=Win, board=game2048_lib:init_board(), pen=Pen, fonts=Fs,parent=ParentPid}}.

%% @private
%% @doc init will be call by pain callback :pain 4*4 lines
handle_sync_event(#wx{event=#wxPaint{}}, _Obj, State = #state{win=Win}) ->
    Size = wxWindow:getSize(Win),
    DC = wxPaintDC:new(Win),
    wxDC:destroyClippingRegion(DC),
    redraw(DC,Size,State),
    wxPaintDC:destroy(DC),
    ok.
%% @private
handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{win=Win}) ->
    wxWindow:setFocus(Win), %% Get keyboard focus
    {noreply,State};
handle_event(#wx{event=#wxKey{keyCode=?DOWN}},State) ->
    NewState = do_down(State),
    {noreply, NewState};
handle_event(#wx{event=#wxKey{keyCode=?UP}},State) ->
    NewState = do_up(State),
    {noreply, NewState};
handle_event(#wx{event=#wxKey{keyCode=?RIGHT}},State) ->
    NewState = do_right(State),
    {noreply, NewState};
handle_event(#wx{event=#wxKey{keyCode=?LEFT}},State) ->
    NewState = do_left(State),
    {noreply, NewState};

handle_event(#wx{event = #wxKey{type = key_up,keyCode = ?ENTER}},State=#state{name = Name}) ->
    catch game2048_chat_board:send_chat_msg(Name),
    {noreply, State};
handle_event(_Ev, State) ->
    io:format("~p::::unknown event:::~p",[?MODULE,_Ev]),
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%
%% @private
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
handle_call({set_new_name,Name},_From,State) ->
    {reply,ok,State#state{name=Name}};
handle_call(Msg,_From,S) ->
    io:format("~p:::::unknow call ~p~n",[?MODULE,Msg]),
    {reply, ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
handle_cast(Msg, State) ->
    io:format("~p:Got cast ~p~n",[?MODULE,Msg]),
    {noreply,State}.

%% @private
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

%% @private
handle_info(Msg, State) ->
    {stop, {info, Msg}, State}.

%% @private
terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
redraw(S = #state{win=Win}) ->
    DC0  = wxClientDC:new(Win),
    DC   = wxBufferedDC:new(DC0),
    Size = wxWindow:getSize(Win),
    redraw(DC, Size, S),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.

%% @private
redraw(DC, Size, S) ->
    wx:batch(fun() ->
        wxDC:setBackground(DC, ?wxWHITE_BRUSH),
        wxDC:clear(DC),
        draw_board(DC,Size,S),
        Fonts = S#state.fonts,
        [draw_number(DC,Fonts,Sq) || Sq = #pos{val= Val} <- S#state.board,Val =/=0]
    end).

%% @private
draw_number(DC,Fonts,#pos{xy={PosX, PosY}, val = Num}) ->
    NumList = integer_to_list(Num),
    Length = erlang:length(NumList),
    F = game2048_lib:select_font(Length,Fonts),
    {XInit,YInit,Color} = game2048_lib:select_dis_color(Length),
    wxFont:setWeight(F,?wxNORMAL),
    wxDC:setTextForeground(DC,Color),
    wxDC:setFont(DC,F),

    wxDC:drawText(DC, NumList, {(PosX-1)*95+XInit,YInit+(PosY-1)*95}),
    ok.

%% @private
draw_board(DC,{W0,H0},#state{pen=Pen}) ->
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

%% @private
do_right(State=#state{board = Board,score = OldScore,name = Name}) ->
    {NewBoard, AddScore} = do_right2(Board,OldScore,Name),
    Score = OldScore+AddScore,
    NewState = State#state{board = NewBoard, score = Score},
    redraw(NewState),
    game2048_client:update_board_to_other(NewBoard,Score),
    game2048_gui:update_score_from_self(Score),
    NewState.
%% @private
do_right2(Board,OldScore,Name) ->
    BoardListT = classify_by_xy(Board,y),
    Fun = fun(#pos{xy={X1,_Y1}},#pos{xy={X2,_Y2}}) -> X1> X2 end,
    {NewBoardList1,Score} = lists:foldl(fun(BoardT,{Acc,Acc1}) ->
        BoardListT1 = lists:sort(Fun, BoardT),
        BoardListT2 = classify_out_zero(BoardListT1,right),
        BoardListT3 = lists:sort(Fun,BoardListT2),
        {BoardListT4,ScoreT} = merge_list(BoardListT3,right),
        {BoardListT4++Acc,Acc1+ScoreT}
    end, {[],0}, BoardListT),
    HasChange = is_change(NewBoardList1,Board),
    {add_new_num(NewBoardList1,Score,OldScore,Name,HasChange),Score}.

%% @private
do_left(State=#state{board=Board,score = OldScore,name = Name}) ->
    {NewBoard,AddScore} = do_left2(Board,OldScore,Name),
    Score = OldScore+AddScore,
    NewState = State#state{board = NewBoard, score= Score},
    redraw(NewState),
    game2048_client:update_board_to_other(NewBoard,Score),
    game2048_gui:update_score_from_self(Score),
    NewState.

%% @private
do_left2(Board,OldScore,Name) ->
    BoardListT = classify_by_xy(Board,y),
    Fun = fun(#pos{xy={X1,_Y1}},#pos{xy={X2,_Y2}}) -> X1> X2 end,
    {NewBoardList1,Score} = lists:foldl(fun(BoardT,{Acc,Acc1}) ->
        BoardListT1 = lists:sort(Fun, BoardT),
        BoardListT2 = classify_out_zero(BoardListT1,left),
        BoardListT3 = lists:sort(Fun,BoardListT2),
        {BoardListT4,ScoreT} = merge_list(BoardListT3,left),
        {BoardListT4++Acc,ScoreT+Acc1}
    end, {[],0}, BoardListT),
    HasChange = is_change(NewBoardList1,Board),
    {add_new_num(NewBoardList1,Score,OldScore,Name,HasChange),Score}.

%% @private
do_up(State=#state{board=Board,score = OldScore,name = Name}) ->
    {NewBoard, AddScore} = do_up2(Board,OldScore,Name),
    Score = OldScore+AddScore,
    NewState = State#state{board = NewBoard, score=Score},
    redraw(NewState),
    game2048_client:update_board_to_other(NewBoard,Score),
    game2048_gui:update_score_from_self(Score),
    NewState.

%% @private
do_up2(Board,OldScore,Name) ->
    BoardListT = classify_by_xy(Board,x),
    Fun = fun(#pos{xy={_X1,Y1}},#pos{xy={_X2,Y2}}) -> Y1> Y2 end,
    {NewBoardList1,Score} = lists:foldl(fun(BoardT,{Acc,Acc1}) ->
        BoardListT2 = classify_out_zero(BoardT,up),
        BoardListT3 = lists:sort(Fun,BoardListT2),
        {BoardListT4,ScoreT} = merge_list(BoardListT3,up),
        {BoardListT4++Acc,ScoreT+Acc1}
    end, {[],0}, BoardListT),
    HasChange = is_change(NewBoardList1,Board),
    {add_new_num(NewBoardList1,Score,OldScore,Name,HasChange),Score}.

%% @private
do_down(State=#state{board=Board, score = OldScore,name =Name}) ->
    {NewBoard,AddScore} = do_down2(Board,OldScore,Name),
    Score = OldScore+AddScore,
    NewState = State#state{board = NewBoard,score =Score},
    redraw(NewState),
    game2048_client:update_board_to_other(NewBoard,Score),
    game2048_gui:update_score_from_self(Score),
    NewState.

%% @private
do_down2(Board,OldScore,Name) ->
    BoardListT = classify_by_xy(Board,x),
    Fun = fun(#pos{xy={_X1,Y1}},#pos{xy={_X2,Y2}}) -> Y1> Y2 end,
    {NewBoardList1,Score} = lists:foldl(fun(BoardT,{Acc,Acc1}) ->
        BoardListT1 = lists:sort(Fun, BoardT),
        BoardListT2 = classify_out_zero(BoardListT1,down),
        BoardListT3 = lists:sort(Fun,BoardListT2),
        {BoardListT4,ScoreT} = merge_list(BoardListT3,down),
        {BoardListT4++Acc,ScoreT+Acc1}
    end, {[],0}, BoardListT),
    HasChange = is_change(NewBoardList1,Board),
    {add_new_num(NewBoardList1,Score,OldScore,Name,HasChange),Score}.

%% @private
%% x ->[#pos{xy={X,1},#pos{xy={X,2},#pos{xy={X,3},#pos{xy={X,4}]
%% y ->[#pos{xy={1,Y},#pos{xy={2,Y},#pos{xy={3,Y},#pos{xy={4,Y}]
%% tip: not guarantee order
classify_by_xy(List,Dir) ->
    lists:foldl(fun(Pos = #pos{xy = {X,Y}},[Acc1,Acc2,Acc3,Acc4]) ->
        Classify = case Dir of  y -> Y; x -> X end,
        case Classify  of
            1-> [[Pos|Acc1], Acc2, Acc3, Acc4];
            2 -> [Acc1, [Pos|Acc2], Acc3, Acc4];
            3 -> [Acc1, Acc2, [Pos|Acc3], Acc4];
            4 -> [Acc1, Acc2, Acc3, [Pos|Acc4]]
        end
    end, [[],[],[],[]],List).

%% @private
%% @return List1++List2, List1-->#pos{} val=/=0,List-->#pos{} val=:=0
%% tip: not guarantee order
classify_out_zero([#pos{xy={X,_Y}}|_]=ListT,up) ->
    List = lists:sort(fun(#pos{xy = {_X1,Y1}},#pos{xy={_X2,Y2}})-> Y1<Y2 end,ListT),
    {NewList,_} = lists:foldl(fun(#pos{val=Val},{Acc,Sum}) ->
        case Val of
            0 -> {Acc,Sum};
            _ -> {[#pos{xy={X,Sum},val=Val}|Acc],Sum+1}
        end
    end,{[],1},List),
    NewList++make_zero_list(erlang:length(NewList),X,up);
classify_out_zero([#pos{xy={X,_Y}}|_]=ListT,down) ->
    List = lists:sort(fun(#pos{xy = {_X1,Y1}},#pos{xy={_X2,Y2}})-> Y1>Y2 end,ListT),
    {NewList,_} = lists:foldl(fun(#pos{val=Val},{Acc,Sum}) ->
        case Val of
            0 -> {Acc,Sum};
            _ -> {[#pos{xy={X,Sum},val=Val}|Acc],Sum-1}
        end
    end,{[],4},List),
    NewList++make_zero_list(erlang:length(NewList),X,down);

classify_out_zero([#pos{xy={_,Y}}|_]=ListT,right) ->
    List = lists:sort(fun(#pos{xy = {X1,_Y1}},#pos{xy={X2,_Y2}})-> X1>X2 end,ListT),
    {NewList,_} = lists:foldl(fun(#pos{val=Val},{Acc,Sum}) ->
        case Val of
            0 -> {Acc,Sum};
            _ -> {[#pos{xy={Sum,Y},val=Val}|Acc],Sum-1}
        end
    end,{[],4},List),
    NewList++make_zero_list(erlang:length(NewList),Y,right);
classify_out_zero([#pos{xy={_,Y}}|_]=ListT,left) ->
    List = lists:sort(fun(#pos{xy = {X1,_Y1}},#pos{xy={X2,_Y2}})-> X1<X2 end,ListT),
    {NewList,_} = lists:foldl(fun(#pos{val=Val},{Acc,Sum}) ->
        case Val of
            0 -> {Acc,Sum};
            _ -> {[#pos{xy={Sum,Y},val=Val}|Acc],Sum+1}
        end
    end,{[],1},List),
    NewList++make_zero_list(erlang:length(NewList),Y,left).

%% @private
make_zero_list(4,_,_) ->
    [];
make_zero_list(3,X,left) ->
    [#pos{xy={4,X},val=0}];
make_zero_list(2,X,left) ->
    [#pos{xy={4,X},val=0},#pos{xy={3,X},val=0}];
make_zero_list(1,X,left) ->
    [#pos{xy={4,X},val=0},#pos{xy={3,X},val=0},#pos{xy={2,X},val=0}];
make_zero_list(0,X,left) ->
    [#pos{xy={4,X},val=0},#pos{xy={3,X},val=0},#pos{xy={2,X},val=0},#pos{xy={1,X},val=0}];

make_zero_list(3,X,right) ->
    [#pos{xy={1,X},val=0}];
make_zero_list(2,X,right) ->
    [#pos{xy={1,X},val=0},#pos{xy={2,X},val=0}];
make_zero_list(1,X,right) ->
    [#pos{xy={1,X},val=0},#pos{xy={2,X},val=0},#pos{xy={3,X},val=0}];
make_zero_list(0,X,right) ->
    [#pos{xy={4,X},val=0},#pos{xy={3,X},val=0},#pos{xy={2,X},val=0},#pos{xy={1,X},val=0}];

make_zero_list(3,Y,up) ->
    [#pos{xy={Y,4},val=0}];
make_zero_list(2,Y,up) ->
    [#pos{xy={Y,4},val=0},#pos{xy={Y,3},val=0}];
make_zero_list(1,Y,up) ->
    [#pos{xy={Y,4},val=0},#pos{xy={Y,3},val=0},#pos{xy={Y,2},val=0}];
make_zero_list(0,Y,up) ->
    [#pos{xy={Y,4},val=0},#pos{xy={Y,3},val=0},#pos{xy={Y,2},val=0},#pos{xy={Y,1},val=0}];

make_zero_list(3,Y,down) ->
    [#pos{xy={Y,1},val=0}];
make_zero_list(2,Y,down) ->
    [#pos{xy={Y,1},val=0},#pos{xy={Y,2},val=0}];
make_zero_list(1,Y,down) ->
    [#pos{xy={Y,1},val=0},#pos{xy={Y,2},val=0},#pos{xy={Y,3},val=0}];
make_zero_list(0,Y,down) ->
    [#pos{xy={Y,4},val=0},#pos{xy={Y,3},val=0},#pos{xy={Y,2},val=0},#pos{xy={Y,1},val=0}].

%% @private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  --X1--X2--X3--X4 -->X
%%% |
%%% |Y1
%%% |
%%% |Y2
%%% |
%%% |Y3
%%% |
%%% |Y4
%%% Y
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge_list([T4=#pos{val=TV4},T3=#pos{val=TV3},T2=#pos{val=TV2},T1=#pos{val=TV1}],up) ->
    if TV1 =:= TV2 andalso TV3=:=TV4 ->
        Score = TV1*2+TV3*2,
        T11 = T1#pos{val = TV1*2}, T22 = T2#pos{val=TV3*2},
        T33 = T3#pos{val = 0}, T44 = T4#pos{val = 0};
        TV1 =:= TV2 ->
            Score = TV1*2,
            T11 = T1#pos{val = TV1*2}, T22 = T2#pos{val=TV3},
            T33 = T3#pos{val = TV4}, T44 = T4#pos{val = 0};
        TV3 =:= TV4 andalso TV3 =:= TV2->
            Score = TV2*2,
            T11 = T1, T22 = T2#pos{val = TV2*2},
            T33 = T3#pos{val = TV4}, T44 = T4#pos{val = 0};
        TV3 =:= TV4 ->
            Score = TV3*2,
            T11 = T1, T22 = T2,
            T33 = T3#pos{val = TV3*2}, T44 = T4#pos{val = 0};
        TV3 =:= TV2 ->
            Score = TV2*2,
            T11 = T1, T22 = T2#pos{val = TV2*2},
            T33 = T3#pos{val = TV4}, T44 = T4#pos{val = 0};
        true ->
            Score = 0,
            T11 = T1, T22 = T2,
            T33 = T3, T44 = T4
    end,
    {[T44,T33,T22,T11],Score};
merge_list([T4=#pos{val=TV4},T3=#pos{val=TV3},T2=#pos{val=TV2},T1=#pos{val=TV1}],down) ->
    if TV4 =:= TV3 andalso TV2=:=TV1 ->
        Score = TV4*2+TV2*2,
        T44 = T4#pos{val = TV4*2}, T33 = T3#pos{val=TV2*2},
        T22 = T2#pos{val = 0}, T11 = T1#pos{val = 0};
        TV4 =:= TV3 ->
            Score = TV4*2,
            T44 = T4#pos{val = TV4*2}, T33 = T3#pos{val=TV2},
            T22 = T2#pos{val = TV1}, T11 = T1#pos{val = 0};
        TV2 =:= TV1 andalso TV2 =:= TV3->
            Score = TV3*2,
            T44 = T4, T33 = T3#pos{val = TV3*2},
            T22 = T2, T11 = T1#pos{val = 0};
        TV2 =:= TV1 ->
            Score = TV2*2,
            T44 = T4, T33 = T3,
            T22 = T2#pos{val = TV2*2}, T11 = T1#pos{val = 0};
        TV3 =:= TV2 ->
            Score = TV3*2,
            T44 = T4, T33 = T3#pos{val = TV3*2},
            T22 = T2#pos{val = TV1}, T11 = T1#pos{val = 0};
        true ->
            Score = 0,
            T11 = T1, T22 = T2,
            T33 = T3, T44 = T4
    end,
    {[T44,T33,T22,T11],Score};
merge_list([T4=#pos{val=TV4},T3=#pos{val=TV3},T2=#pos{val=TV2},T1=#pos{val=TV1}],right) ->
    if TV3 =:= TV4 andalso TV1=:=TV2 ->
        Score = TV4*2+TV2*2,
        T33 = T3#pos{val = TV2*2}, T44 = T4#pos{val=TV4*2},
        T22 = T2#pos{val = 0}, T11 = T1#pos{val = 0};
        TV3 =:= TV4 ->
            Score = TV4*2,
            T44 = T4#pos{val=TV4*2}, T33 = T3#pos{val=TV2},
            T22 = T2#pos{val = TV1}, T11 = T1#pos{val = 0};
        TV1 =:= TV2 andalso TV2 =:= TV3 ->
            Score = TV3*2,
            T44 = T4, T33 = T3#pos{val = TV3*2},
            T22 = T2, T11 = T1#pos{val = 0};
        TV1 =:= TV2 ->
            Score = TV2*2,
            T44 = T4, T33 = T3,
            T22 = T2#pos{val = TV2*2}, T11 = T1#pos{val = 0};
        TV3 =:= TV2 ->
            Score = TV3*2,
            T44 = T4, T33 = T3#pos{val = TV3*2},
            T22 = T2#pos{val = TV1}, T11 = T1#pos{val = 0};
        true ->
            Score = 0,
            T11 = T1, T22 = T2,
            T33 = T3, T44 = T4
    end,
    {[T44,T33,T22,T11],Score};
merge_list([T4=#pos{val=TV4},T3=#pos{val=TV3},T2=#pos{val=TV2},T1=#pos{val=TV1}],left) ->
    if TV1 =:= TV2 andalso TV3=:=TV4 ->
        Score = TV1*2+TV3*2,
        T11 = T1#pos{val = TV1*2}, T22 = T2#pos{val=TV3*2},
        T33 = T3#pos{val = 0}, T44 = T4#pos{val = 0};
        TV1 =:= TV2 ->
            Score = TV1*2,
            T11 = T1#pos{val = TV1*2}, T22 = T2#pos{val=TV3},
            T33 = T3#pos{val = TV4}, T44 = T4#pos{val = 0};
        TV3 =:= TV4 andalso TV2 =:= TV3->
            Score = TV2*2,
            T11 = T1, T22 = T2#pos{val = TV2*2},
            T33 = T3, T44 = T4#pos{val = 0};
        TV3 =:= TV4 ->
            Score = TV3*2,
            T11 = T1, T22 = T2,
            T33 = T3#pos{val = TV3*2}, T44 = T4#pos{val = 0};
        TV3 =:= TV2 ->
            Score = TV2*2,
            T11 = T1, T22 = T2#pos{val = TV2*2},
            T33 = T3#pos{val = TV4}, T44 = T4#pos{val = 0};
        true ->
            Score = 0,
            T11 = T1, T22 = T2,
            T33 = T3, T44 = T4
    end,
    {[T44,T33,T22,T11],Score}.

%% @private
add_new_num(BoardList,Score,OldScore,Name,IsChange) ->
    case find_zero_pos(BoardList) of
        []  ->
            is_game_over(Score,BoardList,OldScore,Name),
            BoardList;
        ZeroList when IsChange ->
            Index  = random:uniform(length(ZeroList)),
            ZeroPosT = lists:nth(Index,ZeroList),
            ZeroPos = case random:uniform(2) of
                          1 -> ZeroPosT#pos{val= 2};
                          2 -> ZeroPosT#pos{val= 4}
                      end,
            lists:keyreplace(ZeroPos#pos.xy,#pos.xy,BoardList,ZeroPos);
        _ ->
           BoardList
    end.

%% @private
find_zero_pos(BoardList) ->
    lists:filter(fun(#pos{val=Val}) -> Val=:=0 end, BoardList).

%% @private
is_game_over(0,BoardList,Score,Name) ->
    IsAlive = lists:any(fun(#pos{xy = {X,Y},val = Val}) ->
        Alive1 = case lists:keyfind({X-1,Y},#pos.xy,BoardList) of
                     false -> false;
                     #pos{val = ValT}  ->
                         Val =:= ValT
                 end,
        Alive2 = case lists:keyfind({X,Y-1},#pos.xy,BoardList) of
                     false -> false;
                     #pos{val = ValT1}  ->
                         Val =:= ValT1
                 end,
        Alive1 orelse Alive2
    end,BoardList),
    case IsAlive of
        true -> ok;
        false ->
            {ok,{OtherName,OtherScore}} = game2048_gui:get_other_client_info(),
            Msg = io_lib:format("You Got:~w, Challenge:~p Got: ~w,Game over!!!Try again.....",[Score,OtherName,OtherScore]),
            game2048_gui:msg_box(Msg),
            ChatMsg = io_lib:format("Wow, Challenge:~p Got: ~w score!!!!",[Name,Score]),
            game2048_client:send_chat_msg_to_center(ChatMsg),
            game2048_chat_board:print_chat_msg(Msg),
            game2048_chat_board:print_chat_msg(ChatMsg)
    end;
is_game_over(_,_,_,_)->
    ok.

%% @private
is_change(NewList,OldList) ->
    not lists:all(fun(Mem) -> lists:member(Mem,NewList) end,OldList).
