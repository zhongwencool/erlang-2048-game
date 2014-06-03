%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%    game2048 gui manager
%%% @end
%%% Created : 18. May 2014 2:50 PM
%%%-------------------------------------------------------------------
-module(game2048_gui).

%% API
-export([new/1,start_new_game/0,msg_box/1,update_signup_info/1,
    start_challenge_game/2,board_list_from_other/2,update_score_from_other/2,
    update_score_from_self/1,get_other_client_info/0
]).
%% CallBack
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
    terminate/2, code_change/3]).


-behaviour(wx_object).

-include("game2048.hrl").

%%%%%%%%%%  Graphic engine %%%%%%%%%%%%%%

-record(gs,{board::pid(),%% self board pid
    compare_board::pid(),%% compare board pid
    game::pid(),%% parent pid
    frame,other_client= {"nobody",0}::tuple() %%{name,score}
}).

%% @doc new gui manger
new(Game) ->
    wx:new(),
    wx_object:start_link(?MODULE, [Game], []).

%% @doc print Info to  staus bar
-spec print_staus(Msg::list()) ->
    ok.
print_staus(Info) when is_list(Info)->
    erlang:send(?MODULE,{binary_to_list(list_to_binary(Info))}).

%% @doc start a init game
-spec start_new_game() -> ok.
start_new_game() ->
    erlang:send(?MODULE,{init}).

%% @doc popup message on screen
-spec msg_box(Msg::list()) ->
    ok.
msg_box(Msg) ->
    erlang:send(?MODULE,{msg_box,Msg}).

%% @doc client update signup list to gui,so that gui can transport to compare board
-spec update_signup_info(SignupList::list()) ->
    ok.
update_signup_info(SignupList) ->
    erlang:send(?MODULE,{udpate_signup_info,SignupList}).

%% @doc client server start challenge game
-spec start_challenge_game(Self::player(),Other::player()) ->
    ok.
start_challenge_game(Self,Other) ->
    erlang:send(?MODULE,{start_challenge_game,Self,Other}).

%% @doc client server send boardlist and score to gui
-spec board_list_from_other(BoardList::list(),OtherInfo::integer()) ->
    ok.
board_list_from_other(BoardList,Score) ->
    erlang:send(?MODULE,{board_list_from_other,BoardList,Score}).

%% @doc compare board send other name and score to gui
-spec update_score_from_other(OtherName::list(),OtherScore::integer()) ->
    ok.
update_score_from_other(OtherName,OtherScore) ->
    erlang:send(?MODULE,{update_score_from_other,OtherName,OtherScore}).

%% @doc single board send score to gui
-spec update_score_from_self(SelfScore::integer()) ->
    ok.
update_score_from_self(SelfScore) ->
    erlang:send(?MODULE,{update_score_from_self,SelfScore}).

%% @doc single board call  gui to get challenge player name and score
get_other_client_info() ->
    gen_server:call(?MODULE,get_other_client_info).

%%%%%%%%%%%%%%%%%%%%% Server callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
init([Game]) ->
    erlang:register(?MODULE,self()),
    {Frame, {Board,BoardCompare}} = wx:batch(fun() -> create_window() end),
    Game ! {gui_init_ok, self()},
    random:seed(erlang:now()),
    {Frame, #gs{board=Board,game=Game,frame=Frame,compare_board=BoardCompare}}.

%% @private
create_window() ->
    Frame = wxFrame:new(wx:null(), -1, "2048 Rule--A:left--D:right--W:Up--S:Down", []),
    wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),

    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),
    Online  = wxMenu:new([]),

    wxMenu:append(File, ?NEW,  "&Single Game"),
    wxMenu:append(File, ?OPEN, "&Open Game"),
    wxMenu:append(File, ?SAVE, "&Save Game"),
    wxMenu:appendSeparator(File),
    wxMenu:append(File, ?QUIT, "&Quit Game"),

    wxMenu:append(Online,?SIGNUP,"Signup"),
    wxMenu:append(Help, ?ABOUT, "About"),

    wxMenuBar:append(MenuBar, File, "&File"),
    wxMenuBar:append(MenuBar, Online, "&Online"),
    wxMenuBar:append(MenuBar, Help, "&Help"),

    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:connect(Frame, command_menu_selected),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Top    = wxBoxSizer:new(?wxHORIZONTAL),

    Panel = wxPanel:new(Frame),

    wxSizer:addSpacer(MainSz,2),
    SF = wxSizerFlags:new(),
    wxSizerFlags:proportion(SF,1),
    wxSizer:addSpacer(MainSz,3),

    wxSizer:addSpacer(MainSz,5),
    wxSizer:add( MainSz, Top, wxSizerFlags:center(wxSizerFlags:proportion(SF,0))),
    wxSizer:addSpacer(MainSz,10),

    Board = game2048_single_board:new(Panel),
    BoardCompare = game2048_compare_board:new(Panel),
    BoardChat = game2048_chat_board:new(Panel),

    wxSizer:add(Top, Board, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),
    wxSizer:add(Top, BoardCompare, wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),
    wxSizer:addSpacer(MainSz,10),
    wxSizer:add(MainSz, BoardChat,wxSizerFlags:proportion(wxSizerFlags:expand(SF),1)),

    wxWindow:setSizer(Panel,MainSz),
    wxSizer:fit(MainSz, Frame),
    wxSizer:setSizeHints(MainSz,Frame),
    wxWindow:show(Frame),
    {Frame, {Board,BoardCompare}}.

%% @private
status(Win, F, A) ->
    Str = lists:flatten(io_lib:format(F, A)),
    wxFrame:setStatusText(Win, Str).

%%%%%%%%%%%%%%%% Info i.e. messages %%%%%%%%%%%%%%%%%%%%%
%% @private
handle_info({init}, S = #gs{board=Board,frame=F}) ->
    Pos1 = #pos{xy= {random:uniform(4),random:uniform(4)},val = 2},
    Pos2 = get_no_overlap_pos(Pos1),
    NewBoard = lists:foldl(fun(New=#pos{xy=XY},Acc) ->
        lists:keyreplace(XY, #pos.xy, Acc, New)
    end,game2048_lib:init_board(),[Pos1,Pos2]),
    game2048_single_board:setup_board(Board, NewBoard),
    status(F, "New   ", []),
    {noreply, S};
handle_info({msg_box,Msg}, S = #gs{frame=Frame}) ->
    MD = wxMessageDialog:new(Frame,Msg,
        [{style, ?wxOK bor ?wxICON_INFORMATION},{caption, "2048"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    {noreply, S};
handle_info({udpate_signup_info,SignupList},State = #gs{compare_board = CompareBoard}) ->
    game2048_compare_board:update_signup_info(CompareBoard,SignupList),
    {noreply,State};
handle_info({start_challenge_game,Self,Other},State = #gs{compare_board = CompareBoard}) ->
    start_new_game(),
    game2048_compare_board:start_challenge_game(CompareBoard,Self,Other),
    {noreply,State};
handle_info({board_list_from_other,BoardList,{Name,Score}},State = #gs{compare_board = CompareBoard}) ->
    game2048_compare_board:board_list_from_other(CompareBoard,BoardList,{Name,Score}),
    {noreply,State};
handle_info({update_score_from_other,OtherName,OtherScore},State = #gs{board = SelfBoard}) ->
    {ok,[SelfScore|_]} = game2048_single_board:get_board_score_data(SelfBoard),
    print_staus(io_lib:format("You: ~w, Challenge: ~w",[SelfScore,OtherScore])),
    {noreply,State#gs{other_client = {OtherName,OtherScore}}};
handle_info({update_score_from_self,SelfScore},State = #gs{other_client ={_OtherName,OtherScore}}) ->
    print_staus(io_lib:format("You: ~w, Challenge: ~w",[SelfScore,OtherScore])),
    {noreply,State};
handle_info({busy, Mode},S) ->
    case Mode of
        start -> wx_misc:beginBusyCursor();
        stop  -> wx_misc:endBusyCursor()
    end,
    {noreply, S};
handle_info({Info},S = #gs{frame = F}) ->
    status(F, " ~p", [Info]),
    {noreply, S};
handle_info(Info,S) ->
    io:format("~p:unknow msg:~p~n",[?MODULE,Info]),
    {noreply, S}.

%%%%%%%%%%%%%%%%% GUI-Events %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
handle_event(#wx{event=#wxClose{}},
    S = #gs{game=G,frame=F}) ->
    catch wxWindow:'Destroy'(F),
    G ! quit,
    {stop, shutdown, S};

handle_event(#wx{id=?QUIT, event=#wxCommand{type=command_menu_selected}},
    S = #gs{game=G,frame=F}) ->
    wxWindow:close(F,[]),
    G ! quit,
    {stop, shutdown, S};

%% type=command_button_clicked,
handle_event(#wx{id=?NEW, event=#wxCommand{}},S ) ->
    start_new_game(),
    {noreply, S};
handle_event(#wx{id=ID, event=#wxCommand{}}, S) when ID > 125 ->
    New = dialog(ID, S),
    {noreply, New};
handle_event(Msg,S) ->
    io:format("~p: Unhandled event ~p~n",[?MODULE, Msg]),
    {noreply, S}.
%% @private
handle_call(get_other_client_info,_From,State) ->
    {reply,{ok,State#gs.other_client},State};
handle_call(What, _From, State) ->
    {stop, {call, What}, State}.
%% @private
handle_cast(Msg, State) ->
    io:format("~p:Got cast ~p~n",[?MODULE,Msg]),
    {noreply,State}.
%% @private
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
%% @private
terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
dialog(?SAVE, S=#gs{frame=Frame, board=Board}) ->
    FD = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE bor
        ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
        ?wxID_OK ->
            Path = wxFileDialog:getPath(FD),
            {ok,Fd} = file:open(Path, [write]),
            {ok,List} = game2048_single_board:get_board_score_data(Board),
            io:format(Fd, "~w.~n", [List]),
            file:close(Fd);
        _ ->
            ignore
    end,
    wxDialog:destroy(FD),
    S;
dialog(?OPEN, S=#gs{game=Server, frame=Frame, board=Board}) ->
    FD = wxFileDialog:new(Frame,[{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
    case wxDialog:showModal(FD) of
        ?wxID_OK ->
            Path = wxFileDialog:getPath(FD),
            case file:consult(Path) of
                {ok, [[Score|Game]]} when is_list(Game) ->
                    Vals = game2048_single_board:set_board_score_data(Board, [Score|Game]),
                    Server ! {loaded, {Vals,Score}};
                _T ->
                    io:format("open file error:~p~n",[_T]),
                    ignore
            end;
        _ ->
            ignore
    end,
    wxFileDialog:destroy(FD),
    S;
dialog(?ABOUT,  S=#gs{frame=Frame}) ->
    Str = "zhongwencool@gmail.com  \n",
    MD = wxMessageDialog:new(Frame,Str,
        [{style, ?wxOK bor ?wxICON_INFORMATION},{caption, "2048"}]),
    wxDialog:showModal(MD),
    wxDialog:destroy(MD),
    S;
dialog(?SIGNUP,  S=#gs{frame=Frame,board = Board}) ->
    Dialog = wxTextEntryDialog:new(Frame, "Enter you Name!",[{value, "Enter youName"}]),
    case wxTextEntryDialog:showModal(Dialog) of
        ?wxID_OK ->
            Name = wxTextEntryDialog:getValue(Dialog),
            case erlang:length(Name) > ?MAX_NAME_LENGHT of
                true -> msg_box("please set name length < 15 ");
                false -> game2048_client:sign_up(Board,Name)
                end,
            ok;
        ?wxID_CANCEL -> cancel;
        Any -> io:format("Any: ~p\n", [Any])
    end,
    wxTextEntryDialog:destroy(Dialog),
    S;
dialog(Other, S) ->
    io:format("other ~p~n",[Other]),
    S.

%% @private
%% @doc get two poses that not repeated.
get_no_overlap_pos(Pos1) ->
    PosTemp = #pos{xy ={random:uniform(4), random:uniform(4)}, val = 2},
    case PosTemp#pos.xy =:= Pos1#pos.xy of
        true -> get_no_overlap_pos(Pos1);
        false -> PosTemp
    end.