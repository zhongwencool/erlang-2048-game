%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%    manager chat board
%%% @end
%%% Created : 18. May 2014 3:14 PM
%%%-------------------------------------------------------------------
-module(game2048_chat_board).
-author("zhongwencool@gmail.com").

%% API
-export([new/1,send_chat_msg/1,print_chat_msg/1]).

%% Callbacks
-export([init/1,handle_event/2, handle_info/2, handle_call/3, handle_cast/2,code_change/3, terminate/2]).

-include("game2048.hrl").

-record(state, {win, parent,input, output}).

-behaviour(wx_object).

%% @doc init the chat borad
new(ParentObj) ->
    wx_object:start_link(?MODULE, [ParentObj, self()], []).

-spec send_chat_msg(Name::list()|term()) ->
    ok.
%% @doc Player:Name send chat msg to center
send_chat_msg(Name) ->
    erlang:send(?MODULE,{send_chat_msg,Name}).

%% @doc print msg on chat board
-spec print_chat_msg(Msg::list()|term()) ->
    ok.
print_chat_msg(Msg) ->
    erlang:send(?MODULE,{print_chat_msg,Msg}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private init chat board
init([ParentObj, ParentPid]) ->
    random:seed(erlang:now()),
    erlang:register(?MODULE,self()),

    Panel = wxPanel:new(ParentObj, [{style,?wxFULL_REPAINT_ON_RESIZE}]),
    wxWindow:connect(Panel, key_up, [{skip, true}]),
    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    OutSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Chat message:"}]),
    InputSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Enter your words:"}]),

    OutTextCtrl = wxTextCtrl:new(Panel, 2, [{value, "Have a try! \n You are great! \n "}, {style, ?wxTE_DONTWRAP bor ?wxTE_MULTILINE},{size,{100,90}}]),
    InputTextCtrl  = wxTextCtrl:new(Panel, 1, [{value, "Welcome"}, {style, ?wxDEFAULT}]),

    %% Add to sizers
    wxSizer:add(OutSz, OutTextCtrl,  [{flag, ?wxEXPAND}]),
    wxSizer:add(InputSz, InputTextCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),

    wxSizer:add(MainSizer, OutSz, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:addSpacer(MainSizer, 1),
    wxSizer:add(MainSizer, InputSz,  [{flag, ?wxEXPAND}]),

    wxPanel:setSizer(Panel, MainSizer),

    {Panel, #state{win=Panel,parent=ParentPid,input= InputTextCtrl, output = OutTextCtrl}}.

%% @private
handle_event(#wx{event=#wxMouse{type=enter_window}}, State = #state{win=Win}) ->
    wxWindow:setFocus(Win), %% Get keyboard focus
    {noreply,State};

handle_event(_Ev, State) ->
    io:format("~p::::unknown event:::~p",[?MODULE,_Ev]),
    {noreply,State}.

%%%%%%%%%%%%%%%%%%%
%% @private
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
handle_info({send_chat_msg,Name},State = #state{input = Input,output = Output}) ->
    try
        do_send_chat_msg(Name,Input,Output)
    catch _E1:_Reason -> io:format("error:~p:::~p~n",[_E1,_Reason])
    end,
    {noreply,State};
handle_info({print_chat_msg,Msg},State = #state{output = Output}) ->
    clear_output_msg_by_num(Output),
    wxTextCtrl:appendText(Output,Msg),
    {noreply,State};
handle_info(Msg, State) ->
    {stop, {info, Msg}, State}.

%% @private
terminate(_Reason, _State) ->
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(MAX_DISPLY_LINE_NUM,35).

%% @private
do_send_chat_msg(Name,Input,Output)->
    Text = wxTextCtrl:getLineText(Input,0),
    clear_output_msg_by_num(Output),
    Msg = Name ++":  "++Text++"\n ",
    wxTextCtrl:appendText(Output,Msg),
    wxTextCtrl:clear(Input),
    wxWindow:setFocus(Input),
    game2048_client:send_chat_msg_to_center(Msg),
    ok.

%% @private
clear_output_msg_by_num(Output) ->
    case wxTextCtrl:getNumberOfLines(Output) > ?MAX_DISPLY_LINE_NUM of
        true -> wxTextCtrl:clear(Output);
        false -> ignore
    end.
