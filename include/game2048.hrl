%%%-------------------------------------------------------------------
%%% @author zhongwencool@gamil.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2014 2:45 PM
%%%-------------------------------------------------------------------

-ifndef(GAME2048_HRL_).
-define(GMAE2048_HRL_, 1).

-include_lib("wx/include/wx.hrl").


%% ===================================================================
%% Marcos
%% ===================================================================
-define(CENTER_NODE,'game2048_center@127.0.0.1').

-define(NEW,   121).
-define(ONLINE,122).
-define(CHAT,123).

-define(OPEN,  130).
-define(SAVE,  131).
%% compare button 1~8
-define(COMPARE1,132).
-define(COMPARE8,139).

-define(CHAT_BUTTON,141).

-define(QUIT,  ?wxID_EXIT).
-define(ABOUT, ?wxID_ABOUT).
-define(SIGNUP,?wxID_OK).

%%rectangle circular bead
-define(BRD,10).
-define(ARC_R, 10).


-define(UP,87).
-define(DOWN,83).
-define(LEFT,65).
-define(RIGHT,68).
-define(ENTER,13).

%%  sign up name max length
-define(MAX_NAME_LENGHT,15).

%% ===================================================================
%% Records
%% ===================================================================
-record(pos,{
    xy={1,1}:: {integer(),integer()},
    val=2:: integer()
}).

%%online game player.
-record(player,{
    node::atom(),
    pid::pid(),
    name="NotSignUp"::string()
}).

%% ===================================================================
%% Type
%% ===================================================================
-type player()::#player{}.

-endif.