%%%-------------------------------------------------------------------
%%% @author zhongwencool@gamil.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2014 2:45 PM
%%%-------------------------------------------------------------------
-author("zhongwencool@gmail.com").


-include_lib("wx/include/wx.hrl").

-define(TC(Cmd), tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-define(NEW,   121).
-define(ONLINE,122).
-define(CHAT,123).

-define(OPEN,  130).
-define(SAVE,  131).
%% compare 1~8
-define(COMPARE1,132).
-define(COMPARE8,139).

-define(CHAT_BUTTON,141).

-define(QUIT,  ?wxID_EXIT).   %% Use OS specific version if available
-define(ABOUT, ?wxID_ABOUT).  %% Use OS specific
-define(SIGNUP,?wxID_OK).

%%长方形的边角半径
-define(BRD,10).
-define(ARC_R, 10).

-record(pos,{xy={1,1},val=2}).

-define(UP,87).
-define(DOWN,83).
-define(LEFT,65).
-define(RIGHT,68).
-define(ENTER,13).

%%online game
-record(player,{node,pid,name="NotSignUp"}).

%% for sign up
-define(MAX_NAME_LENGHT,15).