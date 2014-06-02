%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2014 2:42 PM
%%%-------------------------------------------------------------------

-module(game2048_lib).
-include("game2048.hrl").

%% API
-export([select_font/2,select_dis_color/1,getGeomSz/2,init_board/0,init_pen_fs/0]).

select_font(_BS,[{_Sz,_H,F}]) ->
    F;
select_font(BS,[{Sz,_H,F}|_]) when BS >= Sz ->
    F;
select_font(BS,[_|Fs]) ->
    select_font(BS,Fs).

select_dis_color(1) ->
    {31,7,{0,0,123}};
select_dis_color(2) ->
    {24,20,{0,123,0}};
select_dis_color(3) ->
    {16,35,{123,0,0}};
select_dis_color(4) ->
    {11,40,{0,255,123}};
select_dis_color(_) ->
    {11,15,{0,255,100}}.

getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 4.

init_board() ->
    [
        #pos{xy={1, 1}, val=128},#pos{xy={1, 2}, val=512},#pos{xy={1, 3}, val=2048},#pos{xy={1, 4}, val=4096},
        #pos{xy={2, 1}, val=64},#pos{xy={2, 2}, val=0},#pos{xy={2, 3}, val=0},#pos{xy={2, 4}, val=0},
        #pos{xy={3, 1}, val=32},#pos{xy={3, 2}, val=0},#pos{xy={3, 3}, val=0},#pos{xy={3, 4}, val=0},
        #pos{xy={4, 1}, val=8},#pos{xy={4, 2}, val=0},#pos{xy={4, 3}, val=0},#pos{xy={4, 4}, val=0}
    ].

init_pen_fs() ->
    {wxPen:new({0,0,0}, [{width, 3}]),
        [{1 ,wxFont:new(72 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])},
            {2 ,wxFont:new(57 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])},
            {3 ,wxFont:new(37 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])},
            {4 ,wxFont:new(25 , ?wxSWISS, ?wxNORMAL, ?wxNORMAL,[])}]}.