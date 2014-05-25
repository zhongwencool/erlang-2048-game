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
-export([select_font/2,select_dis_color/1,getGeomSz/2,init_board/0]).

select_font(_BS,[{_Sz,_H,F}]) ->
    F;
select_font(BS,[{Sz,_H,F}|_]) when BS >= Sz ->
    F;
select_font(BS,[_|Fs]) ->
    select_font(BS,Fs).

select_dis_color(1) ->
    {35,17,{0,0,123}};
select_dis_color(2) ->
    {25,30,{0,123,0}};
select_dis_color(3) ->
    {20,45,{123,0,0}};
select_dis_color(4) ->
    {15,50,{0,255,0}};
select_dis_color(_) ->
    {15,25,{0,255,0}}.

getGeomSz(W,H) ->
    Small = if W < H -> W; true -> H end,
    (Small - 2*?BRD) div 4.

init_board() ->
    [
        #pos{xy={1, 1}, val=0},#pos{xy={1, 2}, val=0},#pos{xy={1, 3}, val=0},#pos{xy={1, 4}, val=0},
        #pos{xy={2, 1}, val=0},#pos{xy={2, 2}, val=0},#pos{xy={2, 3}, val=0},#pos{xy={2, 4}, val=0},
        #pos{xy={3, 1}, val=0},#pos{xy={3, 2}, val=0},#pos{xy={3, 3}, val=0},#pos{xy={3, 4}, val=0},
        #pos{xy={4, 1}, val=0},#pos{xy={4, 2}, val=0},#pos{xy={4, 3}, val=0},#pos{xy={4, 4}, val=0}
    ].