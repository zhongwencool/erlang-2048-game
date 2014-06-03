%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2014 2:42 PM
%%%-------------------------------------------------------------------
-module(game2048).


%% API
-export([start/0]).

-include("game2048.hrl").

-define(TC(Cmd), tc(fun() -> Cmd end, ?MODULE, ?LINE)).

%% ===================================================================
%% Public functions
%% ===================================================================
-spec start() ->
    {ok, pid()}.

%% @doc Starts a new 2048 game.
%% A <b>game2048</b> is a 2048 game application by pure Erlang
start() ->
    erlang:spawn_link(fun() -> init(quit) end),
    {ok,self()}.

%% @private start game gui client
init(Halt) ->
    ?TC(game2048_gui:new(self())),
    receive {gui_init_ok,GFX} -> ok end,
    game2048_gui:start_new_game(),
    game2048_client:start_link(),
    case loop(GFX) of
        Halt -> erlang:halt();
        Stop -> exit(Stop)
    end.

%% @private wait msg from gui
loop(GFX) ->
    receive
        quit ->
            quit;
        {'EXIT', _, Reason} ->
            io:format("The GUI crashed: ~p~n", [Reason]);
        Msg ->
            io:format("~p:receive:~p:~p~n",[?MODULE,?LINE,Msg]),
            loop(GFX)
    end.
%% @private for trace
tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
        {_,{'EXIT',Reason}} -> exit(Reason);
        {T,R} ->
            io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
            R
    end.