-module(pace_app_gs).
-behaviour(gen_server).

-export([start_link/0]).
-export([add/1, remove/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, pace_app_gs}, pace_app_gs, [], []).

add(N) ->
    gen_server:call(pace_app_gs, {add, N}).

remove(N) ->
    gen_server:cast(pace_app_gs, {remove, N}).

init(_Args) ->
    State = [],
    {ok, State}.

handle_call({add, Request}, From, State) ->
    io:format("call: ~p~n", [Request]),
    Reply = From,
    NewState = lists:append([Request], State),
    io:format("new-state: ~p~n", [NewState]),
    {reply, Reply, NewState}.

handle_cast({remove, Request}, State) ->
    io:format("cast: ~p~n", [Request]),
    NewState = lists:delete(Request, State),
    io:format("new-state: ~p~n", [NewState]),
    {noreply, NewState}.
