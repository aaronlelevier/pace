%%%-------------------------------------------------------------------
%% @doc p.187 - Hide `spawn` and `rpc`
%%
%% Pid = area_server3:start().
%% area_server3:area(Pid, {square, 3}).
%%
%% @end
%%%-------------------------------------------------------------------

-module(area_server3).

-export([start/0, area/2, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

% Sends a request to the area servier and waits for returns the response
area(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

% Loop server that calculates rectangle or square area and outputs to stdout
loop() ->
    receive
        {From, {rectangle, H, W}} ->
            lager:info("rectangle"),
            From ! {self(), H * W},
            loop();
        {From, {square, Side}} ->
            lager:info("square"),
            From ! {self(), Side * Side},
            loop();
        {From, Other} ->
            lager:info("other"),
            From ! {self(), Other},
            loop()
    end.
