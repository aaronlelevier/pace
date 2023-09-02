%%%-------------------------------------------------------------------
%% @doc p.187 - Selective receive
%%
%% Pid = spawn(area_server2, loop, []).
%% area_server2:rpc(Pid, {square, 3}).
%%
%% @end
%%%-------------------------------------------------------------------

-module(area_server2).

-export([rpc/2, loop/0]).

% Sends a request to the area servier and waits for returns the response
rpc(Pid, Request) ->
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
