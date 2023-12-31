%%%-------------------------------------------------------------------
%% @doc p.186
%%
%% Pid = spawn(area_server1, loop, []).
%% area_server1:rpc(Pid, {square, 3}).
%%
%% @end
%%%-------------------------------------------------------------------

-module(area_server1).

-export([rpc/2, loop/0]).

% Sends a request to the area servier and waits for returns the response
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response ->
            Response
    end.

% Loop server that calculates rectangle or square area and outputs to stdout
loop() ->
    receive
        {From, {rectangle, H, W}} ->
            lager:info("rectangle"),
            From ! H * W,
            loop();
        {From, {square, Side}} ->
            lager:info("square"),
            From ! Side * Side,
            loop();
        {From, Other} ->
            lager:info("other"),
            From ! Other,
            loop()
    end.
