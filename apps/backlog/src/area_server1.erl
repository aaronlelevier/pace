%%%-------------------------------------------------------------------
%% @doc p.186
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
            io:format("From:~p; Rectagle area: ~p~n", [From, H * W]),
            loop();
        {From, {square, Side}} ->
            io:format("From:~p; Square area: ~p~n", [From, Side * Side]),
            loop();
        {From, Other} ->
            io:format("From:~p; Unsupported request: ~p~n", [From, Other]),
            loop()
    end.
