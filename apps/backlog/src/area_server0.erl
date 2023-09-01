%%%-------------------------------------------------------------------
%% @doc p.184
%% @end
%%%-------------------------------------------------------------------

-module(area_server0).

-export([loop/0]).

% Loop server that calculates rectangle or square area and
% outputs to stdout
loop() ->
    receive
        {rectangle, H, W} ->
            io:format("Rectagle area: ~p~n", [H * W]),
            loop();
        {square, Side} ->
            io:format("Square area: ~p~n", [Side * Side]),
            loop();
        Other ->
            io:format("Unsupported request: ~p~n", [Other]),
            loop()
    end.
