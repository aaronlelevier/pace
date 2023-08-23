%%%-------------------------------------------------------------------
%% @doc ping/pong API
%%
%%  # HTTP
%%  curl http://localhost:8080/ping
%%
%%  % validate effective top tube calculation
%%  1> ping_handler:ett(ping_handler:sta({78.7, 600.0}, {78.1, 700.0}, 614.5), 614.5, 475).
%%  598.7598468837922
%%
%%
%% @end
%%%-------------------------------------------------------------------

-module(ping_handler).
-behavior(cowboy_handler).

-export([
    init/2,
    radians/1,
    sta/3,
    effective_stl/2,
    est_dist/2,
    ett/3
]).

init(Req0, State) ->
    io:format("pace_handler request port: ~p.\n", ["8080"]),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"pong">>,
        Req0),
    {ok, Req, State}.


% Returns the radians using the degrees 'N'
radians(N) -> N * (math:pi()/180).


% Returns the seat tube angle (STA) using:
% Low = {STA, Height}
% Height = {STA, Height}
% Stack
% ex: sta({78.7, 600.0}, {78.1, 700.0}, 614.5).
sta(Low, High, Stack) ->
    {SAL, SHL} = Low,
    {SAH, SHH} = High,
    SAL - ((Stack-SHL)/(SHH-SHL) * (abs(SAH - SAL))).


% Returns the effective seat tube length ~ hypotenuse
effective_stl(STA, Stack) ->
    Stack / math:sin(radians(STA)).


% Returns the effective seat tube distance, or the
% horizontal distance from bottom bracket to the
% effective seat tube angle at the stack height
est_dist(STA, Stack) ->
    math:cos(radians(STA)) * effective_stl(STA, Stack).


% Returns the effective top tube
ett(STA, Stack, Reach) ->
    Reach + est_dist(STA, Stack).
