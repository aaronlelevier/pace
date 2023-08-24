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
%%  Cowboy REST docs:
%%  https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_rest/
%%
%%  Test invoke:
%%  $ curl --header "Content-Type: application/json" localhost:8080/ping
%%  $ curl --header "Content-Type: application/json" localhost:8080/ping --request POST --data '{"username":"xyz","password":"xyz"}'
%%
%% @end
%%%-------------------------------------------------------------------

-module(ping_handler).
-behavior(cowboy_handler).

-export([
    % cowboy
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_post/2,
    content_types_provided/2,
    to_json/2,
    % bike geo
    radians/1,
    sta/3,
    effective_stl/2,
    est_dist/2,
    ett/3
]).

%%%%% Cowboy %%%%%

init(Req, []) ->
    {cowboy_rest, Req, []}.

%% Which HTTP methods are allowed
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%% Which content types are accepted by POST/PUT requests
content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json">>, []}, handle_post}],
        Req,
        State
     }.

%% Handle the POST/PUT request
handle_post(Req, State) ->
    % TODO: not handling: {more, Body, Req2}
    % https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_req.read_body/
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    % TODO: use lager for logging
    io:format("~p~n", [Body]),
    io:format("~p~n", [jsone:decode(Body)]),

    {true, Req2, State}.

%% Which content types we handle for GET/HEAD requests
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.


%% Return counters/counter as json
to_json(Req, State) ->
    Resp = #{key => val},
    {jsone:encode(Resp), Req, State}.


%%%%% Bike geo %%%%%

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
