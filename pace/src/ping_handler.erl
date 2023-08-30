%%%-------------------------------------------------------------------
%% @doc ping/pong API
%%
%%  # HTTP
%%  curl http://localhost:8080/ping
%%
%%  Cowboy REST docs:
%%  https://ninenines.eu/docs/en/cowboy/2.6/manual/cowboy_rest/
%%
%%  Test invoke:
%%  $ curl --header "Content-Type: application/json" localhost:8080/ping
%%  $ curl --header "Content-Type: application/json" localhost:8080/ping --request POST --data '{"sta":78.613,"stack":614.5,"reach":475}'
%%
%% @end
%%%-------------------------------------------------------------------

-module(ping_handler).
-behavior(cowboy_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_post/2,
    content_types_provided/2,
    to_json/2
]).

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
    % https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_req.read_body/
    lager:info("Req: ~p~n", [Req]),
    lager:info("State: ~p~n", [State]),

    % decode body
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    lager:info("Body: ~p~n", [Body]),
    lager:info("Req2: ~p~n", [Req2]),
    M = jsone:decode(Body),
    lager:info("Map: ~p~n", [M]),

    % calculate effective top tube length
    ETT = pace:ett(
         maps:get(<<"sta">>, M),
         maps:get(<<"stack">>, M),
         maps:get(<<"reach">>, M)
    ),
    lager:info("ETT: ~p~n", [ETT]),

    % encode and set response
    Body2 = #{
        % https://www.erlang.org/doc/man/erlang#float_to_binary-2
        <<"ett">> => float_to_binary(ETT, [short])
    },
    Req3 = cowboy_req:set_resp_body(jsone:encode(Body2), Req),
    {true, Req3, State}.


%% Which content types we handle for GET/HEAD requests
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.


%% Return counters/counter as json
to_json(Req, State) ->
    Resp = #{key => val},
    {jsone:encode(Resp), Req, State}.
