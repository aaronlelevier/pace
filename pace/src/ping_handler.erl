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
%%  $ curl --header "Content-Type: application/json" localhost:8080/ping --request POST --data '{"username":"xyz","password":"xyz"}'
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
    lager:info("~p~n", [Body]),
    lager:info("~p~n", [jsone:decode(Body)]),

    {true, Req2, State}.

%% Which content types we handle for GET/HEAD requests
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.


%% Return counters/counter as json
to_json(Req, State) ->
    Resp = #{key => val},
    {jsone:encode(Resp), Req, State}.
