%%%-------------------------------------------------------------------
%% @doc example REST API methods
%%
%% https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_rest/
%%
%% Example usage
%% $ curl --header "Content-Type: application/json" localhost:8080/rest --request POST --data '{"hello":"world"}'
%%
%% @end
%%%-------------------------------------------------------------------

-module(rest_handler).
-behavior(cowboy_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    handle_post/2,
    to_json/2
]).

init(Req, []) ->
    {cowboy_rest, Req, []}.

%% Which HTTP methods are allowed
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%%% GET %%%

%% Which content types we handle for GET/HEAD requests
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

%% Return counters/counter as json
to_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    {jsone:encode(Body), Req2, State}.

%%% POST %%%

%% Which content types are accepted by POST/PUT requests
content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json">>, []}, handle_post}],
        Req,
        State
     }.

%% Handle the POST request
handle_post(Req, State) ->
    % https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_req.read_body/
    lager:info("Req: ~p~n", [Req]),
    lager:info("State: ~p~n", [State]),

    % decode POST request body
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    lager:info("Body: ~p~n", [Body]),
    lager:info("Req2: ~p~n", [Req2]),

    % set response body
    Req3 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
    {true, Req3, State}.
