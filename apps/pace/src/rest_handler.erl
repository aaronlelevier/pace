%%%-------------------------------------------------------------------
%% @doc example REST API methods
%%
%% https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_rest/
%%
%% Example usage:
%% GET:    $ curl --header "Content-Type: application/json" localhost:8080/rest
%% HEAD:   $ curl --header "Content-Type: application/json" -I localhost:8080/rest
%% POST:   $ curl --header "Content-Type: application/json" localhost:8080/rest --request POST --data '{"hello":"world"}'
%% PUT:    $ curl --header "Content-Type: application/json" localhost:8080/rest --request PUT --data '{"hello":"world"}'
%% PATCH:  $ curl --header "Content-Type: application/json" localhost:8080/rest --request PATCH --data '{"hello":"world"}'
%% DELETE: $ curl --header "Content-Type: application/json" localhost:8080/rest --request DELETE --data '{"hello":"world"}'
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
    handle_request/2,
    to_json/2,
    delete_resource/2,
    delete_completed/2
]).

init(Req, []) ->
    {cowboy_rest, Req, []}.

%% Which HTTP methods are allowed
allowed_methods(Req, State) ->
    Allowed = [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>,
               <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>],
    {Allowed, Req, State}.

%%% GET %%%

%% Which content types we handle for GET/HEAD requests
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

%% Return counters/counter as json
to_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    {jsone:encode(Body), Req2, State}.

%%% POST/PUT/PATCH %%%

%% Which content types are accepted by POST/PUT/PATCH requests
content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json">>, []}, handle_request}],
        Req,
        State
     }.

%% Handle the POST request
handle_request(Req, State) ->
    % https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_req.read_body/
    lager:info("Req: ~p~n", [Req]),
    lager:info("State: ~p~n", [State]),

    % decode request body
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    lager:info("Body: ~p~n", [Body]),

    case maps:get(method, Req) of
        <<"POST">> ->
            handle_post(Body, Req2, State);
        <<"PUT">> ->
            handle_put(Body, Req2, State);
        <<"PATCH">> ->
            handle_patch(Body, Req2, State);
        Other ->
            lager:error("handle_request - Other: ~p~n", [Other]),
            {false, Req, State}
    end.

handle_post(Body, Req, State) ->
    lager:info("handle_post"),
    Req2 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
    {false, Req2, State}.

handle_put(Body, Req, State) ->
    lager:info("handle_put"),
    Req2 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
    {false, Req2, State}.

handle_patch(Body, Req, State) ->
    lager:info("handle_patch"),
    Req2 = cowboy_req:set_resp_body(jsone:encode(Body), Req),
    {false, Req2, State}.


%%% DELETE %%%

% Deletes the resources. Can raise an error if Result=false
% https://ninenines.eu/docs/en/cowboy/2.10/manual/cowboy_rest/
delete_resource(Req, State) ->
    % decode request body
    {ok, Body, _Req2} = cowboy_req:read_body(Req),
    lager:info("Body: ~p~n", [Body]),
    lager:info("delete_resource - Req: ~p State: ~p~n", [Req, State]),
    {_Result = true, Req, State}.

delete_completed(Req, State) ->
    lager:info("delete_completed - Req: ~p State: ~p~n", [Req, State]),
    {_Result = true, Req, State}.
