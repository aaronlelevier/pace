%%%-------------------------------------------------------------------
%% @doc ping/pong API
%%
%%  curl http://localhost:8080/ping
%%
%% @end
%%%-------------------------------------------------------------------

-module(ping_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("pace_handler request port: ~p.\n", ["8080"]),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"pong">>,
        Req0),
    {ok, Req, State}.
