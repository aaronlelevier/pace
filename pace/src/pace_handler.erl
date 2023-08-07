%%%-------------------------------------------------------------------
%% @doc pace public API
%% @end
%%%-------------------------------------------------------------------

-module(pace_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("pace_handler request port: ~p.\n", ["8080"]),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Hello Pace Erlang!">>,
        Req0),
    {ok, Req, State}.
