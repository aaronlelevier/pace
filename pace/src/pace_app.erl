%%%-------------------------------------------------------------------
%% @doc pace public API
%% @end
%%%-------------------------------------------------------------------

-module(pace_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % cowboy tutorial puts the start up code here, but
    % docker-erlang-example puts it in the supervisor
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),

    io:format("Launch server port: ~p.\n", ["8080"]),

    Dispatch = cowboy_router:compile([
        {'_', [{"/", pace_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    pace_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
