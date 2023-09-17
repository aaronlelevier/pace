%%%-------------------------------------------------------------------
%% @doc pace public API
%%
%% https://ninenines.eu/docs/en/cowboy/2.10/guide/listeners/
%%
%% @end
%%%-------------------------------------------------------------------

-module(pace_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(HTTP_LISTENER, my_http_listener).

start(_StartType, _StartArgs) ->
    % cowboy tutorial puts the start up code here, but
    % docker-erlang-example puts it in the supervisor
    io:format("Launch server port: ~p.\n", ["8080"]),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ett", ett_handler, []},
            {"/", pace_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(?HTTP_LISTENER,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    pace_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(?HTTP_LISTENER).


%% internal functions
