%%%-------------------------------------------------------------------
%% @doc pace_app public API
%% @end
%%%-------------------------------------------------------------------

-module(pace_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pace_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
