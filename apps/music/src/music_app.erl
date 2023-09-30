%%%-------------------------------------------------------------------
%% @doc music public API
%% @end
%%%-------------------------------------------------------------------

-module(music_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    music_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
