%%%-------------------------------------------------------------------
%% @doc p.193
%%
%% @end
%%%-------------------------------------------------------------------

-module(stimer).

-export([
    start/2,
    cancel/1,
    timer/2
]).

% Start timer to execute Fun after Time milliseconds
start(Time, Fun) -> spawn(?MODULE, timer, [Time, Fun]).

% Cancel timed Fun
cancel(Pid) -> Pid ! cancel.

timer(Time, Fun) ->
    receive
        cancel ->
            void
    after
        Time ->
            Fun()
    end.

