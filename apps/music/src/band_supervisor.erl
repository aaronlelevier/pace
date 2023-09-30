%%%-------------------------------------------------------------------
%% @doc LYSE CH-17 band supervisor
%% @end
%%%-------------------------------------------------------------------

-module(band_supervisor).

-behaviour(supervisor).

-export([]).

-export([
    start_link/1,
    init/1
]).

% https://www.erlang.org/doc/man/supervisor#start_link-2
start_link(Type) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Type).

init(lenient) ->
    init({one_for_one, 3, 60});
init({RestartStrategy, MaxRestart, MaxTime}) ->
    {
        ok,
        {{RestartStrategy, MaxRestart, MaxTime},
         [{singer,
           {musicians, start_link, [singer, good]},
           permanent, 1000, worker, [musicians]},
          {base,
           {musicians, start_link, [base, good]},
           temporary, 1000, worker, [musicians]},
          {drum,
           {musicians, start_link, [drum, bad]},
           transient, 1000, worker, [musicians]},
          {keytar,
           {musicians, start_link, [keytar, good]},
           transient, 1000, worker, [musicians]}
        ]}
    }.
