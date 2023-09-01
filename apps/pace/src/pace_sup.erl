%%%-------------------------------------------------------------------
%% @doc pace top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pace_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% https://www.erlang.org/doc/design_principles/sup_princ#child-specification
%%
%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
        #{id => pace_gs,
          start => {pace_gs, start_link, []},
          modules => [pace_gs],
          restart => permanent,
          shutdown => brutal_kill,
          type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
