%%%-------------------------------------------------------------------
%% @doc p.207
%%
%% Example of starting one or many process and linking them
%%
%% @end
%%%-------------------------------------------------------------------

-module(lib_spawn_links).

-export([
    start_single/0,
    start_many/1,
    fun_echo_or_exit/0,
    start_monitor/0,
    stop_monitor/3
]).

% Starts 1 or more process by the application of Fun to []
%   note: [] is the empty arguments list
% https://www.erlang.org/doc/man/erlang#spawn_link-1
-spec start_many([fun()]) -> pid().
start_many(Fs) ->
  spawn(fun() ->
          [spawn_link(F) || F <- Fs],
          receive
          after
            infinity -> true
          end
        end).

% Starts a single process that is not linked
-spec start_single() -> pid().
start_single() ->
  spawn(fun_echo_or_exit()).

% Returns fun() to be used with start() functions
-spec fun_echo_or_exit() -> fun().
fun_echo_or_exit() ->
  fun() ->
    receive
      {echo, X} ->
        lager:info("echo: ~p~n", [X]);
      {exit, Reason} ->
        exit(Reason)
      end
    end.

% Starts a monitored process
% https://www.erlang.org/doc/man/erlang#monitor-2
-spec start_monitor() -> pid().
start_monitor() ->
  Pid = start_single(),
  Ref = monitor(process, Pid),
  {Pid, Ref}.

% Sends an exit msg to the monitor and receives a response
-spec stop_monitor(pid(), reference(), any()) -> pid().
stop_monitor(Pid, Ref, Reason) ->
  Pid ! {exit, Reason},
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      {matched, Reason}
  after
    % timing matters here, a 0 milliseconds time won't give the
    % monitor enough time to send the 'DOWN' message
    1000 -> true
  end.
