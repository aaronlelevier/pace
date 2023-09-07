%%%-------------------------------------------------------------------
%% @doc p.202
%%
%% Scratch code for linking process and exiting
%%
%% @end
%%%-------------------------------------------------------------------

-module(exiter).

-export([
    start/0,
    loop/0,
    % exits
    exit_with_why/1,
    exit_with_fun/2,
    exit_with_trap/1
]).

% start and link child process
start() ->
    Pid = spawn(?MODULE, loop, []),
    true = link(Pid),
    Pid.

% main loop
loop() ->
    receive
        {exit, Why} ->
            lager:info("{exit ~p}~n", [Why]),
            exit(Why);
        Any ->
            lager:info("Any: ~p~n", [Any]),
            loop()
    end.

% Exit with a reason and log the before and after Pid info
% to see if the Reason cause the Pid to be killed, or the
% shell Pid self() to restart.
% exit with a reason e.g.
% - normal: doesn't cause the linked process to exit
% - AnAtom:
%   if: process is a system process, then it receives the
%       message: {'EXIT', Pid, Reason}
%   else: parent process exits with: "exception exit: Reason"
exit_with_why(Reason) ->
    Pid = start(),
    lager:info("Pid: ~p~n", [Pid]),
    lager:info("process_info(Pid): ~p~n", [process_info(Pid)]),
    lager:info("self(): ~p~n", [self()]),

    Pid ! {exit, Reason},
    lager:info("self(): ~p~n", [self()]),
    lager:info("process_info(Pid): ~p~n", [process_info(Pid)]),
    ok.

% Example of how to pass a callback from a module as an
% anonymous function:
% Pid = exiter:exit_with_fun(normal, fun exiter:start/0).
% Ref:
% https://www.erlang.org/doc/programming_examples/funs
exit_with_fun(Reason, Fun) ->
    Pid = Fun(),
    Pid ! {exit, Reason},
    Pid.

% Become a system process by trapping exits, cause a child process
% to exit, then log the reason for exiting
exit_with_trap(Reason) ->
    process_flag(trap_exit, true),
    ok = exit_with_why(Reason),
    lib_misc:flush_buffer().
