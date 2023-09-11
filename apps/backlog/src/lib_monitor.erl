%%%-------------------------------------------------------------------
%% @doc p.207
%%
%% @end
%%%-------------------------------------------------------------------

-module(lib_monitor).

-export([
    test/1,
    start/0,
    rpc/2,
    loop/0,
    on_exit/2
]).

test(Fun) ->
  Pid = start(),
  lager:info("Pid: ~p~n", [Pid]),

  Monitor = on_exit(Pid, Fun),
  lager:info("Monitor: ~p~n", [Monitor]),

  Reply = rpc(Pid, Fun),
  lager:info("Reply: ~p~n", [Reply]),

  lager:info("Status: ~p~n", [process_info(Pid, status)]),

  exit(Pid, normal),

  ok.

start() ->
  spawn(?MODULE, loop, []).

rpc(Pid, Fun) ->
  Pid ! {self(), Fun},
  receive
    Answer ->
      Answer
  after 1000 ->
    true
  end.

loop() ->
  receive
    {From, Fun} ->
      From ! Fun(),
      loop()
  end.

on_exit(Pid, Fun) ->
  spawn(fun() ->
          Ref = monitor(process, Pid),
          receive
            {'DOWN', Ref, process, Pid, Why} ->
              Fun(Why)
          end
        end).
