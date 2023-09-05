%%%-------------------------------------------------------------------
%% @doc p.191
%%
%% @end
%%%-------------------------------------------------------------------

-module(lib_misc).

-export([
    sleep/1,
    flush_buffer/0,
    priority_receive/0
]).

% Sleep T milliseconds
sleep(T) ->
    receive
    after
      T ->
        true
    end.

% Flush all messages from mailbox
flush_buffer() ->
    receive
      _Any ->
        flush_buffer()
    after
      0 ->
        true
    end.

% Receive one message at a time where priority messages are
% received first before all other messages in the mailbox
priority_receive() ->
  receive
    {alarm, X} ->
      {alarm, X}
  after
    0 ->
      receive
        Any ->
          Any
      after
        0 ->
          true
      end
  end.
