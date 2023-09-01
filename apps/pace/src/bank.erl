-module(bank).
-behaviour(gen_server).

-export([start_link/1]).
-export([deposit/1, withdraw/1, balance/0]).
-export([init/1, handle_call/3, handle_cast/2]).


start_link(Balance) ->
    gen_server:start_link({local, bank}, bank, Balance, []).

% call

balance() ->
    gen_server:call(bank, balance).

% cast

deposit(N) ->
    gen_server:cast(bank, {deposit, N}).

withdraw(N) ->
    gen_server:cast(bank, {withdraw, N}).

% init 'bank' with balance 'Bal'
init(Balance) ->
    {ok, Balance}.

% reply with bank account Balance
handle_call(balance, _From, Balance) ->
    {reply, Balance, Balance}.

handle_cast({withdraw, N}, Balance) ->
    if
        N =< Balance ->
            NewBalance = Balance - N;
        true ->
            NewBalance = Balance
    end,
    {noreply, NewBalance};
handle_cast({deposit, N}, Balance) ->
    {noreply, Balance + N}.
