-module(bank_tests).
-author("Aaron Lelevier").
-include_lib("eunit/include/eunit.hrl").

bank_test() ->
    Balance = 100,
    {ok, _Pid} = bank:start_link(Balance),
    ?assertEqual(100, bank:balance()),

    ok = bank:withdraw(25),
    ?assertEqual(75, bank:balance(), "can withdraw if less than balance"),

    ok = bank:withdraw(100),
    ?assertEqual(75, bank:balance(), "can't withdraw if more than balance"),

    ok = bank:deposit(20),
    ?assertEqual(95, bank:balance(), "can deposit").
