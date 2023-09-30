%%%-------------------------------------------------------------------
%% @doc LYSE CH-17 musician gen_server
%% @end
%%%-------------------------------------------------------------------

-module(musicians).

-behaviour(gen_server).

-export([]).

-export([
    start_link/2,
    stop/1
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {name="", role, skill=good}).

-define(DELAY, 750).

%%% Public %%%

% https://www.erlang.org/doc/man/gen_server#type-server_name
start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
    gen_server:call(Role, stop).

%%% gen_server callbacks

% https://www.erlang.org/doc/man/rand#type-builtin_alg
init([Role, Skill]) ->
    % to know when parent shuts down
    process_flag(trap_exit, true),
    % pick a random name
    rand:seed(exsss),
    TimeToPlay = rand:uniform(3000),
    Name = pick_name(),
    lager:info("Musician ~s, playing the ~s entered the room~n", [Name, Role]),
    {ok, #state{name=Name, role=Role, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, S=#state{}) ->
    {stop, normal, ok, S};
handle_call(_Msg, _From, S) ->
    {noreply, S, ?DELAY}.

handle_cast(_Msg, S) ->
    {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=Name, role=Role, skill=good}) ->
    lager:info("Role:~s Name:~s produced sound!~n", [Role, Name]),
    {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=Name, role=Role, skill=bad}) ->
    case rand:uniform(5) of
        1 ->
            lager:info("Role:~s Name:~s played a false note. Uh oh!~n", [Role, Name]),
            {stop, bad_note, S};
        _ ->
            lager:info("Role:~s Name:~s produced sound!~n", [Role, Name]),
            {noreply, S, ?DELAY}
        end;
handle_info(_Msg, S) ->
    {noreply, S, ?DELAY}.

terminate(normal, S=#state{name=Name, role=Role}) ->
    lager:info("Role:~s Name:~s terminate: normal~n", [Role, Name]),
    {ok, S};
terminate(bad_note, S=#state{name=Name, role=Role}) ->
    lager:info("Role:~s Name:~s terminate: bad_note~n", [Role, Name]),
    {ok, S};
terminate(shutdown, _S) ->
    lager:info("manager is mad and fired the whole band.~n");
terminate(Reason, _S) ->
    lager:info("terminating for reason: ~s.~n", [Reason]).


%%% Private %%%

pick_name() ->
    lists:nth(rand:uniform(5), firstnames())
    ++ " " ++
    lists:nth(rand:uniform(5), lastnames()).

firstnames() -> [
    "Cornelius",
    "Gene",
    "Culla",
    "Cormac",
    "J-Bone"
].

lastnames() -> [
    "Hoghead",
    "Holme",
    "Suttree",
    "Red",
    "McCarthy"
].
