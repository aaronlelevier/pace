%%%-------------------------------------------------------------------
%% @doc pace
%%
%%  Module for calculating bike geometry
%%
%% @end
%%%-------------------------------------------------------------------
-module(pace).
-export([
    sta/3,
    ett/3
]).

%%%%% Public %%%%%

% Returns the effective top tube
% ex: pace:ett(pace:sta({78.7, 600.0}, {78.1, 700.0}, 614.5), 614.5, 475).
ett(STA, Stack, Reach) ->
    a(c(STA, Stack), STA) + Reach.

% Returns the effective seat tube angle (STA) using:
% Low = {STA, Height}
% Height = {STA, Height}
% Stack
% ex: pace:sta({78.7, 600.0}, {78.1, 700.0}, 614.5).
sta(Low, High, Stack) ->
    {SAL, SHL} = Low,
    {SAH, SHH} = High,
    SAL - ((Stack-SHL)/(SHH-SHL) * (abs(SAH - SAL))).

%%%%% Internal %%%%%

% Returns the radians using the degrees 'N'
radians(N) -> N * (math:pi()/180).

% Returns the effective seat tube length ~ hypotenuse
c(Beta, B) ->
    B / math:sin(radians(Beta)).

% Returns the effective seat tube distance, or the
% horizontal distance from bottom bracket to the
% effective seat tube angle at the stack height
a(C, Beta) ->
    C * math:cos(radians(Beta)).
