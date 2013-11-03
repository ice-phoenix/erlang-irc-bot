-module(shuffle).

-export([shuffle/1]).

shuffle(List) -> shuffle(List, []).

shuffle([], Acc) -> Acc;

shuffle(List, Acc) ->
	Pivot = random:uniform(length(List)) - 1,
    {L, [H | T]} = lists:split(Pivot, List),
    shuffle(L ++ T, [H | Acc]).
