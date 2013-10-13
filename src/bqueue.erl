-module(bqueue).

-export([
    new/1,
    in/2,
    out/1,
    to_list/1
]).

-record(bq, {bound, size, q}).

new(Bound) when Bound > 0 ->
    #bq{bound = Bound, size = 0, q = queue:new()};

new(_Bound) ->
    {error, invalid_bound}.


in(Item, #bq{bound = Bound, size = Size, q = Q1} = BQ) when Size == Bound ->
    {_, Q2} = queue:out(Q1),
    Q3 = queue:in(Item, Q2),
    BQ#bq{q = Q3};

in(Item, #bq{bound = Bound, size = Size, q = Q1} = BQ) when Size < Bound ->
    Q2 = queue:in(Item, Q1),
    BQ#bq{size = Size + 1, q = Q2};

in(_Item, _BQ) ->
    {error, invalid_state}.


out(#bq{size = Size, q = _Q1} = BQ) when Size == 0 ->
    {empty, BQ};

out(#bq{size = Size, q = Q1} = BQ) when Size > 0 ->
    {Item, Q2} = queue:out(Q1),
    {Item, BQ#bq{size = Size - 1, q = Q2}};

out(_BQ) ->
    {error, invalid_state}.


to_list(#bq{q = Q1} = _BQ) ->
    queue:to_list(Q1).
