-module(bqueue).

-export([
    new/1,
    is_queue/1,
    is_empty/1,
    len/1,
    in/2,
    out/1,
    to_list/1
]).

-record(bq, {bound, size, q}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Bound) when Bound > 0 ->
    #bq{bound = Bound, size = 0, q = queue:new()};

new(_Bound) ->
    {badarg, invalid_bound}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:is_queue
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_queue(#bq{} = _BQ) -> true;
is_queue(_BQ) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:is_empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_empty(#bq{size = Size} = _BQ) when Size == 0 ->
    true;
is_empty(#bq{size = Size} = _BQ) when Size > 0 ->
    false;
is_empty(#bq{} = _BQ) ->
    {badarg, invalid_state};
is_empty(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:len
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

len(#bq{size = Size} = _BQ) when Size >= 0 ->
    Size;
len(#bq{} = _BQ) ->
    {badarg, invalid_state};
len(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:in
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in(Item, #bq{bound = Bound, size = Size, q = Q1} = BQ) when Size == Bound ->
    {_, Q2} = queue:out(Q1),
    Q3 = queue:in(Item, Q2),
    BQ#bq{q = Q3};

in(Item, #bq{bound = Bound, size = Size, q = Q1} = BQ) when Size < Bound ->
    Q2 = queue:in(Item, Q1),
    BQ#bq{size = Size + 1, q = Q2};

in(_Item, #bq{} = _BQ) ->
    {badarg, invalid_state};

in(_Item, _BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:out
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

out(#bq{size = Size, q = _Q1} = BQ) when Size == 0 ->
    {empty, BQ};

out(#bq{size = Size, q = Q1} = BQ) when Size > 0 ->
    {Item, Q2} = queue:out(Q1),
    {Item, BQ#bq{size = Size - 1, q = Q2}};

out(#bq{} = _BQ) ->
    {badarg, invalid_state};

out(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:to_list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_list(#bq{q = Q1} = _BQ) ->
    queue:to_list(Q1);

to_list(_BQ) ->
    {badarg, invalid_queue}.
