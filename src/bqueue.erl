-module(bqueue).

-export([
    new/1,
    is_queue/1,
    is_empty/1,
    len/1,
    in/2,
    in_r/2,
    out/1,
    out_r/1,
    from_list/1,
    from_list/2,
    to_list/1,
    reverse/1,
    split/2,
    join/2,
    filter/2,
    member/2
]).

-record(bq, {bound, size, q}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:new/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Bound) when Bound > 0 ->
    #bq{bound = Bound, size = 0, q = queue:new()};

new(_Bound) ->
    {badarg, invalid_bound}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:is_queue/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_queue(#bq{} = _BQ) -> true;
is_queue(_BQ) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:is_empty/1
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
%% bqueue:len/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

len(#bq{size = Size} = _BQ) when Size >= 0 ->
    Size;
len(#bq{} = _BQ) ->
    {badarg, invalid_state};
len(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:in/2
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
%% bqueue:in_r/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_r(Item, #bq{bound = Bound, size = Size, q = Q1} = BQ) when Size == Bound ->
    {_, Q2} = queue:out_r(Q1),
    Q3 = queue:in_r(Item, Q2),
    BQ#bq{q = Q3};

in_r(Item, #bq{bound = Bound, size = Size, q = Q1} = BQ) when Size < Bound ->
    Q2 = queue:in_r(Item, Q1),
    BQ#bq{size = Size + 1, q = Q2};

in_r(_Item, #bq{} = _BQ) ->
    {badarg, invalid_state};

in_r(_Item, _BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:out/1
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
%% bqueue:out_r/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

out_r(#bq{size = Size, q = _Q1} = BQ) when Size == 0 ->
    {empty, BQ};

out_r(#bq{size = Size, q = Q1} = BQ) when Size > 0 ->
    {Item, Q2} = queue:out_r(Q1),
    {Item, BQ#bq{size = Size - 1, q = Q2}};

out_r(#bq{} = _BQ) ->
    {badarg, invalid_state};

out_r(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:from_list/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from_list(L1) ->
    Bound = length(L1),
    #bq{bound = Bound, size = Bound, q = queue:from_list(L1)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:from_list/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from_list(Bound, L1) when Bound > 0 ->
    Len = length(L1),
    {Size, L2} = case Len > Bound of
        true -> {Bound, lists:nthtail(Len - Bound, L1)};
        false -> {Len, L1}
    end,
    #bq{bound = Bound, size = Size, q = queue:from_list(L2)};

from_list(_Bound, _L1) ->
    {badarg, invalid_bound}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:to_list/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_list(#bq{q = Q1} = _BQ) ->
    queue:to_list(Q1);

to_list(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:reverse/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse(#bq{q = Q1} = BQ) ->
    BQ#bq{q = queue:reverse(Q1)};

reverse(_BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:split/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split(N, #bq{size = Size, q = Q1} = BQ) when N >= 0 ->
    {SQ1, SQ2} = queue:split(N, Q1),
    {Size1, Size2} = case Size > N of
        true -> {N, Size - N};
        false -> {Size, 0}
    end,
    {
        BQ#bq{size = Size1, q = SQ1},
        BQ#bq{size = Size2, q = SQ2}
    };

split(_N, #bq{} = _BQ) ->
    {badarg, invalid_split_size};

split(_N, _BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:join/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(
        #bq{bound = Bound1, size = Size1, q = Q1} = _BQ1,
        #bq{bound = Bound2, size = Size2, q = Q2} = _BQ2
) ->
    #bq{bound = Bound1 + Bound2, size = Size1 + Size2, q = queue:join(Q1, Q2)};

join(_BQ1, _BQ2) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:filter/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter(Pred, #bq{q = Q1} = BQ) ->
    Q2 = queue:filter(Pred, Q1),
    Size = queue:len(Q2),
    BQ#bq{size = Size, q = Q2};

filter(_Pred, _BQ) ->
    {badarg, invalid_queue}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bqueue:member/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member(Item, #bq{q = Q1} = _BQ) ->
    queue:member(Item, Q1);

member(_Item, _BQ) ->
    {badarg, invalid_queue}.
