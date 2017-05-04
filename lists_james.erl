-module(lists_james).
-export([sum/1, sum_tail/1, mul/1, mul_tail/1, max_j/1, sum_tail/2, mul_tail/2, max_j/2]).

sum([]) -> 0;
sum([X|Xs]) -> X + sum(Xs).

sum_tail([X|Xs]) -> sum_tail([X|Xs], 0).
sum_tail([], S) -> S;
sum_tail([X|Xs], S) -> sum_tail(Xs, X+S).

mul([]) -> 1;
mul([X|Xs]) -> X * mul(Xs).

mul_tail([X|Xs]) -> mul_tail([X|Xs], 1).
mul_tail([],T) -> T;
mul_tail([X|Xs],T) -> mul_tail(Xs, X * T).

max_j([X|Xs]) -> max_j([X|Xs], 0).
max_j([], CurrentMax) -> CurrentMax;
max_j([X|Xs], CurrentMax) ->
  max_j(Xs,
    case CurrentMax < X of
      true -> X;
      false -> CurrentMax
    end).
