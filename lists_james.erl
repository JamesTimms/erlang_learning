-module(lists_james).
-export([sum/1, sum_tail/1, mul/1, mul_tail/1, max_list/1, double_tail/1,
         double/1, evens/1, evens_tail/1,
         sum_tail/2, mul_tail/2, max_list/2, double_tail/2, evens_tail/2,
         take/2]).

take(_N, []) -> [];
take(N, [X|Xs]) when N>0 -> [X | take(N-1, Xs)];
take(N, [X|_Xs]) when N==0 -> [X];
take(N, [_X|_Xs]) when N<0 -> {failed}.


% doubles occurances of items in array.
% [1,2,3] -> [1,1,2,2,3,3].
double([]) -> [];
double([X|Xs]) -> [X,X|double(Xs)].

% doubles occurances of items in array.
% [1,2,3] -> [1,1,2,2,3,3].
double_tail([X|Xs]) -> double_tail([X|Xs], []).
double_tail([], Acc) -> Acc;
double_tail([X|Xs], Acc) -> double_tail(Xs, [X, X|Acc]). % why is this done in reverse?

% finds subset of evens numbers.
% [1,2,3,4,5,6] -> [2,4,6]
evens([]) -> [];
evens([X|Xs]) ->
  case maths_j:even(X) of
    true -> [X|evens(Xs)];
    false -> evens(Xs)
  end.

% return subset of evens numbers.
% [1,2,3,4,5,6] -> [2,4,6]
evens_tail([X|Xs]) -> evens_tail([X|Xs], []).
evens_tail([], Acc) -> Acc;
evens_tail([X|Xs], Acc) ->
  evens_tail(Xs, case maths_j:even(X) of
    true -> [X|Acc];
    false -> Acc
  end).

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

max_list([X|Xs]) -> max_list([X|Xs], 0).
max_list([], CurrentMax) -> CurrentMax;
max_list([X|Xs], CurrentMax) ->
  max_list(Xs, max_j(X, CurrentMax)).

max_j(X, Y) ->
  case X > Y of
    true -> X;
    false -> Y
  end.
