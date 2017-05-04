-module(tail).
-export([tail/1, head/1, second/1]).

head([X|_Xs]) -> X.
tail([_X|Xs]) -> Xs.

second([_X, Y|_Zs]) -> Y;
second([_X]) -> erlang:error(array_too_small);
second(Xs) -> head(tail(Xs)).
