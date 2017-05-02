-module(recursion).
-export([fact/1, fact_better/1, fib/1, pieces/1]).

% Factorial, niave.
fact(0) -> 1;
fact(N) when N > 0 -> fact(N-1)*N;
fact(_) -> false.

% Factorial tail recursion.
fact_better(N) -> fact_better(N, 1).
fact_better(0, P) -> P;
fact_better(N, P) -> fact_better(N-1, P*N).

% Fibonacci sequence, niave.
fib(N) when N > 1 -> fib(N-1) + fib(N-2);
fib(1) -> 1;
fib(N) when N =< 0 -> 0;
fib(_) -> false.

% Fibonacci sequence, tail recursion.
fib_tail(0, P, _C) -> P;
fib_tail(N, P, C) -> fib_tail(N-1, C, P+C).
fib_tail(N) -> fib_tail(N, 0 ,1).

% Fibonacci sequence, Pattern Matching.
fib_pattern(0) -> {0, 1};
fib_pattern(N) ->
  {P, C} = fib_pattern(N-1),
  {C, P+C}.
fibP(N) ->
  {P, _} = fib_pattern(N),
  P.

% Maximum sections N cuts will make on a piece of paper.
pieces(0) -> 1;
pieces(N) -> N + pieces(N-1).
