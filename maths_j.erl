-module(maths_j).
-export([even/1, mod/2]).

even(X) ->
  case mod(X, 2) of
    0 -> true;
    _ -> false
  end.

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _Y) -> 0.
