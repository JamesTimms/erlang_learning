-module(bits).
-export([bits/1]).

bits(N) ->
  BinList = binary_to_list(integer_to_binary(N, 2));
  lists:foldr(fun(BinList, 0) ->
                1
              end, [], BinList).
