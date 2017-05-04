-module(shapes).
-export([perimeter/1, area/1, enclose/1]).

% exampleUseOfPerimeter() ->
%   perimeter({circle, 5}).

% Circle Perimeter: P = 2piR
perimeter({circle, R}) ->
  2 * math:pi() * R;
% Triangle Perimeter: P = a + b + c
perimeter({triangle, {A, B, C}}) ->
  A + B + C;
% Rect Perimeter: P = 2(l+w)
perimeter({rectangle, {L, W}}) ->
  2 * (L + W);
% Regular Polygon: P = Sum of all sides.
% AllSides is expected to be a list.
perimeter({regular_polygon, AllSides}) ->
  lists:sum(AllSides);
perimeter(_) ->
  {failed}.


% Base times Height times 0.5.
area({triangle, {B, H}}) ->
  enclose({triangle, {B, H}}) * 0.5.

% Base times Height equals smallest enclosing rectangle.
enclose({triangle, {B, H}}) ->
  B * H.
