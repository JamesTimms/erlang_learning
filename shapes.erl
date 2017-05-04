-module(shapes).
-export([perimeter/1, area/1, enclose/1, circles/1, areaOfAllShapes/1]).

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
  enclose({triangle, {B, H}}) * 0.5;

area({circle, {_X, _Y}, R}) ->
  math:pi() * R * R.

% Base times Height equals smallest enclosing rectangle.
enclose({triangle, {B, H}}) ->
  B * H.

% Would wokr for all shapes if area function were defined for all shapes.
areaOfAllShapes([]) -> [];
areaOfAllShapes([X|Xs]) -> [area(X)|areaOfAllShapes([Xs])].

circles([]) -> [];
circles([{circle, {_,_},_}=C | Xs]) ->
  [C | circles(Xs)];
circles([_X|Xs]) ->
  circles(Xs).
