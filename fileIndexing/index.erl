-module(index).
-export([get_gettysburg_address/0, analyse_text/0,
         get_file_contents/1, show_file_contents/1, normalize/1,
         remove_short_words/1, nub/1,
         nub/2]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
get_gettysburg_address() -> get_file_contents("gettysburg-address.txt").

analyse_text() -> remove_short_words(normalize(get_gettysburg_address())).

% Remove all duplicate elements. Keeps the first occurances of an element.
nub([[Word|_Line]|_Text]) -> nub([[Word|_Line]|_Text], 0).

nub([], Count) -> [];
nub([[]|Text], Count) -> nub(Text, Count + 1);
nub([[Word|Line]|Text] = All, Count) ->
  [{Word, [{Count}]}|nub([Line|Text], Count)].

% Remove all of an element.
remove_all(_X, []) -> [];
remove_all(Word, [Line|Text]) ->
  [rm_from_line(Word, Line)|remove_all(Word, Text)].

rm_from_line(_Word, []) -> [];
rm_from_line(Word, [Word|Line]) -> rm_from_line(Word, Line);
rm_from_line(W, [Word|Line]) -> [Word|rm_from_line(W, Line)].

% remove capitols and punctuation from argument Text.
normalize([]) -> [];
normalize([Line|Text]) ->
  [lists_james:nocaps(lists_james:nopunct(Line)) | normalize(Text)].

% Remove short words
remove_short_words([]) -> [];
remove_short_words([Line|Text]) ->
  [reduce_lines(string:tokens(Line, " "))|remove_short_words(Text)].

reduce_lines([]) -> [];
reduce_lines([Word|Line]) ->
  case is_short_word(Word) of
    true -> reduce_lines(Line);
    false -> [Word | reduce_lines(Line)]
  end.

is_short_word(Word) -> lists:member(Word, short_words()).

short_words() ->
["is", "and", "i", "and", "our", "of", "for",
 "on", "a", "we", "that", "it", "to", "be"].

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.
get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof -> file:close(File),
          Partial;
    Line -> {Strip,_} = lists:split(length(Line)-1,Line),
          get_all_lines(File,[Strip|Partial])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.
