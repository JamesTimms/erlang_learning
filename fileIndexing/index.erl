-module(index).
-export([get_gettysburg_address/0, analyse_text/0,
         get_file_contents/1, show_file_contents/1, normalize/1,
         removeShortWords/1, nub/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
get_gettysburg_address() -> get_file_contents("gettysburg-address.txt").

analyse_text() -> removeShortWords(normalize(get_gettysburg_address())).

% return list of words with analysis.
% [{ "foo" , [{3,5},{7,7},{11,13}] }, { "bar" , [{1,2},{4,6},{7,7}] }]
% means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the
% file.
% analyse([]) -> [];
% analyse([Line|Text]) -> searchLines([Line|Text]).

% searchLines([]) -> [];
% searchLines([Line|Text]) -> [searchWords(Line)|searchLines(Text)].

% searchWords([]) -> [];
% searchWords([Word|Line]) ->
%   [SearchWord = search()].

% aasdasdasdasdsa() ->
%   case isUnique(Word, ListOfUniqueWords) of
%   true-> [Word|searchWords(line)];
%   false-> searchWords(line)
% end.
% Remove all duplicate elements. Keeps the first occurances of an element.
nub([]) -> [];
nub([[]|Text]) -> nub(Text);
nub([[Word|_Line]|_Text] = All) ->
  [{Word}|nub(removeAll(Word, All))].

% Remove all of an element.
removeAll(_X, []) -> [];
removeAll(Word, [Line|Text]) ->
  [rmFromLine(Word, Line)|removeAll(Word, Text)].

rmFromLine(_Word, []) -> [];
rmFromLine(Word, [Word|Line]) -> rmFromLine(Word, Line);
rmFromLine(W, [Word|Line]) -> [Word|rmFromLine(W, Line)].

% remove capitols and punctuation from argument Text.
normalize([]) -> [];
normalize([Line|Text]) ->
  [lists_james:nocaps(lists_james:nopunct(Line)) | normalize(Text)].

% Remove short words
removeShortWords([]) -> [];
removeShortWords([Line|Text]) ->
  [reduceLines(string:tokens(Line, " "))|removeShortWords(Text)].

reduceLines([]) -> [];
reduceLines([Word|Line]) ->
  case isShortWord(Word) of
    true -> reduceLines(Line);
    false -> [Word | reduceLines(Line)]
  end.

isShortWord(Word) -> lists:member(Word, shortWords()).

shortWords() ->
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
