-module(lexer).
-include_lib("eunit/include/eunit.hrl").
-export([parse/1, main/1]).

-record(token, {type :: atom(), val :: any(), line :: integer()}).

-define(IS_DIGIT(C), ($0 =< C) and (C =< $9)).
-define(IS_CHAR(C), (($a =< C) and (C =< $z)) or (($A =< C) and (C =< $Z))).

parse(Str) -> parse(Str, 1).

-spec parse(Str :: [integer()], Line :: integer()) -> [token].
parse([$+ | T], Line) ->
    [#token{type = binop, val = plussym, line = Line} | parse(T, Line)];
parse([$- | T], Line) ->
    [#token{type = binop, val = minussym, line = Line} | parse(T, Line)];
parse([$* | T], Line) ->
    [#token{type = binop, val = multsym, line = Line} | parse(T, Line)];
parse([$/ | T], Line) ->
    [#token{type = binop, val = slashsym, line = Line} | parse(T, Line)];
parse([$<, $> | T], Line) ->
    [#token{type = relop, val = neqsym, line = Line} | parse(T, Line)];
parse([$<, $= | T], Line) ->
    [#token{type = relop, val = leqsym, line = Line} | parse(T, Line)];
parse([$>, $= | T], Line) ->
    [#token{type = relop, val = geqsym, line = Line} | parse(T, Line)];
parse([$:, $= | T], Line) ->
    [#token{type = separator, val = becomesym, line = Line} | parse(T, Line)];
parse([$< | T], Line) ->
    [#token{type = relop, val = lessym, line = Line} | parse(T, Line)];
parse([$> | T], Line) ->
    [#token{type = relop, val = gtrsym, line = Line} | parse(T, Line)];
parse([$= | T], Line) ->
    [#token{type = relop, val = equalsym, line = Line} | parse(T, Line)];
parse([$, | T], Line) ->
    [#token{type = separator, val = commasym, line = Line} | parse(T, Line)];
parse([$; | T], Line) ->
    [#token{type = separator, val = semicolonsym, line = Line} | parse(T, Line)];
parse([$. | T], Line) ->
    [#token{type = separator, val = periodsym, line = Line} | parse(T, Line)];
parse([$( | T], Line) ->
    [#token{type = separator, val = lparentsym, line = Line} | parse(T, Line)];
parse([$) | T], Line) ->
    [#token{type = separator, val = rparentsym, line = Line} | parse(T, Line)];
parse([$\s | T], Line) ->
    parse(T, Line);
parse([$\t | T], Line) ->
    parse(T, Line);
parse([$\n | T], Line) ->
    parse(T, Line + 1);
parse([C | T], Line) when ?IS_DIGIT(C) ->
    {Number, Tail} = lists:splitwith(fun(X) -> ?IS_DIGIT(X) end, [C | T]),
    {Value, _} = string:to_integer(Number),
    [#token{type = number, val = Value, line = Line} | parse(Tail, Line)];
parse([C | T], Line) when ?IS_CHAR(C) ->
    {Ident, Tail} = lists:splitwith(fun(X) -> ?IS_DIGIT(X) or ?IS_CHAR(X) end, [C | T]),
    case Ident of
        "call" -> [#token{type = keyword, val = callsym, line = Line} | parse(Tail, Line)];
        "begin" -> [#token{type = keyword, val = beginsym, line = Line} | parse(Tail, Line)];
        "read" -> [#token{type = keyword, val = readsym, line = Line} | parse(Tail, Line)];
        "do" -> [#token{type = keyword, val = dosym, line = Line} | parse(Tail, Line)];
        "while" -> [#token{type = keyword, val = whilesym, line = Line} | parse(Tail, Line)];
        "write" -> [#token{type = keyword, val = writesym, line = Line} | parse(Tail, Line)];
        "if" -> [#token{type = keyword, val = ifsym, line = Line} | parse(Tail, Line)];
        "else" -> [#token{type = keyword, val = elsesym, line = Line} | parse(Tail, Line)];
        "end" -> [#token{type = keyword, val = endsym, line = Line} | parse(Tail, Line)];
        "odd" -> [#token{type = keyword, val = oddsym, line = Line} | parse(Tail, Line)];
        "then" -> [#token{type = keyword, val = thensym, line = Line} | parse(Tail, Line)];
        "const" -> [#token{type = keyword, val = constsym, line = Line} | parse(Tail, Line)];
        "int" -> [#token{type = keyword, val = intsym, line = Line} | parse(Tail, Line)];
        "procedure" -> [#token{type = keyword, val = procsym, line = Line} | parse(Tail, Line)];
        "out" -> [#token{type = keyword, val = outsym, line = Line} | parse(Tail, Line)];
        "in" -> [#token{type = keyword, val = insym, line = Line} | parse(Tail, Line)];
        _ -> [#token{type = ident, val = Ident, line = Line} | parse(Tail, Line)]
    end;
parse([], _) ->
    [].

parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} -> parse(binary_to_list(Content));
        {error, Reason} -> {error, Reason}
    end.

main(_Args) ->
    Ans = lists:map(fun(File) -> parse_file(File) end, _Args),
    lists:foreach(fun(X) -> io:write(X) end, Ans),
    halt().
