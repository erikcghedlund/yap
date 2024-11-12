-module(lexer).
-export([parse/1]).

-record(token, {type :: atom(), val :: any(), line :: integer()}).

parse(Str) -> parse(Str, 1).

-spec parse(Str::[integer()], Line::integer()) -> [token].
parse([$+|T], Line) -> [#token{type=token_binop, val=plus, line=Line} | parse(T, Line)];
parse([$-|T], Line) -> [#token{type=token_binop, val=minus, line=Line} | parse(T, Line)];
parse([$*|T], Line) -> [#token{type=token_binop, val=mult, line=Line} | parse(T, Line)];
parse([$/|T], Line) -> [#token{type=token_binop, val=divi, line=Line} | parse(T, Line)];
parse([$ |T], Line) -> parse(T, Line);
parse([$\t|T], Line) -> parse(T, Line);
parse([$\n|T], Line) -> parse(T, Line + 1);
parse([C|T], Line) -> [#token{type=token_digit, val=(C - $0), line=Line} | parse(T, Line)];
parse([], _) -> [].
