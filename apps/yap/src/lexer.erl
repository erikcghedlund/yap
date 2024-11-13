-module(lexer).
-export([parse/1, main/1]).

-record(token, {type :: atom(), val :: any(), line :: integer()}).

-define(IS_DIGIT(C), ($0 =< C) and (C =< $9)).
-define(IS_CHAR(C), (($a =< C) and (C =< $z)) or (($A =< C) and (C =< $Z))).

parse(Str) -> parse(Str, 1).

-spec parse(Str::[integer()], Line::integer()) -> [token].
parse([$+|T], Line) -> [#token{type=binop, val=plussum, line=Line} | parse(T, Line)];
parse([$-|T], Line) -> [#token{type=binop, val=minussym, line=Line} | parse(T, Line)];
parse([$*|T], Line) -> [#token{type=binop, val=multsym, line=Line} | parse(T, Line)];
parse([$/|T], Line) -> [#token{type=binop, val=slashsym, line=Line} | parse(T, Line)];
parse([$<,$>|T], Line) -> [#token{type=relop, val=neqsym, line=Line} | parse(T, Line)];
parse([$<,$=|T], Line) -> [#token{type=relop, val=leqsym, line=Line} | parse(T, Line)];
parse([$>,$=|T], Line) -> [#token{type=relop, val=geqsym, line=Line} | parse(T, Line)];
parse([$:,$=|T], Line) -> [#token{type=separator, val=becomesym, line=Line} | parse(T, Line)];
parse([$<|T], Line) -> [#token{type=relop, val=lessym, line=Line} | parse(T, Line)];
parse([$>|T], Line) -> [#token{type=relop, val=gtrsym, line=Line} | parse(T, Line)];
parse([$=|T], Line) -> [#token{type=relop, val=equal, line=Line} | parse(T, Line)];
parse([$,|T], Line) -> [#token{type=separator, val=commasym, line=Line} | parse(T, Line)];
parse([$;|T], Line) -> [#token{type=separator, val=semicolonsym, line=Line} | parse(T, Line)];
parse([$.|T], Line) -> [#token{type=separator, val=periodsym, line=Line} | parse(T, Line)];
parse([$(|T], Line) -> [#token{type=separator, val=lparentsym, line=Line} | parse(T, Line)];
parse([$)|T], Line) -> [#token{type=separator, val=rparentsym, line=Line} | parse(T, Line)];
% Keywords
% call
parse([$ , $c, $a, $l, $l, $ |T], Line) -> [#token{type=keyword, val=callsym, line=Line} | parse(T, Line)];
% begin
parse([$ , $b, $e, $g, $i, $n, $ |T], Line) -> [#token{type=keyword, val=beginsym, line=Line} | parse(T, Line)];
% do
parse([$ , $d, $o, $ |T], Line) -> [#token{type=keyword, val=dosym, line=Line} | parse(T, Line)];
% while
parse([$ , $w, $h, $i, $l, $e, $ |T], Line) -> [#token{type=keyword, val=whilesym, line=Line} | parse(T, Line)];
% read
parse([$ , $r, $e, $a, $d, $ |T], Line) -> [#token{type=keyword, val=readsym, line=Line} | parse(T, Line)];
% write
parse([$ , $w, $r, $i, $t, $e, $ |T], Line) -> [#token{type=keyword, val=writesym, line=Line} | parse(T, Line)];
% if
parse([$ , $i, $f, $ |T], Line) -> [#token{type=keyword, val=ifsym, line=Line} | parse(T, Line)];
% end
parse([$ , $e, $n, $d, $ |T], Line) -> [#token{type=keyword, val=endsym, line=Line} | parse(T, Line)];
% procedue
parse([$ , $p, $r, $o, $c, $e, $d, $u, $r, $e, $ |T], Line) -> [#token{type=keyword, val=procsym, line=Line} | parse(T, Line)];
% out
parse([$ , $o, $u, $t, $ |T], Line) -> [#token{type=keyword, val=outsym, line=Line} | parse(T, Line)];
% in
parse([$ , $i, $n, $ |T], Line) -> [#token{type=keyword, val=insym, line=Line} | parse(T, Line)];
% else
parse([$ , $e, $l, $s, $e, $ |T], Line) -> [#token{type=keyword, val=elsesym, line=Line} | parse(T, Line)];
% int
parse([$ , $i, $n, $t, $ |T], Line) -> [#token{type=keyword, val=intsym, line=Line} | parse(T, Line)];
% const
parse([$ , $c, $o, $n, $s, $t, $ |T], Line) -> [#token{type=keyword, val=constsym, line=Line} | parse(T, Line)];
% odd
parse([$ , $o, $d, $d, $ |T], Line) -> [#token{type=keyword, val=oddsym, line=Line} | parse(T, Line)];

parse([$ |T], Line) -> parse(T, Line);
parse([$\t|T], Line) -> parse(T, Line);
parse([$\n|T], Line) -> parse(T, Line + 1);
parse([C|T], Line) when ?IS_DIGIT(C) -> {Number, Tail} = lists:splitwith(fun(X) -> ?IS_DIGIT(X) end, [C|T]), [#token{type=number, val=string:to_integer(Number), line=Line} | parse(Tail, Line)];
parse([C|T], Line) when ?IS_CHAR(C) -> {Ident, Tail} = lists:splitwith(fun(X) -> ?IS_DIGIT(X) or ?IS_CHAR(X) end, [C|T]), [#token{type=ident, val=Ident, line=Line} | parse(Tail, Line)];
parse([], _) -> [].

white_space_hack([$\n|T]) -> [$ , $\n, $ |white_space_hack(T)];
white_space_hack([$\t|T]) -> [$ , $\t, $ |white_space_hack(T)];
white_space_hack([H|T]) -> [H|white_space_hack(T)];
white_space_hack([]) -> [].

main(_Args) ->  
        io:write(parse(white_space_hack([32| lists:nth(1,  _Args)]))),
        halt().

