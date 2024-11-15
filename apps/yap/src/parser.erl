-module(parser).
-include_lib("eunit/include/eunit.hrl").
-export([construct/1]).
-record(tree, {left :: any(), op :: any(), right :: any()}).
% -record(ident, {val :: binary:unicode()}).
% -record(number, {val :: binary:unicode()}).
% -record(expression, {type :: atom(), val :: any(), line :: integer()}).

construct([L, {token, binop, Sym, _}, R]) when (Sym == multsym) or (Sym == slashsym) ->
    Left = construct(L),
    Right = construct(R),
    io:write(Left),
    io:write(Right),
    case {Sym, Left, Right} of
        {multsym, {factor, _, _}, {factor, _, _}} ->
            #tree{left = Left, op = mul, right = Right};
        {slashsym, {factor, _, _}, {factor, _, _}} ->
            #tree{left = Left, op = ddiv, right = Right};
        _ ->
            error("Illegal syntax")
    end;
construct({token, ident, Val, _}) ->
    {factor, ident, Val};
construct({token, number, Val, _}) ->
    {factor, number, Val};
construct([]) ->
    [].
