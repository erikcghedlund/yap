-module(parser).
-include_lib("eunit/include/eunit.hrl").
-export([construct/1, construct/2]).

construct(expression, [{token, Type, Val, Line}]) when (Type == number) or (Type == ident) ->
    {expression, construct([{token, Type, Val, Line}])};
construct(expression, Tokens) ->
    case find_symbol([plussym, minussym], Tokens) of
        {Before, undefined, []} ->
            {expression, construct(term, Before)};
        {Before, Symbol, After} ->
            {expression, construct(term, Before), symbol_to_op(Symbol),
                construct(expression, After)}
    end;
construct(term, [{token, Type, Val, Line}]) when (Type == number) or (Type == ident) ->
    {term, construct(factor, [{token, Type, Val, Line}])};
% TODO: This case might be redundant (or not), hmm...
construct(term, [L, {token, binop, Sym, _}, R]) ->
    Left = construct(factor, [L]),
    Right = construct(factor, [R]),
    case {Sym, Left, Right} of
        {multsym, {factor, _, _}, {factor, _, _}} ->
            {term, Left, mul, Right};
        {slashsym, {factor, _, _}, {factor, _, _}} ->
            {term, Left, ddiv, Right};
        _ ->
            error("Illegal syntax")
    end;
construct(term, [L, {token, binop, Sym, _} | R]) when (Sym == multsym) or (Sym == slashsym) ->
    Left = construct(factor, [L]),
    Right = construct(term, R),
    case {Sym, Left, Right} of
        {multsym, {factor, _, _}, {term, _, _, _}} ->
            {term, Left, mul, Right};
        {slashsym, {factor, _, _}, {term, _, _, _}} ->
            {term, Left, ddiv, Right};
        _ ->
            error("Illegal syntax")
    end;
construct(factor, [{token, ident, Val, _}]) ->
    {factor, ident, Val};
construct(factor, [{token, number, Val, _}]) ->
    {factor, number, Val}.
construct(Tokens) -> construct(term, Tokens).

find_symbol(Symbols, Tokens) -> find_symbol(Symbols, Tokens, []).
find_symbol(_, [], Passed) ->
    {lists:reverse(Passed), undefined, []};
find_symbol(Symbols, [{token, Type, TSym, Line} | Tokens], Passed) ->
    Is_member = lists:member(TSym, Symbols),
    if
        Is_member -> {lists:reverse(Passed), TSym, Tokens};
        true -> find_symbol(Symbols, Tokens, [{token, Type, TSym, Line} | Passed])
    end.

symbol_to_op(multsym) -> mul;
symbol_to_op(slashsym) -> ddiv;
symbol_to_op(plussym) -> plus;
symbol_to_op(minussym) -> sub.
