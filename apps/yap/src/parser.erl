-module(parser).
-include_lib("eunit/include/eunit.hrl").
-export([construct/1, construct/2]).

construct(term, [{token, Type, Val, Line}]) when (Type == number) or (Type == ident) ->
    {term, construct(factor, [{token, Type, Val, Line}])};
% TODO: This case might be redundant (or not), hmm...
construct(term, [L, {token, binop, Sym, _}, R]) when (Sym == multsym) or (Sym == slashsym) ->
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
