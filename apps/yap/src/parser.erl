-module(parser).
-include_lib("eunit/include/eunit.hrl").
-export([construct/1, construct/2]).

construct(condition, [{token, keyword, oddsym, _} | Tokens]) ->
    {condition, {odd, construct(expression, Tokens)}};
construct(condition, Tokens) ->
    {Before, Symbol, After} = find_symbol(type, [relop], Tokens),
    {condition, {construct(expression, Before), Symbol, construct(expression, After)}};
construct(expression, Tokens) ->
    case find_symbol(symbol, [plussym, minussym], Tokens) of
        {Before, undefined, []} ->
            {expression, construct(term, Before)};
        {Before, Symbol, After} ->
            {expression, construct(term, Before), symbol_to_op(Symbol),
                construct(expression, After)}
    end;
construct(term, Tokens) ->
    case find_symbol(symbol, [multsym, slashsym], Tokens) of
        {Before, undefined, []} ->
            {term, construct(factor, Before)};
        {Before, Symbol, After} ->
            {term, construct(factor, Before), symbol_to_op(Symbol), construct(term, After)}
    end;
construct(factor, [{token, Type, Val, _}]) when (Type == ident) or (Type == number) ->
    {factor, Type, Val}.
construct(Tokens) -> construct(term, Tokens).

find_symbol(Field, Symbols, Tokens) -> find_symbol(Field, Symbols, Tokens, []).
find_symbol(_, _, [], Passed) ->
    {lists:reverse(Passed), undefined, []};
find_symbol(symbol, Symbols, [{token, Type, TSym, Line} | Tokens], Passed) ->
    Is_member = lists:member(TSym, Symbols),
    if
        Is_member -> {lists:reverse(Passed), TSym, Tokens};
        true -> find_symbol(symbol, Symbols, Tokens, [{token, Type, TSym, Line} | Passed])
    end;
find_symbol(type, Types, [{token, Type, TSym, Line} | Tokens], Passed) ->
    Is_member = lists:member(Type, Types),
    if
        Is_member -> {lists:reverse(Passed), TSym, Tokens};
        true -> find_symbol(symbol, Types, Tokens, [{token, Type, TSym, Line} | Passed])
    end.

symbol_to_op(multsym) -> mul;
symbol_to_op(slashsym) -> ddiv;
symbol_to_op(plussym) -> plus;
symbol_to_op(minussym) -> sub.
