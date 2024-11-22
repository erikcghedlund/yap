-module(parser).
-ifdef(DEBUG).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-else.
-export([construct/2]).
-endif.

-type category() ::
    program
    | block
    | constdec
    | vardec
    | procdec
    | statement
    | condition
    | expression
    | term
    | factor.

-spec construct(Type :: category(), Tokens :: [tuple()]) -> tuple().
construct(statement, [{token, ident, Val, _}, {token, separator, becomesym, _} | Tokens]) ->
    {statement, {Val, become, construct(expression, Tokens)}};
construct(statement, [{token, keyword, Sym, _}, {token, ident, Val, _}]) when
    (Sym == callsym) or (Sym == insym)
->
    {statement, {symbol_to_op(Sym), Val}};
construct(statement, [{token, keyword, outsym, _} | Tokens]) ->
    {statement, {out, construct(expression, Tokens)}};
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

-spec find_symbol(Type :: atom(), Targets :: [atom()], Tokens :: [tuple()]) -> tuple().
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
        true -> find_symbol(type, Types, Tokens, [{token, Type, TSym, Line} | Passed])
    end.

-spec symbol_to_op(Symbol :: atom()) -> atom().
symbol_to_op(multsym) -> mul;
symbol_to_op(slashsym) -> ddiv;
symbol_to_op(plussym) -> plus;
symbol_to_op(minussym) -> sub;
symbol_to_op(callsym) -> call;
symbol_to_op(insym) -> in.
