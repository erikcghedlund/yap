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

-record(parse_error, {error :: string(), help :: string(), line :: integer()}).

-spec construct(Type :: category(), Tokens :: [tuple()]) -> tuple() | #parse_error{}.
construct(block, _) ->
    #parse_error{error = "Not implemented", help = "N/A", line = 0};
construct(vardec, [
    {token, keyword, intsym, _} | Tokens
]) ->
    {vardec, construct_vardec_help(Tokens)};
construct(procdec, [
    {token, keyword, procsym, _}, {token, ident, _, _}, {token, separator, semicolonsym, Line}
]) ->
    #parse_error{
        error = "Illegal procedure declaration",
        help = "A block must follow a declaration",
        line = Line
    };
% This current implementation doesn't support nested procedures
construct(procdec, [
    {token, keyword, procsym, _},
    {token, ident, Ident, _},
    {token, separator, semicolonsym, _}
    | Tokens
]) ->
    case find_symbol(symbol, [procsym], Tokens) of
        {Before, undefined, _} ->
            case lists:last(Before) of
                {token, separator, semicolonsym, _} ->
                    {procdec, [{Ident, construct(block, lists:droplast(Before))}]};
                {_, _, _, Line} ->
                    #parse_error{
                        error = "semicolon expected",
                        help = "procedure must end with semicolon",
                        line = Line
                    }
            end;
        {Before, procsym, After} ->
            case lists:last(Before) of
                {token, separator, semicolonsym, Line} ->
                    {_, Tail} = construct(procdec, [{token, keyword, procsym, Line} | After]),
                    {procdec, [
                        {Ident, construct(block, lists:droplast(Before))}
                        | Tail
                    ]};
                {_, _, _, Line} ->
                    #parse_error{
                        error = "semicolon expected",
                        help = "procedure must end with semicolon",
                        line = Line
                    }
            end
    end;
construct(statement, [{token, keyword, beginsym, Line} | Tokens]) ->
    case find_symbol(symbol, [endsym], Tokens) of
        {_, undefined, _} ->
            #parse_error{
                error = "end expected",
                help = "begin [statement-(s)] must be followed by an end",
                line = Line
            };
        {Inner, _, []} ->
            PartitionedTokens = split_statements(Inner),
            Statements = lists:map(fun(List) -> construct(statement, List) end, PartitionedTokens),
            {statement, {bbegin, Statements}};
        _ ->
            #parse_error{
                error = "tokens after end keyword",
                help = "This shouldn't happen and is likely a bug in the compiler...",
                line = Line
            }
    end;
% while statement
construct(statement, [{token, keyword, whilesym, Line} | Tokens]) ->
    case find_symbol(symbol, [dosym], Tokens) of
        {_, undefined, _} ->
            #parse_error{
                error = "do expected",
                help = "while [condition] must be followed by do [statement]",
                line = Line
            };
        {Condition, _, Statement} ->
            {statement, {while, construct(condition, Condition), construct(statement, Statement)}}
    end;
% if statement
construct(statement, [{token, keyword, ifsym, Line} | Tokens]) ->
    case find_symbol(symbol, [thensym], Tokens) of
        % If we can't find a then statement, error.
        {_, undefined, _} ->
            #parse_error{
                error = "then expected",
                help = "if [condition] must be followed by then [statement]",
                line = Line
            };
        {Condition, _, Expression} ->
            case find_symbol(symbol, [elsesym], Expression) of
                % If we can't find a else statement, construct a tree without an else node
                {_, undefined, _} ->
                    {statement,
                        {iif, construct(condition, Condition), construct(statement, Expression)}};
                % And if we do find the keyword, construct one.
                {IfStatement, _, ElseStatement} ->
                    {statement,
                        {iif, construct(condition, Condition), construct(statement, IfStatement),
                            construct(statement, ElseStatement)}}
            end
    end;
% become statement
construct(statement, [{token, ident, Val, _}, {token, separator, becomesym, _} | Tokens]) ->
    {statement, {Val, become, construct(expression, Tokens)}};
% call or read statement
construct(statement, [{token, keyword, Sym, _}, {token, ident, Val, _}]) when
    (Sym == callsym) or (Sym == insym)
->
    {statement, {symbol_to_op(Sym), Val}};
% write statement
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

-spec construct_vardec_help(Tokens :: [tuple()]) -> [string()].
construct_vardec_help([{token, ident, Ident, _}, {token, separator, semicolonsym, _}]) ->
    [Ident];
construct_vardec_help([
    {token, ident, Ident, _}, {token, separator, commasym, _} | Tokens
]) ->
    [Ident | construct_vardec_help(Tokens)].

-spec symbol_to_op(Symbol :: atom()) -> atom().
symbol_to_op(multsym) -> mul;
symbol_to_op(slashsym) -> ddiv;
symbol_to_op(plussym) -> plus;
symbol_to_op(minussym) -> sub;
symbol_to_op(callsym) -> call;
symbol_to_op(insym) -> in.

-spec split_statements(Tokens :: [tuple()]) -> [[tuple()]].
split_statements(Tokens) -> split_statements(Tokens, [[]]).
-spec split_statements(Tokens :: [tuple()], Accum :: [[tuple()]]) -> [[tuple()]].
split_statements([], Accum) ->
    lists:reverse(lists:map(fun(List) -> lists:reverse(List) end, Accum));
split_statements([{token, separator, semicolonsym, _} | Tokens], Accum) ->
    split_statements(Tokens, [[] | Accum]);
split_statements([Head | Tokens], [Haccum | Accum]) ->
    split_statements(Tokens, [[Head | Haccum] | Accum]).
