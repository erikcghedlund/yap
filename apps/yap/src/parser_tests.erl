-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

construct_term1_test() ->
    ?assertEqual(
        {tree, {factor, number, 1}, mul, {factor, number, 2}},
        parser:construct(lexer:parse("1 * 2"))
    ).
construct_term2_test() ->
    ?assertEqual(
        {tree, {factor, number, 9}, ddiv, {factor, number, 2}},
        parser:construct(lexer:parse("9 / 2"))
    ).
construct_term3_test() ->
    ?assertEqual(
        {tree, {factor, ident, "x"}, mul, {factor, number, 2}},
        parser:construct(lexer:parse("x * 2"))
    ).
construct_term4_test() ->
    ?assertEqual(
        {tree, {factor, number, 1}, ddiv, {factor, ident, "res"}},
        parser:construct(lexer:parse("1 / res"))
    ).
construct_term5_test() ->
    ?assertEqual(
        {tree, {factor, ident, "a"}, mul, {factor, ident, "b"}},
        parser:construct(lexer:parse("a * b"))
    ).
construct_term6_test() ->
    ?assertEqual(
        {tree, {factor, ident, "one"}, ddiv, {factor, ident, "res"}},
        parser:construct(lexer:parse("one / res"))
    ).
