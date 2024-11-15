-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").

construct_factor1_test() ->
    ?assertEqual(
        {factor, ident, "one"},
        parser:construct(factor, lexer:parse("one"))
    ).
construct_factor2_test() ->
    ?assertEqual(
        {factor, number, 1},
        parser:construct(factor, lexer:parse("1"))
    ).

construct_term7_test() ->
    ?assertEqual(
        {term, {factor, ident, "one"}},
        parser:construct(term, lexer:parse("one"))
    ).
construct_term8_test() ->
    ?assertEqual(
        {term, {factor, number, 1}},
        parser:construct(term, lexer:parse("1"))
    ).
construct_term1_test() ->
    ?assertEqual(
        {term, {factor, number, 1}, mul, {factor, number, 2}},
        parser:construct(term, lexer:parse("1 * 2"))
    ).
construct_term2_test() ->
    ?assertEqual(
        {term, {factor, number, 9}, ddiv, {factor, number, 2}},
        parser:construct(term, lexer:parse("9 / 2"))
    ).
construct_term3_test() ->
    ?assertEqual(
        {term, {factor, ident, "x"}, mul, {factor, number, 2}},
        parser:construct(term, lexer:parse("x * 2"))
    ).
construct_term4_test() ->
    ?assertEqual(
        {term, {factor, number, 1}, ddiv, {factor, ident, "res"}},
        parser:construct(term, lexer:parse("1 / res"))
    ).
construct_term5_test() ->
    ?assertEqual(
        {term, {factor, ident, "a"}, mul, {factor, ident, "b"}},
        parser:construct(term, lexer:parse("a * b"))
    ).
construct_term6_test() ->
    ?assertEqual(
        {term, {factor, ident, "one"}, ddiv, {factor, ident, "res"}},
        parser:construct(term, lexer:parse("one / res"))
    ).
construct_term9_test() ->
    ?assertEqual(
        {term, {factor, ident, "a"}, mul,
            {term, {factor, ident, "b"}, mul,
                {term, {factor, ident, "c"}, mul, {factor, ident, "d"}}}},
        parser:construct(term, lexer:parse("a * b * c * d"))
    ).
construct_term10_test() ->
    ?assertEqual(
        {term, {factor, ident, "one"}, ddiv,
            {term, {factor, ident, "res"}, mul,
                {term, {factor, number, 3}, ddiv, {factor, ident, "two"}}}},
        parser:construct(term, lexer:parse("one / res * 3 / two"))
    ).
