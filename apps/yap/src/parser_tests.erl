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
        {term, {factor, number, 1}, mul, {term, {factor, number, 2}}},
        parser:construct(term, lexer:parse("1 * 2"))
    ).
construct_term2_test() ->
    ?assertEqual(
        {term, {factor, number, 9}, ddiv, {term, {factor, number, 2}}},
        parser:construct(term, lexer:parse("9 / 2"))
    ).
construct_term3_test() ->
    ?assertEqual(
        {term, {factor, ident, "x"}, mul, {term, {factor, number, 2}}},
        parser:construct(term, lexer:parse("x * 2"))
    ).
construct_term4_test() ->
    ?assertEqual(
        {term, {factor, number, 1}, ddiv, {term, {factor, ident, "res"}}},
        parser:construct(term, lexer:parse("1 / res"))
    ).
construct_term5_test() ->
    ?assertEqual(
        {term, {factor, ident, "a"}, mul, {term, {factor, ident, "b"}}},
        parser:construct(term, lexer:parse("a * b"))
    ).
construct_term6_test() ->
    ?assertEqual(
        {term, {factor, ident, "one"}, ddiv, {term, {factor, ident, "res"}}},
        parser:construct(term, lexer:parse("one / res"))
    ).
construct_term9_test() ->
    ?assertEqual(
        {term, {factor, ident, "a"}, mul,
            {term, {factor, ident, "b"}, mul,
                {term, {factor, ident, "c"}, mul, {term, {factor, ident, "d"}}}}},
        parser:construct(term, lexer:parse("a * b * c * d"))
    ).
construct_term10_test() ->
    ?assertEqual(
        {term, {factor, ident, "one"}, ddiv,
            {term, {factor, ident, "res"}, mul,
                {term, {factor, number, 3}, ddiv, {term, {factor, ident, "two"}}}}},
        parser:construct(term, lexer:parse("one / res * 3 / two"))
    ).

construct_expression1_test() ->
    ?assertEqual(
        {expression, {term, {factor, ident, "one"}}},
        parser:construct(expression, lexer:parse("one"))
    ).
construct_expression2_test() ->
    ?assertEqual(
        {expression, {term, {factor, number, 1}}},
        parser:construct(expression, lexer:parse("1"))
    ).
construct_expression3_test() ->
    ?assertEqual(
        {expression, {term, {factor, ident, "one"}}, plus,
            {expression, {term, {factor, ident, "two"}}}},
        parser:construct(expression, lexer:parse("one + two"))
    ).
construct_expression4_test() ->
    ?assertEqual(
        {expression, {term, {factor, number, 1}}, sub, {expression, {term, {factor, number, 3}}}},
        parser:construct(expression, lexer:parse("1 - 3"))
    ).
construct_expression5_test() ->
    ?assertEqual(
        {expression, {term, {factor, ident, "one"}}, plus,
            {expression, {term, {factor, ident, "two"}}, sub,
                {expression, {term, {factor, ident, "three"}}}}},
        parser:construct(expression, lexer:parse("one + two - three"))
    ).
construct_expression6_test() ->
    ?assertEqual(
        {expression, {term, {factor, number, 1}}, sub,
            {expression, {term, {factor, number, 3}}, plus,
                {expression, {term, {factor, number, 5}}}}},
        parser:construct(expression, lexer:parse("1 - 3 + 5"))
    ).
construct_expression7_test() ->
    ?assertEqual(
        {expression, {term, {factor, number, 1}}, plus,
            {expression, {term, {factor, number, 2}, mul, {term, {factor, number, 3}}}}},
        parser:construct(expression, lexer:parse("1 + 2 * 3"))
    ).
construct_expression8_test() ->
    ?assertEqual(
        {expression,
            {term, {factor, number, 1}, mul,
                {term, {factor, number, 2}, mul, {term, {factor, number, 3}}}},
            plus, {expression, {term, {factor, number, 4}}}},
        parser:construct(expression, lexer:parse("1 * 2 * 3 + 4"))
    ).

construct_condition1_test() ->
    ?assertEqual(
        {condition, {odd, {expression, {term, {factor, number, 1}}}}},
        parser:construct(condition, lexer:parse("odd 1"))
    ).
construct_condition2_test() ->
    ?assertEqual(
        {condition,
            {odd,
                {expression, {term, {factor, number, 1}}, plus,
                    {expression, {term, {factor, number, 2}}}}}},
        parser:construct(condition, lexer:parse("odd 1 + 2"))
    ).
construct_condition3_test() ->
    ?assertEqual(
        {condition,
            {odd,
                {expression, {term, {factor, number, 1}}, plus,
                    {expression, {term, {factor, number, 2}, mul, {term, {factor, number, 3}}}}}}},
        parser:construct(condition, lexer:parse("odd 1 + 2 * 3"))
    ).
construct_condition4_test() ->
    ?assertEqual(
        {condition,
            {odd,
                {expression, {term, {factor, number, 1}, mul, {term, {factor, number, 2}}}, plus,
                    {expression, {term, {factor, number, 3}, mul, {term, {factor, number, 4}}}}}}},
        parser:construct(condition, lexer:parse("odd 1 * 2 + 3 * 4"))
    ).
