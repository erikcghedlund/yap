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
construct_condition5_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 1}}},
            equalsym,
            {expression, {term, {factor, number, 1}}}
        }},
        parser:construct(condition, lexer:parse("1 = 1"))
    ).
construct_condition6_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 1}}},
            neqsym,
            {expression, {term, {factor, number, 1}}}
        }},
        parser:construct(condition, lexer:parse("1 <> 1"))
    ).
construct_condition7_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 1}}},
            leqsym,
            {expression, {term, {factor, number, 1}}}
        }},
        parser:construct(condition, lexer:parse("1 <= 1"))
    ).
construct_condition8_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 1}}},
            geqsym,
            {expression, {term, {factor, number, 1}}}
        }},
        parser:construct(condition, lexer:parse("1 >= 1"))
    ).
construct_condition9_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 1}}},
            lessym,
            {expression, {term, {factor, number, 1}}}
        }},
        parser:construct(condition, lexer:parse("1 < 1"))
    ).
construct_condition10_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 1}}},
            gtrsym,
            {expression, {term, {factor, number, 1}}}
        }},
        parser:construct(condition, lexer:parse("1 > 1"))
    ).
construct_condition11_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, ident, "one"}}},
            gtrsym,
            {expression, {term, {factor, ident, "two"}}}
        }},
        parser:construct(condition, lexer:parse("one > two"))
    ).
construct_condition12_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 2}, mul, {term, {factor, number, 1}}}},
            equalsym,
            {expression, {term, {factor, number, 1}, mul, {term, {factor, number, 2}}}}
        }},
        parser:construct(condition, lexer:parse("2 * 1 = 1 * 2"))
    ).
construct_condition13_test() ->
    ?assertEqual(
        {condition, {
            {expression, {term, {factor, number, 2}}, plus,
                {expression, {term, {factor, number, 1}}}},
            equalsym,
            {expression, {term, {factor, number, 1}}, plus,
                {expression, {term, {factor, number, 2}}}}
        }},
        parser:construct(condition, lexer:parse("2 + 1 = 1 + 2"))
    ).
construct_statement_become1_test() ->
    ?assertEqual(
        {statement, {"one", become, {expression, {term, {factor, number, 1}}}}},
        parser:construct(statement, lexer:parse("one := 1"))
    ).
construct_statement_become2_test() ->
    ?assertEqual(
        {statement,
            {"two", become,
                {expression, {term, {factor, number, 10}, ddiv, {term, {factor, number, 5}}}}}},
        parser:construct(statement, lexer:parse("two := 10/5"))
    ).
construct_statement_become3_test() ->
    ?assertEqual(
        {statement,
            {"three", become,
                {expression, {term, {factor, number, 8}}, sub,
                    {expression, {term, {factor, number, 5}}}}}},
        parser:construct(statement, lexer:parse("three := 8 - 5"))
    ).
construct_statement_become4_test() ->
    ?assertEqual(
        {statement,
            {"four", become,
                {expression, {term, {factor, number, 2}}, plus,
                    {expression, {term, {factor, ident, "two"}}}}}},
        parser:construct(statement, lexer:parse("four := 2 + two"))
    ).
construct_statement_call1_test() ->
    ?assertEqual(
        {statement, {call, "foo"}},
        parser:construct(statement, lexer:parse("call foo"))
    ).
construct_statement_call2_test() ->
    ?assertEqual(
        {statement, {call, "bar"}},
        parser:construct(statement, lexer:parse("call bar"))
    ).
construct_statement_in1_test() ->
    ?assertEqual(
        {statement, {in, "foo"}},
        parser:construct(statement, lexer:parse("in foo"))
    ).
construct_statement_in2_test() ->
    ?assertEqual(
        {statement, {in, "bar"}},
        parser:construct(statement, lexer:parse("in bar"))
    ).
construct_statement_in3_test() ->
    ?assertEqual(
        {statement, {in, "foo"}},
        parser:construct(statement, lexer:parse("read foo"))
    ).
construct_statement_in4_test() ->
    ?assertEqual(
        {statement, {in, "bar"}},
        parser:construct(statement, lexer:parse("read bar"))
    ).
construct_statement_out1_test() ->
    ?assertEqual(
        {statement, {out, {expression, {term, {factor, ident, "foo"}}}}},
        parser:construct(statement, lexer:parse("out foo"))
    ).
construct_statement_out2_test() ->
    ?assertEqual(
        {statement, {out, {expression, {term, {factor, ident, "bar"}}}}},
        parser:construct(statement, lexer:parse("out bar"))
    ).
construct_statement_out3_test() ->
    ?assertEqual(
        {statement, {out, {expression, {term, {factor, number, 1}}}}},
        parser:construct(statement, lexer:parse("out 1"))
    ).
construct_statement_out4_test() ->
    ?assertEqual(
        {statement,
            {out,
                {expression, {term, {factor, number, 1}}, plus,
                    {expression, {term, {factor, number, 2}}, plus,
                        {expression, {term, {factor, ident, "bar"}}}}}}},
        parser:construct(statement, lexer:parse("out 1 + 2 + bar"))
    ).
construct_statement_out5_test() ->
    ?assertEqual(
        {statement, {out, {expression, {term, {factor, ident, "foo"}}}}},
        parser:construct(statement, lexer:parse("write foo"))
    ).
construct_statement_out6_test() ->
    ?assertEqual(
        {statement, {out, {expression, {term, {factor, ident, "bar"}}}}},
        parser:construct(statement, lexer:parse("write bar"))
    ).
construct_statement_out7_test() ->
    ?assertEqual(
        {statement, {out, {expression, {term, {factor, number, 1}}}}},
        parser:construct(statement, lexer:parse("write 1"))
    ).
construct_statement_out8_test() ->
    ?assertEqual(
        {statement,
            {out,
                {expression, {term, {factor, number, 1}}, plus,
                    {expression, {term, {factor, number, 2}}, plus,
                        {expression, {term, {factor, ident, "bar"}}}}}}},
        parser:construct(statement, lexer:parse("write 1 + 2 + bar"))
    ).
construct_statement_while1_test() ->
    ?assertEqual(
        {statement,
            {while,
                {condition, {
                    {expression, {term, {factor, number, 1}}},
                    equalsym,
                    {expression, {term, {factor, number, 1}}}
                }},
                {statement, {out, {expression, {term, {factor, number, 2}}}}}}},

        parser:construct(statement, lexer:parse("while 1 = 1 do out 2"))
    ).
construct_statement_if1_test() ->
    ?assertEqual(
        {statement,
            {iif,
                {condition, {
                    {expression, {term, {factor, number, 1}}},
                    equalsym,
                    {expression, {term, {factor, number, 1}}}
                }},
                {statement, {out, {expression, {term, {factor, number, 2}}}}}}},

        parser:construct(statement, lexer:parse("if 1 = 1 then out 2"))
    ).
construct_statement_if2_test() ->
    ?assertEqual(
        {statement,
            {iif,
                {condition, {
                    {expression, {term, {factor, number, 1}}},
                    equalsym,
                    {expression, {term, {factor, number, 1}}}
                }},
                {statement, {out, {expression, {term, {factor, number, 2}}}}},
                {statement, {out, {expression, {term, {factor, number, 3}}}}}}},

        parser:construct(statement, lexer:parse("if 1 = 1 then out 2 else out 3"))
    ).
construct_statement_begin1_test() ->
    ?assertEqual(
        {statement,
            {bbegin, [
                {statement,
                    {while,
                        {condition, {
                            {expression, {term, {factor, number, 1}}},
                            equalsym,
                            {expression, {term, {factor, number, 1}}}
                        }},
                        {statement, {out, {expression, {term, {factor, number, 2}}}}}}},
                {statement,
                    {"four", become,
                        {expression, {term, {factor, number, 2}}, plus,
                            {expression, {term, {factor, ident, "two"}}}}}},
                {statement, {call, "bar"}}
            ]}},
        parser:construct(
            statement, lexer:parse("begin while 1 = 1 do out 2; four := 2 + two; call bar end")
        )
    ).
construct_vardec1_test() ->
    ?assertEqual(
        {vardec, ["x"]},
        parser:construct(vardec, lexer:parse("int x;"))
    ).
construct_vardec2_test() ->
    ?assertEqual(
        {vardec, ["x", "y"]},
        parser:construct(vardec, lexer:parse("int x, y;"))
    ).
construct_vardec3_test() ->
    ?assertEqual(
        {vardec, ["x", "y", "z", "q", "really0fucking0long0ident"]},
        parser:construct(
            vardec, lexer:parse("int x, y, z, q,               really0fucking0long0ident;")
        )
    ).
construct_constdec1_test() ->
    ?assertEqual(
        {constdec, [{"x", 1}]},
        parser:construct(constdec, lexer:parse("const x = 1;"))
    ).
construct_constdec2_test() ->
    ?assertEqual(
        {constdec, [{"x", 1}, {"y", 3}]},
        parser:construct(constdec, lexer:parse("const x = 1, y = 3;"))
    ).
