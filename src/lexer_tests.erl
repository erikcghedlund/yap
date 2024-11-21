-module(lexer_tests).
-include_lib("eunit/include/eunit.hrl").

parse_keyword_begin_test() -> ?assertEqual(lexer:parse("begin"), [{token, keyword, beginsym, 1}]).
parse_keyword_read_test() -> ?assertEqual(lexer:parse("read"), [{token, keyword, insym, 1}]).
parse_keyword_call_test() -> ?assertEqual(lexer:parse("call"), [{token, keyword, callsym, 1}]).
parse_keyword_do_test() -> ?assertEqual(lexer:parse("do"), [{token, keyword, dosym, 1}]).
parse_keyword_while_test() -> ?assertEqual(lexer:parse("while"), [{token, keyword, whilesym, 1}]).
parse_keyword_write_test() -> ?assertEqual(lexer:parse("write"), [{token, keyword, outsym, 1}]).
parse_keyword_if_test() -> ?assertEqual(lexer:parse("if"), [{token, keyword, ifsym, 1}]).
parse_keyword_else_test() -> ?assertEqual(lexer:parse("else"), [{token, keyword, elsesym, 1}]).
parse_keyword_end_test() -> ?assertEqual(lexer:parse("end"), [{token, keyword, endsym, 1}]).
parse_keyword_odd_test() -> ?assertEqual(lexer:parse("odd"), [{token, keyword, oddsym, 1}]).
parse_keyword_then_test() -> ?assertEqual(lexer:parse("then"), [{token, keyword, thensym, 1}]).
parse_keyword_const_test() -> ?assertEqual(lexer:parse("const"), [{token, keyword, constsym, 1}]).
parse_keyword_int_test() -> ?assertEqual(lexer:parse("int"), [{token, keyword, intsym, 1}]).
parse_keyword_procedure_test() ->
    ?assertEqual(lexer:parse("procedure"), [{token, keyword, procsym, 1}]).
parse_keyword_in_test() -> ?assertEqual(lexer:parse("in"), [{token, keyword, insym, 1}]).
parse_keyword_out_test() -> ?assertEqual(lexer:parse("out"), [{token, keyword, outsym, 1}]).

parse_operator_plus_test() -> ?assertEqual(lexer:parse("+"), [{token, binop, plussym, 1}]).
parse_operator_plus_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("+ 2 3")), {token, binop, plussym, 1}).
parse_operator_minus_test() -> ?assertEqual(lexer:parse("-"), [{token, binop, minussym, 1}]).
parse_operator_minus_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("- 2 3")), {token, binop, minussym, 1}).
parse_operator_mult_test() -> ?assertEqual(lexer:parse("*"), [{token, binop, multsym, 1}]).
parse_operator_mult_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("* 2 3")), {token, binop, multsym, 1}).
parse_operator_div_test() -> ?assertEqual(lexer:parse("/"), [{token, binop, slashsym, 1}]).
parse_operator_div_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("/ 2 3")), {token, binop, slashsym, 1}).
parse_operator_neq_test() -> ?assertEqual(lexer:parse("<>"), [{token, relop, neqsym, 1}]).
parse_operator_neq_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("<> 2 3")), {token, relop, neqsym, 1}).
parse_operator_leq_test() -> ?assertEqual(lexer:parse("<="), [{token, relop, leqsym, 1}]).
parse_operator_leq_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("<= 2 3")), {token, relop, leqsym, 1}).
parse_operator_geq_test() -> ?assertEqual(lexer:parse(">="), [{token, relop, geqsym, 1}]).
parse_operator_geq_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse(">= 2 3")), {token, relop, geqsym, 1}).
parse_operator_become_test() -> ?assertEqual(lexer:parse(":="), [{token, separator, becomesym, 1}]).
parse_operator_become_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse(":= 2 3")), {token, separator, becomesym, 1}).
parse_operator_less_test() -> ?assertEqual(lexer:parse("<"), [{token, relop, lessym, 1}]).
parse_operator_less_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("< 2 3")), {token, relop, lessym, 1}).
parse_operator_greater_test() -> ?assertEqual(lexer:parse(">"), [{token, relop, gtrsym, 1}]).
parse_operator_greater_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("> 2 3")), {token, relop, gtrsym, 1}).
parse_operator_equal_test() -> ?assertEqual(lexer:parse("="), [{token, relop, equalsym, 1}]).
parse_operator_equal_test_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("= 2 3")), {token, relop, equalsym, 1}).
parse_operator_comma_test() -> ?assertEqual(lexer:parse(","), [{token, separator, commasym, 1}]).
parse_operator_comma_test_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse(", 2 3")), {token, separator, commasym, 1}).
parse_operator_semicolon_test() ->
    ?assertEqual(lexer:parse(";"), [{token, separator, semicolonsym, 1}]).
parse_operator_semicolon_test_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("; 2 3")), {token, separator, semicolonsym, 1}).
parse_operator_period_test() -> ?assertEqual(lexer:parse("."), [{token, separator, periodsym, 1}]).
parse_operator_period_test_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse(". 2 3")), {token, separator, periodsym, 1}).
parse_operator_lparent_test() ->
    ?assertEqual(lexer:parse("("), [{token, separator, lparentsym, 1}]).
parse_operator_lparent_test_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse("( 2 3")), {token, separator, lparentsym, 1}).
parse_operator_rparent_test() ->
    ?assertEqual(lexer:parse(")"), [{token, separator, rparentsym, 1}]).
parse_operator_rparent_test_in_text_test() ->
    ?assertEqual(lists:nth(1, lexer:parse(") 2 3")), {token, separator, rparentsym, 1}).

parse_ident_legal1_test() ->
    ?assertEqual(lexer:parse("x"), [{token, ident, "x", 1}]).
parse_ident_legal2_test() ->
    ?assertEqual(lexer:parse("z"), [{token, ident, "z", 1}]).
parse_ident_legal3_test() ->
    ?assertEqual(lexer:parse("a"), [{token, ident, "a", 1}]).
parse_ident_legal4_test() ->
    ?assertEqual(lexer:parse("X"), [{token, ident, "X", 1}]).
parse_ident_legal5_test() ->
    ?assertEqual(lexer:parse("A"), [{token, ident, "A", 1}]).
parse_ident_legal6_test() ->
    ?assertEqual(lexer:parse("A"), [{token, ident, "A", 1}]).
parse_ident_legal7_test() ->
    ?assertEqual(lexer:parse("AbCdEf"), [{token, ident, "AbCdEf", 1}]).
parse_ident_legal8_test() ->
    ?assertEqual(lexer:parse("bCdEfd"), [{token, ident, "bCdEfd", 1}]).
parse_ident_legal9_test() ->
    ?assertEqual(lexer:parse("A1234"), [{token, ident, "A1234", 1}]).
parse_ident_legal10_test() ->
    ?assertEqual(lexer:parse("b4321"), [{token, ident, "b4321", 1}]).

parse_number_legal1_test() ->
    ?assertEqual(lexer:parse("1"), [{token, number, 1, 1}]).
parse_number_legal2_test() ->
    ?assertEqual(lexer:parse("1337"), [{token, number, 1337, 1}]).
parse_number_legal3_test() ->
    ?assertEqual(lexer:parse("6969696969696969"), [{token, number, 6969696969696969, 1}]).
parse_number_legal4_test() ->
    ?assertEqual(lexer:parse("0"), [{token, number, 0, 1}]).

parse_trims_whitespace1_test() ->
    ?assertEqual(lexer:parse("      2 +      3       "), lexer:parse("2+3")).
parse_trims_whitespace2_test() ->
    ?assertEqual(
        lexer:parse(" begin              helo world 1 "), lexer:parse("begin helo world 1 ")
    ).
parse_trims_whitespace3_test() ->
    ?assertMatch(
        [{token, number, 3, _}, {token, binop, plussym, _}, {token, number, 3, _}],
        lexer:parse("\n \n 3 \n + 3 \n")
    ).

program(loop) ->
    "while done = 0 do\n"
    "    begin\n"
    "        in op;\n"
    "        in y;\n"
    "        if op = ADD then call add\n"
    "        else if op = SUB then call sub\n"
    "        else if op = MULT then call mult\n"
    "        else if op = DIV then call div\n"
    "        else done := 1;\n"
    "    end;\n".

parse_whole_program_test() ->
    ?assertEqual(lexer:parse(program(loop)), [
        {token, keyword, whilesym, 1},
        {token, ident, [100, 111, 110, 101], 1},
        {token, relop, equalsym, 1},
        {token, number, 0, 1},
        {token, keyword, dosym, 1},
        {token, keyword, beginsym, 2},
        {token, keyword, insym, 3},
        {token, ident, [111, 112], 3},
        {token, separator, semicolonsym, 3},
        {token, keyword, insym, 4},
        {token, ident, [121], 4},
        {token, separator, semicolonsym, 4},
        {token, keyword, ifsym, 5},
        {token, ident, [111, 112], 5},
        {token, relop, equalsym, 5},
        {token, ident, [65, 68, 68], 5},
        {token, keyword, thensym, 5},
        {token, keyword, callsym, 5},
        {token, ident, [97, 100, 100], 5},
        {token, keyword, elsesym, 6},
        {token, keyword, ifsym, 6},
        {token, ident, [111, 112], 6},
        {token, relop, equalsym, 6},
        {token, ident, [83, 85, 66], 6},
        {token, keyword, thensym, 6},
        {token, keyword, callsym, 6},
        {token, ident, [115, 117, 98], 6},
        {token, keyword, elsesym, 7},
        {token, keyword, ifsym, 7},
        {token, ident, [111, 112], 7},
        {token, relop, equalsym, 7},
        {token, ident, [77, 85, 76, 84], 7},
        {token, keyword, thensym, 7},
        {token, keyword, callsym, 7},
        {token, ident, [109, 117, 108, 116], 7},
        {token, keyword, elsesym, 8},
        {token, keyword, ifsym, 8},
        {token, ident, [111, 112], 8},
        {token, relop, equalsym, 8},
        {token, ident, [68, 73, 86], 8},
        {token, keyword, thensym, 8},
        {token, keyword, callsym, 8},
        {token, ident, [100, 105, 118], 8},
        {token, keyword, elsesym, 9},
        {token, ident, [100, 111, 110, 101], 9},
        {token, separator, becomesym, 9},
        {token, number, 1, 9},
        {token, separator, semicolonsym, 9},
        {token, keyword, endsym, 10},
        {token, separator, semicolonsym, 10}
    ]).
