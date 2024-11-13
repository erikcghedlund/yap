-module(lexer_tests).
-include_lib("eunit/include/eunit.hrl").

parse_keyword_begin_test() -> ?assertEqual(lexer:parse("begin"), [{token, keyword, beginsym, 1}]).
parse_keyword_read_test() -> ?assertEqual(lexer:parse("read"), [{token, keyword, readsym, 1}]).
parse_keyword_call_test() -> ?assertEqual(lexer:parse("call"), [{token, keyword, callsym, 1}]).
parse_keyword_do_test() -> ?assertEqual(lexer:parse("do"), [{token, keyword, dosym, 1}]).
parse_keyword_while_test() -> ?assertEqual(lexer:parse("while"), [{token, keyword, whilesym, 1}]).
parse_keyword_write_test() -> ?assertEqual(lexer:parse("write"), [{token, keyword, writesym, 1}]).
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
