-module(lexer_tests).
-include_lib("eunit/include/eunit.hrl").

parse_keyword_test() ->
    ?assert(lexer:parse("begin") == [{token, keyword, beginsym, 1}]),
    ?assert(lexer:parse("read") == [{token, keyword, readsym, 1}]),
    ?assert(lexer:parse("call") == [{token, keyword, callsym, 1}]),
    ?assert(lexer:parse("do") == [{token, keyword, dosym, 1}]),
    ?assert(lexer:parse("while") == [{token, keyword, whilesym, 1}]),
    ?assert(lexer:parse("write") == [{token, keyword, writesym, 1}]),
    ?assert(lexer:parse("if") == [{token, keyword, ifsym, 1}]),
    ?assert(lexer:parse("else") == [{token, keyword, elsesym, 1}]),
    ?assert(lexer:parse("end") == [{token, keyword, endsym, 1}]),
    ?assert(lexer:parse("odd") == [{token, keyword, oddsym, 1}]),
    ?assert(lexer:parse("then") == [{token, keyword, thensym, 1}]),
    ?assert(lexer:parse("const") == [{token, keyword, constsym, 1}]),
    ?assert(lexer:parse("int") == [{token, keyword, intsym, 1}]),
    ?assert(lexer:parse("procedure") == [{token, keyword, procsym, 1}]),
    ?assert(lexer:parse("in") == [{token, keyword, insym, 1}]),
    ?assert(lexer:parse("out") == [{token, keyword, outsym, 1}]).
