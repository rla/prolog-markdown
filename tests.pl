:- begin_tests(md).
:- use_module(md_parse).

test(heading_1):-
    parse("abc\n===", [heading(1, abc)]).

test(heading_2):-
    parse("abc\n---", [heading(2, abc)]).

test(heading_3):-
    parse("# abc", [heading(1, abc)]).

test(heading_4):-
    parse("## abc", [heading(2, abc)]).

test(heading_5):-
    parse("# abc #", [heading(1, abc)]).

test(blockquote_1):-
    parse("> abc", [blockquote([paragraph(abc)])]).

test(blockquote_2):-
    parse("> abc\n> def", [blockquote([paragraph('abc\ndef')])]).

test(blockquote_3):-
    parse("> abc\n>\n> def", [blockquote([paragraph(abc), paragraph(def)])]).

test(blockquote_4):-
    parse("> > abc", [blockquote([blockquote([paragraph(abc)])])]).

test(paragraph_1):-
    parse("abc", [paragraph(abc)]).

test(paragraph_2):-
    parse("<em>abc</em>", [paragraph('<em>abc</em>')]).

test(paragraph_3):-
    parse("abc\ndef", [paragraph('abc\ndef')]).

test(list_1):-
    parse("+ a", [unordered_list([[paragraph(a)]])]).

test(list_2):-
    parse("+ a\n+ b", [unordered_list([[paragraph(a)], [paragraph(b)]])]).

test(list_3):-
    parse("+ a\n    b", [unordered_list([[paragraph('a\nb')]])]).

test(list_4):-
    parse("+  a\n    b", [unordered_list([[paragraph('a\nb')]])]).

test(code_1):-
    parse("    abc", [code(abc)]).

test(code_2):-
    parse("\tabc", [code(abc)]).

test(code_3):-
    parse("    abc\n    def", [code('abc\ndef')]).

test(code_4):-
    parse("\tabc\n\tdef", [code('abc\ndef')]).

test(horisontal_rule_1):-
    parse("***", [horisontal_rule]).

test(horisontal_rule_2):-
    parse("---", [horisontal_rule]).

test(horisontal_rule_3):-
    parse("* * *", [horisontal_rule]).

:- end_tests(md).
