:- begin_tests(md_block).
:- use_module(library(md/md_parse)).

% Tests for block-level parser.

test(heading_1):-
    md_parse("abc\n===", [h1(abc)]).

test(heading_2):-
    md_parse("abc\n---", [h2(abc)]).

test(heading_3):-
    md_parse("# abc", [h1(abc)]).

test(heading_4):-
    md_parse("## abc", [h2(abc)]).

test(heading_5):-
    md_parse("# abc #", [h1(abc)]).

test(blockquote_1):-
    md_parse("> abc", [blockquote([\[abc]])]).

test(blockquote_2):-
    md_parse("> abc\n> def", [blockquote([\['abc\ndef']])]).

test(blockquote_3):-
    md_parse("> abc\n>\n> def", [blockquote([p([\[abc]]), p([\[def]])])]).

test(blockquote_4):-
    md_parse("> > abc", [blockquote([blockquote([\[abc]])])]).

test(paragraph_1):-
    md_parse("abc", [p([\[abc]])]).

test(paragraph_3):-
    md_parse("abc\ndef", [p([\['abc\ndef']])]).

test(list_1):-
    md_parse("+ a", [ul([
        li([\[a]])
    ])]).

test(list_2):-
    md_parse("+ a\n+ b", [ul([
        li([\[a]]),
        li([\[b]])
    ])]).

test(list_3):-
    md_parse("+ a\n    b", [ul([
        li([\['a'], '\n', \['b']])
    ])]).

test(list_4):-
    md_parse("+  a\n    b", [ul([
        li([\['a'], '\n', \['b']])
    ])]).

test(list_5):-
    md_parse("+ a\n    - b", [ul([
        li([
            p([\[a]]),
            ul([
                li([\[b]])
            ])
        ])
    ])]).

test(list_6):-
    md_parse("+ a\n+ b\n+ c", [ul([
        li([\[a]]),
        li([\[b]]),
        li([\[c]])
    ])]).

test(list_7):-
    md_parse("+ a\n    + b\n+ c", [ul([
        li([p([\[a]]), ul([
            li([\[b]])
        ])]),
        li([\[c]])
    ])]).

test(code):-
    md_parse("    abc", [pre(code(abc))]).

test(code_tabs):-
    md_parse("\tabc", [pre(code(abc))]).

test(code_multiline):-
    md_parse("    abc\n    def", [pre(code('abc\ndef'))]).

test(code_empty):-
    md_parse("    abc\n\n    def", [pre(code('abc\n\ndef'))]).

test(code_4):-
    md_parse("\tabc\n\tdef", [pre(code('abc\ndef'))]).

test(horisontal_rule_1):-
    md_parse("***", [hr]).

test(horisontal_rule_2):-
    md_parse("---", [hr]).

test(horisontal_rule_3):-
    md_parse("* * *", [hr]).

test(block):-
    md_parse("<div>abc</div>", [\['<div>abc</div>']]).

:- end_tests(md_block).
