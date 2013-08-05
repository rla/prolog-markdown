:- begin_tests(md_block).
:- use_module(library(md/md_parse)).

% Tests for block-level parser.

test(heading_1):-
    parse("abc\n===", [h1(abc)]).

test(heading_2):-
    parse("abc\n---", [h2(abc)]).

test(heading_3):-
    parse("# abc", [h1(abc)]).

test(heading_4):-
    parse("## abc", [h2(abc)]).

test(heading_5):-
    parse("# abc #", [h1(abc)]).

test(blockquote_1):-
    parse("> abc", [blockquote([p([\[abc]])])]).

test(blockquote_2):-
    parse("> abc\n> def", [blockquote([p([\['abc\ndef']])])]).

test(blockquote_3):-
    parse("> abc\n>\n> def", [blockquote([p([\[abc]]), p([\[def]])])]).

test(blockquote_4):-
    parse("> > abc", [blockquote([blockquote([p([\[abc]])])])]).

test(paragraph_1):-
    parse("abc", [p([\[abc]])]).

test(paragraph_3):-
    parse("abc\ndef", [p([\['abc\ndef']])]).

test(list_1):-
    parse("+ a", [ul([
        li([\[a]])
    ])]).

test(list_2):-
    parse("+ a\n+ b", [ul([
        li([\[a]]),
        li([\[b]])
    ])]).

test(list_3):-
    parse("+ a\n    b", [ul([
        li([\['a'], '\n', \['b']])
    ])]).

test(list_4):-
    parse("+  a\n    b", [ul([
        li([\['a'], '\n', \['b']])
    ])]).

test(list_5):-
    parse("+ a\n    - b", [ul([
        li([
            p([\[a]]),
            ul([
                li([\[b]])
            ])
        ])
    ])]).

test(list_6):-
    parse("+ a\n+ b\n+ c", [ul([
        li([\[a]]),
        li([\[b]]),
        li([\[c]])
    ])]).

test(list_7):-
    parse("+ a\n    + b\n+ c", [ul([
        li([p([\[a]]), ul([
            li([\[b]])
        ])]),
        li([\[c]])
    ])]).

test(code):-
    parse("    abc", [pre(code(abc))]).

test(code_tabs):-
    parse("\tabc", [pre(code(abc))]).

test(code_multiline):-
    parse("    abc\n    def", [pre(code('abc\ndef'))]).

test(code_empty):-
    parse("    abc\n\n    def", [pre(code('abc\n\ndef'))]).

test(code_4):-
    parse("\tabc\n\tdef", [pre(code('abc\ndef'))]).

test(horisontal_rule_1):-
    parse("***", [hr]).

test(horisontal_rule_2):-
    parse("---", [hr]).

test(horisontal_rule_3):-
    parse("* * *", [hr]).

test(block):-
    parse("<div>abc</div>", [\['<div>abc</div>']]).

:- end_tests(md_block).
