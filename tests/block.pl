:- begin_tests(md_block).
:- use_module(prolog/md/md_parse).

% Tests for block-level parser.

% Setext-styled first-level heading.

test(heading_1):-
    md_parse_string("abc\n===", [h1(abc)]).

% Setext-styled second-level heading.

test(heading_2):-
    md_parse_string("abc\n---", [h2(abc)]).

% Atx-styled first-level heading.

test(heading_3):-
    md_parse_string("# abc", [h1(abc)]).

% Atx-styled second-level heading.

test(heading_4):-
    md_parse_string("## abc", [h2(abc)]).

% Atx-styled first-level heading. # at end.

test(heading_5):-
    md_parse_string("# abc #", [h1(abc)]).

% Setext-styled first-level heading following a paragraph.
% With empty line.

test(heading_6):-
    md_parse_string("para\n\nabc\n===", [p([\[para]]), h1(abc)]).

% Setext-styled first-level heading following a paragraph.
% Without empty line.

test(heading_7):-
    md_parse_string("para\nabc\n===", [p([\[para]]), h1(abc)]).

% Setext-styled first-level heading following a list.
% With empty line.

test(heading_8):-
    md_parse_string("* item\n\nabc\n===", [ul([li([\[item]])]), h1(abc)]).

% Setext-styled first-level heading following a list.
% Without empty line.

test(heading_9):-
    md_parse_string("* item\nabc\n===", [ul([li([p([\[item]]), h1(abc)])])]).

test(blockquote_1):-
    md_parse_string("> abc", [blockquote([\[abc]])]).

test(blockquote_2):-
    md_parse_string("> abc\n> def", [blockquote([\['abc\ndef']])]).

test(blockquote_3):-
    md_parse_string("> abc\n>\n> def", [blockquote([p([\[abc]]), p([\[def]])])]).

test(blockquote_4):-
    md_parse_string("> > abc", [blockquote([blockquote([\[abc]])])]).

test(paragraph_1):-
    md_parse_string("abc", [p([\[abc]])]).

test(paragraph_3):-
    md_parse_string("abc\ndef", [p([\['abc\ndef']])]).

test(list_1):-
    md_parse_string("+ a", [ul([
        li([\[a]])
    ])]).

test(list_2):-
    md_parse_string("+ a\n+ b", [ul([
        li([\[a]]),
        li([\[b]])
    ])]).

test(list_3):-
    md_parse_string("+ a\n    b", [ul([
        li([\['a\nb']])
    ])]).

test(list_4):-
    md_parse_string("+  a\n    b", [ul([
        li([\['a\nb']])
    ])]).

test(list_5):-
    md_parse_string("+ a\n    - b", [ul([
        li([
            \[a],
            ul([
                li([\[b]])
            ])
        ])
    ])]).

test(list_6):-
    md_parse_string("+ a\n+ b\n+ c", [ul([
        li([\[a]]),
        li([\[b]]),
        li([\[c]])
    ])]).

test(list_7):-
    md_parse_string("+ a\n    + b\n+ c", [ul([
        li([\[a], ul([
            li([\[b]])
        ])]),
        li([\[c]])
    ])]).

test(code):-
    md_parse_string("    abc", [pre(code(abc))]).

test(code_tabs):-
    md_parse_string("\tabc", [pre(code(abc))]).

test(code_multiline):-
    md_parse_string("    abc\n    def", [pre(code('abc\ndef'))]).

test(code_empty):-
    md_parse_string("    abc\n\n    def", [pre(code('abc\n\ndef'))]).

test(code_4):-
    md_parse_string("\tabc\n\tdef", [pre(code('abc\ndef'))]).

test(horisontal_rule_1):-
    md_parse_string("***", [hr]).

test(horisontal_rule_2):-
    md_parse_string("---", [hr]).

test(horisontal_rule_3):-
    md_parse_string("* * *", [hr]).

test(block):-
    md_parse_string("<div>abc</div>", [\['<div>abc</div>']]).

:- end_tests(md_block).
