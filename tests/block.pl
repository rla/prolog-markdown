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

% Setext-styled first-level heading following a blockquote.
% With empty line.

test(heading_10):-
    md_parse_string("> bq\n\nabc\n===", [blockquote([\[bq]]), h1(abc)]).

% Setext-styled first-level heading following a blockquote.
% Without empty line.

test(heading_11):-
    md_parse_string("> bq\nabc\n===", [blockquote([p([\[bq]]), h1(abc)])]).

% Setext-styled first-level heading following a code block.
% With empty line.

test(heading_12):-
    md_parse_string("\ta+b\n\nabc\n===", [pre(code('a+b\n')), h1(abc)]).

% Setext-styled first-level heading following a code block.
% Without empty line.

test(heading_13):-
    md_parse_string("\ta+b\nabc\n===", [pre(code('a+b')), h1(abc)]).

% Setext-styled first-level heading following an HTML block.
% With empty line.

test(heading_14):-
    md_parse_string("<div>html</div>\n\nabc\n===", [\['<div>html</div>'], h1(abc)]).

% Setext-styled first-level heading following a code block.
% Without empty line. Merges header with html block.
% XXX diverges from dingus.

test(heading_15):-
    md_parse_string("<div>html</div>\nabc\n===", [\['<div>html</div>\nabc\n===']]).

% Single line blockquote.

test(blockquote_1):-
    md_parse_string("> abc", [blockquote([\[abc]])]).

% Multiline blockquote.

test(blockquote_2):-
    md_parse_string("> abc\n> def", [blockquote([\['abc\ndef']])]).

% Nested blockquote,

test(blockquote_3):-
    md_parse_string("> abc\n>\n> def", [blockquote([p([\[abc]]), p([\[def]])])]).

% Nested blockquote, single line.

test(blockquote_4):-
    md_parse_string("> > abc", [blockquote([blockquote([\[abc]])])]).

% Blockquote following a paragraph.
% With empty line.

test(blockquote_5):-
    md_parse_string("para\n\n> abc", [p([\[para]]), blockquote([\[abc]])]).

% Blockquote following a paragraph.
% No empty line.

test(blockquote_6):-
    md_parse_string("para\n> abc", [p([\[para]]), blockquote([\[abc]])]).

% Simple paragraph.

test(paragraph_1):-
    md_parse_string("abc", [p([\[abc]])]).

% Paragraph with two lines.

test(paragraph_2):-
    md_parse_string("abc\ndef", [p([\['abc\ndef']])]).

% Two paragraphs.

test(paragraph_3):-
    md_parse_string("abc\n\ndef", [p([\[abc]]), p([\[def]])]).

% Simple list.

test(list_1):-
    md_parse_string("+ a", [ul([
        li([\[a]])
    ])]).

% List with two items.

test(list_2):-
    md_parse_string("+ a\n+ b", [ul([
        li([\[a]]),
        li([\[b]])
    ])]).

% List with two items. Paragraph mode.

test(list_3):-
    md_parse_string("+ a\n\n+ b", [ul([
        li([p([\[a]])]),
        li([p([\[b]])])
    ])]).

% Multiline list item.

test(list_4):-
    md_parse_string("+ a\n    b", [ul([
        li([\['a\nb']])
    ])]).

% List with sublist.

test(list_5):-
    md_parse_string("+ a\n    - b", [ul([
        li([
            \[a],
            ul([li([\[b]])])
        ])
    ])]).

% List with 3 items.

test(list_6):-
    md_parse_string("+ a\n+ b\n+ c", [ul([
        li([\[a]]),
        li([\[b]]),
        li([\[c]])
    ])]).

% List with sublist and item after it.

test(list_7):-
    md_parse_string("+ a\n    + b\n+ c", [ul([
        li([\[a], ul([
            li([\[b]])
        ])]),
        li([\[c]])
    ])]).

% List following a paragraph.
% With empty line.

test(list_8):-
    md_parse_string("para\n\n+ a", [p([\[para]]), ul([li([\[a]])])]).

% List following a paragraph.
% Without empty line.

test(list_9):-
    md_parse_string("para\n+ a", [p([\[para]]), ul([li([\[a]])])]).

% Code block in list item.

test(list_10):-
    md_parse_string("+ a\n        code", [ul([li([p([\[a]]), pre(code(code))])])]).

% Simple code block.

test(code):-
    md_parse_string("    abc", [pre(code(abc))]).

% Code block with tabs.

test(code_tabs):-
    md_parse_string("\tabc", [pre(code(abc))]).

% Multiline code block.

test(code_multiline):-
    md_parse_string("    abc\n    def", [pre(code('abc\ndef'))]).

% Code block with an empty line.

test(code_empty):-
    md_parse_string("    abc\n\n    def", [pre(code('abc\n\ndef'))]).

% Code block with tabs.

test(code_4):-
    md_parse_string("\tabc\n\tdef", [pre(code('abc\ndef'))]).

% Simple horisontal ruler.

test(horisontal_rule_1):-
    md_parse_string("***", [hr]).

% Simple horisontal ruler, dashes.

test(horisontal_rule_2):-
    md_parse_string("---", [hr]).

% Simple horisontal ruler, spaces between stars.

test(horisontal_rule_3):-
    md_parse_string("* * *", [hr]).

% An HTML block.

test(block):-
    md_parse_string("<div>abc</div>", [\['<div>abc</div>']]).

:- end_tests(md_block).
