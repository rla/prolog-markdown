:- begin_tests(md_html).

% Integration tests with SWI-Prolog's html_write
% module.

:- use_module(library(http/html_write)).
:- use_module(library(md/md_span)).

% Helper to turn Markdown span elements
% into HTML code (atom).

span_html(Codes, HTML):-
    span_parse(Codes, Spans),
    phrase(html(Spans), Tokens, []),
    with_output_to(atom(HTML), print_html(Tokens)).

test(strong):-
    span_html("**abc**", '<strong>abc</strong>').

test(emphasis):-
    span_html("*abc*", '<em>abc</em>').

test(link):-
    span_html("[label](http://google.com)", '<a href="http://google.com">label</a>').

test(link_entities):-
    span_html("[label](http://google.com?q=abc&s=def)", '<a href="http://google.com?q=abc&amp;s=def">label</a>').

test(code_2):-
    span_html("``abc``", '<code>abc</code>').

test(code_2_escape):-
    span_html("``<blink>``", '<code>&lt;blink&gt;</code>').

test(strong_emb_html):-
    span_html("**abc<a>anchor</a>**", '<strong>abc<a>anchor</a></strong>').

:- end_tests(md_html).