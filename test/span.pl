:- begin_tests(md_span).
:- use_module(library(md/md_span)).

test(preserve_html):-
    span_parse("abc<em>1</em>", [\['abc<em>1</em>']]).

test(strong):-
    span_parse("**abc**", [strong(\[abc])]).

test(strong_emb_html):-
    span_parse("**abc<a>anchor</a>**", [strong(\['abc<a>anchor</a>'])]).

test(strong_underscore):-
    span_parse("__abc__", [strong(\[abc])]).

test(strong_nonest):-
    span_parse("**__abc__**", [strong(\['__abc__'])]).

test(strong_space):-
    span_parse("** abc**", [strong(\[' abc'])]).

test(strong_space_underscore):-
    span_parse("__ abc__", [strong(\[' abc'])]).

test(emphasis):-
    span_parse("*abc*", [em(\[abc])]).

test(emphasis_underscore):-
    span_parse("_abc_", [em(\[abc])]).

test(no_emphasis):-
    span_parse("* abc*", [\['* abc*']]).

test(no_emphasis_underscore):-
    span_parse("_ abc_", [\['_ abc_']]).

test(code_1):-
    span_parse("`p1:- p2, p3.`", [code('p1:- p2, p3.')]).

test(code_1_entities):-
    span_parse("`<blink>`", [code('<blink>')]). % escaped by html//1.

test(code_2):-
    span_parse("``p1:- p2, p3.``", [code('p1:- p2, p3.')]).

test(code_2_entities):-
    span_parse("``<blink>``", [code('<blink>')]). % escaped by html//1.

test(code_2_tick):-
    span_parse("`` ` ``", [code('`')]).

test(code_2_ticks):-
    span_parse("`` `foo` ``", [code('`foo`')]).

test(link_with_title):-
    span_parse("[label](http://google.com \"Google\")", [a([href='http://google.com', title='Google'], label)]).

test(link_without_title):-
    span_parse("[label](http://google.com)", [a([href='http://google.com'], label)]).

test(script):-
    span_parse("<script>var x = 1+2;</script>", [\['<script>var x = 1+2;</script>']]).

test(script_markdown):-
    span_parse("<script>var x = 1*2*3;</script>", [\['<script>var x = 1*2*3;</script>']]). % contains emphasis *2*.

:- end_tests(md_span).