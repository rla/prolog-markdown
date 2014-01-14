:- begin_tests(md_span).
:- use_module(prolog/md/md_span).

test(entity):-
    md_span_string("&amp;", [\['&amp;']]).

test(special_amp):-
    md_span_string("AT&T", [\['AT'],'&',\['T']]).

test(special_lt):-
    md_span_string("1<2", [\['1'],'<',\['2']]).

test(special_lt_end):-
    md_span_string("1<", [\['1'],'<']).

test(preserve_html):-
    md_span_string("abc<em>1</em>", [\["abc<em>1</em>"]]).

test(strong):-
    md_span_string("**abc**", [strong([\["abc"]])]).

test(strong_in_text):-
    md_span_string("abc **def** ghi", [\["abc "],strong([\["def"]]),\[" ghi"]]).

test(strong_emb_html):-
    md_span_string("**abc<a>anchor</a>**", [strong([\["abc<a>anchor</a>"]])]).

test(strong_underscore):-
    md_span_string("__abc__", [strong([\["abc"]])]).

test(strong_nonest):-
    md_span_string("**__abc__**", [strong([em([\["_abc"]]), \["_"]])]).

test(strong_space):-
    md_span_string("** abc**", [strong([\[" abc"]])]).

test(strong_space_underscore):-
    md_span_string("__ abc__", [strong([\[" abc"]])]).

test(emphasis):-
    md_span_string("*abc*", [em([\["abc"]])]).

test(emphasis_escape):-
    md_span_string("abc \\*def\\* ghi", [\["abc "], '*', \["def"], '*', \[" ghi"]]).

test(emphasis_underscore):-
    md_span_string("_abc_", [em([\["abc"]])]).

test(no_emphasis):-
    md_span_string("* abc*", [\["* abc*"]]).

test(no_emphasis_underscore):-
    md_span_string("_ abc_", [\["_ abc_"]]).

test(code_1):-
    md_span_string("`p1:- p2, p3.`", [code("p1:- p2, p3.")]).

test(code_1_entities):-
    md_span_string("`<blink>`", [code("<blink>")]). % escaped by html//1.

test(code_2):-
    md_span_string("``p1:- p2, p3.``", [code("p1:- p2, p3.")]).

test(code_2_entities):-
    md_span_string("``<blink>``", [code("<blink>")]). % escaped by html//1.

test(code_2_tick):-
    md_span_string("`` ` ``", [code("`")]).

test(code_2_ticks):-
    md_span_string("`` `foo` ``", [code("`foo`")]).

test(link_with_title):-
    md_span_string("[label](http://google.com \"Google\")", [a([href='http://google.com', title='Google'], label)]).

test(link_without_title):-
    md_span_string("[label](http://google.com)", [a([href='http://google.com'], label)]).

test(inline_link):-
    md_span_string("<http://google.com>", [a([href='http://google.com'], 'http://google.com')]).

test(script):-
    md_span_string("<script>var x = 1+2;</script>", [\["<script>var x = 1+2;</script>"]]).

test(script_markdown):-
    md_span_string("<script>var x = 1*2*3;</script>", [\["<script>var x = 1*2*3;</script>"]]). % contains emphasis *2*.

test(line_break):-
    md_span_string("abc  \ndef", [\["abc"],br([]),\["def"]]).

test(no_inline_emphasis):-
    md_span_string("abc_def_ghi", [\["abc_def_ghi"]]).

test(link_in_strong):-
    md_span_string("**[label](http://google.com)**", [strong([a([href='http://google.com'], label)])]).

test(strong_in_emphasis):-
    md_span_string("_**abc**_", [em([strong([\["abc"]])])]).

test(strikethrough):-
    md_span_string("~~abc~~", [del([\["abc"]])]).

test(strong_in_strikethrough):-
    md_span_string("~~**abc**~~", [del([strong([\["abc"]])])]).

test(strikethrough_in_strong):-
    md_span_string("**~~abc~~**", [strong([del([\["abc"]])])]).

test(escaped_strikethrough):-
    md_span_string("\\~~abc\\~~", [~, \["~abc"], ~, \[~]]).

test(plain_link_http):-
    md_span_string("abc http://google.com rest",
        [\['abc '], a([href='http://google.com'], 'http://google.com'), \[" rest"]]).

test(plain_link_https):-
    md_span_string("abc https://google.com rest",
        [\['abc '], a([href='https://google.com'], 'https://google.com'), \[" rest"]]).

:- end_tests(md_span).
