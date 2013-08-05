:- begin_tests(md_html).

% Integration tests with SWI-Prolog's html_write
% module.

:- use_module(library(http/html_write)).
:- use_module(library(apply)).
:- use_module(library(md/md_span)).
:- use_module(library(md/md_parse)).

% Helper to turn Markdown span elements
% into HTML code (atom).

span_html(Codes, HTML):-
    span_parse(Codes, Spans),
    phrase(html(Spans), Tokens, []),
    strip_layout(Tokens, Clean),
    with_output_to(atom(HTML), print_html(Clean)).

% Helper to turn Markdown into HTML code (atom).

html(Codes, HTML):-
    parse(Codes, Blocks),
    phrase(html(Blocks), Tokens, []),
    strip_layout(Tokens, Clean),
    with_output_to(atom(HTML), print_html(Clean)).

strip_layout(In, Out):-
    exclude(layout, In, Out).

layout(nl(_)).
layout(mailbox(_, _)).

test(special_amp):-
    span_html("AT&T", 'AT&amp;T').

test(special_amp_noescape):-
    span_html("AT&amp;T", 'AT&amp;T').

test(special_lt):-
    span_html("1<2", '1&lt;2').

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

test(emphasis_escape):-
    span_html("\\*abc\\*", '*abc*').

test(paragraph):-
    html("abc", '<p>abc</p>').

test(two_paragraphs):-
    html("abc\n\ndef", '<p>abc</p><p>def</p>').

test(html_block):-
    html("<div>*abc*</div>", '<div>*abc*</div>').

test(span_paragraph):-
    html("abc **def** ghi", '<p>abc <strong>def</strong> ghi</p>').

test(heading1):-
    html("Hello\n=====", '<h1>Hello</h1>').

test(code_block):-
    html("\ta = 1;\n\tb = 2;\n\tc = a + b;", '<pre><code>a = 1;\nb = 2;\nc = a + b;</code></pre>').

test(code_block_escape):-
    html("\ta = 1;\n\tb = 2;\n\tc = a < b;", '<pre><code>a = 1;\nb = 2;\nc = a &lt; b;</code></pre>').

test(script_span):-
    html("abc <script>a = 1*2*3</script> def", '<p>abc <script>a = 1*2*3</script> def</p>').

:- end_tests(md_html).