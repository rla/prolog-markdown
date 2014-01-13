:- begin_tests(md_links).
:- use_module(prolog/md/md_links).

% Tests for reference link parsing.

md_links_strings(In, Out, Links):-
    string_codes(In, InCodes),
    md_links(InCodes, OutCodes, Links),
    string_codes(Out, OutCodes).

% No links.

test(link_1):-
    md_links_strings("abc", "abc", []).

% Link with title, double quotes.

test(link_2):-
    md_links_strings("[id]: http://example.com \"Title\"", "",
        [link(id, 'http://example.com', 'Title')]).

% Link with title, single quotes.

test(link_3):-
    md_links_strings("[id]: http://example.com 'Title'", "",
        [link(id, 'http://example.com', 'Title')]).

% Link with title, parentheses.

test(link_4):-
    md_links_strings("[id]: http://example.com (Title)", "",
        [link(id, 'http://example.com', 'Title')]).

% Link with title, on the next line.

test(link_5):-
    md_links_strings("[id]: http://example.com\n    'Title'", "",
        [link(id, 'http://example.com', 'Title')]).

% Some text before link.

test(link_6):-
    md_links_strings("abc\n[id]: http://example.com 'Title'", "abc",
        [link(id, 'http://example.com', 'Title')]).

% Some text before and after the link.

test(link_7):-
    md_links_strings("abc\n[id]: http://example.com 'Title'\ndef", "abc\ndef",
        [link(id, 'http://example.com', 'Title')]).

% Spaces around the identifier.

test(link_8):-
    md_links_strings("[ id ]: http://example.com 'Title'", "",
        [link(id, 'http://example.com', 'Title')]).

% Spaces before the identifier token.

test(link_9):-
    md_links_strings("   [id]: http://example.com 'Title'", "",
        [link(id, 'http://example.com', 'Title')]).

% URL inside < and >.

test(link_10):-
    md_links_strings("[id]: <http://example.com/ a> 'Title'", "",
        [link(id, 'http://example.com/ a', 'Title')]).

% Identifier must be lowercased.

test(link_11):-
    md_links_strings("[ID]: http://example.com 'Title'", "",
        [link(id, 'http://example.com', 'Title')]).

% No title.

test(link_12):-
    md_links_strings("[id]: http://example.com", "",
        [link(id, 'http://example.com', '')]).

% No title, text on the next line.

test(link_13):-
    md_links_strings("[id]: http://example.com\nabc", "\nabc",
        [link(id, 'http://example.com', '')]).

% Title delimiter in link title.

test(link_14):-
    md_links_strings("[id]: http://example.com 'Title' 1'", "",
        [link(id, 'http://example.com', 'Title\' 1')]).

% Link indented deeper than 3 spaces is ignored.

test(link_15):-
    md_links_strings("    [id]: http://example.com 'Title'",
        "    [id]: http://example.com 'Title'", []).

% Line ends replaced with canonical line ends.

test(link_16):-
    md_links_strings("\r\n\r", "\n\n", []).

:- end_tests(md_links).
