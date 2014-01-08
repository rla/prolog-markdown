:- begin_tests(md_header).
:- use_module(prolog/md/md_header).

% Tests for different headers.

md_header_string(String, Header, Rest):-
    string_codes(String, Codes),
    phrase(md_header(Tmp), Codes, RestCodes), !,
    string_codes(Rest, RestCodes),
    Tmp = Header.

% Setext-styled first-level header.

test(header_1):-
    md_header_string("abc\n===\nrest", h1(abc), "rest").

% Setext-styled second-level header.

test(header_2):-
    md_header_string("abc\n---\nrest", h2(abc), "rest").

% Atx-styled level 1.

test(header_3):-
    md_header_string("# abc\nrest", h1(abc), "rest").

% Atx-styled level 2.

test(header_4):-
    md_header_string("## abc\nrest", h2(abc), "rest").

% Atx-styled level 3.

test(header_5):-
    md_header_string("### abc\nrest", h3(abc), "rest").

% Atx-styled level 4.

test(header_6):-
    md_header_string("#### abc\nrest", h4(abc), "rest").

% Atx-styled level 5.

test(header_7):-
    md_header_string("##### abc\nrest", h5(abc), "rest").

% Atx-styled level 6.

test(header_8):-
    md_header_string("###### abc\nrest", h6(abc), "rest").

% Atx-styled, # at end.

test(header_9):-
    md_header_string("# abc#\nrest", h1(abc), "rest").

% Atx-styled, multiple # at end, space.

test(header_10):-
    md_header_string("# abc ##\nrest", h1(abc), "rest").

% Atx-styled, escaped #.

test(header_11):-
    md_header_string("# abc\\#def\nrest", h1('abc#def'), "rest").

% Atx-styled, escaped #, one # at end.

test(header_12):-
    md_header_string("# abc\\#def #\nrest", h1('abc#def'), "rest").

:- end_tests(md_header).
