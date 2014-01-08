:- begin_tests(md_span_links).
:- use_module(prolog/md/md_span_link).

% Tests for span-level links.

md_span_link_string(String, Link, Rest):-
    string_codes(String, Codes),
    phrase(md_span_link(Tmp), Codes, RestCodes), !,
    string_codes(Rest, RestCodes),
    Tmp = Link.

% Tests a normal link.

test(span_link_1):-
    md_span_link_string("[abc](http://example.com \"Title\") def",
        a([href='http://example.com', title='Title'], abc), " def").

% Tests a normal link without title.

test(span_link_2):-
    md_span_link_string("[abc](http://example.com) def",
        a([href='http://example.com'], abc), " def").

% Tests an angular bracket link.

test(span_link_3):-
    md_span_link_string("<http://example.com> def",
        a([href='http://example.com'], 'http://example.com'), " def").

% Tests a reference link (undefined).

test(span_link_4):-
    retractall(md_links:link(_, _, _)),
    (   md_span_link_string("[Google][g] def", _, _)
    ->  fail
    ;   true).

% Tests a reference link.

test(span_link_5):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(g, 'http://google.com', 'Title')),
    md_span_link_string("[Google][g] def",
        a([href='http://google.com', title='Title'], 'Google'), " def").

% Tests a reference link. Uppercase identifier.

test(span_link_6):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(g, 'http://google.com', 'Title')),
    md_span_link_string("[Google][G] def",
        a([href='http://google.com', title='Title'], 'Google'), " def").

% Tests a reference link. Identifier from label.

test(span_link_7):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(google, 'http://google.com', 'Title')),
    md_span_link_string("[Google][] def",
        a([href='http://google.com', title='Title'], 'Google'), " def").

% Tests a reference link. No title.

test(span_link_8):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(g, 'http://google.com', '')),
    md_span_link_string("[Google][g] def",
        a([href='http://google.com'], 'Google'), " def").

% Tests an angular mail link.

test(span_link_9):-
    md_span_link_string("<hel+lo@example.com> def",
        a([href='mailto:hel+lo@example.com'], 'hel+lo@example.com'), " def").

% Tests an image.

test(span_link_10):-
    md_span_link_string("![Alt](/photo.jpg) def",
        img([src='/photo.jpg', alt='Alt']), " def").

% Tests an image with title.

test(span_link_11):-
    md_span_link_string("![Alt](/photo.jpg \"Title\") def",
        img([src='/photo.jpg', alt='Alt', title='Title']), " def").

% Tests a reference-linked image.

test(span_link_12):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(i, 'http://example.com/photo.jpg', 'Title')),
    md_span_link_string("![Photo][i] def",
        img([src='http://example.com/photo.jpg', alt='Photo', title='Title']), " def").

% Tests a reference link. Identifier from label.

test(span_link_13):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(photo, 'http://example.com/photo.jpg', 'Title')),
    md_span_link_string("![Photo][] def",
        img([src='http://example.com/photo.jpg', alt='Photo', title='Title']), " def").

% Tests a reference-linked image. No title.

test(span_link_14):-
    retractall(md_links:link(_, _, _)),
    assertz(md_links:link(i, 'http://example.com/photo.jpg', '')),
    md_span_link_string("![Photo][i] def",
        img([src='http://example.com/photo.jpg', alt='Photo']), " def").

:- end_tests(md_span_links).
