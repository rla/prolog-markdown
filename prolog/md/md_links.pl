:- module(md_links, [
    md_links/3
]).

/** <module> Markdown reference link parser

Parses and removes reference links from
the stream of symbol codes.
*/

:- use_module(library(dcg/basics)).

% md_links(+CodesIn, -CodesOut, -Links) is det.
%
% Markdown reference link definition parser.
% Removes link definitions from the symbol code list.

md_links(CodesIn, CodesOut, Links):-
    phrase(links_begin(TmpCodes, TmpLinks), CodesIn),
    CodesOut = TmpCodes,
    Links = TmpLinks.

links_begin(Codes, [Link|Links]) -->
    link(Link), !, links(Codes, Links).

links_begin(Codes, Links) -->
    links(Codes, Links).

links([], []) --> eos, !.

links(Codes, [Link|Links]) -->
    ln, link(Link), !, links(Codes, Links).

links([Code|Codes], Links) -->
    [Code], links(Codes, Links).

% Recognizes a reference link definition.
% Example: [foo]: http://example.com/ "Optional Title Here"
% Records the link but outputs nothing.

link(link(Id, Url, Title)) -->
    whites, link_id(Id),
    whites, link_url(Url),
    whites, link_title(Title).

% Recognizes a link title.
% When no title is found, Title is
% an empty atom ('').

link_title(Title) -->
    link_title_same_line(Title), !.

link_title(Title) -->
    ln, whites, link_title_same_line(Title), !.

link_title('') --> "".

link_title_same_line(Title) -->
    "'", !, string(Codes), "'",
    whites, la_ln_or_eos,
    { atom_codes(Title, Codes) }.

link_title_same_line(Title) -->
    "(", !, string(Codes), ")",
    whites, la_ln_or_eos,
    { atom_codes(Title, Codes) }.

link_title_same_line(Title) -->
    "\"", string(Codes), "\"",
    whites, la_ln_or_eos,
    { atom_codes(Title, Codes) }.

% Recognizes a link identifier.

link_id(Id) -->
    "[", whites, string(Codes), whites, "]:",
    {
        atom_codes(Tmp, Codes),
        downcase_atom(Tmp, Id)
    }.

% Recognizes a link URL.

link_url(Url) -->
    "<", !, string(Codes), ">",
    { atom_codes(Url, Codes) }.

link_url(Url) -->
    string_without([0'\n, 0'\t, 0' ], Codes),
    { atom_codes(Url, Codes) }.

% Lookahead line end or eos.

la_ln_or_eos -->
    (la_ln ; eos), !.

la_ln, "\n" --> ln.

ln --> "\r\n", !.
ln --> "\n", !.
ln --> "\r".
