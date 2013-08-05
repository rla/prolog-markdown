:- module(md_span, [
    span_parse/2
]).

:- use_module(library(apply)).
:- use_module(library(md/md_dcg)).
:- use_module(library(md/md_trim)).
:- use_module(library(md/md_scan)).

% FIXME no HTML in spans?

%% span_parse(+Text:codes, -Out:list) is det.
%
% Turns the list of codes into a structure acceptable
% by SWI-Prolog's html//1 predicate. More info:
% http://www.swi-prolog.org/pldoc/doc_for?object=html/1

span_parse(Codes, Out):-
    phrase(span(Spans), Codes, []), !,
    atoms(Spans, Out).

% Escape sequences.
% More info:
% http://daringfireball.net/projects/markdown/syntax#backslash
% Processed first.

span([Char|Spans]) -->
    "\\", [ Code ],
    { memberchk(Code, "\\`*_{}[]()#+-.!") }, !,
    { atom_codes(Char, [ Code ]) },
    span(Spans).

% Entities. These must be left alone.
% More info:
% http://daringfireball.net/projects/markdown/syntax#autoescape

span([\[EntityAtom]|Spans]) -->
    "&", any(Codes, 10), ";",
    { maplist(alnum, Codes) }, !,
    { flatten(["&", Codes, ";"], Entity) },
    { atom_codes(EntityAtom, Entity) },
    span(Spans).

% Special characters & and <.
% More info:
% http://daringfireball.net/projects/markdown/syntax#autoescape

span(['&'|Spans]) -->
    "&", !, span(Spans).

% As inline HTML is allowed, < is only escaped
% when the following character is not a letter and / or
% < appears at end of stream.

span(['<'|Spans]) -->
    "<", lookahead(Code),
    { \+ code_type(Code, alpha), Code \= 47 }, !,
    span(Spans).

span(['<']) -->
    "<", at_end, !.

% Recognizes an inline link.
% More info:
% http://daringfireball.net/projects/markdown/syntax#link

span([Link|Spans]) -->
    link_label(Label),
    link_url_title(Url, Title), !,
    { atom_codes(UrlAtom, Url) },
    { atom_codes(TitleAtom, Title) },
    { atom_codes(LabelAtom, Label) },
    { link(UrlAtom, TitleAtom, LabelAtom, Link) },
    span(Spans).

% Recognizes <script ... </script>.
% Protects script contents from being processed as Markdown.

span([\[ScriptAtom]|Spans]) -->
    "<script", any(Codes), "</script>", !,
    { flatten(["<script", Codes, "</script>"], Script) },
    { atom_codes(ScriptAtom, Script) },
    span(Spans).

% Recognizes inline automatic http <link>.

span([Link|Spans]) -->
    "<http", any(Codes), ">", !,
    { append("http", Codes, Url) },
    { atom_codes(UrlAtom, Url) },
    { link(UrlAtom, '', UrlAtom, Link) },
    span(Spans).

% Recognizes inline mail link <address@example.com>

span([Link|Spans]) -->
    "<", any(User), "@", any(Host), ">", !,
    { flatten([User, "@", Host], Address) },
    { mail_link(Address, Link) },
    span(Spans).

% Recognizes strong **something**.
% No nesting.

span([strong(HTML)|Spans]) -->
    "**", any(Strong), "**", !,
    { span_atom(Strong, HTML) },
    span(Spans).

% Recognizes strong __something__.
% No nesting.

span([strong(HTML)|Spans]) -->
    "__", any(Strong), "__", !,
    { span_atom(Strong, HTML) },
    span(Spans).

% Recognizes emhasis *something*.
% The first character following * must be a non-space.

span([em(HTML)|Spans]) -->
    "*", non_space(Code), any(Emph), "*", !,
    { span_atom([Code|Emph], HTML) },
    span(Spans).

% Recognizes emphasis _something_.
% The first character following _ must be a non-space.

span([em(HTML)|Spans]) -->
    "_", non_space(Code), any(Emph), "_", !,
    { span_atom([Code|Emph], HTML) },
    span(Spans).

% Recognizes inline code ``code``.

span([code(Atom)|Spans]) -->
    "``", any(Raw), "``", !,
    { trim(Raw, Trimmed) },
    { atom_codes(Atom, Trimmed) },
    span(Spans).

% Recognizes inline code `code`.

span([code(Atom)|Spans]) -->
    "`", any(Raw), "`", !,
    { trim(Raw, Trimmed) },
    { atom_codes(Atom, Trimmed) },
    span(Spans).

% Recognizes image link ![Alt text](/path/to/img.jpg).
% With optional title: ![Alt text](/path/to/img.jpg "Optional title").

span([image(Url, Alt, Title)|Spans]) -->
    "!", link_label(Alt),
    link_url_title(Url, Title), !,
    span(Spans).

span([Code|Spans]) -->
    [ Code ], !,
    span(Spans).

span([]) -->
    at_end.

%% link(+Url:atom, +Title:atom, +Label:atom, -Element) is det.
%
% Creates an HTML link element. Has no title attribute when
% title is empty.

link(Url, '', Label, Element):- !,
    Element = a([href=Url], Label).

link(Url, Title, Label, Element):-
    Element = a([href=Url, title=Title], Label).

%% mail_link(+Address:codes, -Element) is det.
%
% Created HTML link element for an email address.
% FIXME use special encoding.

mail_link(Address, Element):-
    append("mailto:", Address, Href),
    atom_codes(HrefAtom, Href),
    atom_codes(AddressAtom, Address),
    Element = a([href=HrefAtom], AddressAtom).

%% span_atom(+Codes:list, -HTML:term) is det.
%
% Turns a list of codes into an HTML term
% than can contain embedded HTML.

span_atom(Codes, \[Atom]):-
    atom_codes(Atom, Codes).

%% atoms(+In:list, -Out:html) is det.
%
% Collects remaining codes into atoms suitable
% for SWI-s html//1.
% Atoms will appear as \[text] as they can contain
% raw HTML which must not be escaped.

atoms(In, Out):-
    atoms(In, [], Out).

atoms([], [], []):- !.

atoms([], Acc, [\[Atom]]):-
    reverse(Acc, Rev),
    atom_codes(Atom, Rev).

atoms([Token|In], Acc, Out):-
    number(Token), !,
    atoms(In, [Token|Acc], Out).

% Case for empty atom between span elements.

atoms([Token|In], [], [Token|Out]):-
    \+ number(Token), !,
    atoms(In, [], Out).

atoms([Token|In], Acc, [\[Atom],Token|Out]):-
    \+ number(Token),
    reverse(Acc, Rev),
    atom_codes(Atom, Rev),
    atoms(In, [], Out).

% Recognizes single non-space code.

non_space(Code) -->
    [ Code ], { \+ code_type(Code, space) }.

link_label(Label) -->
    "[", any(Label), "]", !.

% 32 - space, 34 - ", 41 - ).

link_url_title(Url, Title) -->
    "(", any(Url), [32,34], any(Title), [34,41], !.

link_url_title(Url, "") -->
    "(", any(Url), ")", !.

alnum(Code):-
    code_type(Code, alnum).