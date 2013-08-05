:- module(md_span, [
    span_parse/2
]).

:- use_module(library(md/md_dcg)).
:- use_module(library(md/md_trim)).

%% span_parse(+Text:codes, -Out:list) is det.
%
% Turns the list of codes into a structure acceptable
% by SWI-Prolog's html//1 predicate. More info:
% http://www.swi-prolog.org/pldoc/doc_for?object=html/1

span_parse(Codes, Out):-
    phrase(span(Spans), Codes, []), !,
    atoms(Spans, Out).

% Attempts to recognize a link.

span([Link|Spans]) -->
    link_label(Label),
    link_url_title(Url, Title), !,
    { atom_codes(UrlAtom, Url) },
    { atom_codes(TitleAtom, Title) },
    { atom_codes(LabelAtom, Label) },
    { link(UrlAtom, TitleAtom, LabelAtom, Link) },
    span(Spans).

% Escapes a lone < that is not part of HTML tag.

%span([escape(<),Code|Spans]) -->
%    "<", [ Code ],
%    { \+ code_type(Code, alpha) }, !,
%    span(Spans).

% Recognizes <script ... </script>.
% Protects script contents from being processed as Markdown.

span([\[ScriptAtom]|Spans]) -->
    "<script", all_to("</script>", Script), !,
    { append("<script", Script, All) },
    { atom_codes(ScriptAtom, All) },
    span(Spans).

% Recognizes inline automatic http <link>.

span([link(Url)|Spans]) -->
    "<http", all_not(">", Rest), !,
    { append("http", Rest, Url) },
    span(Spans).

% Recognizes inline mail link <address@example.com>

span([mail(Address)|Spans]) -->
    "<", all_not("@> ", User), "@", all_not(">", Host), !,
    { append(User, "@", UserAt) },
    { append(UserAt, Host, Address) },
    span(Spans).

% Recognizes strong **something**.
% No nesting.

span([strong(HTML)|Spans]) -->
    "**", all_to("**", All), !,
    { append(Strong, "**", All) },
    { span_atom(Strong, HTML) },
    span(Spans).

% Recognizes strong __something__.
% No nesting.

span([strong(HTML)|Spans]) -->
    "__", all_to("__", All), !,
    { append(Strong, "__", All) },
    { span_atom(Strong, HTML) },
    span(Spans).

% Recognizes emhasis *something*.
% The first character following * must be a non-space.

span([em(HTML)|Spans]) -->
    "*", non_space(Code), all_to("*", All), !,
    { append(Strong, "*", All) },
    { span_atom([Code|Strong], HTML) },
    span(Spans).

% Recognizes emphasis _something_.
% The first character following _ must be a non-space.

span([em(HTML)|Spans]) -->
    "_", non_space(Code), all_to("_", All), !,
    { append(Strong, "_", All) },
    { span_atom([Code|Strong], HTML) },
    span(Spans).

% Recognizes inline code ``code``.

span([code(Atom)|Spans]) -->
    "``", all_to("``", All), !,
    { append(Raw, "``", All) },
    { trim(Raw, Trimmed) },
    { atom_codes(Atom, Trimmed) },
    span(Spans).

% Recognizes inline code `code`.

span([code(Atom)|Spans]) -->
    "`", all_to("`", All), !,
    { append(Raw, "`", All) },
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

atoms([Token|In], Acc, [\[Atom]|Out]):-
    \+ number(Token),
    reverse(Acc, Rev),
    atom_codes(Atom, Rev),
    atoms(In, [], Out).

% Recognizes single non-space code.

non_space(Code) -->
    [ Code ], { \+ code_type(Code, space) }.

link_label(Label) -->
    "[", all_not("]", Label), "]".

% 32 - space, 34 - ", 41 - ).

link_url_title(Url, Title) -->
    "(", all_not(") ", Url), [32,34], all_not([34,41], Title), [34,41], !.

link_url_title(Url, "") -->
    "(", all_not(")", Url), ")".

% Recognizes all input up to the given stopword.
% Consumes also the stopword.
% Fails when no stopword is ever encountered.

all_to(Stop, Stop) -->
    Stop, !.

all_to(Stop, [Code|Codes]) -->
    [ Code ],
    all_to(Stop, Codes).

% Recognizes as much input as possible.
% Input codes must not be contained in the
% list Check.

all_not(Check, [Code|Codes]) -->
    [ Code ],
    { \+ memberchk(Code, Check) }, !,
    all_not(Check, Codes).

all_not(_, []) --> [].