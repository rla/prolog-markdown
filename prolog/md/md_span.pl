:- module(md_span, [
    md_span_codes/2,
    md_span_string/2
]).

/** <module> Span-level Markdown parser

Parses span-level Markdown elements: emphasis,
inline-code, links and others. More info:
http://daringfireball.net/projects/markdown/syntax#span
*/

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).

:- use_module(md_trim).
:- use_module(md_links).
:- use_module(md_span_link).
:- use_module(md_escape).
:- use_module(md_line).

%% md_span_string(+String, -Out) is det.
%
% Same as md_span_codes/2 but uses a string
% ans input.

md_span_string(String, Out):-
    string_codes(String, Codes),
    md_span_codes(Codes, Out).

%% md_span_codes(+Codes, -Out) is det.
%
% Turns the list of codes into a structure acceptable
% by SWI-Prolog's html//1 predicate. More info:
% http://www.swi-prolog.org/pldoc/doc_for?object=html/1

md_span_codes(Codes, Out):-
    phrase(span(Spans), Codes), !,
    atoms(Spans, Out).

% Escape sequences.
% More info:
% http://daringfireball.net/projects/markdown/syntax#backslash
% Processed first.

span([Atom|Spans]) -->
    "\\", [Code],
    {
        md_escaped_code(Code),
        atom_codes(Atom, [Code])
    }, !,
    span(Spans).

% Entities. These must be left alone.
% More info:
% http://daringfireball.net/projects/markdown/syntax#autoescape

span([\[Atom]|Spans]) -->
    "&", string_limit(Codes, 10), ";",
    {
        maplist(alnum, Codes), !,
        append([0'&|Codes], [0';], Entity),
        atom_codes(Atom, Entity)
    },
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
    {
        \+ code_type(Code, alpha),
        Code \= 47
    }, !,
    span(Spans).

span(['<']) -->
    "<", eos, !.

% Recognizes links and images.

span([Link|Spans]) -->
    md_span_link(Link), !,
    span(Spans).

% Recognizes <script ... </script>.
% Protects script contents from being processed as Markdown.

span([\[Atom]|Spans]) -->
    "<script", string(Codes), "</script>", !,
    {
        atom_codes(Content, Codes),
        atomic_list_concat(['<script', Content, '</script>'], Atom)
    },
    span(Spans).

% Recognizes strong **something**.
% No nesting.

span([strong(Html)|Spans]) -->
    "**", string(Strong), "**", !,
    { span_atom(Strong, Html) },
    span(Spans).

% Recognizes strong __something__.
% No nesting.

span([strong(Html)|Spans]) -->
    "__", string(Strong), "__", !,
    { span_atom(Strong, Html) },
    span(Spans).

% Recognizes emhasis *something*.
% The first character following * must be a non-space.

span([em(Html)|Spans]) -->
    "*", nonblank(Code), string(Emph), "*", !,
    { span_atom([Code|Emph], Html) },
    span(Spans).

% Recognizes emphasis _something_.
% The first character following _ must be a non-space.

span([em(Html)|Spans]) -->
    "_", nonblank(Code), string(Emph), "_", !,
    { span_atom([Code|Emph], Html) },
    span(Spans).

% Recognizes inline code ``code``.

span([code(Atom)|Spans]) -->
    "``", string(Raw), "``", !,
    {
        trim(Raw, Trimmed),
        atom_codes(Atom, Trimmed)
    },
    span(Spans).

% Recognizes inline code `code`.

span([code(Atom)|Spans]) -->
    "`", string(Raw), "`", !,
    {
        trim(Raw, Trimmed),
        atom_codes(Atom, Trimmed)
    },
    span(Spans).

span([Code|Spans]) -->
    [Code], !,
    span(Spans).

span([]) -->
    eos.

% Turns a list of codes into an HTML term
% than can contain embedded HTML.

span_atom(Codes, \[Atom]):-
    atom_codes(Atom, Codes).

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

% Recognizes single symbol code of
% type alnum.

alnum(Code):-
    code_type(Code, alnum).
