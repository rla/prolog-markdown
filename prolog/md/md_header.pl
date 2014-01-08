:- module(md_header, [
    md_header//1
]).

/** <module> Markdown header parser.

Recognizes atx and setext-styled headers.
*/

:- use_module(library(dcg/basics)).

:- use_module(md_line).
:- use_module(md_trim).

%% md_header(-Header)// is semidet.
%
% Recognizes either setext
% or atx-styled headings.

md_header(Header) -->
    setext_first_level(Header), !.

md_header(Header) -->
    setext_second_level(Header), !.

md_header(Header) -->
    atx_header(Header).

% Recognizes setext-styled first
% level heading. Output is a term
% like h1(heading).

setext_first_level(Header) -->
    non_empty_line(Codes),
    equals_line,
    {
        atom_codes(Atom, Codes),
        Header =.. [h1, Atom]
    }.

% Recognizes setext-styles second
% level heading. Output is a term
% like h2(heading).

setext_second_level(Header) -->
    non_empty_line(Codes),
    dashes_line,
    {
        atom_codes(Atom, Codes),
        Header =.. [h2, Atom]
    }.

% Recognizes atx-styled heading.
% Output is a term like h1(heading).

atx_header(Header) -->
    atx_level(Level), whites,
    atx_header_text(Codes), !,
    discard_to_line_end,
    {
        trim(Codes, Trimmed),
        atom_codes(Atom, Trimmed),
        atomic_concat(h, Level, Name),
        Header =.. [Name,Atom]
    }.

% Recognizes atx-styled header
% prefix.

atx_level(1) --> "# ".
atx_level(2) --> "## ".
atx_level(3) --> "### ".
atx_level(4) --> "#### ".
atx_level(5) --> "##### ".
atx_level(6) --> "###### ".

% Recognizes header text for
% atx-styles headings. Such text
% ends with #, line end, or eos.
% Escapes \# must be also processed.
% Line ending is not consumed.

atx_header_text([]) -->
    "#", !.

atx_header_text([]) -->
    lookahead_ln_or_eos, !.

atx_header_text([0'#|Codes]) -->
    "\\#", !,
    atx_header_text(Codes).

atx_header_text([Code|Codes]) -->
    [Code], { Code \= 0'# }, !,
    atx_header_text(Codes).

% Line filles with one or more dashes.

dashes_line -->
    codes_non_empty(0'-), !, ln_or_eos.

% Line filled with one or more equals signs.

equals_line -->
    codes_non_empty(0'=), !, ln_or_eos.

% Non-empty consequtive list
% of given symbol codes.

codes_non_empty(Code) -->
    [Code],
    codes_non_empty(Code).

codes_non_empty(Code) -->
    [Code].
