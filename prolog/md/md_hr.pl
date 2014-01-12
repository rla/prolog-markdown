:- module(md_hr, [
    md_hr//0,
    md_lookahead_hr//0
]).

/** <module> Parser for Markdown horizontal rulers

Recognizes horizontal rulers.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_line).

%! md_hr// is semidet.
%
% Recognizes an horizontal ruler.

% XXX might be a bit slow implementation?

md_hr -->
    non_empty_line(Line),
    { hr(Line) }.

%! md_lookahead_hr// is semidet.
%
% Looks ahead an horizontal ruler.

md_lookahead_hr -->
    lookahead_non_empty_line(Line),
    { hr(Line) }.

% Checks that the line is
% an horizontal ruler.

hr(Line):-
    exclude('='(0' ), Line, Clean),
    length(Clean, Length),
    Length >= 3,
    (   maplist('='(0'*), Clean)
    ;   maplist('='(0'-), Clean)).
