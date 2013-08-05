:- module(md_lex, [
    lex/2
]).

:- use_module(library(apply)).
:- use_module(library(md/md_dcg)).

%% lex(+Codes:list, -Lines:list) is det.
%
% Turns codes into a list of lines.

lex(Codes, Lines):-
    phrase(lines(Lines), Codes, []).

lines([Line|Lines]) -->
    line(Line), ln, !,
    lines(Lines).

lines([Line]) -->
    line(Line),
    at_end.

% Parses one line.

line([Code|Line]) -->
    [ Code ],
    { \+ code_type(Code, end_of_line) }, !,
    line(Line).

line([]) --> "".

% A line end.

ln --> "\n\r", !.
ln --> "\n", !.
ln --> "\r".
