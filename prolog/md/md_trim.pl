:- module(md_trim, [
    trim_left/2,
    trim_right/2,
    trim/2
]).

% Trims whitespaces from the beginning of
% the list of codes.

trim_left([Code|Codes], Result):-
    code_type(Code, space), !,
    trim_left(Codes, Result).

trim_left(Codes, Codes).

% Trims whitespace from the end of the
% list of codes.

trim_right(Codes, Result):-
    reverse(Codes, CodesR),
    trim_left(CodesR, ResultR),
    reverse(ResultR, Result).

% Trims whitespace from both sides of the
% list of codes.

trim(Codes, Result):-
    trim_left(Codes, Tmp1),
    reverse(Tmp1, Tmp2),
    trim_left(Tmp2, Tmp3),
    reverse(Tmp3, Result).
