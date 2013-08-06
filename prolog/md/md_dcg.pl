:- module(md_dcg, [
    rest//1,
    at_end//0,
    pushback//1,
    lookahead//1,
    any//1,
    any//2,
    la//1,
    dcg_get//1,
    dcg_put//1
]).

:- use_module(library(lists)).

% FIXME needs documentation.

% Grammar helper to extract the rest of elements
% of the list.

rest(Rest, Rest, []).

% Grammar helper to detect the end of the input.

at_end([], []).

% Grammar helper to insert new token.

pushback(Term, List, [Term|List]).

% Grammar helper to look ahead the next token.

lookahead(Term, [Term|Rest], [Term|Rest]).

any([]) --> [].

any([Code|Codes]) -->
    [ Code ], any(Codes).

% Same as any with limit.

any([], _) --> [].

any([Code|Codes], Limit) -->
    { Limit >= 0, LimitNext is Limit - 1 },
    [ Code ], any(Codes, LimitNext).

% Looks ahead list prefix.

la(Prefix, List, List):-
    prefix(Prefix, List).

dcg_get(List, List, List).

dcg_put(List, List, List).