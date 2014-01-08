:- module(md_line, [
    inline_string//1, % -Codes
    ln//0,
    string_limit//2,  % -Codes, +Limit
    lookahead//1      % ?Code
]).

/** <module> Line-based parsing primitives.

Contains line-based parsing primitives.
*/

:- use_module(library(dcg/basics)).

%% lookahead(?Code)// is semidet.
%
% Looks ahead a single symbol code.

lookahead(Code), [Code] -->
    [Code].

% string_limit(-Codes, +Limit)// is multidet.
%
% Same as string//1 but with
% a length limit.

string_limit([], Limit) -->
    { Limit =< 0 }, !.

string_limit([], Limit) -->
    { Limit >= 0 }.

string_limit([Code|Codes], Limit) -->
    [Code],
    { Next is Limit - 1 },
    string_limit(Codes, Next).

%% inline_string(-Codes)// is multidet.
%
% Takes as few symbol codes as possible
% up to line end.

inline_string([]) --> "".

inline_string([]) -->
    ln, !.

inline_string([Code|Codes]) -->
    [Code],
    inline_string(Codes).

%% ln// is det.
%
% Recognizes different line
% endings.

ln --> "\r\n", !.
ln --> "\n", !.
ln --> "\r".
