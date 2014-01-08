:- module(md_line, [
    inline_string//1, % -Codes
    ln//0,
    string_limit//2,  % -Codes, +Limit
    lookahead//1,     % ?Code
    lookahead_ln//0,
    lookahead_ln_or_eos//0
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

% lookahead_ln_or_eos// is semidet.
%
% Looks-ahead a line end or
% end-of-stream. Puts back `\n`
% when a line end is recognized.

lookahead_ln_or_eos -->
    lookahead_ln, !.

lookahead_ln_or_eos -->
    eos.

%% lookahead_ln// is semidet.
%
% Looks-ahead a line end. Puts
% back `\n` when it is recognized.

lookahead_ln, "\n" --> ln.

%% ln// is semidet.
%
% Recognizes different line
% endings.

ln --> "\r\n", !.
ln --> "\n", !.
ln --> "\r".
