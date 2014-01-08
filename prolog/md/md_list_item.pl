:- module(md_list_item, [
    md_bullet_list_item//1, % -Codes
    md_ordered_list_item//1 % -Codes
]).

/** <module> List item parser

Parser for items of bulleted and ordered lists.
Separated into its own module for code clarity.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_line).

%% md_bullet_list_item(Codes)// is det.
%
% Recognizes a single bulleted list item.

md_bullet_list_item(Codes) -->
    specific_indent(Indent),
    list_bullet(_), " ", whites, !,
    list_item_text(Indent, Codes).

%% md_ordered_list_item(-Codes)// is det.
%
% Recognizes a single ordered list item.

md_ordered_list_item(Codes) -->
    specific_indent(Indent),
    one_or_more_digits(_), ". ", !,
    list_item_text(Indent, Codes).

% Recognizes list item body.
% This is implemented by recognizing
% end conditions first. Possible
% end conditions are: eos, new list
% element start, empty line following
% no indent.

list_item_text(_, []) -->
    eos, !.

% New list element start. Matched
% element start is put back into
% the input.

list_item_text(Indent, []), PutBack -->
    ln, specific_indent(Indent),
    list_bullet(Bullet), " ", !,
    { append([0'\n|Indent], [Bullet, 0' ], PutBack) }.

% Same as above for ordered
% list element start.

list_item_text(Indent, []), PutBack -->
    ln, specific_indent(Indent),
    one_or_more_digits(Digits), ". ", !,
    { append([0'\n|Indent], [Digits, 0'., 0' ], PutBack) }.

% Empty line following no indent.

list_item_text(_, []) -->
    ln, empty_line, \+ indent, !.

% Other cases, just consume input.

list_item_text(Indent, [Code|Codes]) -->
    [Code], !, list_item_text(Indent, Codes).

% Recognizes specific indent by spaces or
% tabs.

specific_indent([0' |Indent]) -->
    " ", !, specific_indent(Indent).

specific_indent([0'\t|Indent]) -->
    "\t", !, specific_indent(Indent).

specific_indent([]) --> "".

% Recognizes bulleted list item
% token.

list_bullet(0'*) --> "*".
list_bullet(0'-) --> "-".
list_bullet(0'+) --> "+".

% Recognizes sequence of
% one or more digits. Used for
% recognizing ordered list items.

one_or_more_digits([Digit]) -->
    digit(Digit).

one_or_more_digits([Digit|Digits]) -->
    digit(Digit),
    one_or_more_digits(Digits).
