:- module(md_list_item, [
    md_bullet_list_item//2, % -Codes
    md_ordered_list_item//2 % -Codes
]).

/** <module> List item parser

Parser for items of bulleted and ordered lists.
Separated into its own module for code clarity.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_line).

%% md_bullet_list_item(+Codes, -Mode)// is det.
%
% Recognizes a single bulleted list item.

md_bullet_list_item(Codes, Mode) -->
    bullet_start(_), whites, !,
    list_item_unintented(Codes, Mode).

%% md_ordered_list_item(-Codes, -Mode)// is det.
%
% Recognizes a single ordered list item.

md_ordered_list_item(Codes, Mode) -->
    ordered_start(_), whites, !,
    list_item_unintented(Codes, Mode).

% Bulleted-list item start.
% Gives codes that make up the start.

bullet_start(Codes) -->
    item_indent(Indent),
    list_bullet(Bullet),
    marker_follow(Follow),
    { flatten([Indent, Bullet, Follow], Codes) }.

% Ordered-list item start.
% Gives codes that make up the start.

ordered_start(Codes) -->
    item_indent(Indent),
    one_or_more_digits(Number), ".",
    marker_follow(Follow),
    { flatten([Indent, Number, [0'.|Follow]], Codes) }.

% Looks ahead an item start.
% Used for detecting where the
% previous list item ends.

lookahead_item_start, Codes -->
    item_start(Codes).

item_start(Codes) -->
    bullet_start(Codes), !.

item_start(Codes) -->
    ordered_start(Codes).

% List bullet might be indented
% with up to 3 spaces.

item_indent([0' ,0' ,0' ]) --> "   ".
item_indent([0' ,0' ]) --> "  ".
item_indent([0' ]) --> " ".
item_indent([]) --> "".

% List item marker must be followed
% by a space or tab.

marker_follow([0' ]) --> " ".
marker_follow([0'\t]) --> "\t".

% Sames as list_item_text but
% removes possible indentation.

list_item_unintented(Codes, Mode) -->
    list_item_text(Indented, Mode), !,
    { phrase(strip_indent(Codes), Indented) }.

% Recognizes list item body and mode.
% Mode can be either normal or para.
% This is implemented by recognizing
% end conditions first.

list_item_text([], Mode) -->
    list_item_end(Mode), !.

% Other cases, just consume input.

list_item_text([Code|Codes], Mode) -->
    [Code], list_item_text(Codes, Mode).

% Recognizes list item end and
% item mode.

list_item_end(normal) -->
    eos.

% Item and next item are separated
% with an empty line.

list_item_end(para) -->
    ln, empty_line,
    lookahead_item_start.

% No empty line before next item.

list_item_end(normal) -->
    ln, lookahead_item_start.

% Empty line and next line has
% no indent.

list_item_end(normal) -->
    ln, empty_line, lookahead_no_indent.

% Looks ahead non-indented line begin.

lookahead_no_indent -->
    lookahead_no_white.

lookahead_no_indent, [0' ] -->
    " ", lookahead_no_white.

lookahead_no_indent, [0' ,0' ] -->
    "  ", lookahead_no_white.

lookahead_no_indent, [0' ,0' ,0' ] -->
    "   ", lookahead_no_white.

lookahead_no_white, [Code] -->
    [Code], { \+ code_type(Code, white) }.

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

% Strips indent (tab or up to 4 spaces)
% from line beginnings.

strip_indent(Codes) -->
    strip_indent_begin(Codes).

strip_indent_begin(Codes) -->
    strip_line_indent, !,
    strip_rest_indent(Codes).

strip_indent_begin(Codes) -->
    strip_rest_indent(Codes).

strip_rest_indent([0'\n|Codes]) -->
    ln, strip_line_indent, !,
    strip_rest_indent(Codes).

strip_rest_indent([Code|Codes]) -->
    [Code], !, strip_rest_indent(Codes).

strip_rest_indent([]) -->
    eos.

strip_line_indent --> "\n".

strip_line_indent --> "    ".

strip_line_indent --> "   ".

strip_line_indent --> "  ".

strip_line_indent --> " ".
