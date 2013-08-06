:- module(md_blocks2, [
    blocks//1
]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(md/md_dcg)).
:- use_module(library(md/md_trim)).
:- use_module(library(md/md_span)).

% Parses list of blocks.
% Blocks are separated by one or more empty lines.

blocks([]) -->
    empty_lines,
    at_end, !.

blocks([]) -->
    at_end, !.

blocks([Block|Blocks]) -->
    block(Block), !,
    empty_lines,
    blocks(Blocks).

block(Block) -->
    heading(Block).

block(Block) -->
    blockquote(Block).

block(Block) -->
    horisontal_rule(Block).

block(Block) -->
    list(Block).

block(Block) -->
    code(Block).

block(Block) -->
    html(Block).

block(Blob) -->
    paragraph(Blob).

% Recognizes headings:
% More info: http://daringfireball.net/projects/markdown/syntax#header

heading(h1(Heading)) -->
    two_line_heading(Text, Line),
    { trim(Line, Trimmed), Line \= [], maplist('='(61), Trimmed) }, !,
    { atom_codes(Heading, Text) }.

heading(h2(Heading)) -->
    two_line_heading(Text, Line),
    { trim(Line, Trimmed), Line \= [], maplist('='(45), Trimmed) }, !,
    { atom_codes(Heading, Text) }.

% Recognizes heading in the form "# Heading". Gives heading level (1-6)
% and the heading content as an atom.

heading(Heading) -->
    hash_heading_line(Text, Level), { Level >= 0 }, !,
    { atomic_concat('h', Level, Name) },
    { atom_codes(TextAtom, Text) },
    { Heading =.. [ Name, TextAtom ] }.

two_line_heading(Text, Line) -->
    any(Text), ln, any(Line), (ln ; at_end), !.

hash_heading_line(Text, Level) -->
    white, hashes(Level), " ", white,
    any(Text), white, hashes(_), (ln ; at_end), !.

% Recognizes blockquote.
% Can contain nested Markdown.
% More info: http://daringfireball.net/projects/markdown/syntax#blockquote

blockquote(blockquote(Opt)) -->
    "> ", any(Text), ((ln, white, ln) ; at_end), !,
    { phrase(strip_bq(Stripped), Text, "") }, !,
    { phrase(blocks(Quote), Stripped, []) },
    { element_opt(Quote, Opt) }.

% Strips blockquote start from blockquote text.

strip_bq([10|Text]) -->
    ln, "> ", !, strip_bq(Text).

strip_bq([10|Text]) -->
    ln, ">", !, strip_bq(Text).

strip_bq([Code|Text]) -->
    [ Code ], !, strip_bq(Text).

strip_bq([]) --> at_end.

% Recognizes unordered list containing one or more items.
% Items can contain nested Markdown.
% More info: http://daringfireball.net/projects/markdown/syntax#list

list(ul(Items)) -->
    list_items(Items),
    { length(Items, Len), Len > 0 }.

list_items([Item|Items]) -->
    list_item(Item), !,
    list_items(Items).

list_items([]) --> [].

% Recognizes single line item.

list_item(li(Opt)) -->
    list_item_start, !,
    any(Line), (ln ; at_end), !,
    any(Text), list_item_end(Text), !,
    { phrase(strip_indent(Stripped), Text, "") }, !,
    { phrase(blocks(Blocks), Stripped, "") }, !,
    { trim(Line, Trimmed) },
    { list_item_fixup(Trimmed, Blocks, Item) },
    { element_opt(Item, Opt) }.

% List item start.

list_item_start -->
    white, [X], { memberchk(X, "*-+") }, " ", white.

% Strips one indent level.

strip_indent(Text) -->
    strip_indent_begin(Text).

strip_indent_begin(Text) -->
    ("    " ; "\t"), !,
    strip_indent_rest(Text).

strip_indent_begin(Text) -->
    strip_indent_rest(Text).

strip_indent_rest([]) --> at_end, !.

strip_indent_rest([10|Text]) -->
    ln, ("    " ; "\t"), !,
    strip_indent_rest(Text).

strip_indent_rest([Code|Text]) -->
    [ Code ], !, strip_indent_rest(Text).

% Immediate begin of next list item. No optional space.

list_item_end([]) -->
    la([X,32]), { memberchk(X, "*-+") }, !.

% Immediate begin of next list item. Optional space.

list_item_end([]) -->
    la([32,X,32]), { memberchk(X, "*-+") }, !.

% Begin of next list item. No optional space.

list_item_end(_) -->
    ln, la([X,32]), { memberchk(X, "*-+") }, !.

% Begin of next list item. Optional space.

list_item_end(_) -->
    ln, la([32,X,32]), { memberchk(X, "*-+") }, !.

% End of stream.

list_item_end(_) --> at_end, !.

% At least one empty line following non-indented line.

list_item_end(_) -->
    empty_line, empty_lines, la([X]),
    { \+ code_type(X, space) }.

% Merge item line text with next paragraph.

list_item_fixup(Text, Blocks, Item):-
    Blocks = [p(Par)|Rest], !,
    span_parse(Text, Spans),
    append(Spans, ['\n'|Par], NewPar),
    Item = [p(NewPar)|Rest].

% Otherwise add as separated paragraph.

list_item_fixup(Text, Blocks, Item):-
    span_parse(Text, Spans),
    Item = [p(Spans)|Blocks].

%% element_opt(+Item, -Item) is det.
%
% Removes unnecessary p element from
% element when the element contains only
% the single p. Used for list items and blockquotes.

element_opt([p(Spans)], Spans):- !.

element_opt(Elem, Elem).

% Recognizes a code block.

code(pre(code(CodeAtom))) -->
    (la("    ") ; la("\t")), !,
    any(Code), code_block_end, !,
    { phrase(strip_indent(Stripped), Code, "") }, !,
    { atom_codes(CodeAtom, Stripped) }.

% Ends at stream end.

code_block_end --> at_end, !.

% At least one empty line following non-indented line.

code_block_end -->
    empty_line, empty_lines, la([X]),
    { \+ code_type(X, space) }.

% Recognizes block-level HTML.
% No Markdown inside it is processed.

html(\[HTMLAtom]) -->
    la([60,Code]), { code_type(Code, alpha) }, !,
    any(HTML), ((ln, white, ln) ; at_end), !,
    { atom_codes(HTMLAtom, HTML) }.

% Recognizes horisontal rules.
% At least three * or - characters and might
% have spaces between them.

horisontal_rule(hr) -->
    line(Line),
    { exclude('='(32), Line, Clean) },
    { Clean \= [] },
    { maplist('='(42), Clean) ; maplist('='(45), Clean) }.

% Recognizes single line.

line(Line) -->
    any(Line), (ln ; at_end), !.

% Recognizes single paragraph.
% Parses block till first empty line or end.
% Applies span-level parser.

paragraph(p(Spans)) -->
    any(Text), ((ln, white, ln) ; at_end), !,
    { span_parse(Text, Spans) }.

% Recognizes all (zero or more) next empty lines.

empty_lines -->
    empty_line, !,
    empty_lines.

empty_lines --> [].

% Recognizes empty line.

empty_line --> white, ln.

% Consumes as many hash codes as possible.

hashes(Count) -->
    "#", !, hashes(CountNext),
    { Count is CountNext + 1 }.

hashes(0) --> [].

% Consumes as many whitespace (space, tab)
% codes as possible.

white -->
    [ Code ], { code_type(Code, white) },
    !, white.

white --> [].

ln --> "\r\n", !.
ln --> "\n", !.
ln --> "\r".
