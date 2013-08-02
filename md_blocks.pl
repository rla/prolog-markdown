:- module(md_blocks, [
    blocks//1
]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(md_dcg).
:- use_module(md_lex).

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

% A block is:
% heading(Level, Text),
% blockquote(Blocks),
% list(Items),
% code(Text),
% horisontal_rule,
% paragraph(Text).

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

% Blob of text, applied when
% no other block rule matches.

block(Blob) -->
    paragraph(Blob).

% Recognizes headings:
%
% This is an H1
% =============
%
% This is an H2
% -------------
%
% # This is an H1 #
% ## This is an H2 ##

heading(heading(1, Heading)) -->
    [ Text, Bar ],
    { prefix("=", Bar), atom_codes(Heading, Text) }.

heading(heading(2, Heading)) -->
    [ Text, Bar ],
    { prefix("-", Bar), atom_codes(Heading, Text) }.

heading(heading(Level, Heading)) -->
    [ Text ],
    { hash_heading(Text, Level, Heading) }.

hash_heading(Line, Level, Heading):-
    prefix(Hashes, "######"),
    length(Hashes, Level), Level > 0,
    append(Hashes, " ", Prefix),
    append(Prefix, Text, Line),
    delete_from_suffix(Text, 35, Codes),
    trim(Codes, Trimmed),
    atom_codes(Heading, Trimmed).

% Recognizes blockquote.
% Blockquotes are in the form
% > line1
% > line2
% > line3
% Blockquotes can contain nested Markdown.
% Parsing is done by recognizing the the blockquote
% start first ("> "), reading in whole block until first
% empty line, then stripping "> " from lines and applying
% the block parser recursively.

blockquote(blockquote(Quote)) -->
    [ Text ],
    { prefix("> ", Text) },
    rest_of_block(Lines),
    { strip_bq_starts([Text|Lines], Stripped) },
    { phrase(blocks(Quote), Stripped, []) }.

%% strip_bq_starts(+Lines:list, -Lines:list) is det.
%
% Strips blockquote start symbols from given lines.

strip_bq_starts(In, Out):-
    maplist(strip_bq_start, In, Out).

%% strip_bq_start(+Line:codes, -Line:codes) is det.
%
% Strips blockquote symbol (> ) from the beginning
% of the line. Allows for no space following >.

strip_bq_start(In, Out):-
    append("> ", Out, In), !.

strip_bq_start(In, Out):-
    append(">", Out, In), !.

% Recognizes unordered list containing one or more items.
% Unordered lists are in the form:
% + item1
% + item2
% + item3
% Items can span multiple lines:
% + line1
%   line2
% Item can contain nested Markdown.

list(unordered_list(Items)) -->
    list_items(Items),
    { length(Items, Len), Len > 0 }.

list_items([Item|Items]) -->
    list_item(Item), !,
    list_items(Items).

list_items([]) --> [].

% Recognizes single line item.

list_item(Blocks) -->
    [ Line ],
    { Line = [X,32|Text], memberchk(X, "*-+") },
    rest_of_block_indent(Lines),
    { strip_indents(Lines, UnIndented) },
    { trim(Text, Trimmed) },
    { phrase(blocks(Blocks), [Trimmed|UnIndented], []) }.

% Recognizes a code block.

code(code(Code)) -->
    [ Line ],
    { prefix("    ", Line) ; prefix("\t", Line) }, !,
    rest_of_block_indent(Lines),
    { strip_indents([Line|Lines], UnIndented) },
    { merge_with_ln(UnIndented, Code) }.

%% strip_indents(+Lines:list, -Lines:list) is det.
%
% Strips one level of indentation from given lines.

strip_indents(In, Out):-
    maplist(strip_indent, In, Out).

%% strip_indent(+Line:codes, -Line:codes) is det.
%
% Strips one level (4 spaces or one tab) from
% given line. Fails if line has no indent.

strip_indent(In, Out):-
    append("    ", Out, In).

strip_indent(In, Out):-
    append("\t", Out, In).

%% merge_with_ln(+Lines:list, -Result:atom) is det.
%
% Merges list of lines by putting newline (\n)
% between them.

merge_with_ln(Lines, Result):-
    maplist(atom_codes, Atoms, Lines),
    atomic_list_concat(Atoms, '\n', Result).

% Recognizes horisontal rules.
% At least three * or - characters and might
% have spaces between them.

horisontal_rule(horisontal_rule) -->
    [ Line ],
    { delete_all(Line, 32, Trimmed) },
    { Trimmed = "***" }.

horisontal_rule(horisontal_rule) -->
    [ Line ],
    { delete_all(Line, 32, Trimmed) },
    { Trimmed = "---" }.

% Recognizes single paragraph.
% Parses block till first empty line,
% then concatenates block lines with
% newlines between.

paragraph(paragraph(Text)) -->
    rest_of_block(Blob),
    { merge_with_ln(Blob, Text) }.

% Recognizes all (zero or more) next non-empty lines.

rest_of_block([Line|Lines]) -->
    [ Line ],
    { Line \= [] }, !,
    rest_of_block(Lines).

rest_of_block([]) --> [].

% Recognizes all (zero or more) next indented lines.
% FIXME consider empty lines.

rest_of_block_indent([Line|Lines]) -->
    [ Line ],
    { prefix("  ", Line) ; prefix("\t", Line) }, !,
    rest_of_block_indent(Lines).

rest_of_block_indent([]) --> [].

% Recognizes all (zero or more) next empty lines.

empty_lines -->
    empty_line, !,
    empty_lines.

empty_lines --> [].

% Recognizes empty line.

empty_line --> [[]].

% Deletes all unifiying elements
% from the list.

delete_all([], _, []).

delete_all([Elem|In], Elem, Out):- !,
    delete_all(In, Elem, Out).

delete_all([Elem|In], Check, [Elem|Out]):-
    delete_all(In, Check, Out).

% Deletes all unifying elements from
% the beginning of the list.

delete_from_prefix([], _, []).

delete_from_prefix([Elem|In], Elem, Out):- !,
    delete_from_prefix(In, Elem, Out).

delete_from_prefix(In, _, In).

% Deletes all unifying elements from
% the end of the list.

delete_from_suffix(In, Check, Out):-
    reverse(In, RevIn),
    delete_from_prefix(RevIn, Check, RevOut),
    reverse(RevOut, Out).

% Trims whitespaces from the beginning of the list of codes.

trim_left([Code|Codes], Result):-
    code_type(Code, space), !,
    trim_left(Codes, Result).

trim_left(Codes, Codes).

% Trims whitespace from the end of the list of codes.

trim_right(Codes, Result):-
    reverse(Codes, CodesR),
    trim_left(CodesR, ResultR),
    reverse(ResultR, Result).

% Trims whitespace from both sides of the list of codes.

trim(Codes, Result):-
    trim_left(Codes, Tmp1),
    reverse(Tmp1, Tmp2),
    trim_left(Tmp2, Tmp3),
    reverse(Tmp3, Result).