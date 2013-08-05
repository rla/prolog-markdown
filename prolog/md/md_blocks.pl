:- module(md_blocks, [
    blocks//1
]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(md/md_dcg)).
:- use_module(library(md/md_lex)).
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

% A block is:
% heading: h1(Text),
% blockquote: blockquote(Blocks),
% unordered list: ul(Items),
% code: pre(code(Code)),
% horisontal rule: hr,
% paragraph: p(Spans).

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

heading(h1(Heading)) -->
    [ Text, Bar ],
    { trim(Bar, Trimmed) },
    { maplist('='(61), Trimmed) },
    { prefix("=", Trimmed), atom_codes(Heading, Text) }.

heading(h2(Heading)) -->
    [ Text, Bar ],
    { trim(Bar, Trimmed) },
    { maplist('='(45), Trimmed) },
    { prefix("-", Trimmed), atom_codes(Heading, Text) }.

heading(Heading) -->
    [ Text ],
    { hash_heading(Text, Level, TextAtom) },
    { atomic_concat('h', Level, Name) },
    { Heading =.. [ Name, TextAtom ] }.

%% hash_heading(+Line:codes, -Level:number, -Heading:atom) is det.
%
% Recognizes heading in the form "# Heading". Gives heading level (1-6)
% and the heading content as an atom.

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
% Items can contain nested Markdown.

list(ul(Items)) -->
    list_items(Items),
    { length(Items, Len), Len > 0 }.

list_items([Item|Items]) -->
    list_item(Item), !,
    list_items(Items).

list_items([]) --> [].

% Recognizes single line item.

list_item(li(Item)) -->
    [ Line ],
    { Line = [X,32|Text], memberchk(X, "*-+") },
    rest_of_block_indent(Lines), % take until indent ends
    { strip_indents(Lines, UnIndented) }, % strip indents
    { trim(Text, Trimmed) }, % trim line at "+ text"
    { phrase(blocks(Blocks), UnIndented, []) }, % parse rest of item
    { list_item_fixup(Trimmed, Blocks, Item) }. % do something with first line

% Merge item line text with next paragraph.
% XXX this behaviour???

list_item_fixup(Text, Blocks, Item):-
    Blocks = [p(Par)|Rest], !,
    span_parse(Text, Spans),
    append(Spans, ['\n'|Par], NewPar),
    Item = [p(NewPar)|Rest].

% Otherwise add as separated paragraph.

list_item_fixup(Text, Blocks, Item):-
    span_parse(Text, Spans),
    Item = [p(Spans)|Blocks].

% Recognizes a code block.

code(pre(code(CodeAtom))) -->
    [ Line ],
    { prefix("    ", Line) ; prefix("\t", Line) }, !,
    rest_of_block_indent(Lines),
    { strip_indents([Line|Lines], UnIndented) },
    { merge_with_ln(UnIndented, Code) },
    { atom_codes(CodeAtom, Code) }.

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

%% merge_with_ln(+Lines:list, -Result:codes) is det.
%
% Merges list of lines by putting newline (\n)
% between them.

merge_with_ln([], []).

merge_with_ln([Line], Line):- !.

% \n is 10.

merge_with_ln([Line|Lines], Result):-
    merge_with_ln(Lines, Tmp),
    append(Line, [10|Tmp], Result).

% Recognizes horisontal rules.
% At least three * or - characters and might
% have spaces between them.

horisontal_rule(hr) -->
    [ Line ],
    { delete_all(Line, 32, Trimmed) },
    { Trimmed = "***" }.

horisontal_rule(hr) -->
    [ Line ],
    { delete_all(Line, 32, Trimmed) },
    { Trimmed = "---" }.

% Recognizes single paragraph.
% Parses block till first empty line,
% then concatenates block lines with
% newlines between. Applies span elements
% parser at the end.

paragraph(p(Spans)) -->
    rest_of_block(Blob),
    { merge_with_ln(Blob, Text) },
    { span_parse(Text, Spans) }.

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
