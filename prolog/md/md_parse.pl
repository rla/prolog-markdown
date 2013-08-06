:- module(md_parse, [
    md_parse/2
]).

:- use_module(library(readutil)).
:- use_module(library(md/md_blocks)).

% Markdown parser.
% Base rules: http://daringfireball.net/projects/markdown/syntax/
% No linebreak rule: "end a line with two or more spaces, then type return".

md_parse(Codes, Blocks):-
    phrase(blocks(Out), Codes, ""), !,
    Blocks = Out.