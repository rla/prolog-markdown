:- module(md_parse, [
    parse/2
]).

:- use_module(library(readutil)).
:- use_module(library(md/md_blocks)).
:- use_module(library(md/md_lex)).

% Markdown parser. Has stricter rules for
% parsing Markdown than original spec.
% Base rules: http://daringfireball.net/projects/markdown/syntax/
% No lazy rules.
% No linebreak rule: "end a line with two or more spaces, then type return".

parse(Codes, Blocks):-
    % lex(Codes, Tokens),
    phrase(blocks(Out), Codes, ""),
    Blocks = Out.