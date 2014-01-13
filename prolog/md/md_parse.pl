:- module(md_parse, [
    md_parse_codes/2,  % +Codes, -Blocks
    md_parse_stream/2, % +Stream, -Blocks
    md_parse_file/2,   % +File, -Blocks
    md_parse_string/2, % +String, -Blocks
    md_html/2,         % +Codes, -HtmlAtom
    md_html_stream/2,  % +Stream, -HtmlAtom
    md_html_file/2     % +File, -HtmlAtom
]).

/** <module> Prolog Markdown parser

Top-level module for parsing Markdown. Contains
some convenience predicated.
*/

:- use_module(library(http/html_write)).
:- use_module(library(readutil)).

:- use_module(md_links).
:- use_module(md_blocks).

%! md_parse_string(+String, -Blocks) is det.
%
% Same as md_parse_codes/2 but takes
% a string instead.

md_parse_string(String, Blocks):-
    string_codes(String, Codes),
    md_parse_codes(Codes, Blocks).

%! md_parse_codes(+Codes, -Blocks) is det.
%
% Parses Markdown into a structure suitable in use
% with html//1.

md_parse_codes(Codes, Blocks):-
    md_links(Codes, Tmp),
    phrase(md_blocks(Out), Tmp), !,
    Blocks = Out.

%! md_parse_stream(+Stream, -Blocks) is det.
%
% Same as md_parse/2 but reads input from stream.

md_parse_stream(Stream, Blocks):-
    read_stream_to_codes(Stream, Codes),
    md_parse_codes(Codes, Blocks).

%! md_parse_file(+Name, -Blocks) is det.
%
% Same as md_parse/2 but reads input from file.

md_parse_file(File, Blocks):-
    read_file_to_codes(File, Codes, []),
    md_parse_codes(Codes, Blocks).

%! md_html(+Codes, -Html) is det.
%
% Converts Markdown into HTML atom.

md_html(Codes, Html):-
    md_parse_codes(Codes, Blocks),
    phrase(html(Blocks), Tokens),
    with_output_to(atom(Html), print_html(Tokens)).

%! md_html_stream(+Stream, -Html) is det.
%
% Same as md_html/2 but reads input from stream.

md_html_stream(Stream, Html):-
    read_stream_to_codes(Stream, Codes),
    md_html(Codes, Html).

%! md_html_file(+Name, -Html) is det.
%
% Same as md_html/2 but reads input from file.

md_html_file(File, Html):-
    read_file_to_codes(File, Codes, []),
    md_html(Codes, Html).
