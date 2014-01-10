# Prolog-markdown

Markdown parser implemented in Prolog. Compatible with [SWI-Prolog](http://www.swi-prolog.org/) as the
output tree is for direct use by [html/1](http://www.swi-prolog.org/pldoc/doc_for?object=html/1).

The specification for the parser was taken from
<http://daringfireball.net/projects/markdown/syntax> (Gruber's Markdown).

## Example usage

Parse into a structure usable by
[html/1](http://www.swi-prolog.org/pldoc/doc_for?object=html/1).

    :- use_module(library(md/md_parse)).

    md_parse("# Hello #", Blocks).

Convert into an HTML atom:

    :- use_module(library(md/md_parse)).

    md_html("# Hello #", Html).

## Deviations from the Gruber's Markdown

 * Various cases for tight markup (no separate lines between blocks).
 * No special encoding for mail addresses. I think that would confuse
   very few bots.
 * Line break rule creates `<br>` not `<br />`.

## Installation

Requires SWI-Prolog 7.x.

    pack_install('http://packs.rlaanemets.com/markdown/markdown-*.tgz')

## TODO

 * Line break rule with spaces
 * GitHub-flavoured code blocks
 * Sublist with less than 4-space indent
 * Horisontal ruler ending a list item

## License

The MIT License. See the LICENSE file.
