# prolog-markdown

Markdown parser implemented in Prolog. Compatible with [SWI-Prolog](http://www.swi-prolog.org/) as the
output tree is for direct use by [html/1](http://www.swi-prolog.org/pldoc/doc_for?object=html/1).

The "specification" for the parser was taken from <http://daringfireball.net/projects/markdown/syntax>.

## Installation

TODO: needs better description. Currently a package has to be built, then it can be installed
with [pack_install/1](http://www.swi-prolog.org/pldoc/doc_for?object=pack_install/1).

## Usage

Parse into a structure usable by
[html/1](http://www.swi-prolog.org/pldoc/doc_for?object=html/1).

    :- use_module(library(md/md_parse)).

    md_parse("# Hello #", Blocks).

Convert into HTML atom:

    :- use_module(library(md/md_parse)).

    md_html("# Hello #", Html).

## TODO

* Reference links
* Allow single quotes for inline link title
* Ordered lists
* Line break rule with spaces
* GitHub-flavoured code blocks

## License (The MIT License)

    Copyright (c) 2013-2014 Raivo Laanemets

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.