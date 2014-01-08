:- begin_tests(md_file).
:- use_module(prolog/md/md_parse).

test(file):-
    md_parse_file('data/file.md', Blocks),
    assertion(nth0(0, Blocks, h1('Test file'))),
    assertion(nth0(1, Blocks, blockquote([\[_]]))),
    assertion(nth0(2, Blocks, h2('Headings'))),
    assertion(nth0(3, Blocks, blockquote(Headings))),
    assertion(nth0(0, Headings, h1('With line'))),
    assertion(nth0(1, Headings, h2('Second level'))),
    assertion(nth0(3, Headings, h3('Hashed at end'))),
    assertion(nth0(4, Blocks, h2('Lists'))),
    assertion(nth0(5, Blocks, ul(Items))),
    assertion(nth0(0, Items, li(\['item1']))),
    assertion(nth0(1, Items, li(\['item2']))),
    assertion(nth0(2, Items, li(\['item3']))),
    assertion(nth0(6, Blocks, h3('Nested list'))),
    assertion(nth0(7, Blocks, ul(NestItems))),
    assertion(NestItems = [li([ul(_)]), li(ul(_))]),
    assertion(nth0(8, Blocks, p([\['A paragraph.']]))),
    assertion(nth0(9, Blocks, pre(code('// code block\np1 :- p2, p3.\n')))),
    assertion(nth0(10, Blocks, \['<div>\n  <div>HTML</div>\n</div>'])).

:- end_tests(md_file).