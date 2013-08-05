% Sets up path to load Markdown library files from.

:- asserta(user:file_search_path(library, 'prolog')).

% Loads all tests. Names are relative to CWD.

:- load_files([
    test/scan,
    test/block,
    test/span,
    test/html
], [ if(not_loaded) ]).

md_tests:-
    run_tests([ md_block, md_span, md_html, md_scan ]).
