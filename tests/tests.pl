% Loads all tests. Names are relative to CWD.

:- load_files([
    tests/link,
    tests/header,
    tests/block,
    tests/span_link,
    tests/span,
    tests/html,
    tests/file
], [ if(not_loaded) ]).
