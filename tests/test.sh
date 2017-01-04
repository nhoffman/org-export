#!/bin/bash -x

set -e

./org-export html --infile README.org
./org-export html --infile tests/test_python.org

# code block producing XXXXX should not have been evaluated...
test -z $(grep XXXXX tests/test_python.html) || echo fail

# but code block producing YYYYY should have been
grep -q YYYYY tests/test_python.html || echo fail

./org-export html --infile tests/test_shell.org

# code block producing XXXXX should not have been evaluated...
test -z $(grep '1 2 3 4 5' tests/test_shell.html) || echo fail

# but code block producing YYYYY should have been
grep -q '6 7 8 9 10' tests/test_shell.html || echo fail
