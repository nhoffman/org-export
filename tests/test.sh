#!/bin/bash -x

set -e

./org-export html --infile README.org

find ~/.org-export -name 'ob-*.el' | sort

./org-export html --infile tests/test_python.org

# code block producing XXXXX should not have been evaluated...
test -z $(grep XXXXX tests/test_python.html)

# but code block producing YYYYY should have been
grep -q YYYYY tests/test_python.html

./org-export html --infile tests/test_shell.org

# code block producing XXXXX should not have been evaluated...
test -z $(grep XXXXX tests/test_shell.html)

# but code block producing YYYYY should have been
grep -q YYYYY tests/test_shell.html
