#!/bin/bash

set -e

out=test_output
mkdir -p $out

./org-export html --infile README.org --outfile $out/README.html

py_out=$out/test_python.html
./org-export html --infile tests/test_python.org --outfile $py_out

# code block producing XXXXX should not have been evaluated...
test -z $(grep XXXXX $py_out)

# but code block producing YYYYY should have been
grep -q YYYYY $py_out

sh_out=$out/test_shell.html
./org-export html --infile tests/test_shell.org --outfile $sh_out

# code block producing XXXXX should not have been evaluated...
test -z $(grep XXXXX $sh_out)

# but code block producing YYYYY should have been
grep -q YYYYY $sh_out
