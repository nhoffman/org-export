#!/bin/bash

test_pattern_present(){
    # usage: output_yes file_path substring
    echo "Expecting $2 in $1"
    if [[ $(grep $2 $1) ]]; then
        echo ok
    else
        echo failed
        exit 1
    fi
}

test_pattern_missing(){
    # usage: output_yes file_path substring
    echo "Not expecting $2 in $1"
    if [[ $(grep $2 $1) ]]; then
        echo failed
        exit 1
    else
        echo ok
    fi
}

out=test_output
mkdir -p $out

./org-export html --infile README.org --outfile $out/README.html

outfile=$out/py1.1.html
./org-export html --infile tests/py1.org --outfile $outfile
test_pattern_missing $outfile XXXXX

outfile=$out/py1.2.html
./org-export html --infile tests/py1.org --outfile $outfile --evaluate
test_pattern_present $outfile XXXXX

outfile=$out/py2.1.html
./org-export html --infile tests/py2.org --outfile $outfile
test_pattern_present $outfile XXXXX

outfile=$out/sh1.1.html
./org-export html --infile tests/sh1.org --outfile $outfile
test_pattern_present $outfile XXXXX
test_pattern_present $outfile ZZZZZ
test_pattern_missing $outfile NNNNN

outfile=$out/el1.1.html
./org-export html --infile tests/el1.org --outfile $outfile \
             --config '(setq testval "XXXXX")'
test_pattern_present $outfile XXXXX

outfile=$out/el1.2.html
./org-export html --infile tests/el1.org --outfile $outfile \
             --config-file tests/test-config.el
test_pattern_present $outfile YYYYY
