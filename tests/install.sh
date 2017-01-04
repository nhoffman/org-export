#!/bin/bash

set -e

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew update
    brew install emacs
    brew linkapps emacs
elif [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    if [[ "$EMACS_VERSION" == "24" ]]; then
        sudo add-apt-repository -y ppa:cassou/emacs
        sudo apt-get -qq update
        sudo apt-get -qq -f install
        sudo apt-get -qq install emacs24
    elif [[ "$EMACS_VERSION" == "25" ]]; then
        sudo add-apt-repository -y --force-yes ppa:kelleyk/emacs
        sudo apt-get -qq update
        sudo apt-get -qq -f install
        sudo apt-get -qq install emacs25
    fi
else
    echo "unrecognized value of TRAVIS_OS_NAME: $TRAVIS_OS_NAME"
fi
