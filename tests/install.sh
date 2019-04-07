#!/bin/bash

set -e

if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew update
    brew install gnutls
    brew install emacs
    brew linkapps emacs
elif [[ "$TRAVIS_OS_NAME" == "linux" ]]; then
    if [[ "$EMACS_VERSION" == "24" ]]; then
        PPA="ppa:cassou/emacs"
    else
	# emacs 25 and 26
	PPA="ppa:kelleyk/emacs"
    fi
    sudo add-apt-repository -y "$PPA"
    sudo apt-get -qq update
    sudo apt-get -qq -f install
    sudo apt-get -qq install emacs$EMACS_VERSION
else
    echo "unrecognized value of TRAVIS_OS_NAME: $TRAVIS_OS_NAME"
fi
