#!/bin/bash -x

set -e

brew update
brew install emacs
brew linkapps emacs
