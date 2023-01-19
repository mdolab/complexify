#!/bin/bash
set -e
cd tests

# Export variables to make sure they are present for build.
# The user should define them beforehand.
if [[ -z "$COMPLEXIFY_DIR" ]]; then
    export COMPLEXIFY_DIR=$MDOLAB_REPO_DIR/complexify/install
    export LD_LIBRARY_PATH=$COMPLEXIFY_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
fi

# Test the fortran library
make
./test_complexify

# Test python includes (disabled for now)
#testflo -v -n 1 --coverage --coverpkg complexify
