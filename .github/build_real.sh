#!/bin/bash
set -e

# Export variables to make sure they are present for build.
# The user should define them beforehand.
if [[ -z "$COMPLEXIFY_DIR" ]]; then
    export COMPLEXIFY_DIR=$MDOLAB_REPO_DIR/complexify/opt-$COMPILERS
    export LD_LIBRARY_PATH=$COMPLEXIFY_DIR/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
fi

cp $CONFIG_FILE config/config.mk
make
make install
pip install .[testing]
