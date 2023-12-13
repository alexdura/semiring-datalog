#!/usr/bin/env bash

export LD_LIBRARY_PATH=/work/projects/llvm-project/build-release/lib

CLOG_JAR=/work/projects/metadl/compiler.jar

TMP_DIR=`pwd`/tmp
OUT_DIR=`pwd`/out
LD_LIBRARY_PATH=/work/projects/llvm-project/build-release/lib

if [ ! -d $TMP_DIR ] ; then
    mkdir -p $TMP_DIR
fi

if [ ! -d $OUT_DIR ] ; then
    mkdir -p $OUT_DIR
fi

# extract the facts
java -jar "$CLOG_JAR" -e metadl -L c4 -D "$TMP_DIR" -S srcs.csv \
     clog-programs/andersen.mdl \
     -Xclang='--extra-arg -I/work/projects/metadl/tests/clang/evaluation/src/include'

# run the other parts
stack run -- --input $TMP_DIR --output $OUT_DIR
