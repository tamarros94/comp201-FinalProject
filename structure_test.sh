#!/bin/bash

BASE_DIR=$(pwd)
PATCH=$1
AUTHENTICITY=readme.txt
CODE=Makefile
STATEMENT="realize that should our code be found to contain code from other sources, that a formal case shall be opened against"
PROBLEM=0
GIT_REPO="https://www.cs.bgu.ac.il/~comp201/compiler"
LOCAL_REPO=compiler
TMP_FOLDER=$(mktemp -d)

cleanup () {
    #echo "cleaning up temporary files and exiting."
    rm -rf $TMP_FOLDER
}

if [ $# -lt 1 ]; then
    PATCH="compiler/compiler.patch"
fi

if ! [ -f $PATCH ]; then
    echo "ERROR: The patch file '$PATCH' cannot be found. Please provide the relative path filename to your patch file."
    exit 2
fi

cd $TMP_FOLDER
GIT_SSL_NO_VERIFY=true git clone -q $GIT_REPO
if [ "$?" -ne 0 ]; then
    echo "ERROR: There was a problem creating a temporary clone of the project repository. There might be a problem with your network connection. The structure test cannot be completed."
    cleanup
    exit 2
fi

cd $LOCAL_REPO
GIT_SSL_NO_VERIFY=true git apply --ignore-whitespace --whitespace=nowarn $BASE_DIR/$PATCH
if [ "$?" -ne 0 ]; then
    echo "ERROR: The contents of your patch file are invalid and git cannot apply it. The structure test cannot be completed."
    cleanup
    exit 2
fi


if ! [ -f $AUTHENTICITY ]; then
    echo "ERROR: Your submission is missing the authenticity statement file ($AUTHENTICITY)."
    PROBLEM=1
else 
    ID=$(egrep -e '[0-9]{7,10}' $AUTHENTICITY)
    STMNT=$(cat $AUTHENTICITY | tr -d [:space:] | grep -i "$(echo "$STATEMENT" | tr -d [:space:])")

    if [ -z "$ID" ] || [ -z "$STMNT" ] ; then
	echo "ERROR: Your authenticity statement (in $AUTHENTICITY) is incomplete."
	PROBLEM=1
    fi
fi

if ! [ -f $CODE ]; then
    echo "ERROR: Your submission is missing the required file: $CODE."
    PROBLEM=1
fi

cd $TMP_FOLDER
touch tmp.scm
make -f $LOCAL_REPO/$CODE tmp
if ! [ -f tmp ]; then
    echo "ERROR: Tried to compile an empty file. Could not find the output executable. Either there is a problem with your compiler, or your Makefile is placing the executable file in the expected location."
    PROBLEM=1
fi

if [ $PROBLEM -ne 0 ]; then
    echo "!!! Your submission is invalid. Please correct the problems and try again. !!!"
else 
    echo "Your submission passed the structure test.
This does not mean that your assignment is correct, only that we can test it properly."
fi

cleanup
exit $PROBLEM
