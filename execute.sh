#!/bin/bash

# DEPENDENCIES: use opam to install: menhir, str (regex library)
#               MUST HAVE environment variable with name: GOLITEGROUP11 set to the output of pwd command, pwd must be executed in the same directory as this script




#
# Execute the generated code
#
# This script takes in the filename of the ORIGINAL program AFTER the codegen has been run
#   i.e. programs/3-semantics+codegen/valid/test.min
#
# It MUST then
#   (a) Compile the GENERATED file
#         i.e. programs/3-semantics+codegen/valid/test.c
#   (b) Execute the compiled code
#
# (if no compilation is needed, then only perform step b)
#
# To conform with the verification script, this script MUST:
#   (a) Output ONLY the execution
#   (b) Exit with status code 0 for success, not 0 otherwise

# rm ${1%.*}.out 2> /dev/null

# You MUST replace the following line with the command to compile your generated code
# Note the bash replacement which changes:
#   programs/3-semantics+codegen/valid/test.min -> programs/3-semantics+codegen/valid/test.c
# stdout is redirected to /dev/null
if [ ! -d ./generatedCode ]
then
    echo "No generated code directory found, please add this dir to project root where it's subdirectories are: programs, programs-solutions and their subdirectories"
    exit 1
fi


GO_FILE="$1"
JAVA_FILE="$(./src/main.native 'path_to_gen_file' $GO_FILE)"

CLASS_NAME="$(./src/main.native 'class_name' $GO_FILE)"

DIR="$(dirname "$JAVA_FILE")"

if [ ! -f "$JAVA_FILE" ]
then
    echo "Cannot compile generated file since it could not be found"
    exit 1
fi

javac "$JAVA_FILE" > /dev/null


cd "$DIR" && java "$CLASS_NAME"

# return
# gcc -std=c11 -o ${1%.*}.out $FILENAME > /dev/null

# make clean -C ./src > /dev/null
# make -C ./src > /dev/null
# echo "compiled 
# javac "$FILENAME"

# You MUST replace the following line with the command to execute your compiled code
# Note the bash replacement which changes:
#   programs/3-semantics+codegen/valid/test.min -> programs/3-semantics+codegen/valid/test.out
# ./src/main.native "$1" "$2"

# ./${1%.*}.out

# Lastly, we propagate the exit code
exit $?
