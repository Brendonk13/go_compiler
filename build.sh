#!/bin/bash

# Build the compiler
#
# You MUST replace the following commands with the commands for building your compiler

# DEPENDENCIES: use opam to install: menhir, str (regex library)
#               MUST HAVE environment variable with name: GOLITEGROUP11 set to the output of pwd command, pwd must be executed in the same directory as this script
make clean -C ./src
make -C ./src
