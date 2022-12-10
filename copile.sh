#!/bin/bash

if [ $# -eq 1 ];
then
  dune exec -- CoBruh -c test/input.bruh > main.ll
  clang main.ll -o $1  
  exit 1
else
  exit 1
fi
