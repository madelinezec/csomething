#!/bin/bash
./csomething $1 &&
llc-6.0 out.s -filetype=obj &&
g++ out.s.o stdlib/stdlib.a -lgomp -o out

