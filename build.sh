#!/bin/sh
cproto machine.c util.c > prototypes.h
gcc -ggdb -o pm main.c machine.c util.c
etags *.c *.h
