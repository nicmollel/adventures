# -*- mode: makefile-gmake; coding: utf-8; fill-column: 72; -*-
# Makefile for Adventures in C project
# Nic M, Nov 2013

getopt: getopt.o
	cc $(CFLAGS) -o getopt getopt.o
getopt.o: getopt.c
	cc -c $(CFLAGS) getopt.c 
clean:
	rm *.o getopt 
