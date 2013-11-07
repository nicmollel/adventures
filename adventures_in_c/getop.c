/* -*- mode: c; ; fill-column: 80; coding: utf-8-unix; -*- */
/*
  Nic M
  Nov 2013
  
  Adventures in C: Commandline Arguments
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> 		/* getopt */
#include <getopt.h> 		/* getopt_long, getopt_long_only */

static const unsigned short opt_flag; /* useful in option struct declaration */

static int short_options(const int *argc, char ** argv){
  /* Process Short options only with getop and return optind*/
  unsigned short aflag,bflag,cflag = 0;
  char *cvalue,*dvalue = NULL;

  int opt; 
  while((opt = getopt(*argc,argv,"abc:d:")) != 0){
    switch(opt){
    case 'a':
      aflag = 1; 
      break;
    case 'b':
      bflag = 1; 
      break; 
    case 'c':
      cvalue = optarg; 
      break;
    case 'd': 
      dvalue = optarg; 
      break;
    case '?': 			/* failed to process arg */
      if (optopt == 'c'||optopt == 'd')
	fprintf(stderr, "Option -%c requires an argument\n",optopt);
      else if (isprint (optopt))
	fprintf(stderr,"Unknown option -%c\n",optopt);
      else 
	fprintf(stderr,"Unkown option character `\\x%x .\n", optopt);
      return -1;
    default:
      abort();
    }
  }
  return optind; 		/* last position in argv */
}

int main(const int argc, char **argv){
  /*
    main is how the outside world gets into the application/this file
    and cannot therefore be decrared as static!
  */
  int post_optc;
  if (argc > 0)
    post_optc = short_options(&argc,argv);

  return 0;
}

