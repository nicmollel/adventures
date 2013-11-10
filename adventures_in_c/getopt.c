/* -*- mode: c; ; fill-column: 80; coding: utf-8-unix; -*- */
/*
  Nic M
  Nov 2013
  
  Adventures in C: Commandline Arguments
  
  Take arguments to do the following: 
  - List given path (-p)
  - Sleep for given time (-t)
  - Show user's home directory (-h)
  - List user's home directory (-L)
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> 		/* getopt */
#include <getopt.h> 		/* getopt_long, getopt_long_only */
#include <spawn.h>
#include <sys/stat.h>
#include <sys/types.h> 

static const unsigned short opt_flag; /* useful in option struct declaration */
static int list_path(char *path){
  /* If the file exists, spawn `ls` on it */
  struct stat sb;
  pid_t pid;
  char *const ls[] = {
    "ls",
    "-l",
    path,
    NULL};

  if (stat(path,&sb) != 0){
    perror("stat");
    exit(EXIT_FAILURE);
  }

  if(S_ISDIR(sb.st_mode)){
    if(posix_spawnp(&pid,ls[0],NULL,NULL,ls,NULL) != 0){
      perror("posix_spawn");
      exit(EXIT_FAILURE);
    }
  }

  return 0;
}

static int short_options(const int *argc, char ** argv){
  /* Process Short options only with getop and return optind*/
  unsigned short actionFlag = 0;
  char *path = NULL;
  int nsec;

  int opt; 
  while((opt = getopt(*argc,argv,"Lht:p:")) != 0){
    switch(opt){
    case 'L':
      actionFlag = 2;
      break;
    case 'h':
      actionFlag = 1;
      break; 
    case 't':
      nsec = atoi(optarg);
      break;
    case 'p': 
      path  = optarg; 
      break;
    case '?': 			/* failed to process arg */
      if (optopt == 't'||optopt == 'p')
	fprintf(stderr, "Option -%c requires an argument\n",optopt);
      else if (isprint (optopt))
	fprintf(stderr,"Unknown option -%c\n",optopt);
      else 
	fprintf(stderr,"Unkown option character `\\x%x .\n", optopt);
      return -1;
    default:			/* No arguments provided */
      fprintf(stderr, "Usage: %s [-Lh][-t nsec] [-p path] ...\n",argv[0]);
      exit(EXIT_FAILURE);
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

