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
#include <pwd.h>		/* passwd structure */
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h> 

static const unsigned short opt_flag; /* useful in option struct declaration */

static void *run(const unsigned short *actionFlag,
		 char *path,const int *nsec);

static char  *gethomedir(void){
  /* Return the current user home directory irrespctive of the value of
     HOME environment variable */
  struct passwd *p;
  errno = 0;
  p = getpwuid(getuid());
  if (p)
    return p->pw_dir;		/* on some system, fancy getenv("HOME") */
  else{
    if (errno){
      perror("getpwuid");
      exit(EXIT_FAILURE);
    }
    return NULL;
  }

}

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
  }else 
    fprintf(stdout,"FILE:%s\n",path);
  return 0;
}

static int short_options(const int *argc, char ** argv){
  /* Process Short options only with getop and return optind*/
  unsigned short actionFlag = 0;
  char *path = NULL;
  int nsec = 0;

  if (*argc <= 1){
    /* No arguments provided */
    fprintf(stderr, "Usage: %s [-Lh][-t nsec] [-p path] ...\n",argv[0]);
    exit(EXIT_FAILURE);
  } else {

    int opt; 
    while((opt = getopt(*argc,argv,"Lht:p:")) != -1){
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
	exit(EXIT_FAILURE);
      }
    }
  }
  
  run(&actionFlag,path,&nsec);

  return optind; 		/* last position in argv */
}

static void *run(const unsigned short *actionFlag,
		 char *path,const int *nsec)
{
  if (path)
    list_path(path);
  else{
    path = gethomedir();
    switch (*actionFlag){
    case 1: 
      fprintf(stdout,"HOME:%s\n",path);
      break;
    case 2:
      list_path(path);
      break;
    }
  }

  if(*nsec){
    fprintf(stdout,"sleeping for %dsec....\n",*nsec);
    fflush(stdout);
    sleep(*nsec);
    fprintf(stdout,"DONE\n");
  }

  return NULL;
}
int main(const int argc, char **argv){
  /*
    main is how the outside world gets into the application/this file
    and cannot therefore be decrared as static!
  */
  int post_opt = short_options(&argc,argv);

  return 0;
}

