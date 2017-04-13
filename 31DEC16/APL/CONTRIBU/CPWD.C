/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/* Usage: CPWD $AIPS_VERSION `/bin/pwd`                               */
/*--------------------------------------------------------------------*/
/* Given the value of $AIPS_VERSION and any path name - return the    */
/* equivalent standard aips path name if one exists, or the original  */
/* path name if one doesn't. The program searches backwards through   */
/* the given path name for the last component part of the             */
/* $AIPS_VERSION path name (Usually something like 15APR92). It then  */
/* replaces the preceding part of the path name with the prefix of    */
/* $AIPS_VERION, in order to produce a path name in the form of those */
/* in LIBR.DAT.                                                       */
/*                                                                    */
/* The program gets around the problem where $AIPS_ROOT is mounted    */
/* with different names on different host machines.                   */
/* mcs@phobos.caltech.edu   3rd June 1992                             */
/*------------------------------------------------------------------- */
                                       /* We use our own prototypes   */
                                       /* for string functions if we  */
                                       /* are not using an ANSI C     */
                                       /* compiler so that we don't   */
                                       /* have to worry about the use */
                                       /* of <strings.h> on older BSD */
                                       /* systems.                    */
#ifdef __STDC__
#include <string.h>
#else
extern int strcmp();
extern char *strcpy();
extern int strlen();
#endif
#include <stdio.h>

main(argc, argv)
     int argc;
     char *argv[];
{

  char *aips_vers;                      /* String for $AIPS_VERSION. */
  int vers_len;                         /* Length of aips_vers string */
  char *aips_path;                      /* Pathname to be transformed */
					/* to AIPS path name */
  int path_len;                         /* Length of aips_path string */
  char *last_part;                      /* Will point to start of */
					/* last compenent of the */
					/* $AIPS_VERSION path name. */
  int last_len;                         /* Length of last_part */
					/* sub-string */
  char *word_start;                     /* Will hold a pointer to the */
					/* current character while */
					/* searching through */
                                        /* aips_path for "/"s */
  char *word_end;                       /* Will hold a pointer to the */
					/* last "/" in aips_path */
  int i;
                                        /* Need 2 arguments. */
  if(argc != 3) {
    fprintf(stderr, "PWD: Two arguments required\n");
    exit(1);
  }
                                        /* First is $AIPS_VERSION. */
  aips_vers = argv[1];
                                        /* Second is input pathname */
  aips_path = argv[2];
                                        /* Get their lengths */
  vers_len = strlen(aips_vers);
  path_len = strlen(aips_path);
                                        /* Search bakwards through */
					/* $AIPS_VERSION for the last */
					/* "/" symbol to find  start */
					/* of final component of the */
					/* AIPS_VERSION path name. */
  for(i=vers_len-1; i>=0; i--) {
    last_part = &aips_vers[i];
    if( *last_part == '/')
      break;
  };
                                        /* If no / was found, this is */
					/* not a valid path.  */
  if(*last_part != '/') {
    fprintf(stderr, "PWD: Invalid AIPS_VERSION path name given.\n");
    exit(1);
  };
                                        /* Get length of substring */
					/* following last_part. */
  last_len=strlen(last_part);
                                        /* Now search backwards */
					/* through aips_path for each */
					/* occurence of the / */
					/* delimiter. Each time it is */
					/* found, compare the keyword */
					/* following it to the string */
					/* pointed to by last_part. */
  word_start = &aips_path[path_len-1];
  word_end=word_start;
  for(;;word_start--) {
                                        /* If current character is a */
					/* "/", check word following */
					/* it, against last_part. */
                                        /* Break out of the loop if */
					/* the match is successful */
                                        /* Otherwise set up the */
					/* word_end pointer for the */
					/* next component of the */
					/* path. */
    if(*word_start == '/') {
      if(word_end-word_start==last_len-1 &&
	 strncmp(word_start, last_part, last_len) == 0)
	break;
      word_end=word_start-1;
    };
                                        /* No match anywhere in the */
					/* string?  If so, simply */
					/* output the path name */
					/* unmodified. */
    if(word_start <= aips_path) {
      printf("%s\n", aips_path);
      exit(0);
    };
  };
                                        /* Found match!  Concatenate */
					/* the part of $AIPS_VERSION */
					/* before last_part to the */
					/* string following the match */
					/* in aips_path. */
  printf("%.*s%s\n", (int) (last_part-aips_vers), aips_vers,
	 word_start);
  exit(0);
}
