/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 2003                                         */
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
/* Usage: AIPWD                                                       */
/*--------------------------------------------------------------------*/
/* Return the current working directory in a form that is compatible  */
/* with path names in LIBR.DAT. Given that the AIPS root directory    */
/* may be mounted in different places on different hosts, this        */
/* involves replacing the host-specific version prefix of the         */
/* current working directory with the value of the AIPS_VERSION       */
/* environment variable. This is done by searching for a path-name    */
/* component of the current working directory that matches the last   */
/* path-name in AIPS_VERSION. This component should not change from   */
/* one host to the next.                                              */
/* mcs@phobos.caltech.edu  21st July 1995                            */
/*------------------------------------------------------------------- */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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

static char *get_cwd();
static char *find_last_component();
static char *find_tail();


#ifndef PATH_MAX
#define PATH_MAX 1024                  /* Max path-name length */
#endif

int main()
{
  char *cwd;                           /* Current working directory */
  char *aips_version;                  /* The value of $AIPS_VERSION */

  char *last_component;                /* The last path-name component*/
				       /* in aips_version */

  char *cwd_tail;                      /* The part of cwd that follows */
				       /* the version path */

                                       /* Get the current directory */
  cwd = get_cwd();
  if(!cwd)
    return 1;
                                       /* Get the current value of */
				       /* $AIPS_VERSION. */
  aips_version = getenv("AIPS_VERSION");
  if(!aips_version)
    fprintf(stderr, "AIPWD: $AIPS_VERSION is not set.\n");

                                       /* Find the last path-name */
				       /* component of aips_version. */

  last_component = find_last_component(aips_version);

                                       /* Get the part of cwd that */
				       /* follows the last path-name */
				       /* component matching */
				       /* last_component. */

  cwd_tail = find_tail(last_component, cwd);

                                       /* Concatenate the tail of cwd */
				       /* after the aips_version path. */
  if(aips_version && cwd_tail)
    printf("%s%s\n", aips_version, cwd_tail);
  else
    printf("%s\n", cwd);
  free(cwd);
  return 0;
}

/*----------------------------------------------------------------------
 * get_cwd() returns the path-name of the current working directory in
 * a dynamically allocated buffer, or NULL on error.
 *
 *  Output:
 *    return  char *  The path-name of the current working directory,
 *                    or NULL on error.
 */
static char *get_cwd()
{
  char *path = malloc(PATH_MAX);
  if(!path) {
    fprintf(stderr, "AIPWD get_cwd: Insufficient memory.\n");
  } else {
    if(!getcwd(path, PATH_MAX)) {
      perror("AIPWD getcwd()");
      free(path);
      path = NULL;
    };
  };
  return path;
}

/*----------------------------------------------------------------------
 * find_last_component() returns a pointer to the last component of a
 * directory path-name by looking for the last '/' (being careful to
 * ignore a '/' at the end of the string). If no component is found,
 * then NULL is returned.
 *
 *  Input:
 *    path   char *   The path name to search.
 *  Output:
 *    return char *   The pointer to the required component, or NULL if
 *                    not found. Note that the returned string is a
 *                    pointer into path[] and if that path had a
 *                    trailing /, then the returned component will also
 *                    have a trailing /.
 */
static char *find_last_component(path)
     char *path;
{
  char *last_component = NULL;
  int i;
  for(i=strlen(path)-2; i>0; i--) {
    if(path[i] == '/')
      return last_component = path + (i+1);
  };
  return NULL;
}

/*----------------------------------------------------------------------
 * find_tail() compares each path-name component of 'path' with
 * path_component (which may optionally contain a trailing '/').
 * If found, return the part of the string that follows the last such
 * match. If not found, return NULL.
 *
 *  Inputs:
 *    path_component  char *  The path-name component to locate.
 *    path            char *  The path-name to search.
 *  Output:
 *    return          char *  A pointer into path[], pointing at the
 *                            character following the matched path-name
 *                            component, or NULL if not found.
 */
static char *find_tail(path_component, path)
     char *path_component;
     char *path;
{
  if(path_component && path) {
    int word_end = strlen(path) - 1;   /* The start of a component */
    int word_start;                    /* The end of a component */
    int component_len = strlen(path_component);

                                       /* Arrange to ignore trailing / */

    if(path_component[component_len-1] == '/')
      component_len--;
                                       /* Search backwards through */
				       /* path[] for path-name */
				       /* components. */

    for(word_start=word_end-1; word_start >= 0; word_start--) {
      if(path[word_start]=='/') {
	if(word_end - word_start == component_len &&
	 strncmp(path_component, path + (word_start+1), component_len)==0) {
	  return path + (word_end + 1);
	} else {
	  word_end = --word_start;
	};
      };
    };
  };
  return NULL;
}
