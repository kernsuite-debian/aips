#include <stdio.h>
#include <string.h>
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1999                                          */
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

/*--------------------------------------------------------------------*/
/* Usage: REVENV VALUE [-n IGNORE_THIS_ONE [...]]                     */
/*--------------------------------------------------------------------*/
/* Reverse lookup on an environment variable, given its VALUE.        */
/* Will ignore any variables specified on command line via -n.  The   */
/* space between the value and the -n is required, and there are a    */
/* maximum of 512 ignorable names                                     */
/*--------------------------------------------------------------------*/

main(argc, argv)
int argc;
char *argv[];
{
                                        /* UNIX environment           */
   extern char **environ;
   register int i, j, ign=0;
   int null=0, nignore=0;
   char nam[65], *val, foo[512], sep[2], ignore[65][512], *pignore[512],
        findit[65];
/*
 *----------------------------------------------------------------------
 */
   strcpy(sep, "=");
   strcpy(ignore[0], "\0");
   pignore[nignore] = &ignore[0][nignore];
   strcpy(pignore[nignore++], "_");
   if (argc > 1) {
      if (argc > 3) {
	 i=1;
	 while ((strncmp(argv[i], "-n", 2) == 0) && i < argc) {
	    pignore[nignore] = &ignore[0][nignore];
	    strncpy(pignore[nignore++], argv[i+1], 65);
	    i += 2;
	 }
      } else {
	 i=1;
      }
      strncpy(findit, argv[i], sizeof(findit));
                                       /* Change to a directory that */
                                        /* should NOT be cwd          */
      chdir ("/");
                                        /* Go through environ, look   */
                                        /* for rvalue matching argv   */
                                        /* each line is "nam=val"     */
     for (i = 0; environ[i] != NULL; i++) {
                                        /* don't screw up environ!    */
        strncpy(foo, environ[i], sizeof(foo));
                                        /* get lvalue from "="        */
        strncpy (nam, strtok(foo, sep), sizeof(nam));
                                        /* check for ignored stuff    */
	ign=0;
	if (nignore > 0)
	  for (j = 0; j < nignore; j++)
	    if (strcmp(nam, pignore[j]) == 0)
	      ign=1;
                                        /* catch empty vars "VAR="    */
	if (ign == 0) {
	   if ((int) (val = strtok((char *) null, sep)) != null) {
                                        /* compare rvalue to argv[1]  */
	      if (strcmp(val, findit) == 0) {
		 printf("%s\n", nam);
		 break;
	      }
           }
        }
     }
  }
}
