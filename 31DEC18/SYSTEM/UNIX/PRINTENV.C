#include <stdio.h>
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

/*--------------------------------------------------------------------*/
/* Usage: PRINTENV                                                    */
/*--------------------------------------------------------------------*/
/* A simple program to print the UNIX environment.  This is necessary */
/* because not all flavors of UNIX have "printenv" and the output of  */
/* "set" cannot be redirected (it writes directly to /dev/tty instead */
/* of stdout).  This program is just as efficient as the standard     */
/* Berkeley UNIX "printenv" and perhaps more so.  Furthermore, it has */
/* the built in advantage that it changes to the root directory and   */
/* thereby avoids the possibility that a shell variable like "CWD"    */
/* will also match the environment entry that we are looking for      */
/* (i.e., the only way AIPS uses "printenv" is in programming scripts */
/* where we are trying to match a directory name to a programming     */
/* "logical" defined in the environment).                             */
/*--------------------------------------------------------------------*/
main(argc, argv)
int argc;
char *argv[];
{
                                        /* UNIX environment           */
   extern char **environ;

   register int i;
                                        /* Change to a directory that */
                                        /* is highly unlikely to be   */
                                        /* the current working        */
                                        /* directory.  This is to     */
                                        /* avoid environment entries  */
                                        /* like CWD=... being the     */
                                        /* same as the environment    */
                                        /* entry we are trying to     */
                                        /* match.                     */
   chdir ("/");
                                        /* Print the environment on   */
                                        /* stdout.                    */
   for (i = 0; environ[i] != NULL; i++)
      printf("%s\n",environ[i]);
}
