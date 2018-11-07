#define Z_mktmp__
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zmktmp_(int *flen, char filnam[MAXAPATH], int *ierr)
#else
   void zmktmp_(flen, filnam, ierr)
   int *flen, *ierr;
   char filnam[MAXAPATH];
#endif
/*--------------------------------------------------------------------*/
/*! convert a "temporary" file name into a unique name                */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2016                                   */
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
/*  Form a unique, fully qualified, temporary file name.              */
/*  Inputs:                                                           */
/*     flen     I       Length of "filnam"                            */
/*  In/Out:                                                           */
/*     filnam   C*(*)   File name with the extension .XXXXXX, (e.g.,  */
/*                      "ZXLPRT.XXXXXX").  The extension will be      */
/*                      transformed by 'mktemp' to make "filnam" a    */
/*                      unique file name.                             */
/*  Output:                                                           */
/*     ierr     I       Error return code: 0 => no error              */
/*                         1 => inputs wrong                          */
/*                         2 => filenames used up                     */
/*                         3 => filnam too short                      */
/*                         3 => I/O error on access to file           */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   char tmpnam[MAXAPATH+1], cwd[MAXAPATH+1], *pp;
   register int i, j;
   size_t tlen ;
   pid_t  pid ;
   char   cid[10] ;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null-terminated file  */
                                        /* name template out of input;*/
                                        /* Test for "filnam" given as */
                                        /* pathname at same time.     */
   j = 0;
   for (i = 0; i<*flen && filnam[i] != ' ' && filnam[i] != '\0'; i++) {
      tmpnam[i] = filnam[i];
                                        /* Is this a pathname?        */
      if (tmpnam[i] == '/') j = 1;
      }
   tmpnam[i] = '\0';
                                        /* If not already a pathname, */
                                        /* make it one.               */
   if (j == 0) {
                                        /* Get current working        */
                                        /* directory.                 */
      tlen = MAXAPATH+1;
      pp = getcwd (cwd, tlen);
                                        /* Fill in directory name.    */
      for (i = 0; i < *flen && i < MAXAPATH && cwd[i] != ' ' &&
         cwd[i] != '\0' && cwd[i] != '\n'; i++)
         filnam[i] = cwd[i];
                                        /* Append "/" and file name.  */
      filnam[i] = '/';
      i = i + 1;
      for (j = 0; j < MAXAPATH && i+j < *flen && tmpnam[j] != ' ' &&
         tmpnam[j] != '\0'; j++)
         filnam[i+j] = tmpnam[j];
      filnam[i+j] = '\0';
      }
                                        /* Just copy pathname "tmpnam"*/
                                        /* to "filnam".               */
   else {
      sprintf (filnam, "%s", tmpnam);
      }
                                        /* Get unique file name.      */
   tlen = strlen (filnam) ;
   if (tlen < 6)
      *ierr = 3;

   else {
      pid = getpid ( );
      sprintf (cid, "%9d", pid);
      for (i = 1; i < 6; i++) {
         if ((filnam[tlen-i] = cid[9-i]) == ' ')
            filnam[tlen-i] = '0';
         }
      filnam[tlen-6] = 'a';

      *ierr = 2;
      for (i=0; i < 26; i++) {
          errno = 0;
          if (access (filnam, F_OK) != 0) {
             if (errno == ENOENT)
                *ierr = 0;
             else
                *ierr = 4;
             break;
             }
          filnam[tlen-6]++ ;
          }
      }
                                        /* blank fill                 */
   for (i = 1; i < *flen && filnam[i] != '\0'; i++) {
      j = i ;
      }
   for (i = j + 1; i < *flen; i++) {
      filnam[i] = ' ' ;
      }

   return;
}
