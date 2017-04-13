#include <strings.h>
#include <sys/types.h>
#include <sys/dir.h>

#define MAXPATH 128                     /* Maximum path name length.  */

ztxma2_(flen, filspc, nmax, iext, nnam, names, ierr)
/*--------------------------------------------------------------------*/
/*! find all file names matching a given wildcard specification       */
/*# Z2 Text                                                           */
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
/*  Find all file names that match a given wild card specification.   */
/*  Inputs:                                                           */
/*     flen     I          Length in characters of "filspc"           */
/*     filspc   C*(flen)   File specification                         */
/*     nmax     I          Maximum number of names                    */
/*     iext     I          Extension disposition indicator:           */
/*                            0 => remove extension                   */
/*                            1 => retain extension                   */
/*  Output:                                                           */
/*     nnam     I          Number of matching names found             */
/*     names    H*12(nmax) Array of matching names (H for Macro, C...)*/
/*     ierr     I          Error return code:                         */
/*                            0 => no error                           */
/*                            1 => no matching names found            */
/*                            2 => error opening or closing directory */
/*  4.2bsd UNIX version.                                              */
/*--------------------------------------------------------------------*/
int *flen, *nmax, *iext, *nnam, *ierr;
char filspc[], names[];
/*--------------------------------------------------------------------*/
{
   DIR *dirp, *opendir();
   struct dirent *dp;
   int n7 = 7;
   register int i, j, k, l;
   char dspc[MAXPATH], fspc[MAXPATH], msgbuf[81];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *nnam = 0;
                                        /* Determine the beginning of */
                                        /* the file name (end of      */
                                        /* directory name) as "i".    */
   i = 0;
   for (j = 0; j < *flen; j++)
      if (filspc[j] == '/') i = j;
                                        /* Extract directory name.    */
   for (j = 0; j < i; j++)
      dspc[j] = filspc[j];
   dspc[i] = '\0';
                                        /* Build regular expression   */
                                        /* from file specification.   */
   fspc[0] = '^';
   k = 1;
   for (j = i + 1; j < *flen && filspc[j] != '\0' && filspc[j] != ' ';
      j++) {
      if (filspc[j] == '*') {
         fspc[k] = '.';
         fspc[k+1] = '*';
         k = k + 2;
         }
      else if (filspc[j] == '.') {
         fspc[k] = '\\';
         fspc[k+1] = '.';
         k = k + 2;
         }
      else {
         fspc[k] = filspc[j];
         k = k + 1;
         }
      }
   fspc[k] = '$';
   fspc[k+1] = '\0';
                                        /* Compile regular expression */
                                        /* for comparisons (see       */
                                        /* REGEX(3)).                 */
   if (re_comp (fspc) != 0) {
      sprintf (msgbuf, "ZTXMA2: REGULAR EXPRESSION COMPILATION FAILED");
      zmsgwr_ (msgbuf, &n7);
      sprintf (msgbuf, "ZTXMA2: %s", re_comp (fspc));
      zmsgwr_ (msgbuf, &n7);
      *ierr = 1;
      }
   else {
                                        /* Open directory.            */
      if ((dirp = opendir (dspc)) == NULL) {
         *ierr = 3;
         }
                                        /* Collect file names that    */
                                        /* meet the specification.    */
      else {
         while ((dp = readdir (dirp)) != NULL && *nnam < *nmax) {
                                        /* Directory slot not in use? */
            if (dp->d_ino == 0)
               continue;
                                        /* Skip self and parent.      */
            if (strcmp (dp->d_name, ".")  == 0 ||
               strcmp (dp->d_name, "..") == 0)
               continue;
                                        /* Regular expression         */
                                        /* comparison (see REGEX(3)). */
            if (re_exec (dp->d_name) != 1)
               continue;
                                        /* find end & last . of name  */
         k = 0;
         l = 0;
         for (i = 0; i < 12 && dirp->d_name[i] != '\0'; i++) {
             k = i;
             if (dirp->d_name[i] == '.') l = i;
             }
         if (l == 0) l = k + 1;
                                        /* Strip extension.           */
         if (*iext == 0) k = l - 1;
                                        /* copy name, blank fill      */
         for (i = 0; i < 12; i++) {
            j = (*nnam * 12) + i;
            if (i > k)
               names[j] = ' ';
            else
               names[j] = dirp->d_name[i];
            }
                                        /* Increment number found.    */
            *nnam = *nnam + 1;
            continue;
            }
                                        /* Close directory.           */
         closedir (dirp);
         }
                                        /* If more than one, sort     */
                                        /* alphabetically.            */
      if (*nnam > 1)
         qsort (names, *nnam, 12, strcmp);
      else if (*nnam == 0)
         *ierr = 1;
      }

   return;
}
