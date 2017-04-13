#define Z_txma2__
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <dirent.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztxma2_(int *flen, char *filspc, int *nmax, int *iext,
      int *nnam, char *names, int *ierr)
#else
   void ztxma2_(flen, filspc, nmax, iext, nnam, names, ierr)
   int *flen, *nmax, *iext, *nnam, *ierr;
   char filspc[], names[];
#endif
/*--------------------------------------------------------------------*/
/*! find all file names matching a given wildcard specification       */
/*# Text                                                              */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2003, 2006                             */
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
/*  Find all file names that match a given wild card specification.   */
/*  Supports <aa>*<bb> only.                                          */
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
/*  MAC version - POSIX_SOURCE not defined                            */
/*  This is Posix-compliant and simple minded: it can handle name*    */
/*  and *name and not much else.  This is enough for AIPS.            */
/*--------------------------------------------------------------------*/
{
   DIR           *dfd;
   struct dirent *dirp;
   char          dspc[MAXAPATH], fspc[MAXAPATH], bspc[MAXAPATH];
   register int  i, j, k, l;
   int           nf, nb, nt;
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
                                        /* Provide for no directory.  */
   if (i == 0)
      i = -1;
                                        /* Collect chars before '*'   */
   nf = 0;
   for (j = i + 1; j < *flen && nf < MAXAPATH && filspc[j] != '*' &&
      filspc[j] != '\0' && filspc[j] != ' '; j++) {
      fspc[nf] = filspc[j];
      nf = nf + 1;
      }
   fspc[nf] = '\0';
                                        /* Collect chars after '*'    */
   nb = 0;
   if (filspc[j] == '*') {
      for (j = j + 1; j < *flen && nb < MAXAPATH && filspc[j] != '\0'
         && filspc[j] != ' '; j++) {
         bspc[nb] = filspc[j];
         nb = nb + 1;
         }
      }
   bspc[nb] = '\0';
                                        /* Open directory             */
   if ((dfd = opendir (dspc)) == NULL)
      *ierr = 2;
                                        /* Collect filenames that     */
                                        /* meet specifications.       */
   else {
      while (((dirp = readdir (dfd)) != NULL) &&  (*nnam < *nmax)) {
                                        /* Directory slot not in use. */
         if (dirp->d_ino == 0)
            continue;
                                        /* Skip self and parent.      */
         if (strcmp (dirp->d_name,".")  == 0 ||
             strcmp (dirp->d_name,"..") == 0)
            continue;
                                        /* Compare forwards.          */
         if (nf > 0) {
            for (i = 0; i < nf && i < MAXAPATH; i++) {
               if (dirp->d_name[i] != fspc[i]) goto skip;
               }
            }
                                        /* Compare backwards.         */
         if (nb > 0) {
                                        /* Get length                 */
            for (nt = 0; nt < MAXAPATH && dirp->d_name[nt] != '\0'
              && dirp->d_name[nt] != ' '; nt++);
            k = 0;
            for (i = nt - 1; i >= 0 && k < nb; i--) {
               if (dirp->d_name[i] != bspc[nb-k-1]) goto skip;
               k = k + 1;
               }
            }
                                        /* Found a match:             */
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
                                        /* Failed to match: skip      */
   skip:
         continue;
         }
                                        /* Close directory.           */
      if (closedir (dfd) != 0)
         *ierr = 2;
      }
                                        /* If more than one, sort     */
                                        /* alphabetically.            */
   if (*nnam > 1)
#if __STDC__
      qsort ((void *) names, (size_t) *nnam, (size_t) 12,
         (int (*)(const void *, const void *)) strcmp);
#else
      qsort (names, *nnam, 12, strcmp);
#endif
   else if (*nnam == 0)
      *ierr = 1;

   return;
}
