#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <AIPSPROTO.H>

#define PMODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#define CMODE (O_WRONLY | O_CREAT | O_TRUNC | O_EXCL)

#if __STDC__
   void zguest_(int *create, int *nmax, int *nfil)
#else
   void zguest_(create, nmax, nfil)
   int *create, *nmax, *nfil;
#endif
/*--------------------------------------------------------------------*/
/*! creates/deletes lock files for guest TV assignments /tmp/GUESTVn  */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997                                          */
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
/*   creates/deletes lock files for guest TV assignments /tmp/GUESTVn */
/*   Inputs:                                                          */
/*      CREATE   I   1 -> create, 0 -> delete                         */
/*      NMAX     I   on create: largest NFIL allowed                  */
/*   In/Out:                                                          */
/*      NFIL     I   file number to delete, file number created       */
/*--------------------------------------------------------------------*/
{
   int i;
   int  lockfile;                      /* lock file file descriptor   */
   char filename[14];                  /* desired file name.          */
/* static char zhex[36] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;    */
   char *zhex = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   char ahex[2] ;
/*--------------------------------------------------------------------*/
                                       /* create                      */
   ahex[1] = '\0';
   if (*create > 0) {
      *nfil = 0;
      for (i = 1; i <= *nmax; i++) {
         ahex[0] = zhex[i];
         sprintf (filename, "/tmp/GUESTV%s", ahex);
                                       /* Create lock file:           */
         if ((lockfile = open (filename, CMODE, PMODE)) == -1) {
            if (errno != EEXIST) i = *nmax + 1;
            }
         else {
            close (lockfile);
            *nfil = i;
            i = *nmax + 1 ;
            }
         }
      }
                                       /* destroy                     */
   else {
      ahex[0] = zhex[*nfil];
      sprintf (filename, "/tmp/GUESTV%s", ahex);
                                       /* delete file                 */
      unlink (filename);
      }
}
