#define Z_rena2__
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zrena2_(char oldnam[MAXPNAME], char newnam[MAXPNAME],
      int *syserr, int *ierr)
#else
   void zrena2_(oldnam, newnam, syserr, ierr)
   char oldnam[MAXPNAME], newnam[MAXPNAME];
   int *syserr, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! rename a file                                                     */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                         */
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
/*  Renames a file.                                                   */
/*  Inputs:                                                           */
/*     oldnam   C*48   Old physical file name                         */
/*     newnam   C*48   New physical file name                         */
/*  Output:                                                           */
/*     syserr   I      System error code                              */
/*     ierr     I      Error return code: 0 => no error               */
/*                        2 => old file not found                     */
/*                        3 => device not found                       */
/*                        4 => old file busy                          */
/*                        6 => new name already exists                */
/*                        7 => other                                  */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   register int i, j;
   int llen, xlen, xlnb, jerr, lockfd = -99, excl = 1;
   char oldpathname[MAXAPATH], newpathname[MAXAPATH];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
                                        /* Build full old path name.  */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   zpath_ (&llen, oldnam, &xlen, oldpathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 3;
      }
   else {
      oldpathname[xlnb] = '\0';
      if ((lockfd = open (oldpathname, 2)) == -1) {
                                        /* Old path name doesn't exist*/
         if (errno == ENOENT)
            *ierr = 2;
                                        /* Some other open error.     */
         else {
           *syserr = errno;
            *ierr = 7;
            }
         }
                                        /* Apply non-blocking         */
                                        /* exclusive lock to old      */
                                        /* pathname.                  */
      else {
         zlock_ (&lockfd, oldnam, &excl, syserr, ierr);
         if (*ierr != 0) {
            if (*ierr != 4) *ierr = 7;
            }
                                        /* Build new full path name   */
         else {
            llen = MAXPNAME;
            xlen = MAXAPATH;
            zpath_ (&llen, newnam, &xlen, newpathname, &xlnb, &jerr);
            if (jerr != 0) {
               *ierr = 3;
               }
            else {
               newpathname[xlnb] = '\0';
                                        /* Test path name existence.  */
               if (access (newpathname, 0) == 0) {
                  *ierr = 6;
                  }
                                        /* Rename by forming link of  */
                                        /* new to old path name, then */
                                        /* unlink old path name.      */
               else {
                  if (link (oldpathname, newpathname) != 0) {
                    *syserr = errno;
                     *ierr = 7;
                     }
                  else if (unlink (oldpathname) != 0) {
                    *syserr = errno;
                     *ierr = 7;
                     }
                  }
               }
            }
                                        /* Close and unlock file.     */
         close (lockfd);
         }
      }

   return;
}
