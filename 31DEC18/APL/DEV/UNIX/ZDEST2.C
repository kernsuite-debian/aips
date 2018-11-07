#define Z_dest2__
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zdest2_(char pname[MAXPNAME], int *syserr, int *ierr)
#else
   void zdest2_(pname, syserr, ierr)
   char pname[MAXPNAME];
   int *syserr, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! delete a disk file                                                */
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
/*  Destroy (i.e., delete) a file.  The file should already be closed */
/*  Inputs:                                                           */
/*     pname   H*48   Physical file name (H for Macro...              */
/*  Output:                                                           */
/*     syserr  I      System-level error code (for ZERROR)            */
/*     ierr    I      Error return code: 0 => no error                */
/*                       1 => file not found (no message)             */
/*                       2 => device not found                        */
/*                       3 => file in use                             */
/*                       4 => other                                   */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   register int i, j;
   int llen, xlen, xlnb, jerr, lockfd = -99, excl = 1 ;
   char pathname[MAXAPATH];
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Translate name             */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   zpath_ (&llen, pname, &xlen, pathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 2;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Open the disk file         */
                                        /* read/write.                */
      if ((lockfd = open (pathname, 2)) == -1) {
                                        /* File doesn't exist.        */
         if (errno == ENOENT)
            *ierr = 1;
                                        /* Some other open error.     */
         else {
            *syserr = errno;
            *ierr = 4;
            }
         }
                                        /* Apply non-blocking         */
                                        /* exclusive lock.            */
      else {
         zlock_ (&lockfd, pname, &excl, syserr, ierr);
         if (*ierr != 0) {
                                        /* File in use.               */
            if (*ierr == 4) {
               *ierr = 3;
               }
                                        /* Other file locking error.  */
            else {
               *ierr = 4;
               }
            }
         else if ( unlink (pathname) != 0) {
            *syserr = errno;
            *ierr = 4;
            }
                                        /* Close and unlock file.     */
         close (lockfd);
         }
      }
                                        /* Exit.                      */
   return;
}
