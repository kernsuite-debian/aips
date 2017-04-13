#include <stdio.h>
#include <errno.h>
#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#define MAXPATH 128                     /* Maximum path name length.  */

/*  Convex changed its nomenclature at OS8.1 to be more posix compliant.
 */

#ifdef _POSIX_SOURCE
#include <fcntl.h>
#else
#include <sys/file.h>
#define _FASIO FASIO
#endif /* _POSIX_SOURCE */

ztpop2_(fcb, pname, mode, ierr)
/*--------------------------------------------------------------------*/
/*! open a tape device for double-buffer, asymchronous IO             */
/*# Z2 IO-basic Tape                                                  */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 2000                                         */
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
/*  Open a tape drive for "map" (double buffered, asynchronous) I/O.  */
/*  Inputs:                                                           */
/*     pname   C*48   Physical file name                              */
/*     mode    I      Mode indicator: 0 => read only                  */
/*                                    2 => read/write                 */
/*  Output:                                                           */
/*     fcb     I(*)   File control block for opened tape drive        */
/*     ierr    I      Error return code: 0 => no error                */
/*                             2 => no such logical device            */
/*                             3 => device not found                  */
/*                             6 => other open error                  */
/*  Convex version                                                    */
/*     asynchronous I/O implemented with normal tape support.         */
/*     synchronous I/O with remote tape support.                      */
/*--------------------------------------------------------------------*/
int *fcb, *mode, *ierr;
char pname[MAXPNAME];
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # FTAB elements per map    */
                                        /* file control block         */
   extern int Z_mfcb;
   extern int errno;
   int llen, xlen, xlnb, jerr, fd, dupfd;
   register int i, j;
   char lognam[MAXPNAME], pathname[MAXPATH];
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Extract logical device.    */
   for (i = 0; i < MAXPNAME && pname[i] != ':'; i++)
      lognam[i] = pname[i];
   lognam[i] = '\0';
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXPATH;
   ztrlo2_ (&llen, lognam, &xlen, pathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 3;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Open the tape drive        */
                                        /* according to "mode".       */
      if ((fd = open (pathname, *mode)) == -1) {
         *(fcb + Z_fcberr) = errno;
                                        /* Device doesn't exist.      */
         if (errno == ENOENT)
            *ierr = 2;
                                        /* Some other open error.     */
         else
            *ierr = 6;
         }
                                        /* Get duplicate file         */
                                        /* descriptor for 2nd buffer  */
                                        /* (this is only necessary    */
                                        /* for I/O wait servicing).   */
      else {
         *(fcb + Z_fcbfd) = fd;
         if ((dupfd = dup(fd)) == -1) {
            *(fcb + Z_mfcb + Z_fcberr) = errno;
            *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
            close (fd);
            *ierr = 6;
            }
         else {
            *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
                                        /* Enable asynchronous mode   */
                                        /* for both buffers.          */
            if (fcntl (fd, F_SETFL, _FASIO) == -1) {
               *(fcb + Z_fcberr) = errno;
               *ierr = 6;
               close (fd);
               close (dupfd);
               }
                                        /* This causes an error for   */
                                        /* tape but not for disk???   */
/*
            else if (fcntl (dupfd, F_SETFL, FASIO) == -1) {
               *(fcb + Z_mfcb + Z_fcberr) = errno;
               *ierr = 6;
               close (fd);
               close (dupfd);
               }
*/
            }
         }
      }

   return;
}
