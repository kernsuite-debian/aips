#define Z_tpop2__
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/file.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztpop2_(int *fcb, char pname[MAXPNAME], int *mode, int *ierr)
#else
   ztpop2_(fcb, pname, mode, ierr)
   int *fcb, *mode, *ierr;
   char pname[MAXPNAME];
#endif
/*--------------------------------------------------------------------*/
/*! open a tape device for double-buffer, asymchronous IO             */
/*# IO-basic Tape                                                     */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 2000, 2003, 2008                             */
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
/*  Generic UNIX version - synchronous I/O only.                      */
/*  Modified for real-time VLA: if pathname = ON-LINE, call olopen and*/
/*  use fd < 0.                                                       */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # FTAB elements per map    */
                                        /* file control block         */
   extern int Z_mfcb;
   int llen, xlen, xlnb, jerr, fd, dupfd, ifd, ntry, n8 = 8;
   register int i, j;
   char lognam[MAXPNAME], pathname[MAXAPATH], msgbuf[80];
   float secs = 60.0;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Extract logical device.    */
   for (i = 0; i < MAXPNAME && pname[i] != ':'; i++)
      lognam[i] = pname[i];
   lognam[i] = '\0';
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   ztrlo2_ (&llen, lognam, &xlen, pathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 3;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Open the tape drive        */
                                        /* according to "mode".       */
                                        /* If this is Wes' on-line    */
                                        /* device, use special open   */
                                        /* & use fake file descriptor */
      errno = 0;
      if ((strncmp(pathname, "on-line", 7) == 0) ||
         (strncmp(pathname, "ON-LINE", 7)) == 0) {
         for (ntry=0; ntry<12; ntry++) {
            ifd = -99;
            fd = olopen_(&ifd, pathname, strlen(pathname));
            sprintf (msgbuf, "ZTPOP2: olopen %d", fd);
            zmsgwr_ (msgbuf, &n8);
                                        /* open fails */
            if (fd < -99) {
               sprintf (msgbuf,
                  "ZTPOP2: NOT ON-LINE VERSION - QUIT");
               zmsgwr_ (msgbuf, &n8);
               *ierr = 3;
               ntry = 100;
               }
            else if (fd == -99 || fd >= 0) {
               if (ntry == 11) {
                  sprintf (msgbuf,
                     "ZTPOP2: FAILS TO OPEN ON-LINE - QUIT");
                  zmsgwr_ (msgbuf, &n8);
                  *ierr = 6;
                  }
               else {
                  sprintf (msgbuf,
                     "ZTPOP2: FAILS TO OPEN ON-LINE - RETRY");
                  zmsgwr_ (msgbuf, &n8);
                  zdela2_ (&secs, &jerr) ;
                  }
               }
            else
               ntry = 100;
            }
         }
      else {
         fd = open (pathname, *mode);
         if (fd == -1) {
            *(fcb + Z_fcberr) = errno;
                                        /* Device doesn't exist.      */
            if (errno == ENOENT)
               *ierr = 2;
                                        /* Some other open error.     */
            else
               *ierr = 6;
            }
         }
                                        /* Get duplicate file         */
                                        /* descriptor for 2nd buffer  */
                                        /* (this is only necessary    */
                                        /* for I/O wait servicing).   */
      if (*ierr == 0) {
         *(fcb + Z_fcbfd) = fd;
                                        /* Single buffered            */
         if (fd < 0)
            *(fcb + Z_mfcb + Z_fcbfd) = fd;
         else {
            dupfd = dup(fd);
            if (dupfd == -1) {
                                        /* Store 2nd buffer info in   */
                                        /* 1st buffer FCB for error   */
                                        /* processing (kludge).       */
               *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
               for (i = 0; i < Z_mfcb; i++)
                  *(fcb + i) = *(fcb + Z_mfcb + i);
               *(fcb + Z_fcberr) = errno;
               close (fd);
               *ierr = 6;
               }
            else {
               *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
                                        /* Enable asynchronous mode   */
                                        /* for both buffers here (not */
                                        /* implemented in this        */
                                        /* version).                  */
               }
            }
         }
      }

   return;
}
