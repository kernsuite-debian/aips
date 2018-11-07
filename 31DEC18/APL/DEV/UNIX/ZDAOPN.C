#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_daopn__
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zdaopn_(int *fcb, char pname[MAXPNAME],int * map, int *excl,
      int *ierr)
#else
   void zdaopn_(fcb, pname, map, excl, ierr)
   int *fcb, *map, *excl, *ierr ;
   char pname[MAXPNAME] ;
#endif
/*--------------------------------------------------------------------*/
/*! open the specified disk file                                      */
/*# Z2 IO-basic                                                       */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2004, 2011                             */
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
/*  Opens a disk file for map (double buffered) or non-map (single    */
/*  buffered) I/O and shared or exclusive use.                        */
/*  Inputs:                                                           */
/*     pname   H*48   Physical file name (H for Macro, C, ...')        */
/*     map     I      I/O type: 0 => non-map (single buffered)        */
/*                              1 => map (double buffered)            */
/*     excl    I      Usage mode indicator: 0 => shared               */
/*                                          1 => exclusive            */
/*                                         -1 => read-oly             */
/*  Output:                                                           */
/*     fcb     I(*)   File control block for disk file involved       */
/*     ierr    I      Error return code: 0 => no error                */
/*                       2 => file not found                          */
/*                       3 => volume/logical not found                */
/*                       4 => exclusive use requested but not ok      */
/*                       6 => other open error                        */
/*  Generic UNIX version (synchronous I/O only).                      */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # of FTAB elements per map */
                                        /* file control block         */
   extern int Z_mfcb;
   int llen, xlen, xlnb, jerr, syserr, fd, dupfd, n8 = 8;
   off_t lastiob;
   register int i, j;
   char pathname[MAXAPATH], msgbuf[80];
   struct stat fstatbuf;
   float secs = 1.0;
/*--------------------------------------------------------------------*/
   *ierr = 0 ;
                                        /* get full file name         */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   zpath_(&llen, pname, &xlen, pathname, &xlnb, &jerr) ;
   if (jerr != 0) {
      *ierr = 3;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Open the disk file         */
      if (*excl == -1)
         fd = open (pathname, O_RDONLY);
                                        /* read/write.                */
      else
         fd = open (pathname, O_RDWR);
                                        /* try read only              */
      if ((fd == -1) && (errno == EACCES || errno == EROFS) &&
          (*excl == 0))  fd = open (pathname, O_RDONLY);
      if (fd == -1) {
         *(fcb + Z_fcberr) = errno;
                                        /* File doesn't exist.        */
         if (errno == ENOENT)
            *ierr = 2;
                                        /* Some other open error.     */
         else
            *ierr = 6;
         }
      else {
         *(fcb + Z_fcbfd) = fd;
                                        /* Apply non-blocking lock.   */
         zlock_ (&fd, pname, excl, &syserr, ierr);
         if (*ierr != 0) {
            *(fcb + Z_fcberr) = syserr;
            close (fd);
            }
                                        /* Squirrel away file size    */
         else if (fstat (fd, &fstatbuf) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 6;
            close (fd);
            }
         else {
            lastiob = -100;
            if (fstatbuf.st_size <= 0) {
               sprintf (msgbuf, "ZDAOPN: DELAY 1");
               zmsgwr_ (msgbuf, &n8);
               zdela2_ (&secs, &jerr);
               if (fstat (fd, &fstatbuf) == -1) {
                  *(fcb + Z_fcberr) = errno;
                  *ierr = 6;
                  close (fd);
                  return;
                  }
               }
            if (fstatbuf.st_size <= 0) {
               sprintf (msgbuf, "ZDAOPN: DELAY 2");
               zmsgwr_ (msgbuf, &n8);
               zdela2_ (&secs, &jerr);
               if (fstat (fd, &fstatbuf) == -1) {
                  *(fcb + Z_fcberr) = errno;
                  *ierr = 6;
                  close (fd);
                  return;
                  }
               }
            memcpy ((fcb+Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t)) ;
            memcpy ((fcb+Z_fcbsiz+2), &lastiob, sizeof(off_t)) ;
                                        /* If opened for double       */
                                        /* buffered I/O, duplicate    */
                                        /* 1st buffer file control    */
                                        /* block entries in 2nd       */
                                        /* buffer control block.      */
            if (*map == 1) {
                                        /* Get duplicate file         */
                                        /* descriptor for 2nd buffer  */
                                        /* (this is only necessary    */
                                        /* for I/O wait servicing).   */
               if ((dupfd = dup(fd)) == -1) {
                  *(fcb + Z_mfcb + Z_fcberr) = errno;
                  close (fd);
                  *ierr = 6;
                  }
               else {
                  *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
                                        /* Copy file size info.       */
                  memcpy ((fcb+Z_mfcb+Z_fcbsiz), &fstatbuf.st_size,
                      sizeof(off_t)) ;
                  memcpy ((fcb+Z_mfcb+Z_fcbsiz+2), &lastiob,
                      sizeof(off_t)) ;
                                        /* Enable asynchronous mode   */
                                        /* for both buffers here (not */
                                        /* implemented this version)  */
                  }
               }
            }
         }
      }
                                        /* Exit.                      */
   return;
}
