#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_drch2__
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zdrch2_(char pname[MAXPNAME], int *ierr)
#else
   void zdrch2_(pname, ierr)
   int *ierr ;
   char pname[MAXPNAME] ;
#endif
/*--------------------------------------------------------------------*/
/*! check read/write file status                                      */
/*# Z2 IO-basic                                                       */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 2004                                               */
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
/*  Tests read/write status of CA files for disk usage                */
/*  buffered) I/O and shared or exclusive use.                        */
/*  Inputs:                                                           */
/*     pname   H*48   Physical file name (H for Macro, C, ...')       */
/*  Output:                                                           */
/*     ierr    I      Error return code: 0 => no error writable       */
/*                       1 => no error, read only                     */
/*                       2 => all other => no use                     */
/*  Generic UNIX version                                              */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # of FTAB elements per map */
                                        /* file control block         */
   extern int Z_mfcb;
   int llen, xlen, xlnb, jerr, syserr, fd, dupfd;
   off_t lastiob;
   register int i, j;
   char pathname[MAXAPATH];
   struct stat fstatbuf;
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
                                        /* read/write.                */
      fd = open (pathname, O_RDWR);
                                        /* try read only              */
      if (fd == -1) {
         *ierr = 1;
         fd = open (pathname, O_RDONLY);
         }
      if (fd == -1)
         *ierr = 2;
      else
         close (fd);
      }
                                        /* Exit.                      */
   return;
}
