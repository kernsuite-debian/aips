#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>

#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#define MAXPATH 128                     /* Maximum path name length.  */
#define PMODE 0666                      /* Creation permission bits.  */

ztpopd_(lun, fcb, pname, mode, ierr)
/*--------------------------------------------------------------------*/
/*! open a pseudo-tape, sequential disk file for FITS                 */
/*# Z2 Tape FITS                                                      */
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
/*  Open a logical, sequential (pseudo-tape) disk file (exclusive use */
/*  and wait for availability are assumed) for map (double buffered)  */
/*  IO.                                                               */
/*  Inputs:                                                           */
/*     lun     I      Logical unit number (not used)                  */
/*     pname   C*48   Physical file name                              */
/*     mode    I      0 => read only, 2 => read/write                 */
/*  In/Out:                                                           */
/*     fcb     I(*)   File I/O control block                          */
/*  Output:                                                           */
/*     ierr    I      Error return code: 0 = no error                 */
/*                       2 = file not found                           */
/*                       6 = other open errors                        */
/*                       7 = illegal file name (translation fails)    */
/*                       8 = 'write' file already exists              */
/*  UNICOS version (synchronous I/O only).                            */
/*--------------------------------------------------------------------*/
int *lun, *fcb, *mode, *ierr ;
char pname[MAXPNAME] ;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # of FTAB elements per map */
                                        /* file control block         */
   extern int Z_mfcb;
   extern int errno;
   int llen, xlen, xlnb, jerr, syserr, fd, dupfd, excl;
   register int i, j;
   char pathname[MAXPATH];
/*--------------------------------------------------------------------*/
   *ierr = 0 ;
                                        /* get full file name         */
   llen = MAXPNAME;
   xlen = MAXPATH;
   zpath_(&llen, pname, &xlen, pathname, &xlnb, &jerr) ;
   if (jerr != 0) {
      *ierr = 7;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Open the disk file read    */
                                        /* only or write only.        */
      if (*mode == 0) {
                                        /* Open read only.            */
         excl = 0;
         if ((fd = open (pathname, O_RDONLY)) == -1) {
            *(fcb + Z_fcberr) = errno;
                                        /* File doesn't exist.        */
            if (errno == ENOENT)
               *ierr = 2;
                                        /* Some other open error.     */
            else
               *ierr = 6;
            }
         }
      else if (*mode == 2) {
                                        /* Open read/write.           */
         excl = 1;
         if ((fd = open (pathname, O_CREAT | O_EXCL | O_RDWR, PMODE))
            == -1) {
            *(fcb + Z_fcberr) = errno;
                                        /* File already exists.       */
            if (errno == EEXIST)
               *ierr = 8;
                                        /* Some other open error.     */
            else
               *ierr = 6;
            }
         }
      else {
                                        /* Invalid mode indicator.    */
         *ierr = 6;
         }
                                        /* Proceed if all ok so far.  */
      if (*ierr == 0) {
         *(fcb + Z_fcbfd) = fd;
                                        /* Apply non-blocking         */
                                        /* exclusive lock.            */
         zlock_ (&fd, pname, &excl, &syserr, ierr);
         if (*ierr != 0) {
            *(fcb + Z_fcberr) = syserr;
            close (fd);
            }
         else {
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
                                        /* Enable asynchronous mode   */
                                        /* for both buffers here (not */
                                        /* implemented this version)  */
               }
            }
         }
      }
                                        /* Exit.                      */
   return;
}
