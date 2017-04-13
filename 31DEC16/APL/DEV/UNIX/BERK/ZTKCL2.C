#include <sgtty.h>
#include <sys/types.h>
#include <sys/stat.h>

ztkcl2_(fcb, ierr)
/*--------------------------------------------------------------------*/
/*! close a Tektronix device                                          */
/*# Graphics                                                          */
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
/*   All rights reserved.                                             */
/*--------------------------------------------------------------------*/
/*  Close a Tektronix device.                                         */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for opened Tektronix device   */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => ok                       */
/*                      1 => error                                    */
/*  Berkeley UNIX version - turns off raw mode                        */
/*  JDE 89/Mar/23: Jonathan D. Eisenhamer, UCLA, added check for      */
/*                 socket.                                            */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   extern int errno;
   int fd;
   struct sgttyb tek;
                                        /* Status of file descriptor. */
   struct stat fdstat;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *(fcb + Z_fcberr) = 0;
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTKOP2.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Turn off RAW mode          */
                                        /* JDE 89/Mar/23:             */
                                        /* Is this is a socket?       */
   fstat (fd, &fdstat );
   if ((fdstat.st_mode & S_IFIFO) == 0) {
      ioctl (fd, TIOCGETP, &tek);
      tek.sg_flags = tek.sg_flags & ~RAW;
      ioctl (fd, TIOCSETP, &tek);
    }
                                        /* Turn off exclusive use.    */
   ioctl (fd, TIOCNXCL, 0);
                                        /* Close Tektronix device.    */
   if (close (fd) == -1) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 1;
      }

   return;
}
