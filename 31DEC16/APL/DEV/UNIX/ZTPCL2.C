#define Z_tpcl2__
#include <unistd.h>
#include <errno.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztpcl2_(int *fcb, int *ierr)
#else
   void ztpcl2_(fcb, ierr)
   int *fcb, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! close a tape device                                               */
/*# Tape                                                              */
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
/*  Close a tape drive.                                               */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for opened tape drive         */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => ok                       */
/*                      1 => error                                    */
/*  Generic UNIX version - synchronous I/O only; masks error 1        */
/*  Modified for real-time VLA to call olclose on fd < 0              */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # FTAB elements per map    */
                                        /* file control block         */
   extern int Z_mfcb;
   int fd, i;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Close first buffer file    */
                                        /* descriptor.                */
   fd = *(fcb + Z_fcbfd);
                                        /* If this is Wes' on-line    */
                                        /* device, use special close. */
   if (fd < 0) {
      olclose_(&fd);
      }
   else {
      if (close (fd) != 0) {
                                        /* In at least the case of a  */
                                        /* Solbourne running Sun OS   */
                                        /* 4.0 a spurious errno=1 is  */
                                        /* returned.  Ignore it.      */
         if (errno != 1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 1;
            }
      }
                                        /* Close second buffer file   */
                                        /* descriptor.                */
      fd = *(fcb + Z_mfcb + Z_fcbfd);
      if ((close (fd) != 0) && (*ierr == 0)) {
          if (errno !=1 ) {
             *(fcb + Z_mfcb + Z_fcberr) = errno;
             *ierr = 1;
             }
          }
      }

   return;
}
