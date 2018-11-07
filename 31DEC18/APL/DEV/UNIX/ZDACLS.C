#define Z_dacls__
#include <unistd.h>
#include <errno.h>

#if __STDC__
   void zdacls_(int *fcb, int *map, int *ierr)
#else
   void zdacls_(fcb, map, ierr)
   int *fcb, *map, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! close a disk file                                                 */
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
/*  Close a disk file.                                                */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for open disk file            */
/*     map    I      I/O type: 0 => non-map (single buffered)         */
/*                             1 => map (double buffered)             */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => ok                       */
/*                      1 => error                                    */
/*  Generic UNIX version.                                             */
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
                                        /* Close file.                */
   fd = *(fcb + Z_fcbfd);
   if (close (fd) != 0) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 1;
      }
                                        /* If a map file, free up 2nd */
                                        /* buffer file descriptor too */
   if (*map == 1) {
      fd = *(fcb + Z_mfcb + Z_fcbfd);
      if (close (fd) != 0 && *ierr == 0) {
         *(fcb + Z_mfcb + Z_fcberr) = errno;
         *ierr = 1;
         }
      }
                                        /* Exit.                      */
   return;
}
