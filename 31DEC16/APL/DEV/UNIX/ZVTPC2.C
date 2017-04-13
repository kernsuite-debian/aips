#define Z_vtpc2__
#include <unistd.h>
#include <errno.h>

#if __STDC__
   void zvtpc2_(int *fcb, int *ierr)
#else
   void zvtpc2_(fcb, ierr)
   int *fcb, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! close virtual tape connection to remote, real-tape computer       */
/*# Tape Z2                                                           */
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
/*  ZVTPC2 closes the connection in the Virtual tape program to the   */
/*  remote computer which provides the actual tape device.            */
/*  In/out:                                                           */
/*     fcb   I(*)   File descriptor                                   */
/*  Output:                                                           */
/*     ierr  I      Error: 0 => okay                                  */
/*  BSD 4.2 version                                                   */
/*  deassigns via 'close' certain devices i.e. socket                 */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcberr, Z_mfcb;
   int fd;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Close first buffer file    */
                                        /* descriptor.                */
   fd = *(fcb + Z_fcbfd);
   if (close (fd) != 0) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 1;
      }
                                        /* Close second buffer file   */
                                        /* descriptor.                */
   fd = *(fcb + Z_mfcb + Z_fcbfd);
   if (close (fd) != 0 && *ierr == 0) {
      *(fcb + Z_mfcb + Z_fcberr) = errno;
      *ierr = 1;
      }

   return;
}
