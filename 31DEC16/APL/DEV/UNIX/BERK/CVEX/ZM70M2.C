#define  NII 1
#include <sys/ioctl.h>
#include <errno.h>
#define  IIS_MCLEAR _IO(i, 1)           /* IIS masterclear for Convex */

zm70m2_(fcb, ierr)
/*--------------------------------------------------------------------*/
/*! issues a master clear to an IIS Model 70 TV                       */
/*# Z2 IO-TV                                                          */
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
/*  Send a master clear to an IIS model 70 TV device.                 */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for opened TV device          */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => no error                 */
/*                      1 => error                                    */
/*  Convex version.                                                   */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   extern int errno;
   int fd;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZM70O2.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Issue the master clear.    */
   if (ioctl (fd, IIS_MCLEAR) == -1) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 1;
      }

   return;
}
