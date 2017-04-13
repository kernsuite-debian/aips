#include <errno.h>

ztpwa2_(fcb, ierr)
/*--------------------------------------------------------------------*/
/*! wait for read/write from/to a tape device                         */
/*# IO-basic Tape                                                     */
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
/*  Wait for the completion of an asynchronous tape I/O operation and */
/*  returns its completion status and 8-bit byte count.               */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for opened tape drive & buffer*/
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => no error                 */
/*                      3 => I/O error                                */
/*                      4 => end of file                              */
/*  Generic UNIX version - only synchronous I/O.                      */
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
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* If non-zero byte request,  */
                                        /* wait for I/O to complete   */
                                        /* and check status.          */
   if (*(fcb + Z_fcbreq) != 0) {
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTPOP2.    */
      fd = *(fcb + Z_fcbfd);
                                        /* Call to wait function goes */
                                        /* here.                      */

                                        /* Error?                     */
      if (*(fcb + Z_fcbxfr) == -1) {
         *ierr = 3;
         if (*(fcb + Z_fcberr) == EIO) *ierr = 6;
         }
                                        /* End of file?               */
      else if (*(fcb + Z_fcbxfr) == 0) {
         *ierr = 4;
         }
      }

   return;
}
