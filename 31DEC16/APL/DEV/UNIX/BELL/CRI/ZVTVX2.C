#include <stdio.h>
#include <errno.h>

zvtvx2_ (fcb, bufsw, bufsr, buffer, ierr)
/*--------------------------------------------------------------------*/
/*! writes/reads to/from server for the client (virtual TV) machine   */
/*# TV-IO                                                             */
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
/*  writes/reads to/from server for the client (virtual TV) machine.  */
/*  ZVTVX2 writes from the Virtual TV program to the remote computer  */
/*  which provides the actual TV device.                              */
/*  Inputs:                                                           */
/*     fcb     I(*)    File descriptor                                */
/*     bufsw   I       Number extra (>5) words to send                */
/*     bufsr   I       Number extra (>5) words to receive back        */
/*  In/out:                                                           */
/*     buffer  I(*)    8-word header plus data buffer: in             */
/*                     FITS-standard 16-bit integer form!!!!!         */
/*  Output:                                                           */
/*     ierr    I       Error: 0 => okay                               */
/*                        3 => IO error                               */
/*                        4 => End of File                            */
/*  UNICOS version.                                                   */
/*--------------------------------------------------------------------*/
int *fcb, *bufsw, *bufsr, *ierr;
char buffer[];
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   extern int errno;
   int nbytes, mbytes, lbytes, sfd, ioff;
   char *iaddr;
/*--------------------------------------------------------------------*/
   errno = 0;
   sfd = *(fcb + Z_fcbfd) ;
   *ierr = 0;
                                        /* write the header + data    */
   ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 of buffer have */
                                        /* been written to the socket */
   for (nbytes = 16; nbytes != 0; nbytes -= mbytes) {
      iaddr = &(buffer[ioff]);
      if ((mbytes = write(sfd, buffer, nbytes)) == -1) {
         perror("ZVTVX2 WRITE ERROR");
         *ierr = 3;
         goto exit;
         }
      ioff += mbytes;
      }
                                        /* Transfer data:             */
   ioff = 16;
                                        /* Loop invariant: elements   */
                                        /* 16 to ioff-1 of buffer     */
                                        /* have been written to the   */
                                        /* socket                     */
   for (nbytes = 2 * (*bufsw) ; nbytes > 0 ; nbytes -= mbytes) {
      lbytes = (nbytes > 1024) ? 1024 : nbytes ;
      iaddr = &(buffer[ioff]) ;
      if ((mbytes = write(sfd, iaddr, lbytes)) == -1) {
         perror("ZVTVX2 WRITE ERROR");
         *ierr = 3;
         goto exit;
         }
      ioff += mbytes;
      }
                                        /* read the header + data     */
   ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 have been read */
                                        /* from the socket            */
   for (nbytes = 2 * (8 + *bufsr) ; nbytes > 0; nbytes -= mbytes) {
      lbytes = (nbytes > 1024) ? 1024 : nbytes ;
      iaddr = &(buffer[ioff]) ;
      if ((mbytes = read(sfd, iaddr, lbytes)) <= 0) {
         if (mbytes == 0)              /* End of file indicator      */
            *ierr = 4;
         else
            *ierr = 3;
         perror("ZVTVX2 READ ERROR");
         goto exit;
         }
      ioff += mbytes;
      }

exit:
   if (*ierr != 0) *(fcb + Z_fcberr) = errno;
   return ;
}

